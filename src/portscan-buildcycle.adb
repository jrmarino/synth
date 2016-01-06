--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Direct_IO;
with GNAT.OS_Lib;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Processes;

package body PortScan.Buildcycle is

   package ACA renames Ada.Calendar.Arithmetic;
   package ACF renames Ada.Calendar.Formatting;
   package OSL renames GNAT.OS_Lib;
   package STR renames Util.Streams;


   ---------------------
   --  build_package  --
   ---------------------
   function build_package (id : builders; sequence_id : port_id) return Boolean
   is
      R : Boolean;
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      if uselog then
         initialize_log (id);
      end if;
      for phase in phases'Range loop
         trackers (id).phase := phase;
         case phase is
            when check_sanity | fetch | checksum | extract | patch |
                 configure | build | stage | pkg_package =>
               R := exec_phase_generic (id, phase2str (phase));

            when pkg_depends | fetch_depends | extract_depends |
                 patch_depends | build_depends | lib_depends | run_depends =>
               R := exec_phase_depends (id, phase2str (phase));

            when install_mtree | install | check_plist =>
               if testing then
                  R := exec_phase_generic (id, phase2str (phase));
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id);
               end if;
         end case;
         exit when R = False;
      end loop;
      if uselog then
         finalize_log (id);
      end if;
      return R;
   end build_package;


   ----------------------
   --  initialize_log  --
   ----------------------
   procedure initialize_log (id : builders)
   is
      FA    : access TIO.File_Type;
      H_ENV : constant String := "Environment";
      H_OPT : constant String := "Options";
      H_CFG : constant String := "/etc/make.conf";
      UNAME : constant String := JT.USS (uname_mrv);
      BENV  : constant String := get_environment (id);
      COPTS : constant String := get_options_configuration (id);
      MCONF : constant String := dump_make_conf (id);
      PTVAR : JT.Text         := get_port_variables (id);
   begin
      trackers (id).dynlink.Clear;
      trackers (id).head_time := CAL.Clock;
      declare
         log_path : constant String := log_name (trackers (id).seq_id);
      begin
         if AD.Exists (log_path) then
            AD.Delete_File (log_path);
         end if;
         TIO.Create (File => trackers (id).log_handle,
                     Mode => TIO.Out_File,
                     Name => log_path);
         FA := trackers (id).log_handle'Access;
      exception
         when error : others =>
            raise cycle_log_error
              with "failed to create log " & log_path;
      end;

      TIO.Put_Line (FA.all, "=> Building " &
                      get_catport (all_ports (trackers (id).seq_id)));
      TIO.Put_Line (FA.all, "Started : " & timestamp (trackers (id).head_time));
      TIO.Put      (FA.all, "Platform: " & UNAME);
      TIO.Put_Line (FA.all, LAT.LF & log_section (H_ENV, True));
      TIO.Put      (FA.all, BENV);
      TIO.Put_Line (FA.all, log_section (H_ENV, False) & LAT.LF);
      TIO.Put_Line (FA.all, log_section (H_OPT, True));
      TIO.Put      (FA.all, COPTS);
      TIO.Put_Line (FA.all, log_section (H_OPT, False) & LAT.LF);

      dump_port_variables (id => id, content => PTVAR);

      TIO.Put_Line (FA.all, log_section (H_CFG, True));
      TIO.Put      (FA.all, MCONF);
      TIO.Put_Line (FA.all, log_section (H_CFG, False) & LAT.LF);

   end initialize_log;


   --------------------
   --  finalize_log  --
   --------------------
   procedure finalize_log (id : builders) is
   begin
      trackers (id).tail_time := CAL.Clock;
      TIO.Put_Line (trackers (id).log_handle,
                    "Finished: " & timestamp (trackers (id).tail_time));
      TIO.Put_Line (trackers (id).log_handle,
                    log_duration (start => trackers (id).head_time,
                                  stop  => trackers (id).tail_time));
      TIO.Close (trackers (id).log_handle);
   end finalize_log;


   --------------------
   --  log_duration  --
   --------------------
   function log_duration (start, stop : CAL.Time) return String
   is
      raw : JT.Text := JT.SUS ("Duration:");
      diff_days : ACA.Day_Count;
      diff_secs : Duration;
      leap_secs : ACA.Leap_Seconds_Count;
      use type ACA.Day_Count;
   begin
      ACA.Difference (Left    => stop,
                      Right   => start,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);
      if diff_days > 0 then
         if diff_days = 1 then
            JT.SU.Append (raw, " 1 day and" &
                            ACF.Image (Elapsed_Time => diff_secs));
         else
            JT.SU.Append (raw, diff_days'Img & " days and" &
                            ACF.Image (Elapsed_Time => diff_secs));
         end if;
      else
         JT.SU.Append (raw, " " & ACF.Image (Elapsed_Time => diff_secs));
      end if;
      return JT.USS (raw);
   end log_duration;


   ------------------------
   --  elapsed_HH_MM_SS  --
   ------------------------
   function elapsed_HH_MM_SS (start, stop : CAL.Time) return String
   is
      diff_days : ACA.Day_Count;
      diff_secs : Duration;
      leap_secs : ACA.Leap_Seconds_Count;
      secs_per_hour : constant Integer := 3600;
      total_hours   : Integer;
      total_minutes : Integer;
      work_hours    : Integer;
      work_seconds  : Integer;
      use type ACA.Day_Count;
   begin
      ACA.Difference (Left    => stop,
                      Right   => start,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);
      --  Seems the ACF image is shit, so let's roll our own.  If more than
      --  100 hours, change format to "HHH:MM.M"

      work_seconds := Integer (diff_secs);
      total_hours  := work_seconds / secs_per_hour;
      total_hours  := total_hours + Integer (diff_days) * 24;

      if total_hours < 24 then
         if work_seconds < 0 then
            return "--:--:--";
         else
            work_seconds := work_seconds - (total_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := work_seconds - (total_minutes * 60);
            return
              JT.zeropad (total_hours, 2) & LAT.Colon &
              JT.zeropad (total_minutes, 2) & LAT.Colon &
              JT.zeropad (work_seconds, 2);
         end if;
      elsif total_hours < 100 then
         if work_seconds < 0 then
            return JT.zeropad (total_hours, 2) & ":00:00";
         else
            work_hours := work_seconds / secs_per_hour;
            work_seconds := work_seconds - (work_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := work_seconds - (total_minutes * 60);
            return
              JT.zeropad (total_hours, 2) & LAT.Colon &
              JT.zeropad (total_minutes, 2) & LAT.Colon &
              JT.zeropad (work_seconds, 2);
         end if;
      else
         if work_seconds < 0 then
            return JT.zeropad (total_hours, 3) & ":00.0";
         else
            work_hours := work_seconds / secs_per_hour;
            work_seconds := work_seconds - (work_hours * secs_per_hour);
            total_minutes := work_seconds / 60;
            work_seconds := (work_seconds - (total_minutes * 60)) * 10 / 60;
            return
              JT.zeropad (total_hours, 3) & LAT.Colon &
              JT.zeropad (total_minutes, 2) & '.' &
              JT.int2str (work_seconds);
         end if;
      end if;
   end elapsed_HH_MM_SS;


   -------------------
   --  elapsed_now  --
   -------------------
   function elapsed_now return String is
   begin
      return elapsed_HH_MM_SS (start => start_time, stop => CAL.Clock);
   end elapsed_now;


   -----------------
   --  timestamp  --
   -----------------
   function timestamp (hack : CAL.Time) return String
   is
      function MON   (num : CAL.Month_Number) return String;
      function WKDAY (day : ACF.Day_Name) return String;

      function MON (num : CAL.Month_Number) return String is
      begin
         case num is
            when 1 => return "JAN";
            when 2 => return "FEB";
            when 3 => return "MAR";
            when 4 => return "APR";
            when 5 => return "MAY";
            when 6 => return "JUN";
            when 7 => return "JUL";
            when 8 => return "AUG";
            when 9 => return "SEP";
            when 10 => return "OCT";
            when 11 => return "NOV";
            when 12 => return "DEC";
         end case;
      end MON;
      function WKDAY (day : ACF.Day_Name) return String is
      begin
         case day is
            when ACF.Monday    => return "Monday";
            when ACF.Tuesday   => return "Tuesday";
            when ACF.Wednesday => return "Wednesday";
            when ACF.Thursday  => return "Thursday";
            when ACF.Friday    => return "Friday";
            when ACF.Saturday  => return "Saturday";
            when ACF.Sunday    => return "Sunday";
         end case;
      end WKDAY;
   begin
      return WKDAY (ACF.Day_Of_Week (hack)) & "," & CAL.Day (hack)'Img & " " &
        MON (CAL.Month (hack)) & CAL.Year (hack)'Img & " at" &
        ACF.Image (hack)(11 .. 19) & " UTC";
   end timestamp;


   -----------------------------
   -- generic_system_command  --
   -----------------------------
   function generic_system_command (command : String) return JT.Text
   is
      pipe    : aliased STR.Pipes.Pipe_Stream;
      buffer  : STR.Buffered.Buffered_Stream;
      content : JT.Text;
      status  : Integer;
   begin
      pipe.Open (Command => command, Mode => Util.Processes.READ_ALL);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;
      status := pipe.Get_Exit_Status;
      if status /= 0 then
         raise cycle_cmd_error with "cmd: " & command &
           " (return code =" & status'Img & ")";
      end if;
      return content;
   end generic_system_command;


   ---------------------
   --  set_uname_mrv  --
   ---------------------
   procedure set_uname_mrv
   is
      command : constant String := "/usr/bin/uname -mrv";
   begin
      uname_mrv := generic_system_command (command);
   exception
      when others =>
         uname_mrv := JT.SUS (discerr);
   end set_uname_mrv;


   ----------------
   --  get_root  --
   ----------------
   function get_root (id : builders) return String
   is
      id_image     : constant String := Integer (id)'Img;
      suffix       : String := "/SL00";
   begin
      if id < 10 then
         suffix (5) := id_image (2);
      else
         suffix (4 .. 5) := id_image (2 .. 3);
      end if;
      return JT.USS (PM.configuration.dir_buildbase) & suffix;
   end get_root;


   -----------------------
   --  get_environment  --
   -----------------------
   function get_environment (id : builders) return String
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override;
   begin
      return JT.USS (generic_system_command (command));
   exception
      when others =>
         return discerr;
   end get_environment;


   ---------------------------------
   --  get_options_configuration  --
   ---------------------------------
   function get_options_configuration (id : builders) return String
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override &
        "/usr/bin/make -C /xports/" &
        get_catport (all_ports (trackers (id).seq_id)) &
        " showconfig";
   begin
      return JT.USS (generic_system_command (command));
   exception
      when others =>
         return discerr;
   end get_options_configuration;


   ------------------------
   --  split_collection  --
   ------------------------
   function split_collection (line : JT.Text; title : String) return String
   is
      meat    : JT.Text;
      waiting : Boolean := True;
      quoted  : Boolean := False;
      keepit  : Boolean;
      counter : Natural := 0;
      meatlen : Natural := 0;
      linelen : Natural := JT.SU.Length (line);
      onechar : String (1 .. 1);
      meatstr : String (1 .. linelen);
   begin
      loop
         counter := counter + 1;
         exit when counter > linelen;
         keepit  := True;
         onechar := JT.SU.Slice (Source => line,
                                 Low    => counter,
                                 High   => counter);
         if onechar (1) = LAT.Space then
            if waiting then
               keepit := False;
            else
               if not quoted then
                  --  name-pair ended, reset
                  waiting := True;
                  quoted  := False;
                  onechar (1) := LAT.LF;
               end if;
            end if;
         else
            waiting := False;
            if onechar (1) = LAT.Quotation then
               quoted := not quoted;
            end if;
         end if;
         if keepit then
            meatlen := meatlen + 1;
            meatstr (meatlen) := onechar (1);
         end if;
      end loop;
      return log_section (title, True) & LAT.LF &
        meatstr (1 .. meatlen) & LAT.LF &
        log_section (title, False) & LAT.LF;
   end split_collection;


   --------------------------
   --  get_port_variables  --
   --------------------------
   function get_port_variables (id : builders) return JT.Text
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override &
        "/usr/bin/make -C /xports/" &
        get_catport (all_ports (trackers (id).seq_id)) &
        " -VCONFIGURE_ENV -VCONFIGURE_ARGS -VMAKE_ENV -VMAKE_ARGS" &
        " -VPLIST_SUB -VSUB_LIST";
   begin
      return generic_system_command (command);
   exception
      when others =>
         return JT.SUS (discerr);
   end get_port_variables;


   ---------------------------
   --  dump_port_variables  --
   ---------------------------
   procedure dump_port_variables (id : builders; content : JT.Text)
   is
      LA      : access TIO.File_Type := trackers (id).log_handle'Access;
      topline : JT.Text;
      concopy : JT.Text := content;
      type result_range is range 1 .. 6;
   begin
      for k in result_range loop
         JT.nextline (lineblock => concopy, firstline => topline);
         case k is
            when 1 => TIO.Put_Line
                 (LA.all, split_collection (topline, "CONFIGURE_ENV"));
            when 2 => TIO.Put_Line
                 (LA.all, split_collection (topline, "CONFIGURE_ARGS"));
            when 3 => TIO.Put_Line
                 (LA.all, split_collection (topline, "MAKE_ENV"));
            when 4 => TIO.Put_Line
                 (LA.all, split_collection (topline, "MAKE_ARGS"));
            when 5 => TIO.Put_Line
                 (LA.all, split_collection (topline, "PLIST_SUB"));
            when 6 => TIO.Put_Line
                 (LA.all, split_collection (topline, "SUB_LIST"));
         end case;
      end loop;
   end dump_port_variables;


   ----------------
   --  log_name  --
   ----------------
   function log_name (sid : port_id) return String
   is
      catport : constant String := get_catport (all_ports (sid));
   begin
      return JT.USS (PM.configuration.dir_logs) & "/" &
        JT.part_1 (catport) & "___" & JT.part_2 (catport) & ".log";
   end log_name;


   -----------------
   --  dump_file  --
   -----------------
   function  dump_file (filename : String) return String
   is
      File_Size : Natural := Natural (AD.Size (filename));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => filename);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);
      return String (Contents);
   end dump_file;


   ----------------------
   --  dump_make_conf  --
   ----------------------
   function dump_make_conf (id : builders) return String
   is
      root     : constant String := get_root (id);
      filename : constant String := root & "/etc/make.conf";
   begin
      return dump_file (filename);
   end dump_make_conf;


   ------------------
   --  initialize  --
   ------------------
   procedure initialize (test_mode : Boolean)
   is
      logdir : constant String := JT.USS (PM.configuration.dir_logs);
   begin
      set_uname_mrv;
      testing := test_mode;
      if not AD.Exists (logdir) then
         AD.Create_Path (New_Directory => logdir);
      end if;
   exception
         when error : others =>
            raise cycle_log_error
              with "failed to create " & logdir;
   end initialize;


   -------------------
   --  log_section  --
   -------------------
   function log_section (title : String; header : Boolean) return String
   is
      first_part : constant String := "[ " & title;
   begin
      if header then
         return first_part & " HEAD ]";
      else
         return first_part & " TAIL ]";
      end if;
   end log_section;


   ---------------------
   --  log_phase_end  --
   ---------------------
   procedure log_phase_end (id : builders)
   is
      dash : constant String := "=========================";
   begin
      TIO.Put_Line (trackers (id).log_handle, dash & dash & dash & LAT.LF);
   end log_phase_end;


   -----------------------
   --  log_phase_begin  --
   -----------------------
   procedure log_phase_begin (phase : String; id : builders)
   is
      plast  : constant Natural := 10 + phase'Length;
      dash   : constant String := "========================";
      middle :          String := "< phase :                 >";
   begin
      middle (11 .. plast) := phase;
      TIO.Put_Line (trackers (id).log_handle, dash & middle & dash);
   end log_phase_begin;


   --------------------------
   --  exec_phase_generic  --
   --------------------------
   function exec_phase_generic (id : builders; phase : String) return Boolean is
   begin
      if testing then
         return exec_phase (id => id, phase => phase,
                            phaseenv => "DEVELOPER=1");
      else
         return exec_phase (id => id, phase => phase);
      end if;
   end exec_phase_generic;


   --------------------------
   --  exec_phase_depends  --
   --------------------------
   function exec_phase_depends (id : builders; phase : String) return Boolean
   is
      phaseenv : String := "USE_PACKAGE_DEPENDS_ONLY=1";
   begin
      return exec_phase (id => id, phase => phase, phaseenv => phaseenv,
                         depends_phase => True);
   end exec_phase_depends;


   ----------------------------
   --  exec_phase_deinstall  --
   ----------------------------
   function exec_phase_deinstall (id : builders) return Boolean
   is
      phase : constant String := "deinstall";
   begin
      --  This is only run during "testing" so assume that.
      if uselog then
         log_phase_begin (phase, id);
         log_linked_libraries (id);
      end if;
      return exec_phase (id => id, phase => phase, phaseenv => "DEVELOPER=1",
                         skip_header => True);
   end exec_phase_deinstall;


   -----------------------
   --  generic_execute  --
   -----------------------
   function generic_execute (id : builders; command : String) return Boolean
   is
      Args        : OSL.Argument_List_Access;
      Exit_Status : Integer;
      synthexec   : constant String := host_localbase & "/libexec/synthexec";
      truecommand : constant String := synthexec & " " &
                             log_name (trackers (id).seq_id) & " " & command;
   begin
      Args := OSL.Argument_String_To_List (truecommand);
      Exit_Status := OSL.Spawn (Program_Name => Args (Args'First).all,
                                Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      return Exit_Status = 0;
   end generic_execute;


   ------------------
   --  exec_phase  --
   ------------------
   function exec_phase (id : builders; phase : String; phaseenv : String := "";
                        depends_phase : Boolean := False;
                        skip_header   : Boolean := False)
                        return Boolean
   is
      root       : constant String := get_root (id);
      port_flags : String := " NO_DEPENDS=yes ";
      dev_flags  : String := " DEVELOPER_MODE=yes ";
      pid        : port_id := trackers (id).seq_id;
      catport    : constant String := get_catport (all_ports (pid));
      result     : Boolean;
   begin
      if testing or else depends_phase
      then
         port_flags := (others => LAT.Space);
      else
         dev_flags := (others => LAT.Space);
      end if;

      --  Nasty, we have to switch open and close the log file for each
      --  phase because we have to switch between File_Type and File
      --  Descriptors.  I can't find a safe way to get the File Descriptor
      --  out of the File type.

      if uselog then
         if not skip_header then
            log_phase_begin (phase, id);
         end if;
         TIO.Close (trackers (id).log_handle);
      end if;

      declare
           command : constant String := chroot & root & environment_override &
           phaseenv & dev_flags & port_flags &
           "/usr/bin/make -C /xports/" & catport & " " & phase;
      begin
         result := generic_execute (id, command);
      end;

      --  Reopen the log.  I guess we can leave off the exception check
      --  since it's been passing before

      if uselog then
         TIO.Open (File => trackers (id).log_handle,
                   Mode => TIO.Append_File,
                   Name => log_name (trackers (id).seq_id));
         log_phase_end (id);
      end if;

      return result;
   end exec_phase;


   --------------------
   --  install_pkg8  --
   --------------------
   function install_pkg8 (id : builders) return Boolean
   is
      root    : constant String := get_root (id);
      taropts : constant String := "-C / */pkg-static";
      command : constant String := chroot & root & environment_override &
        "/usr/bin/tar -xf /packages/Latest/pkg.txz " & taropts;
   begin
      return generic_execute (id, command);
   end install_pkg8;


   ------------------------
   --  build_repository  --
   ------------------------
   function build_repository (id : builders) return Boolean
   is
      root    : constant String := get_root (id);
      command : constant String := chroot & root & environment_override &
        host_localbase & "/sbin/pkg-static repo /packages";
   begin
      if not install_pkg8 (id) then
         TIO.Put_Line ("Failed to install pkg-static in builder" & id'Img);
         return False;
      end if;
      return generic_execute (id, command);
   end build_repository;


   --------------------------
   --  dynamically_linked  --
   --------------------------
   function dynamically_linked (base, filename : String) return Boolean
   is
      command : String := chroot & base & " /usr/bin/file -b " &
        "-e ascii -e encoding -e tar -e compress " & filename;
      comres  : JT.Text;
   begin
      comres := generic_system_command (command);
      return JT.contains (comres, "dynamically linked");
   exception
      when others =>
         return False;
   end dynamically_linked;


   ----------------------------
   --  log_linked_libraries  --
   ----------------------------
   procedure stack_linked_libraries (id : builders; base, filename : String)
   is
      command : String := chroot & base & " /usr/bin/objdump -p " & filename;
      comres  : JT.Text;
      topline : JT.Text;
      crlen1  : Natural;
      crlen2  : Natural;
   begin
      comres := generic_system_command (command);
      crlen1 := JT.SU.Length (comres);
      loop
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if not JT.IsBlank (topline) then
            if JT.contains (topline, "NEEDED") then
               if not trackers (id).dynlink.Contains (topline) then
                  trackers (id).dynlink.Append (topline);
               end if;
            end if;
         end if;
      end loop;
   exception
         --  the command result was not zero, so it was an expected format
         --  or static file.  Just skip it.  (Should never happen)
      when bad_result : others => null;
   end stack_linked_libraries;


   ----------------------------
   --  log_linked_libraries  --
   ----------------------------
   procedure log_linked_libraries (id : builders)
   is
      procedure log_dump (cursor : string_crate.Cursor);

      comres  : JT.Text;
      topline : JT.Text;
      crlen1  : Natural;
      crlen2  : Natural;
      pkgfile : constant String := JT.USS
                         (all_ports (trackers (id).seq_id).package_name);
      pkgname : constant String := pkgfile (1 .. pkgfile'Last - 4);
      root    : constant String := get_root (id);
      command : constant String := chroot & root & " " &
        host_localbase & "/sbin/pkg query %Fp " & pkgname;

      procedure log_dump (cursor : string_crate.Cursor) is
      begin
         TIO.Put_Line (trackers (id).log_handle,
                       JT.USS (string_crate.Element (Position => cursor)));
      end log_dump;
   begin
      TIO.Put_Line (trackers (id).log_handle,
                    "=> Checking shared library dependencies");

      comres := generic_system_command (command);
      crlen1 := JT.SU.Length (comres);
      loop
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if dynamically_linked (root, JT.USS (topline)) then
            stack_linked_libraries (id, root, JT.USS (topline));
         end if;
      end loop;
      trackers (id).dynlink.Iterate (log_dump'Access);
   exception
      when others => null;
   end log_linked_libraries;


   ------------------------
   --  external_command  --
   ------------------------
   function external_command (command : String) return Boolean
   is
      Args        : OSL.Argument_List_Access;
      Exit_Status : Integer;
   begin
      Args := OSL.Argument_String_To_List (command);
      Exit_Status := OSL.Spawn (Program_Name => Args (Args'First).all,
                                Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      return Exit_Status = 0;
   end external_command;


   ----------------------------
   --  environment_override  --
   ----------------------------
   function environment_override return String
   is
      PATH : constant String := "PATH=/sbin:/bin:/usr/sbin:/usr/bin:" &
                                "/usr/local/sbin:/usr/local/bin ";
      TERM : constant String := "TERM=cons25 ";
      USER : constant String := "USER=root ";
      HOME : constant String := "HOME=/root ";
      LANG : constant String := "LANG=C ";
   begin
      return " /usr/bin/env -i " & USER & HOME & LANG & TERM & PATH;
   end environment_override;


   ---------------------
   --  set_log_lines  --
   ---------------------
   procedure set_log_lines (id : builders)
   is
      log_path : constant String := log_name (trackers (id).seq_id);
      command  : constant String := "/usr/bin/wc -l " & log_path;
      comres   : JT.Text;
   begin
      if not uselog then
         trackers (id).loglines := 0;
         return;
      end if;
      comres := JT.trim (generic_system_command (command));
      declare
         numtext : constant String :=
           JT.part_1 (S => JT.USS (comres), separator => " ");
      begin
         trackers (id).loglines := Natural'Value (numtext);
      end;
   exception
      when others => null;  -- just skip this cycle
   end set_log_lines;


   -----------------
   --  phase2str  --
   -----------------
   function phase2str (phase : phases) return String is
   begin
      case phase is
         when check_sanity    => return "check-sanity";
         when pkg_depends     => return "pkg-depends";
         when fetch_depends   => return "fetch-depends";
         when fetch           => return "fetch";
         when checksum        => return "checksum";
         when extract_depends => return "extract-depends";
         when extract         => return "checksum";
         when patch_depends   => return "patch-depends";
         when patch           => return "patch";
         when build_depends   => return "build-depends";
         when lib_depends     => return "lib-depends";
         when configure       => return "configure";
         when build           => return "build";
         when run_depends     => return "run-depends";
         when stage           => return "stage";
         when pkg_package     => return "package";
         when install_mtree   => return "install-mtree";
         when install         => return "install";
         when deinstall       => return "deinstall";
         when check_plist     => return "check-plist";
      end case;
   end phase2str;


   -----------------------
   --  format_loglines  --
   -----------------------
   function format_loglines (numlines : Natural) return String
   is
   begin
      if numlines < 10000000 then      --  10 million
         return JT.int2str (numlines);
      end if;
      declare
         kilo    : constant Natural := numlines / 1000;
         kilotxt : constant String  := JT.int2str (kilo);
      begin
         if numlines < 100000000 then      --  100 million
            return kilotxt (1 .. 2) & "." & kilotxt (3 .. 5) & 'M';
         elsif numlines < 1000000000 then  --  1 billion
            return kilotxt (1 .. 3) & "." & kilotxt (3 .. 4) & 'M';
         else
            return kilotxt (1 .. 4) & "." & kilotxt (3 .. 3) & 'M';
         end if;
      end;
   end format_loglines;


   ----------------------
   --  builder_status  --
   ----------------------
   function builder_status (id : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec
   is
      result   : Display.builder_rec;
   begin
      --  123456789 123456789 123456789 123456789 1234
      --   SL  elapsed   phase              lines  origin
      --   01  00:00:00  extract-depends  9999999  www/joe

      result.id       := id;
      result.slavid   := JT.zeropad (Natural (id), 2);
      result.LLines   := (others => ' ');
      result.phase    := (others => ' ');
      result.origin   := (others => ' ');
      result.shutdown := False;
      result.idle     := False;

      if shutdown then
         --  Overrides "idle" if both Shutdown and Idle are True
         result.Elapsed  := "Shutdown";
         result.shutdown := True;
         return result;
      end if;
      if idle then
         result.Elapsed := "Idle    ";
         result.idle    := True;
         return result;
      end if;

      declare
         phasestr : constant String := phase2str (trackers (id).phase);
         catport  : constant String :=
           get_catport (all_ports (trackers (id).seq_id));
         numlines : constant String := format_loglines (trackers (id).loglines);
         linehead : constant Natural := 8 - numlines'Length;
      begin
         result.Elapsed := elapsed_HH_MM_SS (start => trackers (id).head_time,
                                             stop  => CAL.Clock);
         result.LLines (linehead .. 7) := numlines;
         result.phase  (1 .. phasestr'Length) := phasestr;

         if catport'Length > 37 then
            result.origin (1 .. 36) := catport (1 .. 36);
            result.origin (37) := LAT.Asterisk;
         else
            result.origin (1 .. catport'Length) := catport;
         end if;
      end;
      return result;
   end builder_status;


   ---------------------
   --  elapsed_build  --
   ---------------------
   function elapsed_build (id : builders) return String is
   begin
      return elapsed_HH_MM_SS (start => trackers (id).head_time,
                               stop => trackers (id).tail_time);
   end elapsed_build;


   -----------------------------
   --  get_packages_per_hour  --
   -----------------------------
   function get_packages_per_hour (packages_done : Natural;
                                   from_when : CAL.Time)
                                   return Natural
   is
      diff_days    : ACA.Day_Count;
      diff_secs    : Duration;
      leap_secs    : ACA.Leap_Seconds_Count;
      result       : Natural;
      rightnow     : CAL.Time := CAL.Clock;
      work_seconds : Integer;
      work_days    : Integer;
      use type ACA.Day_Count;
   begin
      if packages_done = 0 then
         return 0;
      end if;
      ACA.Difference (Left    => rightnow,
                      Right   => from_when,
                      Days    => diff_days,
                      Seconds => diff_secs,
                      Leap_Seconds => leap_secs);

      work_seconds := Integer (diff_secs);
      work_days    := Integer (diff_days);
      work_seconds := work_seconds + (work_days * 3600 * 24);

      if work_seconds < 0 then
         --  should be impossible to get here.
         return 0;
      end if;
      result := packages_done * 3600;
      result := result / work_seconds;
      return result;
   exception
      when others => return 0;
   end get_packages_per_hour;


end PortScan.Buildcycle;
