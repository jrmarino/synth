--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Direct_IO;
with Unix;

package body PortScan.Buildcycle is

   package ACA renames Ada.Calendar.Arithmetic;
   package ACF renames Ada.Calendar.Formatting;


   ---------------------
   --  build_package  --
   ---------------------
   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : phases  := fetch) return Boolean
   is
      R : Boolean;
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      if uselog then
         if not initialize_log (id) then
            finalize_log (id);
            return False;
         end if;
      end if;
      for phase in phases'Range loop
         trackers (id).phase := phase;
         case phase is
            when check_sanity | fetch | checksum | extract | patch |
                 pkg_package =>
               R := exec_phase_generic (id, phase);

            when pkg_depends | fetch_depends | extract_depends |
                 patch_depends | build_depends | lib_depends | run_depends =>
               R := exec_phase_depends (id, phase);

            when configure =>
               if testing then
                  if lock_localbase then
                     set_localbase_protection (id, True);
                  end if;
                  mark_file_system (id, "preconfig");
               end if;
               R := exec_phase_generic (id, phase);

            when build =>
               R := exec_phase_build (id);

            when stage =>
               if testing then
                  mark_file_system (id, "prestage");
               end if;
               R := exec_phase_generic (id, phase);

            when install_mtree | install | check_plist =>
               if testing then
                  R := exec_phase_generic (id, phase);
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id);
               end if;
         end case;
         exit when R = False;
         exit when interactive and then interphase = phase;
      end loop;
      if uselog then
         finalize_log (id);
      end if;
      if interactive then
         interact_with_builder (id);
      end if;
      return R;
   end build_package;


   ----------------------
   --  initialize_log  --
   ----------------------
   function initialize_log (id : builders) return Boolean
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
      if BENV = discerr then
         TIO.Put_Line (FA.all, LAT.LF & "Environment definition failed, " &
                         "aborting entire build");
         return False;
      end if;
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
      return True;

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
      content : JT.Text;
      status  : Integer;
   begin
      content := Unix.piped_command (command, status);
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
   procedure initialize (test_mode : Boolean; jail_env : JT.Text) is
   begin
      set_uname_mrv;
      testing := test_mode;
      lock_localbase := testing and then Unix.env_variable_defined ("LOCK");
      slave_env := jail_env;
      declare
         logdir : constant String := JT.USS (PM.configuration.dir_logs);
      begin
         if not AD.Exists (logdir) then
            AD.Create_Path (New_Directory => logdir);
         end if;
      exception
         when error : others =>
            raise cycle_log_error
              with "failed to create " & logdir;
      end;
      obtain_custom_environment;
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
   function exec_phase_generic (id : builders; phase : phases) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
   begin
      return exec_phase (id => id, phase => phase, time_limit => time_limit);
   end exec_phase_generic;


   --------------------------
   --  exec_phase_depends  --
   --------------------------
   function exec_phase_depends (id : builders; phase : phases) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
      phaseenv : String := "USE_PACKAGE_DEPENDS_ONLY=1";
   begin
      return exec_phase (id => id, phase => phase, phaseenv => phaseenv,
                         time_limit => time_limit,
                         depends_phase => True);
   end exec_phase_depends;


   ------------------------
   --  exec_phase_build  --
   ------------------------
   function exec_phase_build (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (build);
      passed : Boolean;
   begin
      passed := exec_phase (id          => id,
                            phase       => build,
                            time_limit  => time_limit,
                            skip_header => False,
                            skip_footer => True);
      if testing and then passed then
         if lock_localbase then
            set_localbase_protection (id, False);
         end if;
         passed := detect_leftovers_and_MIA
           (id, "preconfig", "between port configure and build");
      end if;
      if uselog then
         log_phase_end (id);
      end if;
      return passed;
   end exec_phase_build;


   ----------------------------
   --  exec_phase_deinstall  --
   ----------------------------
   function exec_phase_deinstall (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (deinstall);
      result     : Boolean;
   begin
      --  This is only run during "testing" so assume that.
      if uselog then
         log_phase_begin (phase2str (deinstall), id);
         log_linked_libraries (id);
      end if;
      result := exec_phase (id          => id,
                            phase       => deinstall,
                            time_limit  => time_limit,
                            skip_header => True,
                            skip_footer => True);
      if not result then
         if uselog then
            log_phase_end (id);
         end if;
         return False;
      end if;
      if uselog then
         result := detect_leftovers_and_MIA
           (id, "prestage", "between staging and package deinstallation");
         log_phase_end (id);
      end if;
      return result;
   end exec_phase_deinstall;


   -----------------------
   --  generic_execute  --
   -----------------------
   function generic_execute (id : builders; command : String;
                             dogbite : out Boolean;
                             time_limit : execution_limit) return Boolean
   is
      subtype time_cycle is execution_limit range 1 .. time_limit;
      subtype one_minute is Positive range 1 .. 230;  --  lose 10 in rounding
      type dim_watchdog is array (time_cycle) of Natural;
      use type Unix.process_exit;
      watchdog    : dim_watchdog;
      squirrel    : time_cycle := time_cycle'First;
      cycle_done  : Boolean := False;
      pid         : Unix.pid_t;
      status      : Unix.process_exit;
      lock_lines  : Natural;
      quartersec  : one_minute := one_minute'First;
      hangmonitor : Boolean := PM.configuration.enable_watchdog;
      synthexec   : constant String := host_localbase & "/libexec/synthexec";
      truecommand : constant String := synthexec & " " &
                             log_name (trackers (id).seq_id) & " " & command;
   begin
      dogbite := False;
      watchdog (squirrel) := trackers (id).loglines;

      pid := Unix.launch_process (truecommand);
      if Unix.fork_failed (pid) then
         return False;
      end if;
      loop
         delay 0.25;
         if quartersec = one_minute'Last then
            quartersec := one_minute'First;
            --  increment squirrel
            if squirrel = time_cycle'Last then
               squirrel := time_cycle'First;
               cycle_done := True;
            else
               squirrel := squirrel + 1;
            end if;
            if hangmonitor then
               lock_lines := trackers (id).loglines;
               if cycle_done then
                  if watchdog (squirrel) = lock_lines then
                     --  Log hasn't advanced in a full cycle so bail out
                     dogbite := True;
                     Unix.kill_process_tree (process_group => pid);
                     delay 5.0;  --  Give some time for error to write to log
                     return False;
                  end if;
               end if;
               watchdog (squirrel) := lock_lines;
            end if;
         else
            quartersec := quartersec + 1;
         end if;
         status := Unix.process_status (pid);
         if status = Unix.exited_normally then
            return True;
         end if;
         if status = Unix.exited_with_error then
            return False;
         end if;
      end loop;
   end generic_execute;


   ------------------
   --  exec_phase  --
   ------------------
   function exec_phase (id : builders; phase : phases;
                        time_limit    : execution_limit;
                        phaseenv      : String := "";
                        depends_phase : Boolean := False;
                        skip_header   : Boolean := False;
                        skip_footer   : Boolean := False)
                        return Boolean
   is
      root       : constant String := get_root (id);
      port_flags : String := " NO_DEPENDS=yes ";
      pid        : port_id := trackers (id).seq_id;
      catport    : constant String := get_catport (all_ports (pid));
      result     : Boolean;
      timed_out  : Boolean;
   begin
      if testing or else depends_phase
      then
         port_flags := (others => LAT.Space);
      end if;

      --  Nasty, we have to switch open and close the log file for each
      --  phase because we have to switch between File_Type and File
      --  Descriptors.  I can't find a safe way to get the File Descriptor
      --  out of the File type.

      if uselog then
         if not skip_header then
            log_phase_begin (phase2str (phase), id);
         end if;
         TIO.Close (trackers (id).log_handle);
      end if;

      declare
           command : constant String := chroot & root & environment_override &
           phaseenv & port_flags &
           "/usr/bin/make -C /xports/" & catport & " " & phase2str (phase);
      begin
         result := generic_execute (id, command, timed_out, time_limit);
      end;

      --  Reopen the log.  I guess we can leave off the exception check
      --  since it's been passing before

      if uselog then
         TIO.Open (File => trackers (id).log_handle,
                   Mode => TIO.Append_File,
                   Name => log_name (trackers (id).seq_id));
         if timed_out then
            TIO.Put_Line (trackers (id).log_handle,
                          "###  Watchdog killed runaway process!  ###");
         end if;
         if not skip_footer then
            log_phase_end (id);
         end if;
      end if;

      return result;
   end exec_phase;


   --------------------------
   --  dynamically_linked  --
   --------------------------
   function dynamically_linked (base, filename : String) return Boolean
   is
      command : String := chroot & base & " /usr/bin/file -b " &
        "-e ascii -e encoding -e tar -e compress " &
        LAT.Quotation & filename & LAT.Quotation;
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


   ----------------------------
   --  environment_override  --
   ----------------------------
   function environment_override (enable_tty : Boolean := False) return String
   is
      function set_terminal (enable_tty : Boolean) return String;
      function set_terminal (enable_tty : Boolean) return String is
      begin
         if enable_tty then
            return "TERM=cons25 ";
         end if;
         return "TERM=dumb ";
      end set_terminal;

      PATH : constant String := "PATH=/sbin:/bin:/usr/sbin:/usr/bin:" &
                                "/usr/local/sbin:/usr/local/bin ";
      TERM : constant String := set_terminal (enable_tty);
      USER : constant String := "USER=root ";
      HOME : constant String := "HOME=/root ";
      LANG : constant String := "LANG=C ";
      CENV : constant String := JT.USS (customenv);
      JENV : constant String := JT.USS (slave_env);
   begin
      return " /usr/bin/env -i " &
        USER & HOME & LANG & TERM & PATH & JENV & CENV;
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
         when extract         => return "extract";
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


   -------------------------------
   --  max_time_without_output  --
   -------------------------------
   function max_time_without_output (phase : phases) return execution_limit
   is
      base : Integer;
   begin
      case phase is
         when check_sanity     => base := 1;
         when pkg_depends      => base := 3;
         when fetch_depends    => base := 3;
         when fetch | checksum => return 480;  --  8 hours
         when extract_depends  => base := 3;
         when extract          => base := 20;
         when patch_depends    => base := 3;
         when patch            => base := 3;
         when build_depends    => base := 5;
         when lib_depends      => base := 5;
         when configure        => base := 15;
         when build            => base := 20;
         when run_depends      => base := 15;   --  octave-forge is driver
         when stage            => base := 20;   --  desire 15 but too many rogue builders-in-stage
         when check_plist      => base := 3;
         when pkg_package      => base := 80;
         when install_mtree    => base := 3;
         when install          => base := 10;
         when deinstall        => base := 10;
      end case;
      declare
         multiplier_x10 : constant Positive := timeout_multiplier_x10;
      begin
         return execution_limit (base * multiplier_x10 / 10);
      end;
   end max_time_without_output;


   ------------------------
   --  mark_file_system  --
   ------------------------
   procedure mark_file_system (id : builders; action : String)
   is
      function attributes (action : String) return String;
      function attributes (action : String) return String
      is
         core : constant String := "uid,gid,mode,md5digest";
      begin
         if action = "preconfig" then
            return core & ",time";
         else
            return core;
         end if;
      end attributes;

      path_mm  : String := JT.USS (PM.configuration.dir_buildbase) & "/Base";
      path_sm  : String := JT.USS (PM.configuration.dir_buildbase) & "/SL" &
                           JT.zeropad (Natural (id), 2);
      mtfile   : constant String := path_mm & "/mtree." & action & ".exclude";
      command  : constant String := "/usr/sbin/mtree -X " & mtfile &
                          " -cn -k " & attributes (action) & " -p " & path_sm;
      filename : constant String := path_sm & "/tmp/mtree." & action;
      result   : JT.Text;
      resfile  : TIO.File_Type;
   begin
      result := generic_system_command (command);
      TIO.Create (File => resfile, Mode => TIO.Out_File, Name => filename);
      TIO.Put (resfile, JT.USS (result));
      TIO.Close (resfile);
   exception
      when cycle_cmd_error => null;
      when others =>
         if TIO.Is_Open (resfile) then
            TIO.Close (resfile);
         end if;
   end mark_file_system;


   --------------------------------
   --  detect_leftovers_and_MIA  --
   --------------------------------
   function detect_leftovers_and_MIA (id : builders; action : String;
                                      description : String) return Boolean
   is
      package crate is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => JT.Text,
                                       "="          => JT.SU."=");
      package sorter is new crate.Generic_Sorting ("<" => JT.SU."<");
      function  ignore_modifications return Boolean;
      procedure print (cursor : crate.Cursor);
      procedure close_active_modifications;
      path_mm  : String := JT.USS (PM.configuration.dir_buildbase) & "/Base";
      path_sm  : String := JT.USS (PM.configuration.dir_buildbase) & "/SL" &
                           JT.zeropad (Natural (id), 2);
      mtfile   : constant String := path_mm & "/mtree." & action & ".exclude";
      filename : constant String := path_sm & "/tmp/mtree." & action;
      command  : constant String := "/usr/sbin/mtree -X " & mtfile & " -f " &
                                    filename & " -p " & path_sm;
      status    : Integer;
      comres    : JT.Text;
      topline   : JT.Text;
      crlen1    : Natural;
      crlen2    : Natural;
      toplen    : Natural;
      skiprest  : Boolean;
      passed    : Boolean := True;
      activemod : Boolean := False;
      modport   : JT.Text := JT.blank;
      reasons   : JT.Text := JT.blank;
      leftover  : crate.Vector;
      missing   : crate.Vector;
      changed   : crate.Vector;

      function ignore_modifications return Boolean
      is
         --  Some modifications need to be ignored
         --  A) */ls-R
         --     #ls-R files from texmf are often regenerated
         --  B) share/xml/catalog.ports
         --     # xmlcatmgr is constantly updating catalog.ports, ignore
         --  C) share/octave/octave_packages
         --     # Octave packages database, blank lines can be inserted
         --     # between pre-install and post-deinstall
         --  D) info/dir | */info/dir
         --  E) lib/gio/modules/giomodule.cache
         --     # gio modules cache could be modified for any gio modules
         --  F) etc/gconf/gconf.xml.defaults/%gconf-tree*.xml
         --     # gconftool-2 --makefile-uninstall-rule is unpredictable
         --  G) %%PEARDIR%%/.depdb | %%PEARDIR%%/.filemap
         --     # The is pear database cache
         --  H) "." with timestamp modification
         --     # this happens when ./tmp or ./var is used, which is legal
         filename : constant String := JT.USS (modport);
         fnlen    : constant Natural := filename'Length;
      begin
         if filename = "usr/local/share/xml/catalog.ports" or else
           filename = "usr/local/share/octave/octave_packages" or else
           filename = "usr/local/info/dir" or else
           filename = "usr/local/lib/gio/modules/giomodule.cache" or else
           filename = "usr/local/share/pear/.depdb" or else
           filename = "usr/local/share/pear/.filemap"
         then
            return True;
         end if;
         if filename = "." and then JT.equivalent (reasons, "modification") then
            return True;
         end if;
         if fnlen > 17 and then filename (1 .. 10) = "usr/local/"
         then
            if filename (fnlen - 4 .. fnlen) = "/ls-R" or else
              filename (fnlen - 8 .. fnlen) = "/info/dir"
            then
               return True;
            end if;
         end if;
         if fnlen > 56 and then filename (1 .. 39) =
           "usr/local/etc/gconf/gconf.xml.defaults/" and then
           filename (fnlen - 3 .. fnlen) = ".xml"
         then
            if JT.contains (filename, "/%gconf-tree") then
               return True;
            end if;
         end if;
         return False;
      end ignore_modifications;

      procedure close_active_modifications is
      begin
         if activemod and then not ignore_modifications then
            JT.SU.Append (modport, " [ ");
            JT.SU.Append (modport, reasons);
            JT.SU.Append (modport, " ]");
            if not changed.Contains (modport) then
               changed.Append (modport);
            end if;
         end if;
         activemod := False;
         reasons := JT.blank;
         modport := JT.blank;
      end close_active_modifications;

      procedure print (cursor : crate.Cursor)
      is
         dossier : constant String := JT.USS (crate.Element (cursor));
      begin
         TIO.Put_Line (trackers (id).log_handle, LAT.HT & dossier);
      end print;

   begin
      --  we can't use generic_system_command because exit code /= 0 normally
      comres := Unix.piped_command (command, status);
      crlen1 := JT.SU.Length (comres);
      loop
         skiprest := False;
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         toplen := JT.SU.Length (topline);
         if not skiprest and then JT.SU.Length (topline) > 6 then
            declare
               sx : constant Natural := toplen - 5;
               caboose  : constant String := JT.SU.Slice (topline, sx, toplen);
               filename : JT.Text := JT.SUS (JT.SU.Slice (topline, 1, sx - 1));
            begin
               if caboose = " extra" then
                  close_active_modifications;
                  if not leftover.Contains (filename) then
                     leftover.Append (filename);
                  end if;
                  skiprest := True;
               end if;
            end;
         end if;
         if not skiprest and then JT.SU.Length (topline) > 7 then
            declare
               canopy   : constant String := JT.SU.Slice (topline, 1, 7);
               filename : JT.Text := JT.SUS (JT.SU.Slice (topline, 8, toplen));
            begin
               if canopy = "extra: " then
                  close_active_modifications;
                  if not leftover.Contains (filename) then
                     leftover.Append (filename);
                  end if;
                  skiprest := True;
               end if;
            end;
         end if;
         if not skiprest and then JT.SU.Length (topline) > 10 then
            declare
               sx : constant Natural := toplen - 7;
               caboose  : constant String := JT.SU.Slice (topline, sx, toplen);
               filename : JT.Text := JT.SUS (JT.SU.Slice (topline, 3, sx - 1));
            begin
               if caboose = " missing" then
                  close_active_modifications;
                  if not missing.Contains (filename) then
                     missing.Append (filename);
                  end if;
                  skiprest := True;
               end if;
            end;
         end if;
         if not skiprest then
            declare
               line   : constant String := JT.USS (topline);
               blank8 : constant String := "        ";
               sx     : constant Natural := toplen - 7;
            begin
               if toplen > 5 and then line (1) = LAT.HT then
                  --  reason, but only valid if modification is active
                  if activemod then
                     if JT.IsBlank (reasons) then
                        reasons := JT.SUS (JT.part_1 (line (2 .. toplen), " "));
                     else
                        JT.SU.Append (reasons, " | ");
                        JT.SU.Append (reasons, JT.part_1
                                      (line (2 .. toplen), " "));
                     end if;
                  end if;
                  skiprest := True;
               end if;
               if not skiprest and then line (toplen) = LAT.Colon then
                  close_active_modifications;
                  activemod := True;
                  modport := JT.SUS (line (1 .. toplen - 1));
                  skiprest := True;
               end if;
               if not skiprest and then
                 JT.SU.Slice (topline, sx, toplen) = " changed"
               then
                  close_active_modifications;
                  activemod := True;
                  modport := JT.SUS (line (1 .. toplen - 8));
                  skiprest := True;
               end if;
            end;
         end if;
      end loop;
      close_active_modifications;
      sorter.Sort (Container => changed);
      sorter.Sort (Container => missing);
      sorter.Sort (Container => leftover);

      TIO.Put_Line (trackers (id).log_handle, LAT.LF & "=> Checking for " &
                      "system changes " & description);
      if not leftover.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF &
                      "   Left over files/directories:");
         leftover.Iterate (Process => print'Access);
      end if;
      if not missing.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF &
                       "   Missing files/directories:");
         missing.Iterate (Process => print'Access);
      end if;
      if not changed.Is_Empty then
         passed := False;
         TIO.Put_Line (trackers (id).log_handle, LAT.LF &
                       "   Modified files/directories:");
         changed.Iterate (Process => print'Access);
      end if;
      if passed then
         TIO.Put_Line (trackers (id).log_handle, "Everything is fine.");
      end if;
      return passed;
   end detect_leftovers_and_MIA;


   -----------------------------
   --  interact_with_builder  --
   -----------------------------
   procedure interact_with_builder (id : builders)
   is
      root      : constant String := get_root (id);
      command   : constant String := chroot & root &
                  environment_override (enable_tty => True) & "/bin/tcsh";
      result    : Boolean;
   begin
      TIO.Put_Line ("Entering interactive test mode at the builder root " &
                      "directory.");
      TIO.Put_Line ("Type 'exit' when done exploring.");
      result := Unix.external_command (command);
   end interact_with_builder;


   ------------------------
   --  valid_test_phase  --
   ------------------------
   function valid_test_phase (afterphase : String) return phases is
   begin
      if afterphase = "extract" then
         return extract;
      elsif afterphase = "patch" then
         return patch;
      elsif afterphase = "configure" then
         return configure;
      elsif afterphase = "build" then
         return build;
      elsif afterphase = "stage" then
         return check_plist;
      elsif afterphase = "install" then
         return install;
      elsif afterphase = "deinstall" then
         return deinstall;
      else
         return check_sanity;
      end if;
   end valid_test_phase;


   ---------------------------------
   --  obtain_custom_environment  --
   ---------------------------------
   procedure obtain_custom_environment
   is
      target_name : constant String := PM.synth_confdir & "/" &
                    JT.USS (PM.configuration.profile) & "-environment";
      fragment : TIO.File_Type;
   begin
      customenv := JT.blank;
      if AD.Exists (target_name) then
         TIO.Open (File => fragment, Mode => TIO.In_File, Name => target_name);
         while not TIO.End_Of_File (fragment) loop
            declare
               Line : String := TIO.Get_Line (fragment);
            begin
               if JT.contains (Line, "=") then
                  JT.SU.Append (customenv, JT.trim (Line) & " ");
               end if;
            end;
         end loop;
         TIO.Close (fragment);
      end if;
   exception
      when others =>
         if TIO.Is_Open (fragment) then
            TIO.Close (fragment);
         end if;
   end obtain_custom_environment;


   --------------------------------
   --  set_localbase_protection  --
   --------------------------------
   procedure set_localbase_protection (id : builders; lock : Boolean)
   is
      procedure remount (readonly : Boolean);
      procedure dismount;

      smount        : constant String := get_root (id);
      lbase         : constant String := "/usr/local";
      slave_local   : constant String := smount & "_localbase";


      procedure remount (readonly : Boolean)
      is
         cmd_freebsd   : String := "/sbin/mount_nullfs ";
         cmd_dragonfly : String := "/sbin/mount_null ";
         points        : String := slave_local & " " & smount & lbase;
         options       : String := "-o ro ";
         cmd           : JT.Text;
         cmd_output    : JT.Text;
      begin
         if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
            cmd := JT.SUS (cmd_freebsd);
         else
            cmd := JT.SUS (cmd_dragonfly);
         end if;
         if readonly then
            JT.SU.Append (cmd, options);
         end if;
         JT.SU.Append (cmd, points);

         if not Unix.piped_mute_command (JT.USS (cmd), cmd_output) then
            if uselog then
               TIO.Put_Line (trackers (id).log_handle,
                             "command failed: " & JT.USS (cmd));
               if not JT.IsBlank (cmd_output) then
                  TIO.Put_Line (trackers (id).log_handle, JT.USS (cmd_output));
               end if;
            end if;
         end if;
      end remount;

      procedure dismount
      is
         cmd_unmount : constant String := "/sbin/umount " & smount & lbase;
         cmd_output  : JT.Text;
      begin
         if not Unix.piped_mute_command (cmd_unmount, cmd_output) then
            if uselog then
               TIO.Put_Line (trackers (id).log_handle,
                             "command failed: " & cmd_unmount);
               if not JT.IsBlank (cmd_output) then
                  TIO.Put_Line (trackers (id).log_handle, JT.USS (cmd_output));
               end if;
            end if;
         end if;
      end dismount;

   begin
      if lock then
         dismount;
         remount (readonly => True);
      else
         dismount;
         remount (readonly => False);
      end if;
   end set_localbase_protection;


   ------------------------------
   --  timeout_multiplier_x10  --
   ------------------------------
   function timeout_multiplier_x10 return Positive
   is
      command : String := "/usr/bin/env LANG=C /sbin/sysctl vm.loadavg";
      comres  : JT.Text;
   begin
      comres := generic_system_command (command);
      declare
         highend  : constant Natural := JT.SU.Length (comres) - 1;
         stripped : constant String := JT.SU.Slice (Source => comres, Low => 15, High => highend);
         section2 : constant String := JT.part_2 (stripped, " ");
         average5 : constant String := JT.part_1 (section2, " ");
         avefloat : Float;
      begin
         avefloat := Float'Value (average5) / Float (number_cores);
         if avefloat <= 1.0 then
            return 10;
         else
            return Integer (avefloat * 10.0);
         end if;
      exception
         when others => return 10;
      end;
   exception
      when others => return 10;
   end timeout_multiplier_x10;

end PortScan.Buildcycle;
