--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Direct_IO;
with GNAT.OS_Lib;
with Util.Streams.Pipes;
with Util.Streams.Buffered;

with Parameters;

package body PortScan.Buildcycle is

   package ACA renames Ada.Calendar.Arithmetic;
   package ACF renames Ada.Calendar.Formatting;
   package ASF renames Ada.Strings.Fixed;
   package AD  renames Ada.Directories;
   package OSL renames GNAT.OS_Lib;
   package STR renames Util.Streams;
   package PM  renames Parameters;


   ---------------------
   --  build_package  --
   ---------------------
   function build_package (id : builders; sequence_id : port_id) return Boolean
   is
      R : Boolean;
   begin
      initialize_log (id, sequence_id);
      for phase in phases'Range loop
         case phase is
            when check_sanity  => R := exec_phase_check_sanity (id);
            when pkg_depends   => R := exec_phase_generic (id, "pkg-depends");
            when fetch_depends => R := exec_phase_generic (id, "fetch-depends");
         end case;
         exit when R = False;
      end loop;
      finalize_log (id);
      return R;
   end build_package;


   ----------------------
   --  initialize_log  --
   ----------------------
   procedure initialize_log (id : builders; sequence_id : port_id)
   is
      FA    : access TIO.File_Type;
      H_ENV : constant String := "Environment";
      H_OPT : constant String := "Options";
      H_CFG : constant String := "/etc/make.conf";
   begin
      trackers (id).seq_id := sequence_id;
      if sequence_id = port_match_failed then
         raise cycle_log_error
           with "initialization attempted with port_id = 0";
      end if;
      trackers (id).head_time := AC.Clock;
      declare
         log_path : constant String := log_name (sequence_id);
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

      TIO.Put_Line (trackers (id).log_handle, "=> Building " &
                      get_catport (all_ports (sequence_id)));
      TIO.Put      (FA.all, "Platform: " & JT.USS (uname_mrv));
      TIO.Put_Line (FA.all, "Started : " & timestamp (trackers (id).head_time));
      TIO.Put_Line (FA.all, LAT.LF & log_section (H_ENV, True));
      TIO.Put      (FA.all, get_environment (id));
      TIO.Put_Line (FA.all, log_section (H_ENV, False) & LAT.LF);
      TIO.Put_Line (FA.all, log_section (H_OPT, True));
      TIO.Put      (FA.all, get_options_configuration (id));
      TIO.Put_Line (FA.all, log_section (H_OPT, False) & LAT.LF);

      dump_port_variables (id);

      TIO.Put_Line (FA.all, log_section (H_CFG, True));
      TIO.Put      (FA.all, dump_make_conf (id));
      TIO.Put_Line (FA.all, log_section (H_CFG, False) & LAT.LF);

   end initialize_log;


   --------------------
   --  finalize_log  --
   --------------------
   procedure finalize_log (id : builders)
   is
     FA    : access TIO.File_Type;
   begin
      trackers (id).tail_time := AC.Clock;
      FA := trackers (id).log_handle'Access;
      TIO.Put_Line (FA.all, "Finished: " & timestamp (trackers (id).tail_time));
      declare
         diff_days : ACA.Day_Count;
         diff_secs : Duration;
         leap_secs : ACA.Leap_Seconds_Count;
         use type ACA.Day_Count;
      begin
         ACA.Difference (Left    => trackers (id).tail_time,
                         Right   => trackers (id).head_time,
                         Days    => diff_days,
                         Seconds => diff_secs,
                         Leap_Seconds => leap_secs);
         TIO.Put (FA.all, "Duration:");
         if diff_days > 0 then
            if diff_days = 1 then
               TIO.Put_Line (FA.all, " 1 day and" &
                             ACF.Image (Elapsed_Time => diff_secs));
            else
               TIO.Put_Line (FA.all, diff_days'Img & " days and" &
                               ACF.Image (Elapsed_Time => diff_secs));
            end if;
         else
            TIO.Put_Line (FA.all, " " & ACF.Image (Elapsed_Time => diff_secs));
         end if;
      end;
      TIO.Close (trackers (id).log_handle);
   end finalize_log;


   -----------------
   --  timestamp  --
   -----------------
   function timestamp (hack : AC.Time) return String
   is
      function MON   (num : AC.Month_Number) return String;
      function WKDAY (day : ACF.Day_Name) return String;

      function MON (num : AC.Month_Number) return String is
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
      return WKDAY (ACF.Day_Of_Week (hack)) & "," & AC.Day (hack)'Img & " " &
        MON (AC.Month (hack)) & AC.Year (hack)'Img & " at" &
        ACF.Image (hack)(11 .. 19);
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
      pipe.Open (Command => command);
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
      command : constant String := "/usr/sbin/chroot " & root & " /usr/bin/env";
   begin
      return JT.USS (generic_system_command (command));
   end get_environment;


   ---------------------------------
   --  get_options_configuration  --
   ---------------------------------
   function get_options_configuration (id : builders) return String
   is
      root    : constant String := get_root (id);
      command : constant String := "/usr/sbin/chroot " & root &
        " /usr/bin/make -C /xports/" &
        get_catport (all_ports (trackers (id).seq_id)) &
        " showconfig";
   begin
      return JT.USS (generic_system_command (command));
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


   ---------------------------
   --  dump_port_variables  --
   ---------------------------
   procedure dump_port_variables (id : builders)
   is
      root    : constant String := get_root (id);
      command : constant String := "/usr/sbin/chroot " & root &
        " /usr/bin/make -C /xports/" &
        get_catport (all_ports (trackers (id).seq_id)) &
        " -VCONFIGURE_ENV -VCONFIGURE_ARGS -VMAKE_ENV -VMAKE_ARGS" &
        " -VPLIST_SUB -VSUB_LIST";
      LA      : access TIO.File_Type := trackers (id).log_handle'Access;
      content : JT.Text;
      topline : JT.Text;
      type result_range is range 1 .. 6;
   begin
      content := generic_system_command (command);
      for k in result_range loop
         nextline (lineblock => content, firstline => topline);
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
   --  nextline  --
   ----------------
   procedure nextline (lineblock, firstline : out JT.Text)
   is
      CR_loc : Natural;
      CR : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      CR_loc := JT.SU.Index (Source => lineblock, Pattern => CR);
      firstline := JT.SUS
        (JT.SU.Slice (Source => lineblock, Low => 1, High => CR_loc - 1));
      JT.SU.Delete (Source => lineblock, From => 1, Through => CR_loc);
   end nextline;


   ----------------
   --  log_name  --
   ----------------
   function log_name (sid : port_id) return String
   is
      catport : constant String := get_catport (all_ports (sid));
      slash   : Integer;
   begin
      slash := ASF.Index (catport, "/");
      return JT.USS (PM.configuration.dir_logs) & "/" &
        catport (1 .. slash - 1) & "___" &
        catport (slash + 1 .. catport'Last) & ".log";
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


   -------------------------------
   --  exec_phase_check_sanity  --
   -------------------------------
   function exec_phase_check_sanity (id : builders) return Boolean
   is
      phase    : constant String := "check-sanity";
      phaseenv : String := "DEVELOPER=1";
   begin
      if not testing then
         phaseenv := (others => LAT.Space);
      end if;
      return exec_phase (id => id, phase => phase, phaseenv => phaseenv);
   end exec_phase_check_sanity;


   ------------------------------
   --  exec_phase_pkg_depends  --
   ------------------------------
   function exec_phase_generic (id : builders; phase : String) return Boolean is
   begin
      return exec_phase (id => id, phase => phase);
   end exec_phase_generic;


   ---------------
   --  execute  --
   ---------------
   function generic_execute (id : builders; command : String) return Boolean
   is
      Args        : OSL.Argument_List_Access;
      Exit_Status : Integer;
      FD          : OSL.File_Descriptor;
   begin
      FD := OSL.Open_Append (Name  => log_name (trackers (id).seq_id),
                             Fmode => OSL.Text);

      Args := OSL.Argument_String_To_List (command);
      OSL.Spawn (Program_Name => Args (Args'First).all,
                 Args         => Args (Args'First + 1 .. Args'Last),
                 Return_Code  => Exit_Status,
                 Output_File_Descriptor => FD);
      OSL.Free (Args);

      OSL.Close (FD);
      return Exit_Status = 0;
   end generic_execute;


   ------------------
   --  exec_phase  --
   ------------------
   function exec_phase (id : builders; phase : String; phaseenv : String := "")
                        return Boolean
   is
      root       : constant String := get_root (id);
      port_flags : String := " NO_DEPENDS=yes ";
      dev_flags  : String := " DEVELOPER_MODE=yes ";
      pid        : port_id := trackers (id).seq_id;
      catport    : constant String := get_catport (all_ports (pid));
      result     : Boolean;
   begin
      if testing then
         port_flags := (others => LAT.Space);
      else
         dev_flags := (others => LAT.Space);
      end if;

      --  Nasty, we have to switch open and close the log file for each
      --  phase because we have to switch between File_Type and File
      --  Descriptors.  I can't find a safe way to get the File Descriptor
      --  out of the File type.

      log_phase_begin (phase, id);
      TIO.Close (trackers (id).log_handle);

      declare
           command : constant String := "/usr/sbin/chroot " & root &
           " /usr/bin/env " & phaseenv & dev_flags & port_flags &
           "/usr/bin/make -C /xports/" & catport & " " & phase;
      begin
         result := generic_execute (id, command);
      end;

      --  Reopen the log.  I guess we can leave off the exception check
      --  since it's been passing before

      TIO.Open (File => trackers (id).log_handle,
                Mode => TIO.Append_File,
                Name => log_name (trackers (id).seq_id));
      log_phase_end (id);

      return result;
   end exec_phase;


end PortScan.Buildcycle;
