--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Direct_IO;
with Util.Streams.Pipes;
with Util.Streams.Buffered;

with Parameters;

package body PortScan.Buildcycle is

   package ACA renames Ada.Calendar.Arithmetic;
   package ACF renames Ada.Calendar.Formatting;
   package ASF renames Ada.Strings.Fixed;
   package AD  renames Ada.Directories;
   package STR renames Util.Streams;
   package PM  renames Parameters;

   ----------------------
   --  initialize_log  --
   ----------------------
   procedure initialize_log (id : builders; sequence_id : port_id)
   is
      function log_name (sid : port_id) return String;
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
      FA : access TIO.File_Type;
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
      TIO.Put_Line (FA.all, LAT.LF & "[ Environment HEAD ]");
      TIO.Put      (FA.all, get_environment (id));
      TIO.Put_Line (FA.all, "[ Environment TAIL ]" & LAT.LF);
      TIO.Put_Line (FA.all, "[ Options Configuration HEAD ]");
      TIO.Put      (FA.all, get_options_configuration (id));
      TIO.Put_Line (FA.all, "[ Options Configuration TAIL ]" & LAT.LF);
      dump_port_variables (id);
      TIO.Put_Line (FA.all, "[ /etc/make.conf HEAD ]");
      TIO.Put      (FA.all, dump_make_conf (id));
      TIO.Put_Line (FA.all, "[ /etc/make.conf TAIL ]" & LAT.LF);

   end initialize_log;


   --------------------
   --  finalize_log  --
   --------------------
   procedure finalize_log (id : builders)
   is
   begin
      trackers (id).tail_time := AC.Clock;
      TIO.Put_Line (trackers (id).log_handle,
                    "Finished: " & timestamp (trackers (id).tail_time));
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
         TIO.Put (trackers (id).log_handle, "Duration:");
         if diff_days > 0 then
            if diff_days = 1 then
               TIO.Put_Line (trackers (id).log_handle, " 1 day and" &
                             ACF.Image (Elapsed_Time => diff_secs));
            else
               TIO.Put_Line (trackers (id).log_handle,
                             diff_days'Img & " days and" &
                               ACF.Image (Elapsed_Time => diff_secs));
            end if;
         else
            TIO.Put_Line (trackers (id).log_handle, " " &
                          ACF.Image (Elapsed_Time => diff_secs));
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
      TIO.Put_Line ("mvr = " & JT.USS (uname_mrv));
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
      return "[ " & title & " HEAD ]" & LAT.LF &
        meatstr (1 .. meatlen) & LAT.LF &
        "[ " & title & " TAIL ]" & LAT.LF;
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
   procedure initialize
   is
      logdir : constant String := JT.USS (PM.configuration.dir_logs);
   begin
      set_uname_mrv;
      if not AD.Exists (logdir) then
         AD.Create_Path (New_Directory => logdir);
      end if;
   exception
         when error : others =>
            raise cycle_log_error
              with "failed to create " & logdir;
   end initialize;


end PortScan.Buildcycle;
