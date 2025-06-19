--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with GNAT.OS_Lib;
with Ada.Text_IO;
with Parameters;
with System;

package body Unix is

   package OSL renames GNAT.OS_Lib;
   package TIO renames Ada.Text_IO;
   package PM  renames Parameters;

   ----------------------
   --  process_status  --
   ----------------------
   function process_status (pid : pid_t) return process_exit
   is
      result : constant uInt8 := nohang_waitpid (pid);
   begin
      case result is
         when 0 => return still_running;
         when 1 => return exited_normally;
         when others => return exited_with_error;
      end case;
   end process_status;


   -----------------------
   --  screen_attached  --
   -----------------------
   function screen_attached return Boolean is
   begin
      return CSM.isatty (handle => CSM.fileno (CSM.stdin)) = 1;
   end screen_attached;


   -----------------------
   --  cone_of_silence  --
   -----------------------
   procedure cone_of_silence (deploy : Boolean)
   is
      result : uInt8;
   begin
      if not screen_attached then
         return;
      end if;

      if deploy then
         result := silent_control;
         if result > 0 then
            TIO.Put_Line ("Notice: tty echo+control OFF command failed");
         end if;
      else
         result := chatty_control;
         if result > 0 then
            TIO.Put_Line ("Notice: tty echo+control ON command failed");
         end if;
      end if;
   end cone_of_silence;


   -----------------------------
   --  ignore_background_tty  --
   -----------------------------
   procedure ignore_background_tty
   is
      result : uInt8;
   begin
      result := ignore_tty_write;
      if result > 0 then
         TIO.Put_Line ("Notice: ignoring background tty write signal failed");
      end if;
      result := ignore_tty_read;
      if result > 0 then
         TIO.Put_Line ("Notice: ignoring background tty read signal failed");
      end if;
   end ignore_background_tty;


   -------------------------
   --  kill_process_tree  --
   -------------------------
   procedure kill_process_tree (process_group : pid_t)
   is
      use type IC.int;
      result : constant IC.int := signal_runaway (process_group);
   begin
      if result /= 0 then
         TIO.Put_Line ("Notice: failed to signal pid " & process_group'Img);
      end if;
   end kill_process_tree;


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


   ------------------------
   --  external_command  --
   ------------------------
   function external_command (program     : String;
                              arguments   : String;
                              output_file : String) return Boolean
   is
      argvector : aliased struct_argv;
      cprogram  : ICS.chars_ptr;
      num_args  : IC.int;
      retcode   : IC.int;
      cfd       : IC.int;
      close_res : IC.int;
      log_fd    : File_Descriptor;

      use type IC.int;
      pragma Unreferenced (close_res);
   begin
      cprogram := ICS.New_String (program);
      set_argument_vector (program & " " & arguments, argvector, num_args);
      log_fd := start_new_log (output_file);
      cfd := IC.int (log_fd);

      retcode := synexec (cfd, cprogram, num_args, argvector'Unchecked_Access);

      close_res := C_Close (cfd);
      if num_args > 0 then
         for x in 0 .. num_args - 1 loop
            ICS.Free (argvector.args (x));
         end loop;
      end if;
      ICS.Free (cprogram);

      return retcode = 0;
   end external_command;


   -------------------
   --  fork_failed  --
   -------------------
   function fork_failed (pid : pid_t) return Boolean is
   begin
      if pid < 0 then
         return True;
      end if;
      return False;
   end fork_failed;


   ----------------------
   --  launch_process  --
   ----------------------
   function launch_process (command : String) return pid_t
   is
      procid : OSL.Process_Id;
      Args   : OSL.Argument_List_Access;
   begin
      Args   := OSL.Argument_String_To_List (command);
      procid := OSL.Non_Blocking_Spawn
        (Program_Name => Args (Args'First).all,
         Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      return pid_t (OSL.Pid_To_Integer (procid));
   end launch_process;


   ----------------------------
   --  env_variable_defined  --
   ----------------------------
   function env_variable_defined (variable : String) return Boolean
   is
      test : String := OSL.Getenv (variable).all;
   begin
      return (test /= "");
   end env_variable_defined;


   --------------------------
   --  env_variable_value  --
   --------------------------
   function env_variable_value (variable : String) return String is
   begin
      return OSL.Getenv (variable).all;
   end env_variable_value;


   ------------------
   --  pipe_close  --
   ------------------
   function pipe_close (OpenFile : CSM.FILEs) return Integer
   is
      res : constant CSM.int := pclose (FileStream => OpenFile);
      u16 : Interfaces.Unsigned_16;
   begin
      u16 := Interfaces.Shift_Right (Interfaces.Unsigned_16 (res), 8);
      if Integer (u16) > 0 then
         return Integer (u16);
      end if;
      return Integer (res);
   end pipe_close;


   ---------------------
   --  piped_command  --
   ---------------------
   function piped_command (command : String; status : out Integer)
                           return JT.Text
   is
      redirect   : constant String := " 2>&1";
      filestream : CSM.FILEs;
      result     : JT.Text;
   begin
      filestream := popen (IC.To_C (command & redirect), IC.To_C ("re"));
      result := pipe_read (OpenFile => filestream);
      status := pipe_close (OpenFile => filestream);
      return result;
   end piped_command;


   --------------------------
   --  piped_mute_command  --
   --------------------------
   function piped_mute_command (command : String; abnormal : out JT.Text) return Boolean
   is
      redirect   : constant String := " 2>&1";
      filestream : CSM.FILEs;
      status     : Integer;
   begin
      filestream := popen (IC.To_C (command & redirect), IC.To_C ("re"));
      abnormal   := pipe_read (OpenFile => filestream);
      status     := pipe_close (OpenFile => filestream);
      return status = 0;
   end piped_mute_command;


   -----------------
   --  pipe_read  --
   -----------------
   function pipe_read (OpenFile : CSM.FILEs) return JT.Text
   is
      --  Allocate 2kb at a time
      buffer  : String (1 .. 2048) := (others => ' ');
      result  : JT.Text := JT.blank;
      charbuf : CSM.int;
      marker  : Natural := 0;
   begin
      loop
         charbuf := CSM.fgetc (OpenFile);
         if charbuf = CSM.EOF then
            if marker >= buffer'First then
               JT.SU.Append (result, buffer (buffer'First .. marker));
            end if;
            exit;
         end if;
         if marker = buffer'Last then
            JT.SU.Append (result, buffer);
            marker := buffer'First;
         else
            marker := marker + 1;
         end if;
         buffer (marker) := Character'Val (charbuf);
      end loop;
      return result;
   end pipe_read;


   -----------------
   --  true_path  --
   -----------------
   function true_path (provided_path : String) return String
   is
      use type ICS.chars_ptr;
      buffer : IC.char_array (0 .. 1024) := (others => IC.nul);
      result : ICS.chars_ptr;
      path   : IC.char_array := IC.To_C (provided_path);
   begin
      result := realpath (pathname => path, resolved_path => buffer);
      if result = ICS.Null_Ptr then
         return "";
      end if;
      return ICS.Value (result);
   exception
      when others => return "";
   end true_path;


   ---------------------
   --  start_new_log  --
   ---------------------
   function start_new_log (filename : String) return File_Descriptor
   is
      path : IC.Strings.chars_ptr;
      cfd : IC.int;
   begin
      path := IC.Strings.New_String (filename);
      cfd := C_Start_Log (path);
      IC.Strings.Free (path);
      return File_Descriptor (cfd);
   end start_new_log;


   ---------------------------
   --  set_argument_vector  --
   ---------------------------
   procedure set_argument_vector
     (Arg_String : String;
      argvector  : in out struct_argv;
      num_args   : out IC.int)
   is
      Idx : Integer;

      use type IC.int;
   begin
      num_args := 0;
      Idx := Arg_String'First;

      loop
         declare
            Quoted   : Boolean := False;
            Backqd   : Boolean := False;
            Old_Idx  : Integer;

         begin
            Old_Idx := Idx;

            loop
               --  A vanilla space is the end of an argument

               if not Backqd and then not Quoted
                 and then Arg_String (Idx) = ' '
               then
                  exit;

               --  Start of a quoted string

               elsif not Backqd and then not Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Quoted := True;

               --  End of a quoted string and end of an argument

               elsif not Backqd and then Quoted
                 and then Arg_String (Idx) = '"'
               then
                  Idx := Idx + 1;
                  exit;

               --  Following character is backquoted

               elsif Arg_String (Idx) = '\' then
                  Backqd := True;

               --  Turn off backquoting after advancing one character

               elsif Backqd then
                  Backqd := False;

               end if;

               Idx := Idx + 1;
               exit when Idx > Arg_String'Last;
            end loop;

            --  Found an argument

            argvector.args (num_args) := ICS.New_String (Arg_String (Old_Idx .. Idx - 1));
            num_args := num_args + 1;
            exit when num_args = MAX_ARGS;

            --  Skip extraneous spaces

            while Idx <= Arg_String'Last and then Arg_String (Idx) = ' '
            loop
               Idx := Idx + 1;
            end loop;
         end;

         exit when Idx > Arg_String'Last;
      end loop;

      argvector.args (num_args) := ICS.Null_Ptr;

   end set_argument_vector;

end Unix;
