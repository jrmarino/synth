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

end Unix;
