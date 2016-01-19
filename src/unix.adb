--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C.Strings;
with GNAT.OS_Lib;
with JohnnyText;
with Parameters;
with System;

package body Unix is

   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package OSL renames GNAT.OS_Lib;
   package JT  renames JohnnyText;
   package PM  renames Parameters;

   ----------------------
   --  process_status  --
   ----------------------
   function process_status (pid : pid_t) return process_exit
   is
      function nohang_waitpid (pid : pid_t) return uInt8;
      pragma Import (C, nohang_waitpid, "__nohang_waitpid");

      result : uInt8;
   begin
      result := nohang_waitpid (pid);
      case result is
         when 0 => return still_running;
         when 1 => return exited_normally;
         when others => return exited_with_error;
      end case;
   end process_status;


   -------------------------
   --  kill_process_tree  --
   -------------------------
   procedure kill_process_tree (process_group : pid_t)
   is
      pgid : constant String := process_group'Img;
      dfly_cmd : constant String := "/usr/bin/pkill -KILL -g";
      free_cmd : constant String := "/bin/pkill -KILL -g";
      killres  : Boolean;
   begin
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         killres := external_command (free_cmd & pgid);
      else
         killres := external_command (dfly_cmd & pgid);
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


end Unix;
