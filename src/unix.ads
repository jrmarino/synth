--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt


package Unix is

   type process_exit is (still_running, exited_normally, exited_with_error);

   type Int32 is private;
   subtype pid_t is Int32;

   --  check if process identified by pid has exited or keeps going
   function process_status (pid : pid_t) return process_exit;

   --  Kill everything with the identified process group
   procedure kill_process_tree (process_group : pid_t);

   --  Allows other packages to call external commands (e.g. Pilot)
   --  Returns "True" on success
   function external_command (command : String) return Boolean;

   --  wrapper for nonblocking spawn
   function launch_process (command : String) return pid_t;

   --  Returns True if pid is less than zero
   function fork_failed (pid : pid_t) return Boolean;

   --  Returns True if "variable" is defined in the environment.  The
   --  value of variable is irrelevant
   function env_variable_defined (variable : String) return Boolean;

private

   type uInt8 is mod 2 ** 16;
   type Int32 is range -(2 ** 31) .. +(2 ** 31) - 1;

end Unix;
