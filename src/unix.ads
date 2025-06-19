--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Interfaces.C;
with Interfaces.C_Streams;
with Interfaces.C.Strings;
with JohnnyText;

package Unix is

   package JT  renames JohnnyText;
   package IC  renames Interfaces.C;
   package ICS renames Interfaces.C.Strings;
   package CSM renames Interfaces.C_Streams;

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

   --  Spawn command, writing stdout+stderr to the output file
   --  Returns "True" on success
   function external_command (command     : String;
                              output_file : String) return Boolean;

   --  wrapper for nonblocking spawn
   function launch_process (command : String) return pid_t;

   --  Returns True if pid is less than zero
   function fork_failed (pid : pid_t) return Boolean;

   --  Returns True if "variable" is defined in the environment.  The
   --  value of variable is irrelevant
   function env_variable_defined (variable : String) return Boolean;

   --  Return value of "variable" defined in environment.  If it's not
   --  defined than an empty string is returned;
   function env_variable_value (variable : String) return String;

   --  Execute popen and return stdout+stderr combined
   --  Also the result status is returned as an "out" variable
   function piped_command (command : String; status : out Integer)
                           return JT.Text;

   --  Run external command that is expected to have no output to standard
   --  out, but catch stdout anyway.  Don't return any output, but do return
   --  True of the command returns status of zero.
   function piped_mute_command (command : String; abnormal : out JT.Text) return Boolean;

   --  When the cone of silence is deployed, the terminal does not echo
   --  and Control-Q/S keystrokes are not captured (and vice-versa)
   procedure cone_of_silence (deploy : Boolean);

   --  Returns True if a TTY device is detected
   function screen_attached return Boolean;

   --  Equivalent of libc's realpath() function
   --  It resolves symlinks and relative directories to get the true path
   function true_path (provided_path : String) return String;

   --  Ignore SIGTTOU signal (background process trying to write to terminal)
   --  If that happens, synth will suspend and ultimately fail.
   procedure ignore_background_tty;

private

   type uInt8 is mod 2 ** 16;
   type Int32 is range -(2 ** 31) .. +(2 ** 31) - 1;

   function popen (Command, Mode : IC.char_array) return CSM.FILEs;
   pragma Import (C, popen);

   function pclose (FileStream : CSM.FILEs) return CSM.int;
   pragma Import (C, pclose);

   function realpath (pathname, resolved_path : IC.char_array)
                      return ICS.chars_ptr;
   pragma Import (C, realpath, "realpath");

   function nohang_waitpid (pid : pid_t) return uInt8;
   pragma Import (C, nohang_waitpid, "__nohang_waitpid");

   function silent_control return uInt8;
   pragma Import (C, silent_control, "__silent_control");

   function chatty_control return uInt8;
   pragma Import (C, chatty_control, "__chatty_control");

   function signal_runaway (pid : pid_t) return IC.int;
   pragma Import (C, signal_runaway, "__shut_it_down");

   function ignore_tty_write return uInt8;
   pragma Import (C, ignore_tty_write, "__ignore_background_tty_writes");

   function ignore_tty_read return uInt8;
   pragma Import (C, ignore_tty_read, "__ignore_background_tty_reads");

   --  internal pipe close command
   function pipe_close (OpenFile : CSM.FILEs) return Integer;

   --  internal pipe read command
   function pipe_read (OpenFile : CSM.FILEs) return JT.Text;

end Unix;
