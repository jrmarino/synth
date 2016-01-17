/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * __nohang_waitpid is linked directly into Synth but it's related to synthexec
 * so let's keep the C files together.
 * return:
 *    0 when process is still running
 *    1 when process exited normally
 *    2 when process exited with error
 */

#include <sys/types.h>
#include <sys/wait.h>

u_int8_t
__nohang_waitpid (pid_t process_pid)
{
  int status = 0;

  int pid = waitpid (process_pid, &status, WNOHANG);

  if (pid == 0)
    {
      return 0;
    }
  if (WIFEXITED (status))
    {
      return 1;
    }
  else
    {
      return 2;
    }
}
