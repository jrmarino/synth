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
#include <termios.h>
#include <unistd.h>

u_int8_t
__nohang_waitpid (pid_t process_pid)
{
  int status = 0;

  int pid = waitpid (process_pid, &status, WNOHANG);

  if (pid == 0)
    {
      return 0;
    }
  if (WIFEXITED (status) && (WEXITSTATUS (status) == 0))
    {
      return 1;
    }
  else
    {
      return 2;
    }
}

u_int8_t
__silent_control ()
{
  struct termios tp;

  /* Retrieve current terminal settings */
  if (tcgetattr(STDIN_FILENO, &tp) == -1)
    {
       return 1;
    }
  /* ECHO off, other bits unchanged */
  tp.c_lflag &= ~ECHO;

  /* Disable output flow control */
  tp.c_iflag &= ~IXON;

  /* update terminal settings */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tp) == -1)
    {
       return 2;
    }
  return 0;
}

u_int8_t
__chatty_control ()
{
  struct termios tp;

  /* Retrieve current terminal settings */
  if (tcgetattr(STDIN_FILENO, &tp) == -1)
    {
       return 1;
    }
  /* ECHO on, other bits unchanged */
  tp.c_lflag |= ECHO;

  /* Enable output flow control */
  tp.c_iflag |= IXON;

  /* update terminal settings */
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &tp) == -1)
    {
       return 2;
    }
  return 0;
}
