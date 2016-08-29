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
#include <sys/procctl.h>
#include <signal.h>

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

/*
 * reap_process kills per given pid and waits for it to return
 * returns 0 on success, -1 on failures
 */
int reap_process (pid_t dead_pid_walking)
{
   int status = 0;

   if (kill(dead_pid_walking, SIGKILL) == 0)
   {
      waitpid (dead_pid_walking, &status, 0);
      return (0);
   }
   return (-1);
}

int
__kill_process_tree (pid_t reaper_pid)
{
#ifdef PROC_REAP_STATUS
#  ifdef __FreeBSD__
   struct procctl_reaper_status info;
   struct procctl_reaper_kill killemall;

   if (reaper_pid > 0)
   {
      if (procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info) == 0)
      {
         if (info.rs_children != 0)
         {
            killemall.rk_sig = SIGKILL;
            killemall.rk_flags = 0;
            if (procctl(P_PID, reaper_pid, PROC_REAP_KILL, &killemall) != 0)
            {
                return (-1);
            }
         }
      }
      return (reap_process (reaper_pid));
   }
#  endif  /* __FreeBSD__ */
#  ifdef __DragonFly__
   union reaper_info info;
   int keep_going = 1;

   if (reaper_pid > 0)
   {
      while (keep_going)
      {
         keep_going = 0;
         if (procctl(P_PID, reaper_pid, PROC_REAP_STATUS, &info) == 0)
         {
            if (info.status.pid_head > 0)
            {
               if (reap_process (info.status.pid_head) == 0)
               {
                  keep_going = 1;
               }
               else
               {
                  return (-1);
               }
            }
         }
      }
      return (reap_process (reaper_pid));
   }
#  endif  /* __DragonFly__ */
#else     /* PROC_REAP_STATUS */
  return (-1);
#endif    /* PROC_REAP_STATUS */
}
