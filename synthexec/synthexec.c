/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * The first argument is the path to the log
 * The remaining arguments are passed to the spawn
 * We expect a minimum of 3 arguments
 */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/procctl.h>

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
kill_process_tree (pid_t reaper_pid)
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
      return (0);
   }
   return (-1);
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
      return (0);
   }
   return (-1);
#  endif  /* __DragonFly__ */
#else
/* process reaping is not supported on this system */
   return (0);
#endif    /* PROC_REAP_STATUS */
}

void
handler (int signo __unused)
{
   kill_process_tree (getpid());
   exit(1);
}

int
main (int argc, char *argv[])
{
   if (argc < 4)
   {
      return (-2);
   }

   int fd = open (argv[1], O_RDWR | O_APPEND | O_CREAT, 0644);
   if (fd < 0)
   {
      return (-3);
   }
   if (fd <= STDERR_FILENO)
   {
      return (-4);
   }

   dup2 (fd, STDOUT_FILENO);
   dup2 (fd, STDERR_FILENO);
   close (fd);
   closefrom (3);

#ifdef PROC_REAP_ACQUIRE
   /*
    * Set current process as the reaper for itself and future children
    * Interface identical for FreeBSD and DragonFly
    */
   if (procctl(P_PID, getpid(), PROC_REAP_ACQUIRE, NULL) < 0)
   {
      return (-5);
   }
#endif

   pid_t pid;
   int status = 0;

   signal(SIGUSR1, handler);

   pid = vfork();
   if (pid < 0)
   {
      return (-6);
   }
   else if (pid == 0)
   {
      /* child */
      execv (argv[2], (argv + 2));
      _exit (1);
   }

   /* Parent */
   while ((waitpid (pid, &status, 0) < 0) && (errno == EINTR)) { };

   kill_process_tree (getpid());
   return (WEXITSTATUS (status));
}
