/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: /License.txt
 *
 * This provides a shell-free alternative the popen piped command mechanism.
 * The output is written to a file.
 */

#ifndef _WIN32

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/wait.h>
#ifndef __unused
#define __unused __attribute__((__unused__))
#endif
#ifndef W_EXITCODE
#define W_EXITCODE(ret, sig)	((ret) << 8 | (sig))
#endif


/*
 * --------------------------
 * --  diagnostic_message  --
 * --------------------------
 */
static void
diagnostic_message (int fd, char *prog, int argc, char *argv[])
{
   write (fd, "Command execution failed: ", 26);
   write (fd, prog, strlen (prog));
   write (fd, "\n               arguments:", 26);
   for (int x = 0; x < argc; x++) {
      write (fd, " ", 1);
      write (fd, argv[x], strlen (argv[x]));
   }
   write (fd, "\n", 1);
}


/*
 * --------------------------
 * --  start_new_log_file  --
 * --------------------------
 */
int
start_new_log_file (const char *path)
{
   int flags = O_CREAT | O_WRONLY | O_TRUNC;

   return (open (path, flags, 0644));
}


/*
 * ---------------
 * --  synexec  --
 * ---------------
 * argument fd      : file description to open log file
 * argument prog    : full path to program to execute
 * argument argc    : number of arguments to pass to program
 * argument argv    : vector of arguments to pass to program
 *
 * return code  pid : success   (positive number)
 *               -1 : negative file descriptor
 *               -2 : file descriptor below standard error
 *               -3 : first fork failed
 *               -4 : second fork failed
 *               -5 : setsid failed
 *               -6 : execv failed
 */
int
synexec (int fd, char *prog, int argc, char *argv[])
{
  pid_t fork1_pid;
  pid_t fork2_pid;
  pid_t child_pid;
  pid_t wait_result;
  int grand_status = 0;

  if (fd < 0)
    {
      return (-1);
    }
  if (fd <= STDERR_FILENO)
    {
      return (-2);
    }

  fork1_pid = fork();
  if (fork1_pid < 0)
    {
      return (-3);
    }


  /***************************
   *  First fork successful  *
   ***************************/
  if (fork1_pid == 0)
    {
      /* child of fork #1 */
      child_pid = getpid();
      if (setsid() < 0)
        {
          _exit (-5);
        }
      signal(SIGINT, SIG_IGN);

      fork2_pid = fork();
      if (fork2_pid < 0)
        {
          _exit (-4);
        }


      /****************************
       *  Second fork successful  *
       ****************************/
      if (fork2_pid == 0)
        {
          /* grandchild */

          dup2 (fd, STDOUT_FILENO);
          dup2 (fd, STDERR_FILENO);

          execv (prog, argv);

          /* execv doesn't normally return, so command failed to execute */
          diagnostic_message(fd, prog, argc, argv);
          _exit (-6);
        }

      /* parent of grandchild (a.k.a child) */
      wait_result = waitpid (fork2_pid, &grand_status, 0);
      if (wait_result < 0)
        {
          grand_status = W_EXITCODE(1, 0);
        };

      _exit (WEXITSTATUS (grand_status));
    }

  /* Parent of fork #1 */
  return fork1_pid;
}


#endif /* __WIN32 */
