/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * The first argument is the path to the log
 * The remaining arguments are passed to the spawn
 * We expect a minimum of 3 arguments
 */

#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#ifdef USE_FORK
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>
#endif

int main (int argc, char *argv[])
{
  int   fd;
  int   status;
  pid_t child_pid;

  if (argc < 4)
    {
      return (-2);
    }

  fd = open (argv[1], O_RDWR | O_APPEND | O_CREAT, 0644);
  if (fd < 0)
    {
      return (errno);
    }

  dup2 (fd, STDOUT_FILENO);
  dup2 (fd, STDERR_FILENO);
  close (fd);

#ifdef USE_FORK
  child_pid = fork ();

  if (child_pid >= 0)
    {
      if (child_pid == 0)
	{
	  /* child */
	  return execv (argv[2], (argv + 2));
	}
      else
	{
	  /* parent */
	  waitpid (child_pid, &status, WSTOPPED);
	  exit (status);
	}
    }
  else
    {
      /* fork failure */
      exit (-1);
    }
#else
  return execv (argv[2], (argv + 2));
#endif
}
