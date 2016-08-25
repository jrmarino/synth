/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../License.txt
 *
 * The first argument is the path to the log
 * The second argument is "0" or "1", where "1" sets the process group
 * The remaining arguments are passed to the spawn
 * We expect a minimum of 4 arguments
 */

#include <unistd.h>
#include <string.h>
#include <fcntl.h>

int main (int argc, char *argv[])
{
  if (argc < 5)
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
      return (-5);
    }

  dup2 (fd, STDOUT_FILENO);
  dup2 (fd, STDERR_FILENO);
  close (fd);

  if (strncmp (argv[2], "1", 1) == 0)
  {
    int result = setpgid (0, 0);
    if (result != 0)
      {
        return (-4);
      }
  }
  closefrom (3);
  return execv (argv[3], (argv + 3));
}
