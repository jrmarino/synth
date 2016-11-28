--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Pilot is

   --  Called when command line argument counts is known to be 2 or more.
   --  Argument 2 should either be a path to a file or a port origin.
   --  If it's a file, there can be no more arguments (by definition; there's
   --  no technical issue).  If there are more than 2 arguments, arguments 2+
   --  must all be port origins.  Returns "True" when all the port origins
   --  (command line or inside file) are verified and arguments are correct.
   function store_origins return Boolean;

   --  Iterate through stack of individual build requests and scan each one.
   --  If any scan fails, return False.
   function scan_stack_of_single_ports (testmode : Boolean;
                                        always_build : Boolean := False)
                                        return Boolean;

   --  Runs post-scan sanity check
   --  If successful, then scans for all "ignored" ports, failing them
   --  For each ignored port, cascade the failures (designated "skipped" ports)
   --  Starts the build log documenting all this.
   --  Return True if no problems are encountered.
   function sanity_check_then_prefail (delete_first : Boolean := False;
                                       dry_run : Boolean := False)
                                       return Boolean;

   --  Everything is fine so kick of the parallel builders.  They will
   --  continue until everything is complete.
   procedure perform_bulk_run (testmode : Boolean);

   --  This procedure removes the single CLI listed port from the queue,
   --  executes "perform_bulk_run" and then builds the final port, but
   --  breaks into the jail at the specified (by ENTERAFTER env) point
   --  The test mode is always "True" so that's not an argument.
   procedure bulk_run_then_interact_with_final_port;

   --  Return "true" if the user confirms the repository should be re/built.
   function verify_desire_to_rebuild_repository return Boolean;

   --  Return "true" if the user confirms to run "pkg upgrade" against the
   --  local repository.
   function verify_desire_to_install_packages return Boolean;

   --  Post-build, rebuild the local repository with pkg(8)
   --  Returns True on success
   function rebuild_local_respository (remove_invalid_packages : Boolean) return Boolean;

   --  gather every single distfile in ports tree (via distinfo) and then
   --  search the actual package directory.  Remove non-matches without asking.
   procedure purge_distfiles;

   --  Create a pkg repo conf file (requires root permission)
   function write_pkg_repos_configuration_file return Boolean;

   --  There are no specific packages to update.  It's just a generic
   --  "pkg upgrade" limited to local repository with "yes" confirmed.
   procedure upgrade_system_everything (skip_installation : Boolean := False;
                                        dry_run : Boolean := False);

   --  Similar to above, but updates the packages already specified by
   --  the "list" commands
   procedure upgrade_system_exactly;

   --  Returns True if the root users didn't execute Synth.
   function insufficient_privileges return Boolean;

   --  Returns True if a pidfile is found and it's a valid synth process
   function already_running return Boolean;

   --  Create a pidfile on major actions and remove it when complete.
   procedure create_pidfile;
   procedure destroy_pidfile;

   --  Checks if things are mounted from aborted previous run.
   --  The action upon "True" would be to try to clean them up (else abort)
   function previous_run_mounts_detected return Boolean;

   --  Checks if work directories are left over from aborted previous run
   --  The action upon "True" would be to remove them completely
   function previous_realfs_work_detected return Boolean;

   --  Returns True if all the old mounts were unmounted without issue.
   --  If not, it will emit messages so Synth can just eject directly.
   function old_mounts_successfully_removed return Boolean;

   --  Returns True if all the old SL*_(work|localbase) directories
   --  were removed without any problems
   function old_realfs_work_successfully_removed return Boolean;

   --  libexec/synthexec is required, make sure it's installed!
   function synthexec_missing return Boolean;

   --  Scan entire ports tree
   function fully_scan_ports_tree return Boolean;

   --  List every port to be built and the final tally.  This is only done
   --  for the status options
   procedure display_results_of_dry_run;

   --  If ENTERAFTER is defined to valid phase and there's only one port
   --  given on the command line, then return True
   function interact_with_single_builder return Boolean;

   --  The current working directory will cause synth to fail
   --  Known issue for when cwd is within <system root>/usr/local
   function synth_launch_clash return Boolean;

   --  Returns False if additional checks against the given system root
   --  fail.  Currently it's only checking for FreeBSD's /boot/modules
   function valid_system_root return Boolean;

   --  returns True if CONSERVATIVE_UPGRADE option on host pkg(8) is enabled
   function host_pkg8_conservative_upgrade_set return Boolean;

   --  Pre-initialized replicant because some functionality differs depending
   --  on the platform
   procedure set_replicant_platform;

   --  framework specific
   --  On FreeBSD ports collection, it prebuilds pkg(8) if necessary
   --  On Pkgsrc, it prebuilds bmake, bootstrap files and pkg(8) as necessary
   --  Returns True unless a failure was encountered.
   function prerequisites_available return Boolean;

   pilot_log : exception;

private

   subtype logname_field is String (1 .. 19);
   type dim_logname  is array (count_type) of logname_field;
   type verdiff is (newbuild, rebuild, change);

   badport : constant String := "Invalid port origin: ";
   bailing : constant String := "  (Synth must exit)";
   shutreq : constant String := "Graceful shutdown requested, exiting ...";
   pkgng   : constant String := "ports-mgmt/pkg";
   pidfile : constant String := "/var/run/synth.pid";
   brkname : constant String := "ENTERAFTER";
   logname : constant dim_logname := ("00_last_results.log",
                                      "01_success_list.log",
                                      "02_failure_list.log",
                                      "03_ignored_list.log",
                                      "04_skipped_list.log");
   noprocs : constant REP.slave_options := (others => False);
   duplist : portkey_crate.Map;

   --  scan given file.  Everything line must be either blank (whitespace
   --  ignored) or a valid port origin, and returns true if it is.
   --  Internally, the ports are stacked.
   function valid_file (path : String) return Boolean;

   --  return true if "cat" exists and "port" exists
   function valid_catport (catport : String) return Boolean;

   --  wrapper for portlist.insert that prevents duplicate key inserts
   procedure plinsert (key : String; dummy : Natural);

   --  build log operations
   procedure start_logging (flavor : count_type);
   procedure stop_logging (flavor : count_type);

   --  generic function to return first line of file.
   function head_n1 (filename : String) return String;

   --  helper for create_pidfile
   function Get_PID return Integer;
   pragma Import (C, Get_PID, "getpid");

   --  Query pkg(8)'s repos_dir configuration instead of assuming default
   function get_repos_dir return String;

   --  Returns octal failure of file permissions or "000" upon command failure
   function file_permissions (full_path : String) return String;

   --  Given a port ID, search for existing package in the packages directory
   --  If the exact package exists, return " (rebuild <version>)"
   --  If no package exists, return " (new)"
   --  If previous package exists, return " (<oldversion> => <version>)"
   function version_difference (id : port_id; kind : out verdiff) return String;

   --  This checks for existence of both [profile]-public.key and
   --  [profile]-private.key.  If only one exists, a non-fatal notice is
   --  emitted saying signing configuration is incomplete (repo will not be
   --  signed).  The permissions for the private key will be checked, and if
   --  not 400 and owned by root, it will fail fatally.
   --  Returns False with fatal fail, otherwises it always returns True
   function acceptable_RSA_signing_support return Boolean;

   --  The check for existence of both [profile]-signing_command and
   --  [profile]-fingerprint.  If only one exists, a non-fatal notice is
   --  emitted without signing the repository.  Returns True if both files
   --  exist and aren't empty.
   function valid_signing_command return Boolean;

   --  Return the contents of the profile's signing command
   function signing_command return String;

   --  Return the contents of the profile's fingerprint
   function profile_fingerprint return String;

   --  Return True if repo is configured to be built with RSA
   function set_synth_conf_with_RSA return Boolean;

   --  converts file contents (limited to 1 line) of given file to a string
   function one_line_file_contents (filename : String) return String;

   --  Prebuilds pkg(8) in scan builder if necessary.
   --  Returns True unless a failure was encountered.
   function build_pkg8_as_necessary return Boolean;

   --  Prebuilds bootstrap-mk files, bmake, and pkg(8)
   --  Returns True unless a failure was encountered.
   function build_pkgsrc_prerequisites return Boolean;

end PortScan.Pilot;
