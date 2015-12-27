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

   --  Prebuilds pkg(8) in SL01 builder if necessary.
   --  Returns True unless a failure was encountered.
   function build_pkg8_as_necessary return Boolean;

   --  Iterate through stack of individual build requests and scan each one.
   --  If any scan fails, return False.
   function scan_stack_of_single_ports return Boolean;

   --  Runs post-scan sanity check
   --  If successful, then scans for all "ignored" ports, failing them
   --  For each ignored port, cascade the failures (designated "skipped" ports)
   --  Starts the build log documenting all this.
   --  Return True if no problems are encountered.
   function sanity_check_then_prefail (delete_first : Boolean := False)
                                       return Boolean;

   --  Everything is fine so kick of the parallel builders.  They will
   --  continue until everything is complete.
   procedure perform_bulk_run (testmode : Boolean);

   --  Return "true" if the user confirms the repository should be re/built.
   function verify_desire_to_rebuild_repository return Boolean;

   --  Return "true" if the user confirms to run "pkg upgrade" against the
   --  local repository.
   function verify_desire_to_install_packages return Boolean;

   --  Post-build, rebuild the local repository with pkg(8)
   procedure rebuild_local_respository;

   --  post-repo, upgrade the same packages request
   procedure install_new_packages_to_live_system;

   --  gather every single distfile in ports tree (via distinfo) and then
   --  search the actual package directory.  Remove non-matches without asking.
   procedure purge_distfiles;

   pilot_log : exception;

private

   subtype logname_field is String (1 .. 19);
   type dim_logname  is array (count_type) of logname_field;

   portlist : portkey_crate.Map;

   badport : constant String := "Invalid port origin: ";
   bailing : constant String := "  (Synth must exit)";
   pkgng   : constant String := "ports-mgmt/pkg";
   logname : constant dim_logname := ("00_last_results.log",
                                      "01_success_list.log",
                                      "02_failure_list.log",
                                      "03_ignored_list.log",
                                      "04_skipped_list.log");

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

end PortScan.Pilot;
