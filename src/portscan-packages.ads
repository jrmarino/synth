--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Packages is

   --  This routine first removes all invalid packages (package from removed
   --  port or older version) and inserts the origins of the remaining packages
   --  into the port list for a limited tree scan.
   procedure preclean_repository (repository : String);

   --  If performing a limited build run (likely 99% of the use cases), only
   --  the queued packages will be checked.  The checks are limited to finding
   --  options changes and dependency changes.  Obsolete packages (related or
   --  unrelated to upcoming build) are not removed; this would occur in
   --  clean_repository().  These old packages will not interfere at this step.
   procedure limited_sanity_check (repository : String; dry_run : Boolean;
                                   suppress_remote : Boolean);

   --  Iterate through the final build queue to remove any packages that
   --  match the current package names (currently unused)
   procedure remove_queue_packages (repository : String);

   --  This procedure empties the given repository without discrimination.
   --  (Well, it's limited to "*.txz" matches, but normally that's everything)
   --  (currently unused)
   procedure wipe_out_repository (repository : String);

   --  Sometimes, especially with the single ports-mgmt/pkg check, there is
   --  nothing left to do after the sanity check.  Let's provide a way to
   --  detect that case.
   function queue_is_empty return Boolean;

   --  Returns the size of the queue before it was pared down.
   function original_queue_size return Natural;

   --  After the initial queue is created, and before the limited sanity
   --  check, we go through each port and check if it has cached options.
   --  If it does, then it's checked for validity.  If it has too many or
   --  too few options, or an option's name doesn't match, the port is
   --  printed to stdout.  The rest of the ports are checked, but at that
   --  point the function has failed.
   function limited_cached_options_check return Boolean;

   --  Returns True on success; stores value in global external_repository
   function located_external_repository return Boolean;

   --  Returns the value of the stored external repository
   function top_external_repository return String;

   --  Given the path components for a package, query it for the port origin
   function query_origin (fullpath : String) return String;

private

   type dim_packages is array (scanners) of string_crate.Vector;

   stored_packages     : dim_packages;
   stored_origins      : dim_packages;
   pkgscan_progress    : dim_progress := (others => 0);
   pkgscan_total       : Natural := 0;
   calculated_abi      : JT.Text;
   calculated_alt_abi  : JT.Text;
   calc_abi_noarch     : JT.Text;
   calc_alt_abi_noarch : JT.Text;
   external_repository : JT.Text;
   original_queue_len  : AC.Count_Type;

   --  Debugging purposes only, can be turned on by environment variable
   debug_dep_check : Boolean := False;
   debug_opt_check : Boolean := False;

   --  This function returns "True" if the scanned options exactly match
   --  the options in the already-built package.  Usually it's already known
   --  that a package exists before the function is called, but an existence
   --  check will be performed just in case (failure returns "False")
   function passed_option_check (repository : String; id : port_id;
                                 skip_exist_check : Boolean := False)
                                 return Boolean;

   --  This function returns "True" if the scanned dependencies match exactly
   --  what the current ports tree has.
   function passed_dependency_check (query_result : JT.Text; id : port_id)
                                     return Boolean;

   --  This function returns "True" if the scanned package has the expected
   --  package ABI, e.g. dragonfly:4.6:x86:64, freebsd:10:amd64
   function passed_abi_check (repository : String; id : port_id;
                              skip_exist_check : Boolean := False)
                              return Boolean;

   --  This calculates the ABI for the platform and stores it.  The value is
   --  used by passed_abi_check()
   procedure establish_package_architecture;

   --  Scan directory that contains the packages (*.txz) and stores the
   --  file names in the container.  Returns False if no packages are found.
   function scan_repository (repository : String) return Boolean;

   --  standard method to spawn commands in this package (and get output)
   function generic_system_command (command : String) return JT.Text;

   --  Evaluates the stored options.  If none exists, return True
   --  If Exists and all the options match exactly what has already been
   --  scanned for the port (names, not values) then return True else False.
   function passed_options_cache_check (id : port_id) return Boolean;

   --  For each package in the query, check the ABI and options (this is the
   --  only time they are checked).  If those pass, query the dependencies,
   --  store the result, and check them.  Set the "deletion" flag as needed.
   --  The dependency check is NOT performed yet.
   procedure initial_package_scan (repository : String; id : port_id);

   --  Same as above, but for packages in the external repository
   procedure remote_package_scan (id : port_id);

   --  The result of the dependency query giving "id" port_id
   function result_of_dependency_query (repository : String; id : port_id)
                                        return JT.Text;

   --  Using the same make_queue as was used to scan the ports, use tasks
   --  (up to 32) to do the initial scanning of the ports, including getting
   --  the pkg dependency query.
   procedure parallel_package_scan (repository : String; remote_scan : Boolean;
                                    show_progress : Boolean);

   --  Prior to this procedure, the list of existing packages is split as
   --  a balanced array so this scan will query the package for its origin
   --  and package name.  If the origin still exists, the port will be
   --  scanned for the current package name.  If either check fails, the
   --  package will be deleted, otherwise the origin will be preserved for
   --  a further in-depth check.
   procedure parallel_preliminary_package_scan (repository : String;
                                                show_progress : Boolean);

   --  given a port_id, return the package name (no .txz extension!)
   function id2pkgname (id : port_id) return String;

   --  Turn on option and dependency debug checks programmatically
   procedure activate_debugging_code;

   --  Given an origin (already validated) and the name of the package in
   --  focus, return True if "make -V PKGFILE:T" matches the filename
   function current_package_name (origin, file_name : String) return Boolean;

   --  Dedicated progress meter for prescanning packages
   function package_scan_progress return String;

end PortScan.Packages;
