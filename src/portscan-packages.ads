--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Packages is

   --  This can only be executed if a full scan has already occurred.
   --  This is a deep scan of existing packages that is intended to be run
   --  just prior to building a repository. It is also executed before
   --  a full bulk run (in which case it will not be run before rebuilding
   --  the repository.
   procedure clean_repository (repository : String);

   --  If performing a limited build run (likely 99% of the use cases), only
   --  the queued packages will be checked.  The checks are limited to finding
   --  options changes and dependency changes.  Obsolete packages (related or
   --  unrelated to upcoming build) are not removed; this would occur in
   --  clean_repository().  These old packages will not interfere at this step.
   procedure limited_sanity_check (repository : String);


private

   stored_packages : package_crate.Map;

   --  This function returns "True" if the scanned options exactly match
   --  the options in the already-built package.  Usually it's already known
   --  that a package exists before the function is called, but an existence
   --  check will be performed just in case (failure returns "False")
   function passed_option_check (repository : String; id : port_id;
                                 skip_exist_check : Boolean := False) return
     Boolean;

   procedure scan_repository (repository : String);
   procedure nextline (lineblock, firstline : out SU.Unbounded_String);

end PortScan.Packages;
