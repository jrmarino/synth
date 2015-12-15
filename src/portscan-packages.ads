--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Packages is

   --  This can only be executed if a full scan has already occurred.
   --  This is only required
   procedure clean_repository (repository : String);

   --  This function returns "True" if the scanned options exactly match
   --  the options in the already-built package.  Usually it's already known
   --  that a package exists before the function is called, but an existence
   --  check will be performed just in case (failure returns "False")
   function passed_option_check (repository : String; id : port_id) return
     Boolean;


private

   stored_packages   : package_crate.Map;

   procedure scan_repository (repository : String);
   procedure nextline (lineblock, firstline : out SU.Unbounded_String);

end PortScan.Packages;
