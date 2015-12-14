--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Packages is

   --  This can only be executed if a full scan has already occurred.
   --  This is only required
   procedure clean_repository (repository : String);

private

   stored_packages   : package_crate.Map;

   procedure scan_repository (repository : String);

end PortScan.Packages;
