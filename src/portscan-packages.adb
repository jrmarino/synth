--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;

package body PortScan.Packages is

   package AD  renames Ada.Directories;


   ------------------------
   --  clean_repository  --
   ------------------------
   procedure clean_repository (repository : String)
   is
      procedure save_package (cursor : ranking_crate.Cursor);
      procedure save_package (cursor : ranking_crate.Cursor)
      is
         QR  : constant queue_record := ranking_crate.Element (cursor);
         pcc : package_crate.Cursor;
         use type package_crate.Cursor;
      begin
         pcc := stored_packages.Find
           (Key => all_ports (QR.ap_index).package_name);
         if pcc /= package_crate.No_Element then
            stored_packages.Delete (Position => pcc);
         end if;
      end save_package;
   begin
      scan_repository (repository);
      TIO.Put_Line ("Scanned");

      rank_queue.Iterate (save_package'Access);

      declare
         cursor : package_crate.Cursor := stored_packages.First;
         use type package_crate.Cursor;
      begin
         while cursor /= package_crate.No_Element loop
            TIO.Put_Line ("remove " &
                            SU.To_String (package_crate.Key (cursor)));
            cursor := package_crate.Next (cursor);
         end loop;
      end;

   end clean_repository;


   -----------------------
   --  scan_repository  --
   -----------------------
   procedure scan_repository (repository : String)
   is
      pkg_search : AD.Search_Type;
      dirent     : AD.Directory_Entry_Type;
   begin
      stored_packages.Clear;
      AD.Start_Search (Search    => pkg_search,
                       Directory => repository,
                       Filter    => (AD.Ordinary_File => True, others => False),
                       Pattern   => "");
      while AD.More_Entries (Search => pkg_search) loop
         AD.Get_Next_Entry (Search => pkg_search,
                            Directory_Entry => dirent);
         declare
            pkgname  : SU.Unbounded_String :=
              SU.To_Unbounded_String (AD.Simple_Name (dirent));
         begin
            stored_packages.Insert (Key => pkgname, New_Item => False);
         end;
      end loop;
   end scan_repository;

end PortScan.Packages;
