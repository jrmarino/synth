--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Util.Streams.Pipes;
with Util.Streams.Buffered;

package body PortScan.Packages is

   package AD  renames Ada.Directories;
   package STR renames Util.Streams;

   ----------------------------
   --  limited_sanity_check  --
   ----------------------------
   procedure limited_sanity_check (repository : String)
   is
      procedure check_package (cursor : ranking_crate.Cursor);
      procedure check_package (cursor : ranking_crate.Cursor)
      is
         QR   : constant queue_record := ranking_crate.Element (cursor);
         good : Boolean;
      begin
         good := passed_option_check (repository, QR.ap_index);
         if not good then
            TIO.Put_Line (get_catport (all_ports (QR.ap_index)) &
                            " failed option check, removing ...");
         end if;
      end check_package;
   begin
      rank_queue.Iterate (check_package'Access);
   end limited_sanity_check;


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


   ---------------------------
   --  passed_option_check  --
   ---------------------------
   function passed_option_check (repository : String; id : port_id) return
     Boolean
   is
   begin
      if id = port_match_failed or else not all_ports (id).scanned then
         return False;
      end if;
      declare
         fullpath : constant String := repository & "/" &
           SU.To_String (all_ports (id).package_name);
         command  : constant String := "pkg query -F " & fullpath & " %Ok:%Ov";
         pipe     : aliased STR.Pipes.Pipe_Stream;
         buffer   : STR.Buffered.Buffered_Stream;
         content  : SU.Unbounded_String;
         topline  : SU.Unbounded_String;
         status   : Integer;
         pkg_opts : package_crate.Map;
         colon    : Natural;

         use type SU.Unbounded_String;
      begin
         if not AD.Exists (Name => fullpath) then
            return False;
         end if;
         pipe.Open (Command => command);
         buffer.Initialize (Output => null,
                            Input  => pipe'Unchecked_Access,
                            Size   => 4096);
         buffer.Read (Into => content);
         pipe.Close;
         status := pipe.Get_Exit_Status;
         if status /= 0 then
            raise pkgng_execution with "pkg query " &
              SU.To_String (all_ports (id).package_name) &
              " (return code =" & status'Img & ")";
         end if;
         loop
            nextline (lineblock => content, firstline => topline);
            exit when topline = SU.Null_Unbounded_String;
            colon := SU.Index (Source => topline, Pattern => ":");
            if colon < 2 then
               raise unknown_format with SU.To_String (topline);
            end if;
            declare
               knob : String := SU.Slice (Source => topline,
                                          Low    => colon + 1,
                                          High   => SU.Length (topline));
               name : SU.Unbounded_String := SU.To_Unbounded_String
                 (SU.Slice (Source => topline,
                            Low    => 1,
                            High   => colon - 1));
               insres : Boolean;
               dummy  : package_crate.Cursor;
            begin
               if knob = "on" then
                  pkg_opts.Insert (Key      => name,
                                   New_Item => True,
                                   Position => dummy,
                                   Inserted => insres);
               elsif knob = "off" then
                  pkg_opts.Insert (Key      => name,
                                   New_Item => False,
                                   Position => dummy,
                                   Inserted => insres);
               else
                  raise unknown_format
                    with "knob=" & knob & "(" & SU.To_String (topline) & ")";
               end if;
            end;
         end loop;
         declare
            num_opts : Natural := Natural (all_ports (id).options.Length);
            arrow    : package_crate.Cursor;
            arrowkey : SU.Unbounded_String;
            knobval  : Boolean;
            use type package_crate.Cursor;
         begin
            if num_opts /= Natural (pkg_opts.Length) then
               --  Different number of options, FAIL!
               return False;
            end if;
            arrow := pkg_opts.First;
            while arrow /= package_crate.No_Element loop
               arrowkey := package_crate.Key (arrow);
               knobval  := pkg_opts.Element (arrowkey);
               if all_ports (id).options.Contains (arrowkey) then
                  if knobval /= all_ports (id).options.Element (arrowkey) then
                     --  port option value doesn't match package option value
                     return False;
                  end if;
               else
                  --  Name of package option not found in port options
                  return False;
               end if;
               arrow := package_crate.Next (arrow);
            end loop;
         end;
         --  If we get this far, the package options must match port options
         return True;
      end;


   end passed_option_check;

   ---------------
   --  nextline  --
   ----------------
   procedure nextline (lineblock, firstline : out SU.Unbounded_String)
   is
      CR_loc : Natural;
      CR : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      --  As long as the string isn't empty, we'll find a carriage return
      if SU.Length (lineblock) = 0 then
         firstline := SU.Null_Unbounded_String;
         return;
      end if;
      CR_loc := SU.Index (Source => lineblock, Pattern => CR);
      firstline := SU.To_Unbounded_String (Source => SU.Slice
                   (Source => lineblock, Low => 1, High => CR_loc - 1));
      SU.Delete (Source => lineblock, From => 1, Through => CR_loc);
   end nextline;

end PortScan.Packages;
