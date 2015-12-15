--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with PortScan.Ops;

package body PortScan.Packages is

   package AD  renames Ada.Directories;
   package STR renames Util.Streams;
   package OPS renames PortScan.Ops;


   ---------------------------
   --  wipe_out_repository  --
   ---------------------------
   procedure wipe_out_repository (repository : String)
   is
      pkg_search : AD.Search_Type;
      dirent     : AD.Directory_Entry_Type;
   begin
      stored_packages.Clear;
      AD.Start_Search (Search    => pkg_search,
                       Directory => repository,
                       Filter    => (AD.Ordinary_File => True, others => False),
                       Pattern   => "*.txz");
      while AD.More_Entries (Search => pkg_search) loop
         AD.Get_Next_Entry (Search => pkg_search,
                            Directory_Entry => dirent);
         declare
            pkgname  : String := repository & "/" & AD.Simple_Name (dirent);
         begin
            AD.Delete_File (pkgname);
         end;
      end loop;
   end wipe_out_repository;


   -----------------------------
   --  remove_queue_packages  --
   -----------------------------
   procedure remove_queue_packages (repository : String)
   is
      procedure remove_package (cursor : ranking_crate.Cursor);
      procedure remove_package (cursor : ranking_crate.Cursor)
      is
         QR       : constant queue_record := ranking_crate.Element (cursor);
         fullpath : constant String := repository & "/" &
           SU.To_String (all_ports (QR.ap_index).package_name);
      begin
         if AD.Exists (fullpath) then
            AD.Delete_File (fullpath);
         end if;
      end remove_package;
   begin
      rank_queue.Iterate (remove_package'Access);
   end remove_queue_packages;


   ------------------------------
   --  limited_package_check   --
   ------------------------------
   procedure limited_package_check (repository : String; id : port_id;
                                    pkg_exists : out Boolean)
   is
   begin
      pkg_exists := False;
      if id = port_match_failed then
         return;
      end if;
      declare
         fullpath : constant String := repository & "/" &
           SU.To_String (all_ports (id).package_name);
         good : Boolean;
      begin
         if not AD.Exists (fullpath) then
            return;
         end if;
         good := passed_option_check (repository, id, True);
         if not good then
            TIO.Put_Line (get_catport (all_ports (id)) &
                            " failed option check, removing ...");
            goto remove_package;
         end if;
         good := passed_dependency_check (repository, id, True);
         if not good then
            TIO.Put_Line (get_catport (all_ports (id)) &
                            " failed dependency check, removing ...");
            goto remove_package;
         end if;
         pkg_exists := True;
         return;

         <<remove_package>>
         pkg_exists := False;
         --  TODO: remove package
      end;
   end limited_package_check;


   ----------------------------
   --  limited_sanity_check  --
   ----------------------------
   procedure limited_sanity_check (repository : String)
   is
      procedure check_package (cursor : ranking_crate.Cursor);
      procedure prune_queue (cursor : subqueue.Cursor);
      already_built : subqueue.Vector;

      procedure check_package (cursor : ranking_crate.Cursor)
      is
         QR : constant queue_record := ranking_crate.Element (cursor);
         package_in_place : Boolean;
      begin
         limited_package_check (repository => repository, id => QR.ap_index,
                                pkg_exists => package_in_place);
         if package_in_place then
            already_built.Append (New_Item => QR.ap_index);
         end if;
      end check_package;

      procedure prune_queue (cursor : subqueue.Cursor)
      is
         id : constant port_index := subqueue.Element (cursor);
      begin
         OPS.cascade_successful_build (id);
      end prune_queue;
   begin
      rank_queue.Iterate (check_package'Access);
      already_built.Iterate (prune_queue'Access);
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
                       Pattern   => "*.txz");
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
   function passed_option_check (repository : String; id : port_id;
                                 skip_exist_check : Boolean := False)
                                 return Boolean
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
         colon    : Natural;
         required : Natural := Natural (all_ports (id).options.Length);
         counter  : Natural := 0;

         use type SU.Unbounded_String;
      begin
         if not skip_exist_check and then not AD.Exists (Name => fullpath)
         then
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
            raise pkgng_execution with "pkg options query " &
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
               namekey : SU.Unbounded_String := SU.To_Unbounded_String
                 (SU.Slice (Source => topline,
                            Low    => 1,
                            High   => colon - 1));
               knobval  : Boolean;
            begin
               if knob = "on" then
                  knobval := True;
               elsif knob = "off" then
                  knobval := False;
               else
                  raise unknown_format
                    with "knob=" & knob & "(" & SU.To_String (topline) & ")";
               end if;
               counter := counter + 1;
               if counter > required then
                  --  package has more options than we are looking for
                  return False;
               end if;
               if all_ports (id).options.Contains (namekey) then
                  if knobval /= all_ports (id).options.Element (namekey) then
                     --  port option value doesn't match package option value
                     return False;
                  end if;
               else
                  --  Name of package option not found in port options
                  return False;
               end if;
            end;
         end loop;
         if counter < required then
            --  The ports tree has more options than the existing package
            return False;
         end if;

         --  If we get this far, the package options must match port options
         return True;
      end;
   end passed_option_check;


   -------------------------------
   --  passed_dependency_check  --
   -------------------------------
   function passed_dependency_check (repository : String; id : port_id;
                                     skip_exist_check : Boolean := False)
                                     return Boolean
   is
   begin
      if id = port_match_failed or else not all_ports (id).scanned then
         return False;
      end if;
      declare
         fullpath : constant String := repository & "/" &
           SU.To_String (all_ports (id).package_name);
         command  : constant String := "pkg query -F " & fullpath &
                                       " %do:%dn-%dv";
         pipe     : aliased STR.Pipes.Pipe_Stream;
         buffer   : STR.Buffered.Buffered_Stream;
         content  : SU.Unbounded_String;
         topline  : SU.Unbounded_String;
         status   : Integer;
         colon    : Natural;
         required : Natural := Natural (all_ports (id).librun.Length);
         counter  : Natural := 0;

         use type SU.Unbounded_String;
      begin
         if not skip_exist_check and then not AD.Exists (Name => fullpath)
         then
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
            raise pkgng_execution with "pkg depends query " &
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
               deppkg : String := SU.Slice (Source => topline,
                                            Low    => colon + 1,
                                            High   => SU.Length (topline))
                                  & ".txz";
               origin : SU.Unbounded_String := SU.To_Unbounded_String
                 (SU.Slice (Source => topline,
                            Low    => 1,
                            High   => colon - 1));
               target_id : port_index := ports_keys.Element (Key => origin);
            begin
               if target_id = port_match_failed then
                  --  package has a dependency that has been removed from
                  --  the ports tree
                  return False;
               end if;
               counter := counter + 1;
               if counter > required then
                  --  package has more dependencies than we are looking for
                  return False;
               end if;
               if deppkg /= SU.To_String (all_ports (target_id).package_name)
               then
                  --  The version that the package requires differs from the
                  --  version that the ports tree will now produce
                  return False;
               end if;
               if not AD.Exists (repository & "/" & SU.To_String (
                                   all_ports (target_id).package_name))
               then
                  --  Even if all the versions are matching, we still need
                  --  the package to be in repository.
                  return False;
               end if;
            end;
         end loop;
         if counter < required then
            --  The ports tree requires more dependencies than the existing
            --  package does
            return False;
         end if;

         --  If we get this far, the package dependencies match what the
         --  port tree requires exactly.  This package passed sanity check.
         return True;
      end;
   end passed_dependency_check;


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
