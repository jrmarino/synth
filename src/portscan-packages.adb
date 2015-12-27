--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Util.Streams.Pipes;
with Util.Streams.Buffered;
with PortScan.Ops;

package body PortScan.Packages is

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
           JT.USS (all_ports (QR.ap_index).package_name);
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
                                    pkg_exists, pkg_removed : out Boolean)
   is
   begin
      pkg_exists  := False;
      pkg_removed := False;
      if id = port_match_failed then
         return;
      end if;
      declare
         fullpath : constant String := repository & "/" &
           JT.USS (all_ports (id).package_name);
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
         good := passed_abi_check (repository, id, True);
         if not good then
            TIO.Put_Line (get_catport (all_ports (id)) &
                            " failed architecture (ABI) check, removing ...");
            goto remove_package;
         end if;
         pkg_exists := True;
         return;

         <<remove_package>>
         pkg_removed := True;
         AD.Delete_File (fullpath);
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
      clean_pass    : Boolean := False;

      procedure check_package (cursor : ranking_crate.Cursor)
      is
         QR : constant queue_record := ranking_crate.Element (cursor);
         package_in_place : Boolean;
         package_removed  : Boolean;
      begin
         limited_package_check (repository  => repository,
                                id          => QR.ap_index,
                                pkg_exists  => package_in_place,
                                pkg_removed => package_removed);
         if package_removed then
            clean_pass := False;
         end if;
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
      establish_package_architecture;
      while not clean_pass loop
         clean_pass := True;
         already_built.Clear;
         rank_queue.Iterate (check_package'Access);
      end loop;
      already_built.Iterate (prune_queue'Access);
   end limited_sanity_check;


   ------------------------
   --  clean_repository  --
   ------------------------
   procedure clean_repository (repository : String)
   is
      procedure mark_package (cursor : ranking_crate.Cursor);
      procedure kill_package (cursor : package_crate.Cursor);

      procedure mark_package (cursor : ranking_crate.Cursor)
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
      end mark_package;

      procedure kill_package (cursor : package_crate.Cursor)
      is
         pkgname : constant String := JT.USS (package_crate.Key (cursor));
      begin
         TIO.Put_Line ("Removed: " & pkgname);
         AD.Delete_File (repository & "/" & pkgname);
      exception
         when others =>
            TIO.Put_Line ("         Failed to remove " & pkgname);
      end kill_package;
   begin
      --  The entire tree must have been scanned *before* this procedure
      --  is executed (see Portscan.scan_entire_ports_tree)
      scan_repository (repository);
      rank_queue.Iterate (mark_package'Access);
      stored_packages.Iterate (kill_package'Access);
      stored_packages.Clear;
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
            pkgname  : JT.Text := JT.SUS (AD.Simple_Name (dirent));
         begin
            stored_packages.Insert (Key => pkgname, New_Item => False);
         end;
      end loop;
   end scan_repository;


   -----------------------------
   -- generic_system_command  --
   -----------------------------
   function generic_system_command (command : String) return JT.Text
   is
      pipe    : aliased STR.Pipes.Pipe_Stream;
      buffer  : STR.Buffered.Buffered_Stream;
      content : JT.Text;
      status  : Integer;
   begin
      pipe.Open (Command => command);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;
      status := pipe.Get_Exit_Status;
      if status /= 0 then
         raise pkgng_execution with "pkg options query cmd: " & command &
           " (return code =" & status'Img & ")";
      end if;
      return content;
   end generic_system_command;


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
           JT.USS (all_ports (id).package_name);
         command  : constant String := "pkg query -F " & fullpath & " %Ok:%Ov";
         content  : JT.Text;
         topline  : JT.Text;
         colon    : Natural;
         required : Natural := Natural (all_ports (id).options.Length);
         counter  : Natural := 0;
      begin
         if not skip_exist_check and then not AD.Exists (Name => fullpath)
         then
            return False;
         end if;
         content := generic_system_command (command);
         loop
            JT.nextline (lineblock => content, firstline => topline);
            exit when JT.IsBlank (topline);
            colon := JT.SU.Index (Source => topline, Pattern => ":");
            if colon < 2 then
               raise unknown_format with JT.USS (topline);
            end if;
            declare
               knob : String := JT.SU.Slice (Source => topline,
                                             Low    => colon + 1,
                                             High   => JT.SU.Length (topline));
               namekey : JT.Text := JT.SUS (JT.SU.Slice (Source => topline,
                                                         Low    => 1,
                                                         High   => colon - 1));
               knobval : Boolean;
            begin
               if knob = "on" then
                  knobval := True;
               elsif knob = "off" then
                  knobval := False;
               else
                  raise unknown_format
                    with "knob=" & knob & "(" & JT.USS (topline) & ")";
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
           JT.USS (all_ports (id).package_name);
         command  : constant String := "pkg query -F " & fullpath &
                                       " %do:%dn-%dv";
         content  : JT.Text;
         topline  : JT.Text;
         colon    : Natural;
         required : Natural := Natural (all_ports (id).librun.Length);
         counter  : Natural := 0;
      begin
         if not skip_exist_check and then not AD.Exists (Name => fullpath)
         then
            return False;
         end if;
         content := generic_system_command (command);
         loop
            JT.nextline (lineblock => content, firstline => topline);
            exit when JT.IsBlank (topline);
            colon := JT.SU.Index (Source => topline, Pattern => ":");
            if colon < 2 then
               raise unknown_format with JT.USS (topline);
            end if;
            declare
               deppkg : String := JT.SU.Slice (Source => topline,
                                               Low    => colon + 1,
                                               High   => JT.SU.Length (topline))
                 & ".txz";
               origin : JT.Text := JT.SUS (JT.SU.Slice (Source => topline,
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
               if deppkg /= JT.USS (all_ports (target_id).package_name)
               then
                  --  The version that the package requires differs from the
                  --  version that the ports tree will now produce
                  return False;
               end if;
               if not AD.Exists (repository & "/" & JT.USS
                                 (all_ports (target_id).package_name))
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


   ------------------------
   --  passed_abi_check  --
   ------------------------
   function passed_abi_check (repository : String; id : port_id;
                              skip_exist_check : Boolean := False)
                              return Boolean
   is
      fullpath : constant String := repository & "/" &
                 JT.USS (all_ports (id).package_name);
      command  : constant String := "pkg query -F " & fullpath & " %q";
      content  : JT.Text;
      topline  : JT.Text;
   begin
      if not skip_exist_check and then not AD.Exists (Name => fullpath)
      then
         return False;
      end if;
      content := generic_system_command (command);
      JT.nextline (lineblock => content, firstline => topline);
      if JT.equivalent (topline, calculated_abi) then
         return True;
      end if;
      if JT.equivalent (topline, calc_abi_noarch) then
         return True;
      end if;
      if JT.equivalent (topline, calculated_alt_abi) then
         return True;
      end if;
      if JT.equivalent (topline, calc_alt_abi_noarch) then
         return True;
      end if;
      return False;
   end passed_abi_check;


   ----------------------
   --  queue_is_empty  --
   ----------------------
   function queue_is_empty return Boolean is
   begin
      return rank_queue.Is_Empty;
   end queue_is_empty;


   --------------------------------------
   --  establish_package_architecture  --
   --------------------------------------
   procedure establish_package_architecture
   is
      function suffix (arch : String) return String;
      function even (group : String) return String;

      command : constant String := JT.USS (PM.configuration.dir_system) &
        "/usr/bin/uname -mr";
      UN : JT.Text;

      function suffix (arch : String) return String is
      begin
         if arch = "amd64" or else arch = "x86_64" then
            return "x86:64";
         elsif arch = "i386" then
            return "x86:32";
         else
            return "unknown:" & arch;
         end if;
      end suffix;
      function even (group : String) return String
      is
         len : constant Natural := group'Length;
         zzz : constant String (1 .. len) := group;
         let : constant Character := zzz (len);
         res : String := zzz;
      begin
         case let is
         when '1' => res (len) := '2';
         when '3' => res (len) := '4';
         when '5' => res (len) := '6';
         when '7' => res (len) := '8';
         when '9' => res (len) := '0';
         when others => null;
         end case;
         return res;
      end even;

   begin
      UN := generic_system_command (command);
      if JT.equivalent (PM.configuration.operating_sys, "DragonFly") then
         declare
            dfly    : constant String := "dragonfly:";
            ndxdot  : constant Natural := JT.SU.Index (UN, ".");
            ndxspc  : constant Natural := JT.SU.Index (UN, " ");
            ndxdash : constant Natural := JT.SU.Index (UN, "-");
            unlen   : constant Natural := JT.SU.Length (UN) - 1;
         begin
            calculated_abi := JT.SUS (dfly);
            JT.SU.Append (calculated_abi,
                          JT.SU.Slice (UN, 1, ndxdot - 1) & ".");
            JT.SU.Append (calculated_abi, even
                          (JT.SU.Slice (UN, ndxdot + 1, ndxdash - 1)) & ":");
            calc_abi_noarch := calculated_abi;
            JT.SU.Append (calculated_abi, suffix
                          (JT.SU.Slice (UN, ndxspc + 1, unlen)));
            JT.SU.Append (calc_abi_noarch, "*");
            calculated_alt_abi  := calculated_abi;
            calc_alt_abi_noarch := calc_abi_noarch;
         end;
      else
         declare
            fbsd1   : constant String := "FreeBSD:";
            fbsd2   : constant String := "freebsd:";
            ndxdot  : Natural := JT.SU.Index (UN, ".");
            ndxspc  : Natural := JT.SU.Index (UN, " ");
            unlen   : constant Natural := JT.SU.Length (UN) - 1;
         begin
            calculated_abi     := JT.SUS (fbsd1);
            calculated_alt_abi := JT.SUS (fbsd2);
            JT.SU.Append (calculated_abi,
                          JT.SU.Slice (UN, 1, ndxdot - 1) & ".");
            JT.SU.Append (calculated_alt_abi,
                          JT.SU.Slice (UN, 1, ndxdot - 1) & ".");
            calc_abi_noarch     := calculated_abi;
            calc_alt_abi_noarch := calculated_alt_abi;
            JT.SU.Append (calculated_abi,
                          JT.SU.Slice (UN, ndxspc + 1, unlen));
            JT.SU.Append (calculated_alt_abi, suffix
                          (JT.SU.Slice (UN, ndxspc + 1, unlen)));
            JT.SU.Append (calc_abi_noarch, "*");
            JT.SU.Append (calc_alt_abi_noarch, "*");
         end;
      end if;
   end establish_package_architecture;


end PortScan.Packages;
