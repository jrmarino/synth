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


   -----------------------------------
   --  passed_initial_package_scan  --
   -----------------------------------
   procedure passed_initial_package_scan (repository : String; id : port_id) is
   begin
      if id = port_match_failed then
         return;
      end if;
      if not all_ports (id).scanned then
         return;
      end  if;
      declare
         pkgname  : constant String := JT.USS (all_ports (id).package_name);
         fullpath : constant String := repository & "/" & pkgname;
      begin
         if AD.Exists (fullpath) then
            all_ports (id).pkg_present := True;
         else
            return;
         end if;
         if not passed_option_check (repository, id, True) then
            TIO.Put_Line (pkgname & " failed option check.");
            all_ports (id).deletion_due := True;
            return;
         end if;
         if not passed_abi_check (repository, id, True) then
            TIO.Put_Line (pkgname & " failed architecture (ABI) check.");
            all_ports (id).deletion_due := True;
            return;
         end if;
      end;
      all_ports (id).pkg_dep_query :=
        result_of_dependency_query (repository, id);

   end passed_initial_package_scan;


   ----------------------------
   --  limited_sanity_check  --
   ----------------------------
   procedure limited_sanity_check (repository : String; dry_run : Boolean)
   is
      procedure prune_packages (cursor : ranking_crate.Cursor);
      procedure check_package (cursor : ranking_crate.Cursor);
      procedure prune_queue (cursor : subqueue.Cursor);

      already_built : subqueue.Vector;
      clean_pass    : Boolean := False;

      procedure check_package (cursor : ranking_crate.Cursor)
      is
         target  : port_id := ranking_crate.Element (cursor).ap_index;
         pkgname : String  := JT.USS (all_ports (target).package_name);
      begin
         if not all_ports (target).pkg_present or else
           all_ports (target).deletion_due
         then
            return;
         end if;

         if not passed_dependency_check
           (query_result => all_ports (target).pkg_dep_query, id => target)
         then
            TIO.Put_Line (pkgname & " failed dependency check.");
            all_ports (target).deletion_due := True;
            clean_pass := False;
         else
            already_built.Append (New_Item => target);
         end if;
      end check_package;
      procedure prune_queue (cursor : subqueue.Cursor)
      is
         id : constant port_index := subqueue.Element (cursor);
      begin
         OPS.cascade_successful_build (id);
      end prune_queue;

      procedure prune_packages (cursor : ranking_crate.Cursor)
      is
         target    : port_id := ranking_crate.Element (cursor).ap_index;
         delete_it : Boolean := all_ports (target).deletion_due;
         pkgname   : String  := JT.USS (all_ports (target).package_name);
         fullpath  : constant String := repository & "/" & pkgname;
      begin
         if delete_it then
            AD.Delete_File (fullpath);
         end if;
      exception
         when others => null;
      end prune_packages;
   begin
      establish_package_architecture;
      original_queue_len := rank_queue.Length;
      for m in scanners'Range loop
         mq_progress (m) := 0;
      end loop;
      parallel_package_scan (repository);

      while not clean_pass loop
         clean_pass := True;
         already_built.Clear;
         rank_queue.Iterate (check_package'Access);
      end loop;
      if not dry_run then
         rank_queue.Iterate (prune_packages'Access);
      end if;
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


   ----------------------------------
   --  result_of_dependency_query  --
   ----------------------------------
   function result_of_dependency_query (repository : String; id : port_id)
                                        return JT.Text
   is
      fullpath : constant String := repository & "/" &
                 JT.USS (all_ports (id).package_name);
      command  : constant String := "pkg query -F " & fullpath & " %do:%dn-%dv";
   begin
      return generic_system_command (command);
   exception
      when others => return JT.blank;
   end result_of_dependency_query;


   -------------------------------
   --  passed_dependency_check  --
   -------------------------------
   function passed_dependency_check (query_result : JT.Text; id : port_id)
                                     return Boolean is
   begin
      declare
         content  : JT.Text := query_result;
         topline  : JT.Text;
         colon    : Natural;
         required : Natural := Natural (all_ports (id).librun.Length);
         counter  : Natural := 0;
      begin
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
               target_pkg : JT.Text := all_ports (target_id).package_name;
            begin
               if target_id = port_match_failed then
                  --  package has a dependency that has been removed from
                  --  the ports tree
                  if debug_dep_check then
                     TIO.Put_Line (JT.USS (origin) &
                                  " has been removed from the ports tree");
                  end if;
                  return False;
               end if;
               counter := counter + 1;
               if counter > required then
                  --  package has more dependencies than we are looking for
                  if debug_dep_check then
                     if debug_dep_check then
                        TIO.Put_Line ("Package has more dependencies than " &
                                        "the port requires (" &
                                        JT.int2str (required) & ")");
                        TIO.Put_Line ("Query: " & JT.USS (query_result));
                        TIO.Put_Line ("Tripped on: " & JT.USS (target_pkg) &
                                        ":" & JT.USS (origin));
                     end if;
                  end if;
                  return False;
               end if;
               if deppkg /= JT.USS (target_pkg)
               then
                  --  The version that the package requires differs from the
                  --  version that the ports tree will now produce
                  if debug_dep_check then
                     TIO.Put_Line (JT.USS (target_pkg) & " is an older " &
                        "version, we need " & deppkg);
                  end if;
                  return False;
               end if;
               if not all_ports (target_id).pkg_present or else
                 all_ports (target_id).deletion_due
               then
                  --  Even if all the versions are matching, we still need
                  --  the package to be in repository.
                  if debug_dep_check then
                     TIO.Put_Line (JT.USS (target_pkg) & " doesn't exist or " &
                        " has been slated for deletion");
                  end if;
                  return False;
               end if;
            end;
         end loop;
         if counter < required then
            --  The ports tree requires more dependencies than the existing
            --  package does
            if debug_dep_check then
               TIO.Put_Line ("Package has less dependencies than the port " &
                               "requires (" & JT.int2str (required) & ")");
               TIO.Put_Line ("Query: " & JT.USS (query_result));
            end if;
            return False;
         end if;

         --  If we get this far, the package dependencies match what the
         --  port tree requires exactly.  This package passed sanity check.
         return True;
      end;
   exception
      when others => return False;
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


   ---------------------------
   --  original_queue_size  --
   ---------------------------
   function original_queue_size return Natural is
   begin
      return Natural (original_queue_len);
   end original_queue_size;


   ----------------------------------
   --  passed_options_cache_check  --
   ----------------------------------
   function passed_options_cache_check (id : port_id) return Boolean
   is
      target_dname : String := JT.replace (get_catport (all_ports (id)),
                                           reject => LAT.Solidus,
                                           shiny  => LAT.Low_Line);
      target_path : constant String := JT.USS (PM.configuration.dir_options) &
        LAT.Solidus & target_dname & LAT.Solidus & "options";
      marker      : constant String := "+=";
      option_file : TIO.File_Type;
      required    : Natural := Natural (all_ports (id).options.Length);
      counter     : Natural := 0;
      result      : Boolean := False;
   begin
      if not AD.Exists (target_path) then
         return True;
      end if;
      TIO.Open (File => option_file,
                Mode => TIO.In_File,
                Name => target_path);
      while not TIO.End_Of_File (option_file) loop
         declare
            Line    : String := TIO.Get_Line (option_file);
            namekey : JT.Text;
            valid   : Boolean := False;
         begin
            --  If "marker" starts at 17, it's OPTIONS_FILES_SET
            --  if "marker" starts at 19, it's OPTIONS_FILES_UNSET
            --  if neither, we don't care.

            if Line (17 .. 18) = marker then
               namekey := JT.SUS (Line (19 .. Line'Last));
               valid   := True;
            elsif Line (19 .. 20) = marker then
               namekey := JT.SUS (Line (21 .. Line'Last));
               valid   := True;
            end if;
            if valid then
               counter := counter + 1;
               if counter > required then
                  --  The port used to have more options, abort!
                  goto clean_exit;
               end if;
               if not all_ports (id).options.Contains (namekey) then
                  --  cached option not found in port anymore, abort!
                  goto clean_exit;
               end if;
            end if;
         end;
      end loop;
      if counter = required then
         result := True;
      end if;

      <<clean_exit>>
      TIO.Close (option_file);
      return result;

   end passed_options_cache_check;


   ------------------------------------
   --  limited_cached_options_check  --
   ------------------------------------
   function limited_cached_options_check return Boolean
   is
      procedure check_port (cursor : ranking_crate.Cursor);
      fail_count : Natural := 0;
      first_fail : queue_record;

      procedure check_port (cursor : ranking_crate.Cursor)
      is
         QR : constant queue_record := ranking_crate.Element (cursor);
         id : port_index := QR.ap_index;
         prelude : constant String := "Cached options obsolete: ";
      begin
         if not passed_options_cache_check (id) then
            if fail_count = 0 then
               first_fail := QR;
            end if;
            fail_count := fail_count + 1;
            TIO.Put_Line (prelude & get_catport (all_ports (id)));
         end if;
      end check_port;
   begin
      rank_queue.Iterate (Process => check_port'Access);
      if fail_count > 0 then
         TIO.Put (LAT.LF & "A preliminary scan has revealed the cached " &
                 "options of");
         if fail_count = 1 then
            TIO.Put_Line (" one port are");
         else
            TIO.Put_Line (fail_count'Img & " ports are");
         end if;
         TIO.Put_Line ("obsolete.  Please update or removed the saved " &
                         "options and try again.");
         declare
            portsdir : String := JT.USS (PM.configuration.dir_portsdir) &
                                 LAT.Solidus;
            catport  : String := get_catport (all_ports (first_fail.ap_index));
         begin
            TIO.Put_Line ("  e.g. make -C " & portsdir & catport &  " config");
            TIO.Put_Line ("  e.g. make -C " & portsdir & catport & " rmconfig");
         end;
      end if;
      return (fail_count = 0);
   end limited_cached_options_check;


   -----------------------------
   --  parallel_package_scan  --
   -----------------------------
   procedure parallel_package_scan (repository : String)
   is
      task type scan (lot : scanners);
      finished : array (scanners) of Boolean := (others => False);
      combined_wait : Boolean := True;
      label_shown   : Boolean := False;

      task body scan
      is
         procedure populate (cursor : subqueue.Cursor);
         procedure populate (cursor : subqueue.Cursor)
         is
            target_port : port_index := subqueue.Element (cursor);
         begin
            passed_initial_package_scan (repository, target_port);
            mq_progress (lot) := mq_progress (lot) + 1;
         end populate;
      begin
         make_queue (lot).Iterate (populate'Access);
         finished (lot) := True;
      end scan;

      scan_01 : scan (lot => 1);
      scan_02 : scan (lot => 2);
      scan_03 : scan (lot => 3);
      scan_04 : scan (lot => 4);
      scan_05 : scan (lot => 5);
      scan_06 : scan (lot => 6);
      scan_07 : scan (lot => 7);
      scan_08 : scan (lot => 8);
      scan_09 : scan (lot => 9);
      scan_10 : scan (lot => 10);
      scan_11 : scan (lot => 11);
      scan_12 : scan (lot => 12);
      scan_13 : scan (lot => 13);
      scan_14 : scan (lot => 14);
      scan_15 : scan (lot => 15);
      scan_16 : scan (lot => 16);
      scan_17 : scan (lot => 17);
      scan_18 : scan (lot => 18);
      scan_19 : scan (lot => 19);
      scan_20 : scan (lot => 20);
      scan_21 : scan (lot => 21);
      scan_22 : scan (lot => 22);
      scan_23 : scan (lot => 23);
      scan_24 : scan (lot => 24);
      scan_25 : scan (lot => 25);
      scan_26 : scan (lot => 26);
      scan_27 : scan (lot => 27);
      scan_28 : scan (lot => 28);
      scan_29 : scan (lot => 29);
      scan_30 : scan (lot => 30);
      scan_31 : scan (lot => 31);
      scan_32 : scan (lot => 32);
   begin
      while combined_wait loop
         delay 1.0;
         combined_wait := False;
         for j in scanners'Range loop
            if not finished (j) then
               combined_wait := True;
               exit;
            end if;
         end loop;
         if combined_wait then
            if not label_shown then
               label_shown := True;
               TIO.Put_Line ("Scanning existing packages.");
            end if;
            TIO.Put (scan_progress);
         end if;
      end loop;
   end parallel_package_scan;

end PortScan.Packages;
