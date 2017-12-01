--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Hash;
with Ada.Calendar.Formatting;
with GNAT.Regpat;
with GNAT.String_Split;
with Signals;
with Unix;

package body PortScan is

   package ACF renames Ada.Calendar.Formatting;
   package RGX renames GNAT.Regpat;
   package GSS renames GNAT.String_Split;
   package SIG renames Signals;


   ------------------------------
   --  scan_entire_ports_tree  --
   ------------------------------
   function scan_entire_ports_tree (portsdir : String) return Boolean
   is
      good_scan  : Boolean;
      using_screen : constant Boolean := Unix.screen_attached;
   begin
      --  tree must be already mounted in the scan slave.
      --  However, prescan works on the real ports tree, not the mount.
      if not prescanned then
         read_flavor_index;
      end if;
      scan_start := CAL.Clock;
      parallel_deep_scan (success => good_scan, show_progress => using_screen);
      scan_stop := CAL.Clock;

      return good_scan;
   end scan_entire_ports_tree;


   ------------------------
   --  scan_single_port  --
   ------------------------
   function scan_single_port (catport : String; always_build : Boolean;
                              fatal : out Boolean)
                              return Boolean
   is
      xports : constant String := JT.USS (PM.configuration.dir_buildbase) &
                                  ss_base & dir_ports;

      procedure dig (cursor : block_crate.Cursor);
      target    : port_index;
      aborted   : Boolean := False;
      indy500   : Boolean := False;
      uscatport : JT.Text := JT.SUS (catport);

      procedure dig (cursor : block_crate.Cursor)
      is
         new_target : port_index := block_crate.Element (cursor);
      begin
         if not aborted then
            if all_ports (new_target).scan_locked then
               --  We've already seen this (circular dependency)
               raise circular_logic;
            end if;
            if not all_ports (new_target).scanned then
               populate_port_data (new_target);
               all_ports (new_target).scan_locked := True;
               all_ports (new_target).blocked_by.Iterate (dig'Access);
               all_ports (new_target).scan_locked := False;
               if indy500 then
                  TIO.Put_Line ("... backtrace " &
                                  get_catport (all_ports (new_target)));
               end if;
            end if;
         end if;
      exception
         when issue : nonexistent_port =>
            aborted := True;
            TIO.Put_Line (LAT.LF & get_catport (all_ports (new_target)) &
                            " scan aborted because dependency could " &
                            "not be located.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : bmake_execution =>
            aborted := True;
            TIO.Put_Line (LAT.LF & get_catport (all_ports (new_target)) &
                            " scan aborted because 'make' encounted " &
                            "an error in the Makefile.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : make_garbage =>
            aborted := True;
            TIO.Put_Line (LAT.LF & get_catport (all_ports (new_target)) &
                            " scan aborted because dependency is malformed.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : circular_logic =>
            aborted := True;
            indy500 := True;
            TIO.Put_Line (LAT.LF & catport &
                            " scan aborted because a circular dependency on " &
                            get_catport (all_ports (new_target)) &
                            " was detected.");
         when issue : others =>
            aborted := True;
            declare
               why : constant String := obvious_problem
                 (xports, get_catport (all_ports (new_target)));
            begin
               if why = "" then
                  TIO.Put_Line (LAT.LF & get_catport (all_ports (new_target)) &
                                  " scan aborted for an unknown reason.");
                  TIO.Put_Line (EX.Exception_Message (issue));
               else
                  TIO.Put_Line (LAT.LF & get_catport (all_ports (new_target)) &
                                  " scan aborted" & why);
               end if;
            end;
      end dig;
   begin
      fatal := False;
      if not AD.Exists (xports & "/" & catport & "/Makefile") then
         return False;
      end if;
      if not prescanned then
         read_flavor_index;
      end if;
      if ports_keys.Contains (Key => uscatport) then
         target := ports_keys.Element (Key => uscatport);
      else
         return False;
      end if;
      begin
         if all_ports (target).scanned then
            --  This can happen when a dependency is also on the build list.
            return True;
         else
            populate_port_data (target);
            all_ports (target).never_remote := always_build;
         end if;
      exception
         when issue : others =>
            TIO.Put ("Encountered issue with " & catport &
                       " or its dependencies" & LAT.LF & "  => ");
            TIO.Put_Line (EX.Exception_Message (issue));
            return False;
      end;
      all_ports (target).scan_locked := True;
      all_ports (target).blocked_by.Iterate (dig'Access);
      all_ports (target).scan_locked := False;
      if indy500 then
         TIO.Put_Line ("... backtrace " & catport);
         fatal := True;
      end if;
      return not aborted;

   end scan_single_port;


   --------------------------
   --  set_build_priority  --
   --------------------------
   procedure set_build_priority is
   begin
      iterate_reverse_deps;
      iterate_drill_down;
   end set_build_priority;


   ------------------------
   --  reset_ports_tree  --
   ------------------------
   procedure reset_ports_tree
   is
      PR : port_record_access;
   begin
      for k in dim_all_ports'Range loop
         PR  := all_ports (k)'Access;

         PR.sequence_id   := 0;
         PR.key_cursor    := portkey_crate.No_Element;
         PR.jobs          := 1;
         PR.ignore_reason := JT.blank;
         PR.port_version  := JT.blank;
         PR.package_name  := JT.blank;
         PR.pkg_dep_query := JT.blank;
         PR.ignored       := False;
         PR.scanned       := False;
         PR.rev_scanned   := False;
         PR.unlist_failed := False;
         PR.work_locked   := False;
         PR.scan_locked   := False;
         PR.pkg_present   := False;
         PR.remote_pkg    := False;
         PR.never_remote  := False;
         PR.deletion_due  := False;
         PR.use_procfs    := False;
         PR.use_linprocfs := False;
         PR.reverse_score := 0;
         PR.min_librun    := 0;
         PR.librun.Clear;
         PR.blocks.Clear;
         PR.blocked_by.Clear;
         PR.all_reverse.Clear;
         PR.options.Clear;
         PR.flavors.Clear;
      end loop;
      ports_keys.Clear;
      rank_queue.Clear;
      lot_number  := 1;
      lot_counter := 0;
      last_port   := 0;
      prescanned  := False;
      wipe_make_queue;
      for m in scanners'Range loop
         mq_progress (m) := 0;
      end loop;
   end reset_ports_tree;


   --  PRIVATE FUNCTIONS  --


   --------------------------
   --  iterate_drill_down  --
   --------------------------
   procedure iterate_drill_down is
   begin

      rank_queue.Clear;
      for port in port_index'First .. last_port loop
         if all_ports (port).scanned then
            drill_down (next_target => port, original_target => port);
            declare
               ndx : constant port_index :=
                 port_index (all_ports (port).reverse_score);
               QR  : constant queue_record :=
                 (ap_index      => port,
                  reverse_score => ndx);
            begin
               rank_queue.Insert (New_Item => QR);
            end;
         end if;
      end loop;

   end iterate_drill_down;


   --------------------------
   --  parallel_deep_scan  --
   --------------------------
   procedure parallel_deep_scan (success : out Boolean; show_progress : Boolean)
   is
      finished : array (scanners) of Boolean := (others => False);
      combined_wait : Boolean := True;
      aborted : Boolean := False;

      task type scan (lot : scanners);
      task body scan
      is
         procedure populate (cursor : subqueue.Cursor);
         procedure populate (cursor : subqueue.Cursor)
         is
            target_port : port_index := subqueue.Element (cursor);
         begin
            if not aborted then
               populate_port_data (target_port);
               mq_progress (lot) := mq_progress (lot) + 1;
            end if;
         exception
            when issue : others =>
               TIO.Put_Line (LAT.LF & "culprit: " &
                               get_catport (all_ports (target_port)));
               EX.Reraise_Occurrence (issue);
         end populate;
      begin
         make_queue (lot).Iterate (populate'Access);
         finished (lot) := True;
      exception
         when issue : nonexistent_port =>
            aborted := True;
            TIO.Put_Line ("Scan aborted because dependency could " &
                            "not be located.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : bmake_execution =>
            aborted := True;
            TIO.Put_Line ("Scan aborted because 'make' encounted " &
                            "an error in the Makefile.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : make_garbage =>
            aborted := True;
            TIO.Put_Line ("Scan aborted because dependency is malformed.");
            TIO.Put_Line (EX.Exception_Message (issue));
         when issue : others =>
            aborted := True;
            TIO.Put_Line ("Scan aborted for an unknown reason.");
            TIO.Put_Line (EX.Exception_Message (issue));
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
      TIO.Put_Line ("Scanning entire ports tree.");
      while combined_wait loop
         delay 1.0;
         if show_progress then
            TIO.Put (scan_progress);
         end if;
         combined_wait := False;
         for j in scanners'Range loop
            if not finished (j) then
               combined_wait := True;
               exit;
            end if;
         end loop;
         if SIG.graceful_shutdown_requested then
            aborted := True;
         end if;
      end loop;
      success := not aborted;
   end parallel_deep_scan;


   -----------------------
   --  wipe_make_queue  --
   -----------------------
   procedure wipe_make_queue is
   begin
      for j in scanners'Range loop
         make_queue (j).Clear;
      end loop;
   end wipe_make_queue;


   ------------------
   --  drill_down  --
   ------------------
   procedure drill_down (next_target     : port_index;
                         original_target : port_index)
   is
      PR : port_record_access := all_ports (next_target)'Access;

      procedure stamp_and_drill (cursor : block_crate.Cursor);
      procedure slurp_scanned (cursor : block_crate.Cursor);

      procedure slurp_scanned (cursor : block_crate.Cursor)
      is
         rev_id  : port_index := block_crate.Element (Position => cursor);
      begin
         if not all_ports (original_target).all_reverse.Contains (rev_id) then
            all_ports (original_target).all_reverse.Insert
              (Key      => rev_id,
               New_Item => rev_id);
         end if;
      end slurp_scanned;

      procedure stamp_and_drill (cursor : block_crate.Cursor)
      is
         pmc : port_index := block_crate.Element (Position => cursor);
      begin
         if not all_ports (original_target).all_reverse.Contains (pmc) then
            all_ports (original_target).all_reverse.Insert
              (Key      => pmc,
               New_Item => pmc);
         end if;
         if pmc = original_target then
            declare
               top_port : constant String :=
                 get_catport (all_ports (original_target));
               this_port : constant String :=
                 get_catport (all_ports (next_target));
            begin
               raise circular_logic with top_port & " <=> " & this_port;
            end;
         end if;

         if not all_ports (pmc).rev_scanned then
            drill_down (next_target => pmc, original_target => pmc);
         end if;
         all_ports (pmc).all_reverse.Iterate (slurp_scanned'Access);
      end stamp_and_drill;

   begin
      if not PR.scanned then
         return;
      end if;
      if PR.rev_scanned then
         --  It is possible to get here if an earlier port scanned this port
         --  as a reverse dependencies
         return;
      end if;
      PR.blocks.Iterate (stamp_and_drill'Access);
      PR.reverse_score := port_index (PR.all_reverse.Length);
      PR.rev_scanned := True;
   end drill_down;


   ----------------------------
   --  iterate_reverse_deps  --
   -----------------------------
   procedure iterate_reverse_deps
   is
      madre : port_index;
      procedure set_reverse (cursor : block_crate.Cursor);
      procedure set_reverse (cursor : block_crate.Cursor) is
      begin
         --  Using conditional insert here causes a finalization error when
         --  the program exists.  Reluctantly, do the condition check manually
         if not all_ports (block_crate.Element (cursor)).blocks.Contains
           (Key => madre)
         then
            all_ports (block_crate.Element (cursor)).blocks.Insert
              (Key => madre, New_Item => madre);
         end if;
      end set_reverse;
   begin
      for port in port_index'First .. last_port loop
         if all_ports (port).scanned then
            madre := port;
            all_ports (port).blocked_by.Iterate (set_reverse'Access);
         end if;
      end loop;
   end iterate_reverse_deps;


   --------------------
   --  get_pkg_name  --
   --------------------
   function get_pkg_name (origin : String) return String
   is
      fullport : constant String := dir_ports & "/" & origin;
      ssroot   : constant String := chroot &
                 JT.USS (PM.configuration.dir_buildbase) & ss_base;
      command  : constant String := ssroot & " " & chroot_make_program &
                 " .MAKE.EXPAND_VARIABLES=yes -C " & fullport & " -VPKGFILE:T";
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;
   begin
      --  Same command for both ports collection and pkgsrc
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise bmake_execution with origin &
           " (return code =" & status'Img & ")";
      end if;
      JT.nextline (lineblock => content, firstline => topline);
      return JT.USS (topline);
   end get_pkg_name;


   ----------------------------
   --  populate_set_depends  --
   ----------------------------
   procedure populate_set_depends (target  : port_index;
                                   catport : String;
                                   line    : JT.Text;
                                   dtype   : dependency_type)
   is
      subs       : GSS.Slice_Set;
      deps_found : GSS.Slice_Number;
      trimline   : constant JT.Text := JT.trim (line);
      zero_deps  : constant GSS.Slice_Number := GSS.Slice_Number (0);
      dirlen     : constant Natural := dir_ports'Length;
      bracketed  : Natural := 0;

      use type GSS.Slice_Number;
   begin
      if JT.IsBlank (trimline) then
         return;
      end if;

      GSS.Create (S          => subs,
                  From       => JT.USS (trimline),
                  Separators => " " & LAT.HT,
                  Mode       => GSS.Multiple);
      deps_found :=  GSS.Slice_Count (S => subs);
      if deps_found = zero_deps then
         return;
      end if;
      for j in 1 .. deps_found loop
         declare
            workdep : constant String  := GSS.Slice (subs, j);
            fulldep : constant String (1 .. workdep'Length) := workdep;
            flen    : constant Natural := fulldep'Length;
            colon   : constant Natural := find_colon (fulldep);
            colon1  : constant Natural := colon + 1;
            deprec  : portkey_crate.Cursor;

            use type portkey_crate.Cursor;
         begin
            if colon < 2 then
               raise make_garbage
                 with dtype'Img & ": " & JT.USS (trimline) &
                 " (" & catport & ")";
            end if;
            if flen > colon1 + dirlen + 1 and then
              fulldep (colon1 .. colon1 + dirlen) = dir_ports & "/"
            then
               deprec := ports_keys.Find
                 (Key => scrub_phase
                    (fulldep (colon + dirlen + 2 .. fulldep'Last)));
            elsif flen > colon1 + 5 and then
              fulldep (colon1 .. colon1 + 5) = "../../"
            then
               deprec := ports_keys.Find
                 (Key => scrub_phase
                    (fulldep (colon1 + 6 .. fulldep'Last)));
            else
               deprec := ports_keys.Find
                 (Key => scrub_phase
                    (fulldep (colon1 .. fulldep'Last)));
            end if;

            if deprec = portkey_crate.No_Element then
               raise nonexistent_port
                 with fulldep &
                 " (required dependency of " & catport & ") does not exist.";
            end if;
            declare
               depindex : port_index := portkey_crate.Element (deprec);
            begin
               if not all_ports (target).blocked_by.Contains (depindex) then
                  all_ports (target).blocked_by.Insert
                    (Key      => depindex,
                     New_Item => depindex);
               end if;
               if dtype in LR_set then
                  if not all_ports (target).librun.Contains (depindex) then
                     all_ports (target).librun.Insert
                       (Key      => depindex,
                        New_Item => depindex);
                     all_ports (target).min_librun :=
                       all_ports (target).min_librun + 1;
                     case software_framework is
                        when ports_collection => null;
                        when pkgsrc =>
                           if fulldep (1) = '{' then
                              bracketed := bracketed + 1;
                           end if;
                     end case;
                  end if;
               end if;
            end;
         end;
      end loop;
      if bracketed > 0 and then all_ports (target).min_librun > 1 then
         declare
            newval : Integer := all_ports (target).min_librun - bracketed;
         begin
            if newval <= 1 then
               all_ports (target).min_librun := 1;
            else
               all_ports (target).min_librun := newval;
            end if;
         end;
      end if;
   end populate_set_depends;


   ----------------------------
   --  populate_set_options  --
   ----------------------------
   procedure populate_set_options (target : port_index;
                                   line   : JT.Text;
                                   on     : Boolean)
   is
      subs       : GSS.Slice_Set;
      opts_found : GSS.Slice_Number;
      trimline   : constant JT.Text := JT.trim (line);
      zero_opts  : constant GSS.Slice_Number := GSS.Slice_Number (0);

      use type GSS.Slice_Number;
   begin
      if JT.IsBlank (trimline) then
         return;
      end if;
      GSS.Create (S          => subs,
                  From       => JT.USS (trimline),
                  Separators => " ",
                  Mode       => GSS.Multiple);
      opts_found :=  GSS.Slice_Count (S => subs);
      if opts_found = zero_opts then
         return;
      end if;
      for j in 1 .. opts_found loop
         declare
            opt : JT.Text  := JT.SUS (GSS.Slice (subs, j));
         begin
            if not all_ports (target).options.Contains (opt) then
               all_ports (target).options.Insert (Key => opt,
                                                  New_Item => on);
            end if;
         end;
      end loop;
   end populate_set_options;


   --------------------------
   --  populate_flavors  --
   --------------------------
   procedure populate_flavors (target : port_index; line : JT.Text)
   is
      subs       : GSS.Slice_Set;
      flav_found : GSS.Slice_Number;
      trimline   : constant JT.Text := JT.trim (line);
      zero_flav  : constant GSS.Slice_Number := GSS.Slice_Number (0);

      use type GSS.Slice_Number;
   begin
      if JT.IsBlank (trimline) then
         return;
      end if;
      GSS.Create (S          => subs,
                  From       => JT.USS (trimline),
                  Separators => " ",
                  Mode       => GSS.Multiple);
      flav_found :=  GSS.Slice_Count (S => subs);
      if flav_found = zero_flav then
         return;
      end if;
      for j in 1 .. flav_found loop
         declare
            flavor : JT.Text := JT.SUS (GSS.Slice (subs, j));
         begin
            if not all_ports (target).flavors.Contains (flavor) then
               all_ports (target).flavors.Append (flavor);
            end if;
         end;
      end loop;
   end populate_flavors;


   --------------------------
   --  populate_port_data  --
   --------------------------
   procedure populate_port_data (target : port_index) is
   begin
      case software_framework is
         when ports_collection =>
            populate_port_data_fpc (target);
         when pkgsrc =>
            populate_port_data_nps (target);
      end case;
   end populate_port_data;


   ------------------------------
   --  populate_port_data_fpc  --
   ------------------------------
   procedure populate_port_data_fpc (target : port_index)
   is
      function get_fullport return String;

      catport  : constant String := get_catport (all_ports (target));

      function get_fullport return String is
      begin
         --  if format is cat/port@something then
         --     return "cat/port FLAVOR=something"
         --  else
         --     return $catport

         if JT.contains (catport, "@") then
            return dir_ports & "/" & JT.part_1 (catport, "@") &
              " FLAVOR=" & JT.part_2 (catport, "@");
         else
            return dir_ports & "/" & catport;
         end if;
      end get_fullport;

      fullport : constant String := get_fullport;
      ssroot   : constant String := chroot & JT.USS (PM.configuration.dir_buildbase) & ss_base;
      command  : constant String :=
                 ssroot & " " & chroot_make_program & " -C " & fullport &
                 " -VPKGVERSION -VPKGFILE:T -VMAKE_JOBS_NUMBER -VIGNORE" &
                 " -VFETCH_DEPENDS -VEXTRACT_DEPENDS -VPATCH_DEPENDS" &
                 " -VBUILD_DEPENDS -VLIB_DEPENDS -VRUN_DEPENDS" &
                 " -VSELECTED_OPTIONS -VDESELECTED_OPTIONS -VUSE_LINUX" &
                 " -VFLAVORS";
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;

      type result_range is range 1 .. 14;

   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise bmake_execution with catport &
           " (return code =" & status'Img & ")";
      end if;

      for k in result_range loop
         JT.nextline (lineblock => content, firstline => topline);
         case k is
            when 1 => all_ports (target).port_version := topline;
            when 2 => all_ports (target).package_name := topline;
            when 3 =>
               begin
                  all_ports (target).jobs :=
                    builders (Integer'Value (JT.USS (topline)));
               exception
                  when others =>
                     all_ports (target).jobs := PM.configuration.num_builders;
               end;
            when 4 => all_ports (target).ignore_reason := topline;
                      all_ports (target).ignored := not JT.IsBlank (topline);
            when 5 => populate_set_depends (target, catport, topline, fetch);
            when 6 => populate_set_depends (target, catport, topline, extract);
            when 7 => populate_set_depends (target, catport, topline, patch);
            when 8 => populate_set_depends (target, catport, topline, build);
            when 9 => populate_set_depends (target, catport, topline, library);
            when 10 => populate_set_depends (target, catport, topline, runtime);
            when 11 => populate_set_options (target, topline, True);
            when 12 => populate_set_options (target, topline, False);
            when 13 =>
               if not JT.IsBlank (JT.trim (topline)) then
                  all_ports (target).use_linprocfs := True;
               end if;
            when 14 => populate_flavors (target, topline);
         end case;
      end loop;
      all_ports (target).scanned := True;
      if catport = "x11-toolkits/gnustep-gui" then
         all_ports (target).use_procfs := True;
      end if;
      if catport = "emulators/linux_base-c6" or else
        catport = "emulators/linux_base-f10" or else
        catport = "sysutils/htop"
      then
         all_ports (target).use_linprocfs := True;
      end if;
   exception
      when issue : others =>
         EX.Reraise_Occurrence (issue);

   end populate_port_data_fpc;


   ------------------------------
   --  populate_port_data_nps  --
   ------------------------------
   procedure populate_port_data_nps (target : port_index)
   is
      catport  : String := get_catport (all_ports (target));
      fullport : constant String := dir_ports & "/" & catport;
      ssroot   : constant String := chroot &
                 JT.USS (PM.configuration.dir_buildbase) & ss_base;
      command  : constant String :=
                 ssroot & " " & chroot_make_program & " -C " & fullport &
                 " .MAKE.EXPAND_VARIABLES=yes " &
                 " -VPKGVERSION -VPKGFILE:T -V_MAKE_JOBS:C/^-j//" &
                 " -V_CBBH_MSGS -VTOOL_DEPENDS -VBUILD_DEPENDS -VDEPENDS" &
                 " -VPKG_OPTIONS -VPKG_DISABLED_OPTIONS" &
                 " -VEMUL_PLATFORMS";
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;

      type result_range is range 1 .. 10;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise bmake_execution with catport &
           " (return code =" & status'Img & ")";
      end if;
      for k in result_range loop
         JT.nextline (lineblock => content, firstline => topline);
         case k is
            when 1 => all_ports (target).port_version := topline;
            when 2 => all_ports (target).package_name := topline;
            when 3 =>
               if JT.IsBlank (topline) then
                  all_ports (target).jobs := PM.configuration.num_builders;
               else
                  begin
                     all_ports (target).jobs :=
                       builders (Integer'Value (JT.USS (topline)));
                  exception
                     when others =>
                        all_ports (target).jobs :=
                          PM.configuration.num_builders;
                  end;
               end if;
            when 4 =>
               all_ports (target).ignore_reason :=
                 clean_up_pkgsrc_ignore_reason (JT.USS (topline));
               all_ports (target).ignored := not JT.IsBlank (topline);
            when 5 => populate_set_depends (target, catport, topline, build);
            when 6 => populate_set_depends (target, catport, topline, build);
            when 7 => populate_set_depends (target, catport, topline, runtime);
            when 8 => populate_set_options (target, topline, True);
            when 9 => populate_set_options (target, topline, False);
            when 10 =>
               if JT.contains (topline, "linux") then
                  all_ports (target).use_linprocfs := True;
               end if;
         end case;
      end loop;
      all_ports (target).scanned := True;
      if catport = "x11/gnustep-gui" then
         all_ports (target).use_procfs := True;
      end if;
      if catport = "sysutils/htop" then
         all_ports (target).use_linprocfs := True;
      end if;
   exception
      when issue : others =>
         EX.Reraise_Occurrence (issue);
   end populate_port_data_nps;


   -----------------
   --  set_cores  --
   -----------------
   procedure set_cores
   is
      number : constant Positive := Replicant.Platform.get_number_cpus;
   begin
      if number > Positive (cpu_range'Last) then
         number_cores := cpu_range'Last;
      else
         number_cores := cpu_range (number);
      end if;
   end set_cores;


   -----------------------
   --  cores_available  --
   -----------------------
   function cores_available return cpu_range is
   begin
      return number_cores;
   end cores_available;


   --------------------------
   --  prescan_ports_tree  --
   --------------------------
   procedure prescan_ports_tree (portsdir : String)
   is
      package sorter is new string_crate.Generic_Sorting ("<" => JT.SU."<");

      procedure quick_scan (cursor : string_crate.Cursor);
      Search     : AD.Search_Type;
      Dir_Ent    : AD.Directory_Entry_Type;
      categories : string_crate.Vector;

      --  scan entire ports tree, and for each port hooked into the build,
      --  push an initial port_rec into the all_ports container
      procedure quick_scan (cursor : string_crate.Cursor)
      is
         category : constant String :=
           JT.USS (string_crate.Element (Position => cursor));
      begin
         if AD.Exists (portsdir & "/" & category & "/Makefile") then
            grep_Makefile (portsdir => portsdir, category => category);
         else
            walk_all_subdirectories (portsdir => portsdir,
                                     category => category);
         end if;
      end quick_scan;
   begin
      AD.Start_Search (Search    => Search,
                       Directory => portsdir,
                       Filter    => (AD.Directory => True, others => False),
                       Pattern   => "[a-z]*");

      while AD.More_Entries (Search => Search) loop
         AD.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            category : constant String := AD.Simple_Name (Dir_Ent);
            good_directory : Boolean := True;
         begin
            case software_framework is
               when ports_collection =>
                  if category = "base"     or else
                    category = "distfiles" or else
                    category = "packages"
                  then
                     good_directory := False;
                  end if;
               when pkgsrc =>
                  if category = "bootstrap" or else
                    category = "distfiles"  or else
                    category = "doc"        or else
                    category = "licenses"   or else
                    category = "mk"         or else
                    category = "packages"   or else
                    category = "regress"
                  then
                     good_directory := False;
                  end if;
            end case;
            if good_directory then
               categories.Append (New_Item => JT.SUS (category));
            end if;
         end;
      end loop;
      AD.End_Search (Search => Search);
      sorter.Sort (Container => categories);
      categories.Iterate (Process => quick_scan'Access);
      prescanned := True;
   end prescan_ports_tree;


   ---------------------------------
   --  tree_newer_than_reference  --
   ---------------------------------
   function tree_newer_than_reference
     (portsdir  : String;
      reference : CAL.Time;
      valid     : out Boolean) return Boolean
   is
      procedure quick_scan (cursor : string_crate.Cursor);

      Search      : AD.Search_Type;
      Dir_Ent     : AD.Directory_Entry_Type;
      categories  : string_crate.Vector;
      top_modtime : CAL.Time;
      keep_going  : Boolean := True;

      procedure quick_scan (cursor : string_crate.Cursor)
      is
         category : constant String :=
           JT.USS (string_crate.Element (Position => cursor));
      begin
         if keep_going then
            keep_going := subdirectory_is_older (portsdir  => portsdir,
                                                 category  => category,
                                                 reference => reference);
         end if;
      end quick_scan;

      use type CAL.Time;
   begin
      valid := True;
      top_modtime := AD.Modification_Time (portsdir);
      if reference < top_modtime then
         return True;
      end if;

      AD.Start_Search (Search    => Search,
                       Directory => portsdir,
                       Filter    => (AD.Directory => True, others => False),
                       Pattern   => "[a-z]*");

      while keep_going and then
        AD.More_Entries (Search => Search)
      loop
         AD.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            category : constant String := AD.Simple_Name (Dir_Ent);
            good_directory : Boolean := True;
         begin
            case software_framework is
               when ports_collection =>
                  if category = "base"     or else
                    category = "distfiles" or else
                    category = "packages"
                  then
                     good_directory := False;
                  end if;
               when pkgsrc =>
                  if category = "bootstrap" or else
                    category = "distfiles"  or else
                    category = "doc"        or else
                    category = "licenses"   or else
                    category = "mk"         or else
                    category = "packages"   or else
                    category = "regress"
                  then
                     good_directory := False;
                  end if;
            end case;
            if good_directory then
               if reference < AD.Modification_Time (Dir_Ent) then
                  keep_going := False;
               else
                  categories.Append (New_Item => JT.SUS (category));
               end if;
            end if;
         end;
      end loop;
      AD.End_Search (Search => Search);
      if not keep_going then
         return True;
      end if;
      categories.Iterate (Process => quick_scan'Access);
      return not keep_going;
   exception
      when others =>
         valid := False;
         return False;
   end tree_newer_than_reference;


   ------------------
   --  find_colon  --
   ------------------
   function find_colon (Source : String) return Natural
   is
      result : Natural := 0;
      strlen : constant Natural := Source'Length;
   begin
      for j in 1 .. strlen loop
         if Source (j) = LAT.Colon then
            result := j;
            exit;
         end if;
      end loop;
      return result;
   end find_colon;


   -------------------
   --  scrub_phase  --
   -------------------
   function scrub_phase (Source : String) return JT.Text
   is
      reset : constant String (1 .. Source'Length) := Source;
      colon : constant Natural := find_colon (reset);
   begin
      if colon = 0 then
         return JT.SUS (reset);
      end if;
      return JT.SUS (reset (1 .. colon - 1));
   end scrub_phase;


   --------------------------
   --  determine_max_lots  --
   --------------------------
   function get_max_lots return scanners
   is
      first_try : constant Positive := Positive (number_cores) * 3;
   begin
      if first_try > Positive (scanners'Last) then
         return scanners'Last;
      else
         return scanners (first_try);
      end if;
   end get_max_lots;


   ---------------------
   --  grep_Makefile  --
   ---------------------
   procedure grep_Makefile (portsdir, category : String)
   is
      archive  : TIO.File_Type;
      matches  : RGX.Match_Array (0 .. 1);
      pattern  : constant String := "^SUBDIR[[:space:]]*[:+:]=[[:space:]]*([[:graph:]]*)";
      regex    : constant RGX.Pattern_Matcher := RGX.Compile (pattern);
      max_lots : constant scanners := get_max_lots;
   begin
      TIO.Open (File => archive,
                Mode => TIO.In_File,
                Name => portsdir & "/" & category & "/Makefile");
      while not TIO.End_Of_File (File => archive) loop
         declare
            line      : constant String := JT.trim (TIO.Get_Line (File => archive));
            blank_rec : port_record;
            kc        : portkey_crate.Cursor;
            success   : Boolean;
            use type RGX.Match_Location;
         begin
            RGX.Match (Self => regex, Data => line, Matches => matches);
            if matches (0) /= RGX.No_Match then
               declare
                  portkey : constant JT.Text := JT.SUS (category & '/' &
                    line (matches (1).First .. matches (1).Last));
               begin
                  ports_keys.Insert (Key      => portkey,
                                     New_Item => lot_counter,
                                     Position => kc,
                                     Inserted => success);

                  last_port := lot_counter;
                  all_ports (lot_counter).sequence_id := lot_counter;
                  all_ports (lot_counter).key_cursor := kc;
                  make_queue (lot_number).Append (lot_counter);
               end;
            end if;
         end;

         lot_counter := lot_counter + 1;
         if lot_number = max_lots then
            lot_number := 1;
         else
            lot_number := lot_number + 1;
         end if;
      end loop;
      TIO.Close (File => archive);
   end grep_Makefile;


   -------------------------------
   --  walk_all_subdirectories  --
   -------------------------------
   procedure walk_all_subdirectories (portsdir, category : String)
   is
      inner_search : AD.Search_Type;
      inner_dirent : AD.Directory_Entry_Type;
      max_lots     : constant scanners := get_max_lots;
   begin
      AD.Start_Search (Search    => inner_search,
                       Directory => portsdir & "/" & category,
                       Filter    => (AD.Directory => True, others => False),
                       Pattern   => "");
      while AD.More_Entries (Search => inner_search) loop
         AD.Get_Next_Entry (Search => inner_search,
                            Directory_Entry => inner_dirent);
         declare
            portname  : constant String := AD.Simple_Name (inner_dirent);
            portkey   : constant JT.Text := JT.SUS (category & "/" & portname);
            kc        : portkey_crate.Cursor;
            success   : Boolean;
         begin
            if portname /= "." and then portname /= ".." then
                ports_keys.Insert (Key      => portkey,
                                   New_Item => lot_counter,
                                   Position => kc,
                                   Inserted => success);

               last_port := lot_counter;
               all_ports (lot_counter).sequence_id := lot_counter;
               all_ports (lot_counter).key_cursor := kc;
               make_queue (lot_number).Append (lot_counter);
               lot_counter := lot_counter + 1;
               if lot_number = max_lots then
                  lot_number := 1;
               else
                  lot_number := lot_number + 1;
               end if;
            end if;
         end;
      end loop;
      AD.End_Search (inner_search);
   end walk_all_subdirectories;


   -----------------------------
   --  subdirectory_is_older  --
   -----------------------------
   function subdirectory_is_older (portsdir, category : String;
                                   reference : CAL.Time) return Boolean
   is
      inner_search  : AD.Search_Type;
      inner_dirent  : AD.Directory_Entry_Type;
      already_older : Boolean := False;

      use type CAL.Time;
   begin
      AD.Start_Search (Search    => inner_search,
                       Directory => portsdir & "/" & category,
                       Filter    => (others => True),
                       Pattern   => "");
      while not already_older and then
        AD.More_Entries (Search => inner_search)
      loop
         AD.Get_Next_Entry (Search => inner_search, Directory_Entry => inner_dirent);

         --  We're going to get "." and "..".  It's faster to check them (always older)
         --  than convert to simple name and exclude them.
         if reference < AD.Modification_Time (inner_dirent) then
            already_older := True;
         end if;
      end loop;
      AD.End_Search (inner_search);
      return already_older;
   end subdirectory_is_older;


   -----------------
   --  port_hash  --
   -----------------
   function port_hash (key : JT.Text) return AC.Hash_Type is
   begin
      return Ada.Strings.Hash (JT.USS (key));
   end port_hash;


   ------------------
   --  block_hash  --
   ------------------
   function block_hash (key : port_index) return AC.Hash_Type is
       preresult : constant AC.Hash_Type := AC.Hash_Type (key);
      use type AC.Hash_Type;
   begin
      --  Equivalent to mod 128
      return preresult and 2#1111111#;
   end block_hash;


   ------------------
   --  block_ekey  --
   ------------------
   function block_ekey (left, right : port_index) return Boolean is
   begin
      return left = right;
   end block_ekey;


   --------------------------------------
   --  "<" function for ranking_crate  --
   --------------------------------------
   function "<" (L, R : queue_record) return Boolean is
   begin
      if L.reverse_score = R.reverse_score then
         return R.ap_index > L.ap_index;
      end if;
      return L.reverse_score > R.reverse_score;
   end "<";


   -------------------
   --  get_catport  --
   -------------------
   function get_catport (PR : port_record) return String
   is
      use type portkey_crate.Cursor;
   begin
      if PR.key_cursor = portkey_crate.No_Element then
         return "get_catport: invalid key_cursor";
      end if;
      return JT.USS (portkey_crate.Key (PR.key_cursor));
   end get_catport;


   ---------------------
   --  scan_progress  --
   ---------------------
   function scan_progress return String
   is
      type percent is delta 0.01 digits 5;
      complete : port_index := 0;
      pc : percent;
   begin
      for k in scanners'Range loop
         complete := complete + mq_progress (k);
      end loop;
      pc := percent (100.0 * Float (complete) / Float (last_port));
      return " progress:" & pc'Img & "%              " & LAT.CR;
   end scan_progress;


   -----------------------
   --  obvious_problem  --
   -----------------------
   function obvious_problem (portsdir, catport : String) return String
   is
      fullpath : constant String := portsdir & "/" & catport;
   begin
      if AD.Exists (fullpath) then
         declare
            Search : AD.Search_Type;
            dirent : AD.Directory_Entry_Type;
            has_contents : Boolean := False;
         begin
            AD.Start_Search (Search    => Search,
                             Directory => fullpath,
                             Filter    => (others => True),
                             Pattern   => "*");
            while AD.More_Entries (Search => Search) loop
               AD.Get_Next_Entry (Search => Search, Directory_Entry => dirent);
               if AD.Simple_Name (dirent) /= "." and then
                 AD.Simple_Name (dirent) /= ".."
               then
                  has_contents := True;
                  exit;
               end if;
            end loop;
            AD.End_Search (Search => Search);
            if not has_contents then
               return " (directory empty)";
            end if;
            if AD.Exists (fullpath & "/Makefile") then
               return "";
            else
               return " (Makefile missing)";
            end if;
         end;
      else
         return " (port deleted)";
      end if;
   end obvious_problem;


   -------------------------------------
   --  clean_up_pkgsrc_ignore_reason  --
   -------------------------------------
   function clean_up_pkgsrc_ignore_reason (dirty_string : String) return JT.Text
   is
      --  1. strip out all double-quotation marks
      --  2. strip out all single reverse solidus ("\")
      --  3. condense contiguous spaces to a single space
      function strip_this_package_has_set (raw : String) return String;
      function strip_this_package_has_set (raw : String) return String
      is
         pattern : constant String := "This package has set ";
      begin
         if JT.contains (raw, pattern) then
            declare
               uno : String := JT.part_1 (raw, pattern);
               duo : String := JT.part_2 (raw, pattern);
            begin
               return strip_this_package_has_set (uno & duo);
            end;
         else
            return raw;
         end if;
      end strip_this_package_has_set;

      product : String (1 .. dirty_string'Length);
      dstx    : Natural := 0;
      keep_it : Boolean;
      last_was_slash : Boolean := False;
      last_was_space : Boolean := False;
   begin
      for srcx in dirty_string'Range loop
         keep_it := True;
         case dirty_string (srcx) is
            when LAT.Quotation | LAT.LF =>
               keep_it := False;
            when LAT.Reverse_Solidus =>
               if not last_was_slash then
                  keep_it := False;
               end if;
               last_was_slash := not last_was_slash;
            when LAT.Space =>
               if last_was_space then
                  keep_it := False;
               end if;
               last_was_space := True;
               last_was_slash := False;
            when others =>
               last_was_space := False;
               last_was_slash := False;
         end case;
         if keep_it then
            dstx := dstx + 1;
            product (dstx) := dirty_string (srcx);
         end if;
      end loop;
      return JT.SUS (strip_this_package_has_set (product (1 .. dstx)));
   end clean_up_pkgsrc_ignore_reason;


   -----------------
   --  timestamp  --
   -----------------
   function timestamp (hack : CAL.Time; www_format : Boolean := False) return String
   is
      function MON   (num : CAL.Month_Number) return String;
      function WKDAY (day : ACF.Day_Name) return String;

      function MON (num : CAL.Month_Number) return String is
      begin
         case num is
            when 1 => return "JAN";
            when 2 => return "FEB";
            when 3 => return "MAR";
            when 4 => return "APR";
            when 5 => return "MAY";
            when 6 => return "JUN";
            when 7 => return "JUL";
            when 8 => return "AUG";
            when 9 => return "SEP";
            when 10 => return "OCT";
            when 11 => return "NOV";
            when 12 => return "DEC";
         end case;
      end MON;
      function WKDAY (day : ACF.Day_Name) return String is
      begin
         case day is
            when ACF.Monday    => return "Monday";
            when ACF.Tuesday   => return "Tuesday";
            when ACF.Wednesday => return "Wednesday";
            when ACF.Thursday  => return "Thursday";
            when ACF.Friday    => return "Friday";
            when ACF.Saturday  => return "Saturday";
            when ACF.Sunday    => return "Sunday";
         end case;
      end WKDAY;
   begin
      if www_format then
         return CAL.Day (hack)'Img & " " & MON (CAL.Month (hack)) & CAL.Year (hack)'Img & ", " &
           ACF.Image (hack)(11 .. 19) & " UTC";
      end if;

      return WKDAY (ACF.Day_Of_Week (hack)) & "," & CAL.Day (hack)'Img & " " &
        MON (CAL.Month (hack)) & CAL.Year (hack)'Img & " at" &
        ACF.Image (hack)(11 .. 19) & " UTC";
   end timestamp;


   ----------------------------
   --  generate_ports_index  --
   ----------------------------
   function generate_ports_index (index_file, portsdir : String) return Boolean
   is
      package sorter is new string_crate.Generic_Sorting ("<" => JT.SU."<");

      procedure add_flavor (cursor : string_crate.Cursor);
      procedure write_line (cursor : string_crate.Cursor);

      good_scan    : Boolean;
      handle       : TIO.File_Type;
      all_flavors  : string_crate.Vector;
      basecatport  : JT.Text;
      using_screen : constant Boolean := Unix.screen_attached;
      error_prefix : constant String  := "Flavor index generation failed: ";
      index_full   : constant String  := index_path & "/" & JT.USS (PM.configuration.profile) &
                                         "-index";

      procedure add_flavor (cursor : string_crate.Cursor)
      is
         line : JT.Text := basecatport;
      begin
         JT.SU.Append (line, "@");
         JT.SU.Append (line, string_crate.Element (Position => cursor));
         all_flavors.Append (line);
      end add_flavor;

      procedure write_line (cursor : string_crate.Cursor)
      is
         line : constant String := JT.USS (string_crate.Element (Position => cursor));
      begin
         TIO.Put_Line (handle, line);
      end write_line;

   begin
      begin
         AD.Create_Path (index_path);
      exception
         when others =>
            TIO.Put_Line (error_prefix & "could not create " & index_path);
            return False;
      end;

      TIO.Put_Line ("Regenerating flavor index: this may take a while ...");
      prescan_ports_tree (portsdir);

      case software_framework is
         when ports_collection =>
            scan_start := CAL.Clock;
            parallel_deep_scan (success => good_scan, show_progress => using_screen);
            scan_stop := CAL.Clock;

            if not good_scan then
               TIO.Put_Line (error_prefix & "ports scan");
               return False;
            end if;
         when pkgsrc =>
            null;
      end case;

      begin
         for port in port_index'First .. last_port loop
            basecatport := portkey_crate.Key (all_ports (port).key_cursor);
            if all_ports (port).flavors.Is_Empty then
               all_ports (port).flavors.Iterate (add_flavor'Access);
            else
               all_flavors.Append (basecatport);
            end if;
         end loop;
         sorter.Sort (Container => all_flavors);

         TIO.Create (File => handle, Mode => TIO.Out_File, Name => index_full);
         all_flavors.Iterate (write_line'Access);
         TIO.Close (handle);
      exception
         when others =>
            if TIO.Is_Open (handle) then
               TIO.Close (handle);
            end if;
            TIO.Put_Line (error_prefix & "writing out index");
            return False;
      end;

      reset_ports_tree;
      return True;
   end generate_ports_index;


   ----------------------------
   --  read_flavor_index  --
   ----------------------------
   procedure read_flavor_index
   is
      handle     : TIO.File_Type;
      max_lots   : constant scanners := get_max_lots;
      index_full : constant String :=
        index_path & "/" & JT.USS (PM.configuration.profile) & "-index";
   begin
      TIO.Open (File => handle,
                Mode => TIO.In_File,
                Name => index_full);
      while not TIO.End_Of_File (handle) loop
         declare
            portkey   : JT.Text := JT.SUS (JT.trim (TIO.Get_Line (handle)));
            blank_rec : port_record;
            kc        : portkey_crate.Cursor;
            success   : Boolean;
         begin
            ports_keys.Insert (Key      => portkey,
                               New_Item => lot_counter,
                               Position => kc,
                               Inserted => success);
            last_port := lot_counter;
            all_ports (last_port).sequence_id := last_port;
            all_ports (last_port).key_cursor := kc;
            make_queue (lot_number).Append (last_port);
         end;

         lot_counter := lot_counter + 1;
         if lot_number = max_lots then
            lot_number := 1;
         else
            lot_number := lot_number + 1;
         end if;
      end loop;
      TIO.Close (handle);
      prescanned := True;
   exception
      when others =>
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
   end read_flavor_index;

end PortScan;
