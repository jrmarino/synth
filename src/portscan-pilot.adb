--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Ada.Strings.Fixed;
with PortScan.Ops;
with PortScan.Packages;
with PortScan.Buildcycle.Ports;
with PortScan.Buildcycle.Pkgsrc;
with Signals;
with Unix;

package body PortScan.Pilot is

   package CLI renames Ada.Command_Line;
   package ASF renames Ada.Strings.Fixed;
   package OPS renames PortScan.Ops;
   package PKG renames PortScan.Packages;
   package CYC renames PortScan.Buildcycle;
   package FPC renames PortScan.Buildcycle.Ports;
   package NPS renames PortScan.Buildcycle.Pkgsrc;
   package SIG renames Signals;

   ---------------------
   --  store_origins  --
   ---------------------
   function store_origins return Boolean
   is
      function trimmed_catport (S : String) return String;
      function trimmed_catport (S : String) return String
      is
         last : constant Natural := S'Last;
      begin
         if S (last) = '/' then
            return S (S'First .. last - 1);
         else
            return S (S'First .. last);
         end if;
      end trimmed_catport;
   begin
      if CLI.Argument_Count <= 1 then
         return False;
      end if;
      portlist.Clear;
      load_index_for_store_origins;

      if CLI.Argument_Count = 2 then
         --  Check if this is a file
         declare
            Arg2 : constant String := trimmed_catport (CLI.Argument (2));
            vfresult : Boolean;
         begin
            if AD.Exists (Arg2) then
               vfresult := valid_file (Arg2);
               clear_store_origin_data;
               return vfresult;
            end if;
            if input_origin_valid (candidate => Arg2) then
               if Arg2 /= pkgng then
                  plinsert (Arg2, 2);
               end if;
               clear_store_origin_data;
               return True;
            else
               suggest_flavor_for_bad_origin (candidate => Arg2);
               clear_store_origin_data;
               return False;
            end if;
         end;
      end if;
      for k in 2 .. CLI.Argument_Count loop
         declare
            Argk : constant String := trimmed_catport (CLI.Argument (k));
         begin
            if input_origin_valid (candidate => Argk) then
               if Argk /= pkgng then
                  plinsert (Argk, k);
               end if;
            else
               suggest_flavor_for_bad_origin (candidate => Argk);
               clear_store_origin_data;
               return False;
            end if;
         end;
      end loop;
      clear_store_origin_data;
      return True;
   end store_origins;


   -------------------------------
   --  prerequisites_available  --
   -------------------------------
   function prerequisites_available return Boolean is
   begin
      case software_framework is
         when ports_collection => return build_pkg8_as_necessary;
         when pkgsrc           => return build_pkgsrc_prerequisites;
      end case;
   end prerequisites_available;


   -------------------------------
   --  build_pkg8_as_necessary  --
   -------------------------------
   function build_pkg8_as_necessary return Boolean
   is
      pkg_good  : Boolean;
      good_scan : Boolean;
      stop_now  : Boolean;
      selection : PortScan.port_id;
      result    : Boolean := True;
   begin
      OPS.initialize_hooks;
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      good_scan := PortScan.scan_single_port (catport      => pkgng,
                                              always_build => False,
                                              cache_var    => "",    --  don't precache single scan
                                              fatal        => stop_now);

      if good_scan then
         PortScan.set_build_priority;
      else
         TIO.Put_Line ("Unexpected pkg(8) scan failure!");
         result := False;
         goto clean_exit;
      end if;

      PKG.limited_sanity_check
        (repository => JT.USS (PM.configuration.dir_repository),
         dry_run    => False, suppress_remote => True);

      if SIG.graceful_shutdown_requested or else PKG.queue_is_empty then
         goto clean_exit;
      end if;

      CYC.initialize (test_mode => False, jail_env => REP.jail_environment);
      selection := OPS.top_buildable_port;
      if SIG.graceful_shutdown_requested or else selection = port_match_failed
      then
         goto clean_exit;
      end if;
      TIO.Put ("Stand by, building pkg(8) first ... ");

      pkg_good := FPC.build_package (id => PortScan.scan_slave,
                                     sequence_id => selection);
      OPS.run_hook_after_build (pkg_good, selection);

      if not pkg_good then
         TIO.Put_Line ("Failed!!" & bailing);
         result := False;
         goto clean_exit;
      end if;

      TIO.Put_Line ("done!");

      <<clean_exit>>
      if SIG.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
         result := False;
      end if;
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      reset_ports_tree;
      return result;

   end build_pkg8_as_necessary;


   ----------------------------------
   --  build_pkgsrc_prerequisites  --
   ----------------------------------
   function build_pkgsrc_prerequisites return Boolean
   is
      function scan_it (the_catport : String) return Boolean;
      function build_it (desc : String) return Boolean;

      mk_files  : constant String := "pkgtools/bootstrap-mk-files";
      cp_bmake  : constant String := "devel/bmake";
      cp_digest : constant String := "pkgtools/digest";
      result    : Boolean := True;

      function scan_it (the_catport : String) return Boolean
      is
         good_scan : Boolean;
         stop_now  : Boolean;
      begin
         good_scan := PortScan.scan_single_port (catport      => the_catport,
                                                 always_build => False,
                                                 cache_var    => "",  --  don't precache single scan
                                                 fatal        => stop_now);
         if good_scan then
            PortScan.set_build_priority;
         else
            TIO.Put_Line ("Unexpected " & the_catport & " scan failure!");
            return False;
         end if;

         PKG.limited_sanity_check
           (repository => JT.USS (PM.configuration.dir_repository),
            dry_run    => False, suppress_remote => True);

         if SIG.graceful_shutdown_requested then
            return False;
         end if;

         return True;
      end scan_it;

      function build_it (desc : String) return Boolean
      is
         pkg_good  : Boolean;
         selection : PortScan.port_id;
      begin
         CYC.initialize (test_mode => False, jail_env => REP.jail_environment);
         selection := OPS.top_buildable_port;
         if SIG.graceful_shutdown_requested or else selection = port_match_failed
         then
            return False;
         end if;
         TIO.Put ("Stand by, building " & desc & " package ... ");
         pkg_good := NPS.build_package (id => PortScan.scan_slave,
                                        sequence_id => selection);
         OPS.run_hook_after_build (pkg_good, selection);
         if not pkg_good then
            TIO.Put_Line ("Failed!!" & bailing);
            return False;
         end if;
         TIO.Put_Line ("done!");
         return True;
      end build_it;
   begin
      OPS.initialize_hooks;
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => npsboot);
      if not PLAT.host_pkgsrc_mk_install (id => PortScan.scan_slave) or else
        not PLAT.host_pkgsrc_bmake_install (id => PortScan.scan_slave) or else
        not PLAT.host_pkgsrc_pkg8_install (id => PortScan.scan_slave)
      then
         TIO.Put_Line ("Failed to install programs from host system.");
         result := False;
         goto clean_exit;
      end if;

      result := scan_it (mk_files);
      if not result then
         goto clean_exit;
      end if;

      if not PKG.queue_is_empty then
         --  the mk files package does not exist or requires rebuilding
         result := build_it ("mk files");
         if not result then
            goto clean_exit;
         end if;
      end if;
      reset_ports_tree;

      result := scan_it (cp_bmake);
      if not result then
         goto clean_exit;
      end if;

      if not PKG.queue_is_empty then
         --  the bmake package does not exist or requires rebuilding
         result := build_it ("bmake");
         if not result then
            goto clean_exit;
         end if;
      end if;
      reset_ports_tree;

      result := scan_it (cp_digest);
      if not result then
         goto clean_exit;
      end if;

      if not PKG.queue_is_empty then
         --  the digest package does not exist or requires rebuilding
         result := build_it ("digest");
         if not result then
            goto clean_exit;
         end if;
      end if;
      reset_ports_tree;

      result := scan_it (pkgng);
      if not result then
         goto clean_exit;
      end if;

      if not PKG.queue_is_empty then
         --  the pkg(8) package does not exist or requires rebuilding
         result := build_it ("pkg(8)");
      end if;

      <<clean_exit>>
      if SIG.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
         result := False;
      end if;
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      reset_ports_tree;
      return result;

   end build_pkgsrc_prerequisites;


   ----------------------------------
   --  scan_stack_of_single_ports  --
   ----------------------------------
   function scan_stack_of_single_ports (testmode : Boolean;
                                        always_build : Boolean := False)
                                        return Boolean
   is
      successful : Boolean := True;
      just_stop_now : Boolean;
   begin
      REP.initialize (testmode, PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      if SIG.graceful_shutdown_requested then
         goto clean_exit;
      end if;
      if not PLAT.standalone_pkg8_install (PortScan.scan_slave) then
         TIO.Put_Line ("Failed to install pkg(8) scanner" & bailing);
         successful := False;
         goto clean_exit;
      end if;
      declare
         procedure scan (plcursor : portkey_crate.Cursor);
         cache_var : constant String := set_port_cache_variables;

         procedure scan (plcursor : portkey_crate.Cursor)
         is
            origin : constant String := JT.USS (portkey_crate.Key (plcursor));
         begin
            if not successful then
               return;
            end if;
            if origin = pkgng then
               --  we've already built pkg(8) if we get here, just skip it
               return;
            end if;
            if SIG.graceful_shutdown_requested then
               successful := False;
               return;
            end if;
            if not PortScan.scan_single_port (origin, always_build, cache_var, just_stop_now)
            then
               if just_stop_now then
                  successful := False;
               else
                  TIO.Put_Line
                    ("Scan of " & origin & " failed" & PortScan.obvious_problem
                       (JT.USS (PM.configuration.dir_portsdir), origin) &
                       ", it will not be considered.");
               end if;
            end if;
         end scan;
      begin
         portlist.Iterate (Process => scan'Access);
      end;
      if successful then
         PortScan.set_build_priority;
         if PKG.queue_is_empty then
            successful := False;
            TIO.Put_Line ("There are no valid ports to build." & bailing);
         end if;
      end if;

      <<clean_exit>>
      if SIG.graceful_shutdown_requested then
         successful := False;
         TIO.Put_Line (shutreq);
      end if;
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      return successful;
   end scan_stack_of_single_ports;


   ---------------------------------
   --  sanity_check_then_prefail  --
   ---------------------------------
   function sanity_check_then_prefail (delete_first : Boolean := False;
                                       dry_run : Boolean := False)
                                       return Boolean
   is
      procedure force_delete (plcursor : portkey_crate.Cursor);
      ptid : PortScan.port_id;
      num_skipped : Natural;
      block_remote : Boolean := True;
      update_external_repo : constant String := host_pkg8 &
                    " update --quiet --repository ";
      no_packages : constant String :=
                    "No prebuilt packages will be used as a result.";

      procedure force_delete (plcursor : portkey_crate.Cursor)
      is
         origin : JT.Text := portkey_crate.Key (plcursor);
         pndx   : constant port_index := ports_keys.Element (origin);
         tball  : constant String := JT.USS (PM.configuration.dir_repository) &
                           "/" & JT.USS (all_ports (pndx).package_name);
      begin
         if AD.Exists (tball) then
            AD.Delete_File (tball);
         end if;
      end force_delete;
   begin
      start_time := CAL.Clock;

      if delete_first and then not dry_run then
         portlist.Iterate (Process => force_delete'Access);
      end if;

      case software_framework is
         when ports_collection =>
            if not PKG.limited_cached_options_check then
               --  Error messages emitted by function
               return False;
            end if;
         when pkgsrc =>
            --  There's no analog to cached options on pkgsc.
            --  We could detect unused settings, but that's it.
            --  And maybe that should be detected by the framework itself
            null;
      end case;

      if PM.configuration.defer_prebuilt then
         --  Before any remote operations, find the external repo
         if PKG.located_external_repository then
            block_remote := False;
            --  We're going to use prebuilt packages if available, so let's
            --  prepare for that case by updating the external repository
            TIO.Put ("Stand by, updating external repository catalogs ... ");
            if not Unix.external_command (update_external_repo &
                                            PKG.top_external_repository)
            then
               TIO.Put_Line ("Failed!");
               TIO.Put_Line ("The external repository could not be updated.");
               TIO.Put_Line (no_packages);
               block_remote := True;
            else
               TIO.Put_Line ("done.");
            end if;
         else
            TIO.Put_Line ("The external repository does not seem to be " &
                            "configured.");
            TIO.Put_Line (no_packages);
         end if;
      end if;

      OPS.run_start_hook;
      PKG.limited_sanity_check
        (repository => JT.USS (PM.configuration.dir_repository),
         dry_run    => dry_run, suppress_remote => block_remote);
      bld_counter := (OPS.queue_length, 0, 0, 0, 0);
      if dry_run then
         return True;
      end if;
      if SIG.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
         return False;
      end if;

      OPS.delete_existing_web_history_files;

      start_logging (total);
      start_logging (ignored);
      start_logging (skipped);
      start_logging (success);
      start_logging (failure);

      loop
         ptid := OPS.next_ignored_port;
         exit when ptid = PortScan.port_match_failed;
         exit when SIG.graceful_shutdown_requested;
         bld_counter (ignored) := bld_counter (ignored) + 1;
         TIO.Put_Line (Flog (total), CYC.elapsed_now & " " &
                         OPS.port_name (ptid) & " has been ignored: " &
                         OPS.ignore_reason (ptid));
         TIO.Put_Line (Flog (ignored), CYC.elapsed_now & " " &
                         OPS.port_name (ptid) & ": " &
                         OPS.ignore_reason (ptid));
         OPS.cascade_failed_build (id         => ptid,
                                   numskipped => num_skipped,
                                   logs       => Flog);
         OPS.record_history_ignored (elapsed   => CYC.elapsed_now,
                                     origin    => OPS.port_name (ptid),
                                     reason    => OPS.ignore_reason (ptid),
                                     skips     => num_skipped);
         bld_counter (skipped) := bld_counter (skipped) + num_skipped;
      end loop;
      stop_logging (ignored);
      TIO.Put_Line (Flog (total), CYC.elapsed_now & " Sanity check complete. "
                    & "Ports remaining to build:" & OPS.queue_length'Img);
      TIO.Flush (Flog (total));
      if SIG.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
      else
         if OPS.integrity_intact then
            return True;
         end if;
      end if;
      --  If here, we either got control-C or failed integrity check
      if not SIG.graceful_shutdown_requested then
         TIO.Put_Line ("Queue integrity lost! " & bailing);
      end if;
      stop_logging (total);
      stop_logging (skipped);
      stop_logging (success);
      stop_logging (failure);
      return False;
   end sanity_check_then_prefail;


   ------------------------
   --  perform_bulk_run  --
   ------------------------
   procedure perform_bulk_run (testmode : Boolean)
   is
      num_builders : constant builders := PM.configuration.num_builders;
      show_tally   : Boolean := True;
   begin
      if PKG.queue_is_empty then
         TIO.Put_Line ("After inspection, it has been determined that there " &
                         "are no packages that");
         TIO.Put_Line ("require rebuilding; the task is therefore complete.");
         show_tally := False;
      else
         REP.initialize (testmode, PortScan.cores_available);
         CYC.initialize (testmode, REP.jail_environment);
         OPS.initialize_web_report (num_builders);
         OPS.initialize_display (num_builders);
         OPS.parallel_bulk_run (num_builders, Flog);
         REP.finalize;
      end if;
      stop_time := CAL.Clock;
      stop_logging (total);
      stop_logging (success);
      stop_logging (failure);
      stop_logging (skipped);
      if show_tally then
         TIO.Put_Line (LAT.LF & LAT.LF);
         TIO.Put_Line ("The task is complete.  Final tally:");
         TIO.Put_Line ("Initial queue size:" & bld_counter (total)'Img);
         TIO.Put_Line ("    packages built:" & bld_counter (success)'Img);
         TIO.Put_Line ("           ignored:" & bld_counter (ignored)'Img);
         TIO.Put_Line ("           skipped:" & bld_counter (skipped)'Img);
         TIO.Put_Line ("            failed:" & bld_counter (failure)'Img);
         TIO.Put_Line ("");
         TIO.Put_Line (CYC.log_duration (start_time, stop_time));
         TIO.Put_Line ("The build logs can be found at: " &
                         JT.USS (PM.configuration.dir_logs));
      end if;
   end perform_bulk_run;


   -------------------------------------------
   --  verify_desire_to_rebuild_repository  --
   -------------------------------------------
   function verify_desire_to_rebuild_repository return Boolean
   is
      answer : Boolean;
      YN : Character;
      screen_present : constant Boolean := Unix.screen_attached;
   begin
      if not screen_present then
         return False;
      end if;
      if SIG.graceful_shutdown_requested then
         --  catch previous shutdown request
         return False;
      end if;
      Unix.cone_of_silence (deploy => False);
      TIO.Put ("Would you like to rebuild the local repository (Y/N)? ");
      loop
         TIO.Get_Immediate (YN);
         case YN is
            when 'Y' | 'y' =>
               answer := True;
               exit;
            when 'N' | 'n' =>
               answer := False;
               exit;
            when others => null;
         end case;
      end loop;
      TIO.Put (YN & LAT.LF);
      Unix.cone_of_silence (deploy => True);
      return answer;
   end verify_desire_to_rebuild_repository;


   -----------------------------------------
   --  verify_desire_to_install_packages  --
   -----------------------------------------
   function verify_desire_to_install_packages return Boolean is
      answer : Boolean;
      YN : Character;
   begin
      Unix.cone_of_silence (deploy => False);
      TIO.Put ("Would you like to upgrade your system with the new packages now (Y/N)? ");
      loop
         TIO.Get_Immediate (YN);
         case YN is
            when 'Y' | 'y' =>
               answer := True;
               exit;
            when 'N' | 'n' =>
               answer := False;
               exit;
            when others => null;
         end case;
      end loop;
      TIO.Put (YN & LAT.LF);
      Unix.cone_of_silence (deploy => True);
      return answer;
   end verify_desire_to_install_packages;


   -----------------------------
   --  fully_scan_ports_tree  --
   -----------------------------
   function fully_scan_ports_tree return Boolean
   is
      goodresult : Boolean;
   begin
      PortScan.reset_ports_tree;
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      case software_framework is
         when ports_collection => null;
         when pkgsrc =>
            if not PLAT.standalone_pkg8_install (PortScan.scan_slave) then
               TIO.Put_Line ("Full Tree Scan: Failed to bootstrap builder");
            end if;
      end case;
      goodresult := PortScan.scan_entire_ports_tree
        (JT.USS (PM.configuration.dir_portsdir));
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      if goodresult then
         PortScan.set_build_priority;
         return True;
      else
         if SIG.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
         else
            TIO.Put_Line ("Failed to scan ports tree " & bailing);
         end if;
         CLI.Set_Exit_Status (1);
         return False;
      end if;
   end fully_scan_ports_tree;


   ---------------------------------
   --  rebuild_local_respository  --
   ---------------------------------
   function rebuild_local_respository (remove_invalid_packages : Boolean) return Boolean
   is
      repo : constant String := JT.USS (PM.configuration.dir_repository);
      main : constant String := JT.USS (PM.configuration.dir_packages);
      packed_meta    : constant String := main & "/meta.pkg";
      packed_digest  : constant String := main & "/digests.pkg";
      packed_pkgsite : constant String := main & "/packagesite.pkg";
      bs_error   : constant String := "Rebuild Repository: Failed to bootstrap builder";
      build_res  : Boolean;
   begin
      if SIG.graceful_shutdown_requested then
         --  In case it was previously requested
         return False;
      end if;

      if remove_invalid_packages then
         REP.initialize (testmode => False,
                         num_cores => PortScan.cores_available);
         REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
         case software_framework is
         when ports_collection => null;
         when pkgsrc =>
            if not PLAT.standalone_pkg8_install (PortScan.scan_slave) then
               TIO.Put_Line (bs_error);
            end if;
         end case;

         PKG.preclean_repository (repo);

         REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
         REP.finalize;
         if SIG.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
            return False;
         end if;
      end if;

      TIO.Put ("Stand by, recursively scanning");
      if Natural (portlist.Length) = 1 then
         TIO.Put (" 1 port");
      else
         TIO.Put (portlist.Length'Img & " ports");
      end if;
      TIO.Put_Line (" serially.");
      PortScan.reset_ports_tree;
      if scan_stack_of_single_ports (testmode => False) then
         PKG.limited_sanity_check (repository      => repo,
                                   dry_run         => False,
                                   suppress_remote => True);
         if SIG.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
            return False;
         end if;
      else
         return False;
      end if;

      if AD.Exists (packed_meta) then
         AD.Delete_File (packed_meta);
      end if;
      if AD.Exists (packed_digest) then
         AD.Delete_File (packed_digest);
      end if;
      if AD.Exists (packed_pkgsite) then
         AD.Delete_File (packed_pkgsite);
      end if;
      TIO.Put_Line ("Packages validated, rebuilding local repository.");
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      case software_framework is
         when ports_collection => null;
         when pkgsrc =>
            if not PLAT.standalone_pkg8_install (PortScan.scan_slave) then
               TIO.Put_Line (bs_error);

            end if;
      end case;
      if valid_signing_command then
         build_res := REP.build_repository (id => PortScan.scan_slave,
                                            sign_command => signing_command);
      elsif acceptable_RSA_signing_support then
         build_res := REP.build_repository (PortScan.scan_slave);
      else
         build_res := False;
      end if;
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      if build_res then
         TIO.Put_Line ("Local repository successfully rebuilt");
         return True;
      else
         TIO.Put_Line ("Failed to rebuild repository" & bailing);
         return False;
      end if;
   end rebuild_local_respository;


   ------------------
   --  valid_file  --
   ------------------
   function valid_file (path : String) return Boolean
   is
      handle : TIO.File_Type;
      good   : Boolean;
      total  : Natural := 0;
   begin
      TIO.Open (File => handle, Mode => TIO.In_File, Name => path);
      good := True;
      while not TIO.End_Of_File (handle) loop
         declare
            line : constant String := JT.trim (TIO.Get_Line (handle));
         begin
            if not JT.IsBlank (line) then
               if input_origin_valid (candidate => line) then
                  plinsert (line, total);
                  total := total + 1;
               else
                  suggest_flavor_for_bad_origin (candidate => line);
                  good := False;
                  exit;
               end if;
            end if;
         end;
      end loop;
      TIO.Close (handle);
      return (total > 0) and then good;
   exception
      when others => return False;
   end valid_file;


   ----------------
   --  plinsert  --
   ----------------
   procedure plinsert (key : String; dummy : Natural)
   is
      key2 : JT.Text := JT.SUS (key);
      ptid : constant PortScan.port_id := PortScan.port_id (dummy);
   begin
      if not portlist.Contains (key2) then
         portlist.Insert (key2, ptid);
         duplist.Insert (key2, ptid);
      end if;
   end plinsert;


   ---------------------
   --  start_logging  --
   ---------------------
   procedure start_logging (flavor : count_type)
   is
      logpath : constant String := JT.USS (PM.configuration.dir_logs)
        & "/" & logname (flavor);
   begin
      if AD.Exists (logpath) then
         AD.Delete_File (logpath);
      end if;
      TIO.Create (File => Flog (flavor),
                  Mode => TIO.Out_File,
                  Name => logpath);
      if flavor = total then
         TIO.Put_Line (Flog (flavor), "-=>  Chronology of last build  <=-");
         TIO.Put_Line (Flog (flavor), "Started: " & timestamp (start_time));
         TIO.Put_Line (Flog (flavor), "Ports to build:" &
                                      PKG.original_queue_size'Img);
         TIO.Put_Line (Flog (flavor), "");
         TIO.Put_Line (Flog (flavor), "Purging any ignored/broken ports " &
                                      "first ...");
         TIO.Flush (Flog (flavor));
      end if;
      exception
      when others =>
         raise pilot_log
           with "Failed to create or delete " & logpath & bailing;
   end start_logging;


   --------------------
   --  stop_logging  --
   --------------------
   procedure stop_logging (flavor : count_type) is
   begin
      if flavor = total then
         TIO.Put_Line (Flog (flavor), "Finished: " & timestamp (stop_time));
         TIO.Put_Line (Flog (flavor), CYC.log_duration (start => start_time,
                                                        stop  => stop_time));
         TIO.Put_Line
           (Flog (flavor), LAT.LF &
              "---------------------------" & LAT.LF &
              "--  Final Statistics" & LAT.LF &
              "---------------------------" & LAT.LF &
              " Initial queue size:" & bld_counter (total)'Img & LAT.LF &
              "     packages built:" & bld_counter (success)'Img & LAT.LF &
              "            ignored:" & bld_counter (ignored)'Img & LAT.LF &
              "            skipped:" & bld_counter (skipped)'Img & LAT.LF &
              "             failed:" & bld_counter (failure)'Img);
      end if;
      TIO.Close (Flog (flavor));
   end stop_logging;


   -----------------------
   --  purge_distfiles  --
   -----------------------
   procedure purge_distfiles
   is
      type disktype is mod 2**64;
      procedure scan (plcursor : portkey_crate.Cursor);
      procedure kill (plcursor : portkey_crate.Cursor);
      procedure walk (name : String);
      function display_kmg (number : disktype) return String;
      abort_msg    : constant String := "Distfile purge operation aborted.";
      abort_purge  : Boolean := False;
      bytes_purged : disktype := 0;
      distfiles    : portkey_crate.Map;
      rmfiles      : portkey_crate.Map;

      procedure scan (plcursor : portkey_crate.Cursor)
      is
         origin   : JT.Text := portkey_crate.Key (plcursor);
         tracker  : constant port_id := portkey_crate.Element (plcursor);
         pndx     : constant port_index := ports_keys.Element (origin);
         distinfo : constant String := JT.USS (PM.configuration.dir_portsdir) &
                     "/" & JT.USS (origin) & "/distinfo";
         handle   : TIO.File_Type;
         bookend  : Natural;
      begin
         TIO.Open (File => handle, Mode => TIO.In_File, Name => distinfo);
         while not TIO.End_Of_File (handle) loop
            declare
               Line : String := TIO.Get_Line (handle);
            begin
               if Line (1 .. 4) = "SIZE" then
                  bookend := ASF.Index (Line, ")");
                  declare
                     S : JT.Text := JT.SUS (Line (7 .. bookend - 1));
                  begin
                     if not distfiles.Contains (S) then
                        distfiles.Insert (S, tracker);
                     end if;
                  exception
                     when failed : others =>
                        TIO.Put_Line ("purge_distfiles::scan: " & JT.USS (S));
                        TIO.Put_Line (EX.Exception_Information (failed));
                  end;
               end if;
            end;
         end loop;
         TIO.Close (handle);
      exception
         when others =>
            if TIO.Is_Open (handle) then
               TIO.Close (handle);
            end if;
      end scan;

      procedure walk (name : String)
      is
         procedure walkdir (item : AD.Directory_Entry_Type);
         procedure print (item : AD.Directory_Entry_Type);
         uniqid     : port_id := 0;
         leftindent : Natural :=
           JT.SU.Length (PM.configuration.dir_distfiles) + 2;

         procedure walkdir (item : AD.Directory_Entry_Type) is
         begin
            if AD.Simple_Name (item) /= "." and then
              AD.Simple_Name (item) /= ".."
            then
               walk (AD.Full_Name (item));
            end if;
         exception
            when AD.Name_Error =>
               abort_purge := True;
               TIO.Put_Line ("walkdir: " & name & " directory does not exist");
         end walkdir;
         procedure print (item : AD.Directory_Entry_Type)
         is
            FN    : constant String := AD.Full_Name (item);
            tball : JT.Text := JT.SUS (FN (leftindent .. FN'Last));
         begin
            if not distfiles.Contains (tball) then
               if not rmfiles.Contains (tball) then
                  uniqid := uniqid + 1;
                  rmfiles.Insert (Key => tball, New_Item => uniqid);
                  bytes_purged := bytes_purged + disktype (AD.Size (FN));
               end if;
            end if;
         end print;
      begin
         AD.Search (name, "*", (AD.Ordinary_File => True, others => False),
                    print'Access);
         AD.Search (name, "", (AD.Directory => True, others => False),
                    walkdir'Access);
      exception
         when AD.Name_Error =>
            abort_purge := True;
            TIO.Put_Line ("The " & name & " directory does not exist");
         when AD.Use_Error =>
            abort_purge := True;
            TIO.Put_Line ("Searching " & name & " directory is not supported");
         when failed : others =>
            abort_purge := True;
            TIO.Put_Line ("purge_distfiles: Unknown error - directory search");
            TIO.Put_Line (EX.Exception_Information (failed));
      end walk;

      function display_kmg (number : disktype) return String
      is
         type kmgtype is delta 0.01 digits 6;
         kilo : constant disktype := 1024;
         mega : constant disktype := kilo * kilo;
         giga : constant disktype := kilo * mega;
         XXX  : kmgtype;
      begin
         if number > giga then
            XXX := kmgtype (number / giga);
            return XXX'Img & " gigabytes";
         elsif number > mega then
            XXX := kmgtype (number / mega);
            return XXX'Img & " megabytes";
         else
            XXX := kmgtype (number / kilo);
            return XXX'Img & " kilobytes";
         end if;
      end display_kmg;

      procedure kill (plcursor : portkey_crate.Cursor)
      is
         tarball : String := JT.USS (portkey_crate.Key (plcursor));
         path    : JT.Text := PM.configuration.dir_distfiles;
      begin
         JT.SU.Append (path, "/" & tarball);
         TIO.Put_Line ("Deleting " & tarball);
         AD.Delete_File (JT.USS (path));
      end kill;

   begin
      TIO.Put ("Scanning the distinfo file of every port in the tree ... ");
      if not read_flavor_index then
         TIO.Put_Line (abort_msg);
         return;
      end if;
      ports_keys.Iterate (Process => scan'Access);
      TIO.Put_Line ("done");
      walk (name => JT.USS (PM.configuration.dir_distfiles));
      if abort_purge then
         TIO.Put_Line (abort_msg);
      else
         rmfiles.Iterate (kill'Access);
         TIO.Put_Line ("Recovered" & display_kmg (bytes_purged));
      end if;
   end purge_distfiles;


   ------------------------------------------
   --  write_pkg_repos_configuration_file  --
   ------------------------------------------
   function write_pkg_repos_configuration_file return Boolean
   is
      repdir : constant String := get_repos_dir;
      target : constant String := repdir & "/00_synth.conf";
      pkgdir : constant String := JT.USS (PM.configuration.dir_packages);
      pubkey : constant String := PM.synth_confdir & "/" &
               JT.USS (PM.configuration.profile) & "-public.key";
      keydir : constant String := PM.synth_confdir & "/keys";
      tstdir : constant String := keydir & "/trusted";
      autgen : constant String := "# Automatically generated." & LAT.LF;
      fpfile : constant String := tstdir & "/fingerprint." &
               JT.USS (PM.configuration.profile);
      handle : TIO.File_Type;
      vscmd  : Boolean := False;
   begin
      if AD.Exists (target) then
         AD.Delete_File (target);
      elsif not AD.Exists (repdir) then
         AD.Create_Path (repdir);
      end if;
      TIO.Create (File => handle, Mode => TIO.Out_File, Name => target);
      TIO.Put_Line (handle, autgen);
      TIO.Put_Line (handle, "Synth: {");
      TIO.Put_Line (handle, "  url      : file://" & pkgdir & ",");
      TIO.Put_Line (handle, "  priority : 0,");
      TIO.Put_Line (handle, "  enabled  : yes,");
      if valid_signing_command then
         vscmd := True;
         TIO.Put_Line (handle, "  signature_type : FINGERPRINTS,");
         TIO.Put_Line (handle, "  fingerprints   : " & keydir);
      elsif set_synth_conf_with_RSA then
         TIO.Put_Line (handle, "  signature_type : PUBKEY,");
         TIO.Put_Line (handle, "  pubkey         : " & pubkey);
      end if;
      TIO.Put_Line (handle, "}");
      TIO.Close (handle);
      if vscmd then
         if AD.Exists (fpfile) then
            AD.Delete_File (fpfile);
         elsif not AD.Exists (tstdir) then
            AD.Create_Path (tstdir);
         end if;
         TIO.Create (File => handle, Mode => TIO.Out_File, Name => fpfile);
         TIO.Put_Line (handle, autgen);
         TIO.Put_Line (handle, "function    : sha256");
         TIO.Put_Line (handle, "fingerprint : " & profile_fingerprint);
         TIO.Close (handle);
      end if;
      return True;
   exception
      when others =>
         TIO.Put_Line ("Error: failed to create " & target);
         if TIO.Is_Open (handle) then
            TIO.Close (handle);
         end if;
         return False;
   end write_pkg_repos_configuration_file;


   ---------------------------------
   --  upgrade_system_everything  --
   ---------------------------------
   procedure upgrade_system_everything (skip_installation : Boolean := False;
                                        dry_run : Boolean := False)
   is
      command : constant String := host_pkg8 &
                                   " upgrade --yes --repository Synth";
      query   : constant String := host_pkg8 & " query -a %o:%n";
      sorry   : constant String := "Unfortunately, the system upgrade failed.";
   begin
      if not prescanned then
         if not read_flavor_index then
            TIO.Put_Line (sorry & " (flavor index scan)");
            return;
         end if;
      end if;
      portlist.Clear;
      TIO.Put_Line ("Querying system about current package installations.");
      begin
         declare
            comres  : constant String := JT.USS (CYC.generic_system_command (query));
            markers : JT.Line_Markers;
            uniqid  : Natural := 0;
         begin
            JT.initialize_markers (comres, markers);
            loop
               exit when not JT.next_line_present (comres, markers);
               declare
                  line      : constant String := JT.extract_line (comres, markers);
                  origin    : constant String := JT.part_1 (line, ":");
                  pkgbase   : constant String := JT.part_2 (line, ":");
                  flvquery  : constant String := host_pkg8 & " query %At:%Av " & pkgbase;
                  errprefix : constant String := "Installed package ignored, ";
                  origintxt : JT.Text := JT.SUS (origin);
                  target_id : port_id := port_match_failed;
                  maxprobe  : port_index := port_index (so_serial.Length) - 1;
                  found_it  : Boolean := False;
                  flvresult : JT.Text;
               begin
                  --  This approach isn't the greatest, but we're missing information.
                  --  At this port, all_ports array is not populated, so we can't compare the
                  --  package names to determine flavors.  So what we do is search so_serial
                  --  for the origin.  If it exists, the port has no flavors and we take the
                  --  target id.  Otherwise, we have to query the system for the installed
                  --  flavor.  It it fail on pre-flavor installations, though.  Once all packages
                  --  are completely replaced, this approach should work fine.

                  if so_porthash.Contains (origintxt) then
                     if so_serial.Contains (origintxt) then
                        target_id := so_porthash.Element (origintxt);
                        found_it := True;
                     else
                        flvresult := CYC.generic_system_command (flvquery);
                        declare
                           contents : constant String := JT.USS (flvresult);
                           markers  : JT.Line_Markers;
                        begin
                           JT.initialize_markers (contents, markers);
                           if JT.next_line_with_content_present (contents, "flavor:", markers) then
                              declare
                                 line : constant String := JT.extract_line (contents, markers);
                                 flvorigin : JT.Text;
                              begin
                                 flvorigin := JT.SUS (origin & "@" & JT.part_2 (line, ":"));
                                 if so_serial.Contains (flvorigin) then
                                    target_id := so_serial.Find_Index (flvorigin);
                                    found_it := True;
                                 end if;
                              end;
                           end if;
                        end;
                     end if;
                     if found_it then
                        uniqid := uniqid + 1;
                        plinsert (get_catport (all_ports (target_id)), uniqid);
                     else
                        TIO.Put_Line (errprefix & origin & " package unmatched");
                     end if;
                  else
                     TIO.Put_Line (errprefix & "missing from ports: " & origin);
                  end if;
               end;
            end loop;
         end;
      exception
         when others =>
            TIO.Put_Line (sorry & " (system query)");
            return;
      end;
      TIO.Put_Line ("Stand by, comparing installed packages against the ports tree.");
      if prerequisites_available and then
        scan_stack_of_single_ports (testmode => False) and then
        sanity_check_then_prefail (delete_first => False, dry_run => dry_run)
      then
         if dry_run then
            display_results_of_dry_run;
            return;
         else
            perform_bulk_run (testmode => False);
         end if;
      else
         if not SIG.graceful_shutdown_requested then
            TIO.Put_Line (sorry);
         end if;
         return;
      end if;
      if SIG.graceful_shutdown_requested then
         return;
      end if;
      if rebuild_local_respository (remove_invalid_packages => True) then
         if not skip_installation then
            if not Unix.external_command (command) then
               TIO.Put_Line (sorry);
            end if;
         end if;
      end if;
   end upgrade_system_everything;


   ------------------------------
   --  upgrade_system_exactly  --
   ------------------------------
   procedure upgrade_system_exactly
   is
      procedure build_train (plcursor : portkey_crate.Cursor);
      base_command : constant String := host_pkg8 &
                                        " install --yes --repository Synth";
      caboose : JT.Text;

      procedure build_train (plcursor : portkey_crate.Cursor)
      is
         full_origin : JT.Text renames portkey_crate.Key (plcursor);
         pix : constant port_index := ports_keys.Element (full_origin);
         pkgfile : constant String := JT.USS (all_ports (pix).package_name);
      begin
         JT.SU.Append (caboose, " " & JT.head (pkgfile, "."));
      end build_train;
   begin
      duplist.Iterate (Process => build_train'Access);
      declare
         command : constant String := base_command & JT.USS (caboose);
      begin
         if not Unix.external_command (command) then
            TIO.Put_Line ("Unfortunately, the system upgraded failed.");
         end if;
      end;
   end upgrade_system_exactly;


   -------------------------------
   --  insufficient_privileges  --
   -------------------------------
   function insufficient_privileges return Boolean
   is
      command : constant String := "/usr/bin/id -u";
      result  : JT.Text := CYC.generic_system_command (command);
      topline : JT.Text;
   begin
      JT.nextline (lineblock => result, firstline => topline);
      declare
         resint : constant Integer := Integer'Value (JT.USS (topline));
      begin
         return (resint /= 0);
      end;
   end insufficient_privileges;


   ---------------
   --  head_n1  --
   ---------------
   function head_n1 (filename : String) return String
   is
      handle : TIO.File_Type;
   begin
      TIO.Open (File => handle, Mode => TIO.In_File, Name => filename);
      if TIO.End_Of_File (handle) then
         TIO.Close (handle);
         return "";
      end if;

      declare
         line : constant String := TIO.Get_Line (handle);
      begin
         TIO.Close (handle);
         return line;
      end;
   end head_n1;


   -----------------------
   --  already_running  --
   -----------------------
   function already_running return Boolean
   is
      pid    : Integer;
      comres : JT.Text;
   begin
      if AD.Exists (pidfile) then
         declare
            textpid : constant String := head_n1 (pidfile);
            command : constant String := "/bin/ps -p " & textpid;
         begin
            --  test if valid by converting it (exception if fails)
            pid := Integer'Value (textpid);

            --  exception raised by line below if pid not found.
            comres := CYC.generic_system_command (command);
            if JT.contains (comres, "synth") then
               return True;
            else
               --  pidfile is obsolete, remove it.
               AD.Delete_File (pidfile);
               return False;
            end if;
         exception
            when others =>
               --  pidfile contains garbage, remove it
               AD.Delete_File (pidfile);
               return False;
         end;
      end if;
      return False;
   end already_running;


   -----------------------
   --  destroy_pidfile  --
   -----------------------
   procedure destroy_pidfile is
   begin
      if AD.Exists (pidfile) then
         AD.Delete_File (pidfile);
      end if;
   exception
      when others => null;
   end destroy_pidfile;


   ----------------------
   --  create_pidfile  --
   ----------------------
   procedure create_pidfile
   is
      pidtext : constant String := JT.int2str (Get_PID);
      handle  : TIO.File_Type;
   begin
      TIO.Create (File => handle, Mode => TIO.Out_File, Name => pidfile);
      TIO.Put_Line (handle, pidtext);
      TIO.Close (handle);
   end create_pidfile;


   ------------------------------
   --  set_replicant_platform  --
   ------------------------------
   procedure set_replicant_platform is
   begin
      REP.set_platform;
   end set_replicant_platform;


   ------------------------------------
   --  previous_run_mounts_detected  --
   ------------------------------------
   function previous_run_mounts_detected return Boolean is
   begin
      return REP.synth_mounts_exist;
   end previous_run_mounts_detected;


   -------------------------------------
   --  previous_realfs_work_detected  --
   -------------------------------------
   function previous_realfs_work_detected return Boolean is
   begin
      return REP.disk_workareas_exist;
   end previous_realfs_work_detected;


   ---------------------------------------
   --  old_mounts_successfully_removed  --
   ---------------------------------------
   function old_mounts_successfully_removed return Boolean is
   begin
      if REP.clear_existing_mounts then
         TIO.Put_Line ("Dismounting successful!");
         return True;
      end if;
      TIO.Put_Line ("The attempt failed. " &
                      "Check for stuck or ongoing processes and kill them.");
      TIO.Put_Line ("After that, try running Synth again or just manually " &
                      "unmount everything");
      TIO.Put_Line ("still attached to " &
                      JT.USS (PM.configuration.dir_buildbase));
      return False;
   end old_mounts_successfully_removed;


   --------------------------------------------
   --  old_realfs_work_successfully_removed  --
   --------------------------------------------
   function old_realfs_work_successfully_removed return Boolean is
   begin
      if REP.clear_existing_workareas then
         TIO.Put_Line ("Directory removal successful!");
         return True;
      end if;
      TIO.Put_Line ("The attempt to remove the work directories located at ");
      TIO.Put_Line (JT.USS (PM.configuration.dir_buildbase) & "failed.");
      TIO.Put_Line ("Please remove them manually before continuing");
      return False;
   end old_realfs_work_successfully_removed;


   -------------------------
   --  synthexec_missing  --
   -------------------------
   function synthexec_missing return Boolean
   is
      synthexec : constant String := host_localbase & "/libexec/synthexec";
   begin
      if AD.Exists (synthexec) then
         return False;
      end if;
      TIO.Put_Line (synthexec & " missing!" & bailing);
      return True;
   end synthexec_missing;


   ----------------------------------
   --  display_results_of_dry_run  --
   ----------------------------------
   procedure display_results_of_dry_run
   is
      procedure print (cursor : ranking_crate.Cursor);
      listlog  : TIO.File_Type;
      filename : constant String := "/var/synth/synth_status_results.txt";
      max_lots : constant scanners := get_max_lots;
      elap_raw : constant String := CYC.log_duration (start => scan_start,
                                                      stop  => scan_stop);
      elapsed  : constant String := elap_raw (elap_raw'First + 10 ..
                                                elap_raw'Last);
      goodlog  : Boolean;

      procedure print (cursor : ranking_crate.Cursor)
      is
         id     : port_id := ranking_crate.Element (cursor).ap_index;
         kind   : verdiff;
         diff   : constant String := version_difference (id, kind);
         origin : constant String := get_catport (all_ports (id));
      begin
         case kind is
            when newbuild => TIO.Put_Line ("  N => " & origin);
            when rebuild  => TIO.Put_Line ("  R => " & origin);
            when change   => TIO.Put_Line ("  U => " & origin & diff);
         end case;
         if goodlog then
            TIO.Put_Line (listlog, origin & diff);
         end if;
      end print;
   begin
      begin
         --  Try to defend malicious symlink: https://en.wikipedia.org/wiki/Symlink_race
         if AD.Exists (filename) then
            AD.Delete_File (filename);
         end if;
         TIO.Create (File => listlog, Mode => TIO.Out_File, Name => filename);
         goodlog := True;
      exception
         when others => goodlog := False;
      end;
      TIO.Put_Line ("These are the ports that would be built ([N]ew, " &
                   "[R]ebuild, [U]pgrade):");
      rank_queue.Iterate (print'Access);
      TIO.Put_Line ("Total packages that would be built:" &
                      rank_queue.Length'Img);
      if goodlog then
         TIO.Put_Line
           (listlog, LAT.LF &
              "------------------------------" & LAT.LF &
              "--  Statistics" & LAT.LF &
              "------------------------------" & LAT.LF &
              " Ports scanned :" & last_port'Img & LAT.LF &
              "  Elapsed time : " & elapsed & LAT.LF &
              "   Parallelism :" & max_lots'Img & " scanners" & LAT.LF &
              "          ncpu :" & number_cores'Img);
         TIO.Close (listlog);
         TIO.Put_Line ("The complete build list can also be found at:"
                       & LAT.LF & filename);
      end if;
   end display_results_of_dry_run;


   ---------------------
   --  get_repos_dir  --
   ---------------------
   function get_repos_dir return String
   is
      command : String := host_pkg8 & " config repos_dir";
      content : JT.Text;
      topline : JT.Text;
      crlen1  : Natural;
      crlen2  : Natural;
   begin
      content := CYC.generic_system_command (command);
      crlen1 := JT.SU.Length (content);
      loop
         JT.nextline (lineblock => content, firstline => topline);
         crlen2 := JT.SU.Length (content);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if not JT.equivalent (topline, "/etc/pkg/") then
            return JT.USS (topline);
         end if;
      end loop;
      --  fallback, use default
      return host_localbase & "/etc/pkg/repos";
   end get_repos_dir;


   ------------------------------------
   --  interact_with_single_builder  --
   ------------------------------------
   function interact_with_single_builder return Boolean
   is
      EA_defined : constant Boolean := Unix.env_variable_defined (brkname);
   begin
      if Natural (portlist.Length) /= 1 then
         return False;
      end if;
      if not EA_defined then
         return False;
      end if;
      return CYC.valid_test_phase (Unix.env_variable_value (brkname));
   end interact_with_single_builder;


   ----------------------------------------------
   --  bulk_run_then_interact_with_final_port  --
   ----------------------------------------------
   procedure bulk_run_then_interact_with_final_port
   is
      uscatport : JT.Text := portkey_crate.Key (Position => portlist.First);
      brkphase : constant String := Unix.env_variable_value (brkname);
      buildres : Boolean;
      ptid     : port_id;
   begin
      if ports_keys.Contains (Key => uscatport) then
         ptid := ports_keys.Element (Key => uscatport);
      end if;

      OPS.unlist_port (ptid);
      perform_bulk_run (testmode => True);
      if SIG.graceful_shutdown_requested then
         return;
      end if;
      if bld_counter (ignored) > 0 or else
        bld_counter (skipped) > 0 or else
        bld_counter (failure) > 0
      then
         TIO.Put_Line ("It appears a prerequisite failed, so the interactive" &
                         " build of");
         TIO.Put_Line (JT.USS (uscatport) & " has been cancelled.");
         return;
      end if;
      TIO.Put_Line ("Starting interactive build of " & JT.USS (uscatport));
      TIO.Put_Line ("Stand by, building up to the point requested ...");

      REP.initialize (testmode => True, num_cores => PortScan.cores_available);
      CYC.initialize (test_mode => True, jail_env => REP.jail_environment);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);

      Unix.cone_of_silence (deploy => False);
      case software_framework is
         when ports_collection =>
            buildres := FPC.build_package (id          => PortScan.scan_slave,
                                           sequence_id => ptid,
                                           interactive => True,
                                           interphase  => brkphase);
         when pkgsrc =>
            if PLAT.standalone_pkg8_install (PortScan.scan_slave) then
               buildres := NPS.build_package (id => PortScan.scan_slave,
                                              sequence_id => ptid,
                                              interactive => True,
                                              interphase  => brkphase);
            end if;
      end case;

      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
   end bulk_run_then_interact_with_final_port;


   --------------------------
   --  synth_launch_clash  --
   --------------------------
   function synth_launch_clash return Boolean
   is
      function get_usrlocal return String;
      function get_usrlocal return String is
      begin
         if JT.equivalent (PM.configuration.dir_system, "/") then
            return host_localbase;
         end if;
         return JT.USS (PM.configuration.dir_system) & host_localbase;
      end get_usrlocal;

      cwd      : constant String  := AD.Current_Directory;
      usrlocal : constant String  := get_usrlocal;
      portsdir : constant String  := JT.USS (PM.configuration.dir_portsdir);
      ullen    : constant Natural := usrlocal'Length;
      pdlen    : constant Natural := portsdir'Length;
   begin
      if cwd = usrlocal or else cwd = portsdir then
         return True;
      end if;
      if cwd'Length > ullen and then
        cwd (1 .. ullen + 1) = usrlocal & "/"
      then
         return True;
      end if;
      if cwd'Length > pdlen and then
        cwd (1 .. pdlen + 1) = portsdir & "/"
      then
         return True;
      end if;
      return False;
   exception
      when others => return True;
   end synth_launch_clash;


   --------------------------
   --  version_difference  --
   --------------------------
   function version_difference (id : port_id; kind : out verdiff) return String
   is
      dir_pkg : constant String := JT.USS (PM.configuration.dir_repository);
      current : constant String := JT.USS (all_ports (id).package_name);
      version : constant String := JT.USS (all_ports (id).port_version);
   begin
      if AD.Exists (dir_pkg & "/" & current) then
         kind := rebuild;
         return " (rebuild " & version & ")";
      end if;
      declare
         pkgbase : constant String := JT.head (current, "-");
         pattern : constant String := pkgbase & "-*.pkg";
         upgrade : JT.Text;

         pkg_search : AD.Search_Type;
         dirent     : AD.Directory_Entry_Type;
      begin
         AD.Start_Search (Search    => pkg_search,
                          Directory => dir_pkg,
                          Filter    => (AD.Ordinary_File => True, others => False),
                          Pattern   => pattern);
         while AD.More_Entries (Search => pkg_search) loop
            AD.Get_Next_Entry (Search => pkg_search, Directory_Entry => dirent);
            declare
               sname    : String := AD.Simple_Name (dirent);
               testbase : String := PKG.query_pkgbase (dir_pkg & "/" & sname);
               testver  : String := JT.tail (JT.head (sname, "."), "-");
            begin
               if testbase = pkgbase then
                  upgrade := JT.SUS (" (" & testver & " => " & version & ")");
               end if;
            end;
         end loop;
         if not JT.IsBlank (upgrade) then
            kind := change;
            return JT.USS (upgrade);
         end if;
      end;
      kind := newbuild;
      return " (new " & version & ")";
   end version_difference;


   ------------------------
   --  file_permissions  --
   ------------------------
   function file_permissions (full_path : String) return String
   is
      command : constant String := "/usr/bin/stat -f %Lp " & full_path;
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         return "000";
      end if;
      JT.nextline (lineblock => content, firstline => topline);
      return JT.USS (topline);
   end file_permissions;


   --------------------------------------
   --  acceptable_RSA_signing_support  --
   --------------------------------------
   function acceptable_RSA_signing_support return Boolean
   is
      file_prefix   : constant String := PM.synth_confdir & "/" &
                      JT.USS (PM.configuration.profile) & "-";
      key_private   : constant String := file_prefix & "private.key";
      key_public    : constant String := file_prefix & "public.key";
      found_private : constant Boolean := AD.Exists (key_private);
      found_public  : constant Boolean := AD.Exists (key_public);
      sorry         : constant String := "The generated repository will not " &
                      "be signed due to the misconfiguration.";
      repo_key      : constant String := JT.USS (PM.configuration.dir_buildbase)
                      & ss_base & "/etc/repo.key";
   begin
      if not found_private and then not found_public then
         return True;
      end if;
      if found_public and then not found_private then
         TIO.Put_Line ("A public RSA key file has been found without a " &
                         "corresponding private key file.");
         TIO.Put_Line (sorry);
         return True;
      end if;
      if found_private and then not found_public then
         TIO.Put_Line ("A private RSA key file has been found without a " &
                         "corresponding public key file.");
         TIO.Put_Line (sorry);
         return True;
      end if;
      declare
         mode : constant String := file_permissions (key_private);
      begin
         if mode /= "400" then
            TIO.Put_Line ("The private RSA key file has insecure file " &
                            "permissions (" & mode & ")");
            TIO.Put_Line ("Please change the mode of " & key_private &
                            " to 400 before continuing.");
            return False;
         end if;
      end;
      declare
      begin
         AD.Copy_File (Source_Name => key_private,
                       Target_Name => repo_key);
         return True;
      exception
         when failed : others =>
            TIO.Put_Line ("Failed to copy private RSA key to builder.");
            TIO.Put_Line (EX.Exception_Information (failed));
            return False;
      end;
   end acceptable_RSA_signing_support;


   ----------------------------------
   --  acceptable_signing_command  --
   ----------------------------------
   function valid_signing_command return Boolean
   is
      file_prefix   : constant String := PM.synth_confdir & "/" &
                      JT.USS (PM.configuration.profile) & "-";
      fingerprint   : constant String := file_prefix & "fingerprint";
      ext_command   : constant String := file_prefix & "signing_command";
      found_finger  : constant Boolean := AD.Exists (fingerprint);
      found_command : constant Boolean := AD.Exists (ext_command);
      sorry         : constant String := "The generated repository will not " &
                      "be externally signed due to the misconfiguration.";
   begin
      if found_finger and then found_command then
         if JT.IsBlank (one_line_file_contents (fingerprint)) or else
           JT.IsBlank (one_line_file_contents (ext_command))
         then
            TIO.Put_Line ("At least one of the profile signing command " &
                            "files is blank");
            TIO.Put_Line (sorry);
            return False;
         end if;
         return True;
      end if;

      if found_finger then
         TIO.Put_Line ("The profile fingerprint was found but not the " &
                         "signing command");
         TIO.Put_Line (sorry);
      elsif found_command then
        TIO.Put_Line ("The profile signing command was found but not " &
                        "the fingerprint");
         TIO.Put_Line (sorry);
      end if;

      return False;
   end valid_signing_command;


   -----------------------
   --  signing_command  --
   -----------------------
   function signing_command return String
   is
      filename : constant String := PM.synth_confdir & "/" &
                 JT.USS (PM.configuration.profile) & "-signing_command";
   begin
      return one_line_file_contents (filename);
   end signing_command;


   ---------------------------
   --  profile_fingerprint  --
   ---------------------------
   function profile_fingerprint return String
   is
      filename : constant String := PM.synth_confdir & "/" &
                 JT.USS (PM.configuration.profile) & "-fingerprint";
   begin
      return one_line_file_contents (filename);
   end profile_fingerprint;


   -------------------------------
   --  set_synth_conf_with_RSA  --
   -------------------------------
   function set_synth_conf_with_RSA return Boolean
   is
      file_prefix   : constant String := PM.synth_confdir & "/" &
                      JT.USS (PM.configuration.profile) & "-";
      key_private   : constant String := file_prefix & "private.key";
      key_public    : constant String := file_prefix & "public.key";
      found_private : constant Boolean := AD.Exists (key_private);
      found_public  : constant Boolean := AD.Exists (key_public);
   begin
      return
        found_public and then
        found_private and then
        file_permissions (key_private) = "400";
   end set_synth_conf_with_RSA;


   ------------------------------
   --  one_line_file_contents  --
   ------------------------------
   function one_line_file_contents (filename : String) return String
   is
      target_file : TIO.File_Type;
      contents    : JT.Text := JT.blank;
   begin
      TIO.Open (File => target_file, Mode => TIO.In_File, Name => filename);
      if not TIO.End_Of_File (target_file) then
         contents := JT.SUS (TIO.Get_Line (target_file));
      end if;
      TIO.Close (target_file);
      return JT.USS (contents);
   end one_line_file_contents;


   -------------------------
   --  valid_system_root  --
   -------------------------
   function valid_system_root return Boolean is
   begin
      if REP.boot_modules_directory_missing then
         TIO.Put_Line ("The /boot directory is optional, but when it exists, " &
                         "the /boot/modules directory must also exist.");
         TIO.Put ("Please create the ");
         if not JT.equivalent (PM.configuration.dir_system, "/") then
            TIO.Put (JT.USS (PM.configuration.dir_system));
         end if;
         TIO.Put_Line ("/boot/modules directory and retry.");
         return False;
      end if;
      return True;
   end valid_system_root;


   ------------------------------------------
   --  host_pkg8_conservative_upgrade_set  --
   ------------------------------------------
   function host_pkg8_conservative_upgrade_set return Boolean
   is
      command : constant String := host_pkg8 & " config CONSERVATIVE_UPGRADE";
      content : JT.Text;
      topline : JT.Text;
   begin
      content := CYC.generic_system_command (command);
      JT.nextline (lineblock => content, firstline => topline);
      return JT.equivalent (topline, "yes");
   end host_pkg8_conservative_upgrade_set;


   -----------------------------------
   --  TERM_defined_in_environment  --
   -----------------------------------
   function TERM_defined_in_environment return Boolean
   is
      defined : constant Boolean := Unix.env_variable_defined ("TERM");
   begin
      if not defined then
         TIO.Put_Line ("Please define TERM in environment first and retry.");
         TIO.Put_Line ("If synth is being called in a cron job, define TERM=" & dumterm & ".");
      end if;
      return defined;
   end TERM_defined_in_environment;


   -------------------------
   --  ensure_port_index  --
   -------------------------
   function ensure_port_index return Boolean
   is
      index_file  : constant String := "/var/cache/synth/" &
                    JT.USS (PM.configuration.profile) & "-index";
      needs_gen   : Boolean := True;
      tree_newer  : Boolean;
      valid_check : Boolean;
      answer      : Character;
      portsdir    : constant String := JT.USS (PM.configuration.dir_portsdir);
      command     : constant String := "/usr/bin/touch " & index_file;
      stars : String (1 .. 67) := (others => '*');
      msg1  : String := " The ports tree has at least one file newer than the flavor index.";
      msg2  : String := " However, port directories perfectly match.  Should the index be";
      msg3  : String := " regenerated? (Y/N) ";
   begin
      if AD.Exists (index_file) then
         tree_newer := index_out_of_date (index_file, valid_check);
         if not valid_check then
            return False;
         end if;
         if tree_newer then
            --  TERM guaranteed to already be defined (see TERM_defined_in_environment)
            if Unix.env_variable_value ("TERM") /= dumterm then
               if tree_directories_match (index_file, portsdir) then
                  TIO.Put_Line (stars);
                  TIO.Put_Line (msg1);
                  TIO.Put_Line (msg2);
                  TIO.Put      (msg3);
                  loop
                     Ada.Text_IO.Get_Immediate (answer);
                     case answer is
                        when 'Y' | 'y' =>
                           TIO.Put_Line ("yes");
                           exit;
                        when 'N' | 'n' =>
                           TIO.Put_Line ("no");
                           if Unix.external_command (command) then
                              needs_gen := False;
                           end if;
                           exit;
                        when others => null;
                     end case;
                  end loop;
                  TIO.Put_Line (stars);
               end if;
            end if;
         else
            needs_gen := False;
         end if;
      end if;

      if needs_gen then
         return generate_ports_index (index_file, portsdir);
      else
         return True;
      end if;
   end ensure_port_index;


   -------------------------
   --  index_out_of_date  --
   -------------------------
   function index_out_of_date (index_file : String; valid : out Boolean) return Boolean
   is
      index_file_modtime : CAL.Time;
      result : Boolean;
   begin
      valid := False;
      begin
         index_file_modtime := AD.Modification_Time (index_file);
      exception
         when AD.Use_Error =>
            TIO.Put_Line ("File system doesn't support modification times, must eject!");
            return False;
         when others =>
            TIO.Put_Line ("index_out_of_date: problem getting index file modification time");
            return False;
      end;
      result := PortScan.tree_newer_than_reference
        (portsdir  => JT.USS (PM.configuration.dir_portsdir),
         reference => index_file_modtime,
         valid     => valid);
      if valid then
         return result;
      else
         TIO.Put_Line ("Failed to determine if index is out of date, must eject!");
         return False;
      end if;
   end index_out_of_date;


   ------------------------------
   --  tree_directories_match  --
   ------------------------------
   function tree_directories_match (index_file, portsdir : String) return Boolean
   is
      procedure flavor_line (cursor : string_crate.Cursor);

      last_entry : JT.Text;
      broken     : Boolean := False;

      procedure flavor_line (cursor : string_crate.Cursor)
      is
         line    : constant String := JT.USS (string_crate.Element (Position => cursor));
         catport : JT.Text := JT.SUS (JT.part_1 (line, "@"));
      begin
         if not broken then
            if not JT.equivalent (last_entry, catport) then
               last_entry := catport;
               if ports_keys.Contains (catport) then
                  ports_keys.Delete (catport);
               else
                  broken := True;
               end if;
            end if;
         end if;
      end flavor_line;
   begin
      load_index_for_store_origins;
      prescan_ports_tree (portsdir);

      so_serial.Iterate (flavor_line'Access);
      if not broken then
         --  Every port on the flavor index is present in the ports tree
         --  We still need to check that there are not ports in the tree not listed in index
         if not ports_keys.Is_Empty then
            broken := True;
         end if;
      end if;

      reset_ports_tree;
      clear_store_origin_data;
      return not broken;
   end tree_directories_match;

end PortScan.Pilot;
