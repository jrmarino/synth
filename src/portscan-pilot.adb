--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with Ada.Strings.Fixed;
with PortScan.Ops;
with PortScan.Packages;
with PortScan.Buildcycle;
with Signals;
with Unix;

package body PortScan.Pilot is

   package CLI renames Ada.Command_Line;
   package ASF renames Ada.Strings.Fixed;
   package OPS renames PortScan.Ops;
   package PKG renames PortScan.Packages;
   package CYC renames PortScan.Buildcycle;
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
      if CLI.Argument_Count = 2 then
         --  Check if this is a file
         declare
            Arg2 : constant String := trimmed_catport (CLI.Argument (2));
         begin
            if AD.Exists (Arg2) then
               return valid_file (Arg2);
            end if;
            if valid_catport (catport => Arg2) then
               plinsert (Arg2, 2);
               return True;
            else
               TIO.Put_Line (badport & Arg2);
               return False;
            end if;
         end;
      end if;
      for k in 2 .. CLI.Argument_Count loop
         declare
            Argk : constant String := trimmed_catport (CLI.Argument (k));
         begin
            if valid_catport (catport => Argk) then
               plinsert (Argk, k);
            else
               TIO.Put_Line (badport & "'" & Argk & "'" & k'Img);
               return False;
            end if;
         end;
      end loop;
      return True;
   end store_origins;


   -------------------------------
   --  build_pkg8_as_necessary  --
   -------------------------------
   function build_pkg8_as_necessary return Boolean
   is
      pkg_good  : Boolean;
      good_scan : Boolean;
      selection : PortScan.port_id;
      result    : Boolean := True;
   begin
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      good_scan := PortScan.scan_single_port (catport => pkgng);

      if good_scan then
         PortScan.set_build_priority;
      else
         TIO.Put_Line ("Unexpected pkg(8) scan failure!");
         result := False;
         goto clean_exit;
      end if;

      if SIG.graceful_shutdown_requested then
         goto clean_exit;
      end if;

      PKG.limited_sanity_check
        (repository => JT.USS (PM.configuration.dir_repository),
         dry_run    => False, suppress_remote => True);

      if PKG.queue_is_empty then
         goto clean_exit;
      end if;

      CYC.initialize (test_mode => False, jail_env => REP.jail_environment);
      selection := OPS.top_buildable_port;
      if SIG.graceful_shutdown_requested or else selection = port_match_failed
      then
         goto clean_exit;
      end if;
      TIO.Put ("Stand by, building pkg(8) first ... ");

      pkg_good := CYC.build_package (id => PortScan.scan_slave,
                                     sequence_id => selection);
      if not pkg_good then
         TIO.Put_Line ("Failed!!" & bailing);
         result := False;
         goto clean_exit;
      end if;

      PortScan.reset_ports_tree;
      TIO.Put_Line ("done!");

      <<clean_exit>>
      if SIG.graceful_shutdown_requested then
         TIO.Put_Line (shutreq);
         result := False;
      end if;
      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
      reset_ports_tree;
      prescan_ports_tree (JT.USS (PM.configuration.dir_portsdir));
      return result;

   end build_pkg8_as_necessary;


   ----------------------------------
   --  scan_stack_of_single_ports  --
   ----------------------------------
   function scan_stack_of_single_ports (testmode : Boolean) return Boolean
   is
      procedure scan (plcursor : portkey_crate.Cursor);
      successful : Boolean := True;

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
         if not PortScan.scan_single_port (origin) then
            TIO.Put_Line
              ("Scan of " & origin & " failed" &
                 PortScan.obvious_problem
                 (JT.USS (PM.configuration.dir_portsdir), origin) &
                 ", it will not be considered.");
         end if;
      end scan;

   begin
      REP.initialize (testmode, PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      if SIG.graceful_shutdown_requested then
         goto clean_exit;
      end if;
      if not REP.standalone_pkg8_install (PortScan.scan_slave) then
         TIO.Put_Line ("Failed to install pkg(8) scanner" & bailing);
         successful := False;
         goto clean_exit;
      end if;
      portlist.Iterate (Process => scan'Access);
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

      if not PKG.limited_cached_options_check then
         --  Error messages emitted by function
         return False;
      end if;

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

      OPS.initialize_hooks;
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
      TIO.Put ("Would you like to upgrade your system with the new " &
               "packages now (Y/N)? ");
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
         return False;
      end if;
   end fully_scan_ports_tree;


   ---------------------------------
   --  rebuild_local_respository  --
   ---------------------------------
   function rebuild_local_respository (use_full_scan : Boolean := True)
                                       return Boolean
   is
      repo : constant String := JT.USS (PM.configuration.dir_repository);
      main : constant String := JT.USS (PM.configuration.dir_packages);
      xz_meta    : constant String := main & "/meta.txz";
      xz_digest  : constant String := main & "/digests.txz";
      xz_pkgsite : constant String := main & "/packagesite.txz";
      build_res  : Boolean;
   begin
      if SIG.graceful_shutdown_requested then
         --  In case it was previously requested
         return False;
      end if;
      if use_full_scan then
         REP.initialize (testmode => False,
                         num_cores => PortScan.cores_available);
         REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
         PKG.preclean_repository (repo);
         REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
         REP.finalize;
         if SIG.graceful_shutdown_requested then
            TIO.Put_Line (shutreq);
            return False;
         end if;
         TIO.Put_Line ("Stand by, recursively scanning" & portlist.Length'Img &
                         " ports serially.");
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
      end if;
      if AD.Exists (xz_meta) then
         AD.Delete_File (xz_meta);
      end if;
      if AD.Exists (xz_digest) then
         AD.Delete_File (xz_digest);
      end if;
      if AD.Exists (xz_pkgsite) then
         AD.Delete_File (xz_pkgsite);
      end if;
      TIO.Put_Line ("Packages validated, rebuilding local repository.");
      REP.initialize (testmode => False, num_cores => PortScan.cores_available);
      REP.launch_slave (id => PortScan.scan_slave, opts => noprocs);
      build_res := REP.build_repository (PortScan.scan_slave);
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
               if valid_catport (line) then
                  plinsert (line, total);
                  total := total + 1;
               else
                  TIO.Put_Line (badport & line);
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


   ---------------------
   --  valid_catport  --
   ---------------------
   function valid_catport (catport : String) return Boolean
   is
      use type AD.File_Kind;
   begin
      if catport'Length = 0 then
         return False;
      end if;
      if catport (catport'First) = '/' then
         --  Invalid case where catport starts with "/" will cause an
         --  exception later as "cat" would be unexpectedly empty.
         return False;
      end if;
      if JT.contains (catport, "/") then
         declare
            cat   : constant String := JT.part_1 (catport);
            port  : constant String := JT.part_2 (catport);
            path1 : constant String := JT.USS (PM.configuration.dir_portsdir) &
                                       "/" & cat;
            fpath : constant String := path1 & "/" & port;
            alpha : constant Character := cat (1);
         begin
            if not AD.Exists (path1) then
               return False;
            end if;

            if alpha in 'A' .. 'Z' then
               return False;
            end if;

            if path1 = "distfiles" or else path1 = "packages" then
               return False;
            end if;

            if JT.contains (port, "/") then
               return False;
            end if;

            if not AD.Exists (fpath) then
               return False;
            end if;

            if AD.Kind (fpath) = AD.Directory then
               return True;
            end if;
         end;
      end if;
      return False;
   end valid_catport;


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
         TIO.Put_Line (Flog (flavor), "Started: " & CYC.timestamp (start_time));
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
         TIO.Put_Line (Flog (flavor), "Finished: " & CYC.timestamp (stop_time));
         TIO.Put_Line (Flog (flavor), CYC.log_duration (start => start_time,
                                                        stop  => stop_time));
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
      bytes_purged : disktype := 0;
      tracker      : port_id := 0;
      distfiles    : portkey_crate.Map;
      rmfiles      : portkey_crate.Map;

      procedure scan (plcursor : portkey_crate.Cursor)
      is
         origin   : JT.Text := portkey_crate.Key (plcursor);
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
                        tracker := tracker + 1;
                        distfiles.Insert (S, tracker);
                     end if;
                  end;
               end if;
            end;
         end loop;
      exception
         when others => null;
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
            when AD.Name_Error => null;
         end walkdir;
         procedure print (item : AD.Directory_Entry_Type)
         is
            FN    : constant String := AD.Full_Name (item);
            tball : JT.Text := JT.SUS (FN (leftindent .. FN'Last));
         begin
            if not distfiles.Contains (tball) then
               uniqid := uniqid + 1;
               rmfiles.Insert (Key => tball, New_Item => uniqid);
               bytes_purged := bytes_purged + disktype (AD.Size (FN));
            end if;
         end print;
      begin
         AD.Search (name, "*", (AD.Ordinary_File => True, others => False),
                    print'Access);
         AD.Search (name, "", (AD.Directory => True, others => False),
                    walkdir'Access);
      exception
         when AD.Name_Error =>
            TIO.Put_Line ("The " & name & " directory does not exist");
         when AD.Use_Error =>
            TIO.Put_Line ("Searching " & name & " directory is not supported");
         when others =>
            TIO.Put_Line ("purge_distfiles: Unknown error - directory search");
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
      PortScan.prescan_ports_tree (JT.USS (PM.configuration.dir_portsdir));
      TIO.Put ("Scanning the distinfo file of every port in the tree ... ");
      ports_keys.Iterate (Process => scan'Access);
      TIO.Put_Line ("done");
      walk (name => JT.USS (PM.configuration.dir_distfiles));
      rmfiles.Iterate (kill'Access);
      TIO.Put_Line ("Recovered" & display_kmg (bytes_purged));
   end purge_distfiles;


   ------------------------------------------
   --  write_pkg_repos_configuration_file  --
   ------------------------------------------
   function write_pkg_repos_configuration_file return Boolean
   is
      repdir : constant String := get_repos_dir;
      target : constant String := repdir & "/00_synth.conf";
      pkgdir : constant String := JT.USS (PM.configuration.dir_packages);
      handle : TIO.File_Type;
   begin
      if AD.Exists (target) then
         AD.Delete_File (target);
      elsif not AD.Exists (repdir) then
         AD.Create_Path (repdir);
      end if;
      TIO.Create (File => handle, Mode => TIO.Out_File, Name => target);
      TIO.Put_Line (handle, "# Automatically generated." & LAT.LF);
      TIO.Put_Line (handle, "Synth: {");
      TIO.Put_Line (handle, "  url      : file://" & pkgdir & ",");
      TIO.Put_Line (handle, "  priority : 0,");
      TIO.Put_Line (handle, "  enabled  : yes,");
      TIO.Put_Line (handle, "}");
      TIO.Close (handle);
      return True;
   exception
      when others =>
         TIO.Put_Line ("Error: failed to create " & target);
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
      query   : constant String := host_pkg8 & " query -a %o";
      sorry   : constant String := "Unfortunately, the system upgrade failed.";
   begin
      portlist.Clear;
      TIO.Put_Line ("Querying system about current package installations.");
      declare
         comres  : JT.Text;
         topline : JT.Text;
         crlen1  : Natural;
         crlen2  : Natural;
         uniqid  : Natural := 0;
      begin
         comres := CYC.generic_system_command (query);
         crlen1 := JT.SU.Length (comres);
         loop
            JT.nextline (lineblock => comres, firstline => topline);
            crlen2 := JT.SU.Length (comres);
            exit when crlen1 = crlen2;
            crlen1 := crlen2;
            uniqid := uniqid + 1;
            plinsert (JT.USS (topline), uniqid);
         end loop;
      exception
         when others =>
            TIO.Put_Line (sorry & " (system query)");
            return;
      end;
      TIO.Put_Line ("Stand by, comparing installed packages against the " &
                      "ports tree.");
      if build_pkg8_as_necessary and then
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
      if rebuild_local_respository then
         if not skip_installation then
            if not Unix.external_command (command)
            then
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

      procedure build_train (plcursor : portkey_crate.Cursor) is
      begin
         JT.SU.Append (caboose, " ");
         JT.SU.Append (caboose, portkey_crate.Key (plcursor));
      end build_train;
   begin
      portlist.Iterate (Process => build_train'Access);
      declare
         command : constant String :=
           base_command & JT.USS (caboose);
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
      filename : constant String := "/tmp/synth_status_results.txt";
      goodlog  : Boolean;

      procedure print (cursor : ranking_crate.Cursor)
      is
         id : port_id := ranking_crate.Element (cursor).ap_index;
      begin
         TIO.Put_Line ("  => " & get_catport (all_ports (id)));
         if goodlog then
            TIO.Put_Line (listlog, get_catport (all_ports (id)));
         end if;
      end print;
   begin
      declare
      begin
         TIO.Create (File => listlog, Mode => TIO.Out_File, Name => filename);
         goodlog := True;
      exception
         when others => goodlog := False;
      end;
      TIO.Put_Line ("These are the ports that would be built:");
      rank_queue.Iterate (print'Access);
      TIO.Put_Line ("Total packages that would be built:" &
                      rank_queue.Length'Img);
      if goodlog then
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
      use type CYC.phases;
      EA_defined : constant Boolean := Unix.env_variable_defined (brkname);
   begin
      if Natural (portlist.Length) /= 1 then
         return False;
      end if;
      if not EA_defined then
         return False;
      end if;
      return CYC.valid_test_phase (Unix.env_variable_value (brkname)) /=
        CYC.check_sanity;
   end interact_with_single_builder;


   ----------------------------------------------
   --  bulk_run_then_interact_with_final_port  --
   ----------------------------------------------
   procedure bulk_run_then_interact_with_final_port
   is
      uscatport : JT.Text := portkey_crate.Key (Position => portlist.First);
      brkphase : constant CYC.phases := CYC.valid_test_phase
                                        (Unix.env_variable_value (brkname));
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
      buildres := CYC.build_package (id          => PortScan.scan_slave,
                                     sequence_id => ptid,
                                     interactive => True,
                                     interphase  => brkphase);

      REP.destroy_slave (id => PortScan.scan_slave, opts => noprocs);
      REP.finalize;
   end bulk_run_then_interact_with_final_port;


   --------------------------
   --  synth_launch_clash  --
   --------------------------
   function synth_launch_clash return Boolean
   is
      function get_usrlocal return String;
      function get_usrlocal return String
      is
         ul : constant String  := "/usr/local";
      begin
         if JT.equivalent (PM.configuration.dir_system, "/") then
            return ul;
         end if;
         return JT.USS (PM.configuration.dir_system) & ul;
      end get_usrlocal;

      cwd      : constant String  := AD.Current_Directory;
      usrlocal : constant String  := get_usrlocal;
      ullen    : constant Natural := usrlocal'Length;
   begin
      if cwd = usrlocal then
         return True;
      end if;
      if cwd'Length > ullen and then
        cwd (1 .. ullen + 1) = usrlocal & "/"
      then
         return True;
      end if;
      return False;
   exception
      when others => return True;
   end synth_launch_clash;

end PortScan.Pilot;
