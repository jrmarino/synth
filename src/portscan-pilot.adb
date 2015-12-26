--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;
with PortScan.Ops;
with PortScan.Packages;
with PortScan.Buildcycle;
with Replicant;

package body PortScan.Pilot is

   package CLI renames Ada.Command_Line;
   package OPS renames PortScan.Ops;
   package PKG renames PortScan.Packages;
   package CYC renames PortScan.Buildcycle;
   package REP renames Replicant;

   ---------------------
   --  store_origins  --
   ---------------------
   function store_origins return Boolean
   is
   begin
      if CLI.Argument_Count <= 1 then
         return False;
      end if;
      portlist.Clear;
      if CLI.Argument_Count = 2 then
         --  Check if this is a file
         if AD.Exists (CLI.Argument (2)) then
            return valid_file (CLI.Argument (2));
         end if;
         if valid_catport (catport => CLI.Argument (2)) then
            plinsert (CLI.Argument (2), 2);
            return True;
         else
            TIO.Put_Line (badport & CLI.Argument (2));
            return False;
         end if;
      end if;
      for k in 2 .. CLI.Argument_Count loop
         if valid_catport (catport => CLI.Argument (k)) then
            plinsert (CLI.Argument (k), k);
         else
            TIO.Put_Line (badport & "'" & CLI.Argument (k) & "'" & k'Img);
            return False;
         end if;
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
   begin
      REP.initialize;
      good_scan := PortScan.scan_single_port
        (portsdir   => JT.USS (PM.configuration.dir_portsdir),
         catport    => pkgng,
         repository => JT.USS (PM.configuration.dir_repository));

      if good_scan then
         PortScan.set_build_priority;
      else
         TIO.Put_Line ("Unexpected pkg(8) scan failure!");
         Replicant.finalize;
         return False;
      end if;

      PKG.limited_sanity_check (JT.USS (PM.configuration.dir_repository));

      if PKG.queue_is_empty then
         return True;
      end if;

      CYC.initialize (test_mode => False);
      selection := OPS.top_buildable_port;
      TIO.Put ("Stand by, building pkg(8) first ... ");

      REP.launch_slave (id => 1);
      pkg_good := CYC.build_package (id => 1, sequence_id => selection);
      REP.destroy_slave (id => 1);
      if not pkg_good then
         TIO.Put_Line ("Failed!!" & bailing);
         REP.finalize;
         return False;
      end if;

      PortScan.reset_ports_tree;
      TIO.Put_Line ("done!");
      REP.finalize;
      return True;

   end build_pkg8_as_necessary;


   ----------------------------------
   --  scan_stack_of_single_ports  --
   ----------------------------------
   function scan_stack_of_single_ports return Boolean
   is
      procedure scan (plcursor : portkey_crate.Cursor);
      successful : Boolean := True;

      procedure scan (plcursor : portkey_crate.Cursor)
      is
         origin  : constant String := JT.USS (portkey_crate.Key (plcursor));
      begin
         if not successful then
            return;
         end if;
         if origin = pkgng then
            --  we've already built pkg(8) if we get here, just skip it
            return;
         end if;
         successful := PortScan.scan_single_port
           (portsdir   => JT.USS (PM.configuration.dir_portsdir),
            catport    => origin,
            repository => JT.USS (PM.configuration.dir_repository));
         if not successful then
            TIO.Put_Line ("Scan of " & origin & " failed!" & bailing);
         end if;
      end scan;

   begin
      portlist.Iterate (Process => scan'Access);
      if successful then
         PortScan.set_build_priority;
      end if;
      return successful;
   end scan_stack_of_single_ports;


   ---------------------------------
   --  sanity_check_then_prefail  --
   ---------------------------------
   function sanity_check_then_prefail return Boolean
   is
      ptid : PortScan.port_id;
      num_skipped : Natural;
   begin
      start_time := CAL.Clock;

      PKG.limited_sanity_check (JT.USS (PM.configuration.dir_repository));
      bld_counter := (OPS.queue_length, 0, 0, 0, 0);

      start_logging (total);
      start_logging (ignored);
      start_logging (skipped);
      start_logging (success);
      start_logging (failure);

      loop
         ptid := OPS.next_ignored_port;
         exit when ptid = PortScan.port_match_failed;
         bld_counter (ignored) := bld_counter (ignored) + 1;
         TIO.Put_Line (Flog (total), CYC.elapsed_now & " " &
                         OPS.port_name (ptid) & " has been ignored: " &
                         OPS.ignore_reason (ptid));
         TIO.Put_Line (Flog (ignored), CYC.elapsed_now & " Reason: " &
                          OPS.ignore_reason (ptid));
         OPS.cascade_failed_build (id         => ptid,
                                   numskipped => num_skipped,
                                   logs       => Flog);
         bld_counter (skipped) := bld_counter (skipped) + num_skipped;
      end loop;
      stop_logging (ignored);
      TIO.Put_Line (Flog (total), CYC.elapsed_now & " Sanity check complete. "
                    & "Ports remaining to build:" & OPS.queue_length'Img);
      if OPS.integrity_intact then
         return True;
      else
         TIO.Put_Line ("Queue integrity lost! " & bailing);
         return False;
      end if;
   end sanity_check_then_prefail;


   ------------------------
   --  perform_bulk_run  --
   ------------------------
   procedure perform_bulk_run (testmode : Boolean) is
   begin
      if PKG.queue_is_empty then
         TIO.Put_Line ("After inspection, it has been determined that there " &
                         "are no packages that");
         TIO.Put_Line ("require rebuilding.  Therefore the task is complete.");
      else
         REP.initialize;
         CYC.initialize (testmode);
         OPS.parallel_bulk_run (num_builders => PM.configuration.num_builders,
                                logs => Flog);
         REP.finalize;
         stop_time := CAL.Clock;
         stop_logging (total);
         stop_logging (success);
         stop_logging (failure);
         stop_logging (skipped);
         TIO.Put_Line ("The task is complete.  Final tally:");
         TIO.Put_Line ("Initial queue size:" & bld_counter (total)'Img);
         TIO.Put_Line ("    packages built:" & bld_counter (success)'Img);
         TIO.Put_Line ("           ignored:" & bld_counter (ignored)'Img);
         TIO.Put_Line ("           skipped:" & bld_counter (skipped)'Img);
         TIO.Put_Line ("            failed:" & bld_counter (failure)'Img);
         TIO.Put_Line ("");
         TIO.Put_Line (CYC.log_duration (start_time, stop_time));
      end if;
   end perform_bulk_run;


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
                                      bld_counter (total)'Img);
         TIO.Put_Line (Flog (flavor), "");
         TIO.Put_Line (Flog (flavor), "Purging any ignored/broken ports " &
                                      "first ...");
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

end PortScan.Pilot;
