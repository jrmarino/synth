--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Numerics.Discrete_Random;
with GNAT.String_Split;
with PortScan.Buildcycle;
with Replicant;

package body PortScan.Ops is

   package GSS renames GNAT.String_Split;
   package REP renames Replicant;
   package CYC renames PortScan.Buildcycle;


   --------------------------
   --  initialize_display  --
   --------------------------
   procedure initialize_display (num_builders : builders) is
   begin
      if PM.configuration.avec_ncurses then
         curses_support := DPY.launch_monitor (num_builders);
      end if;
   end initialize_display;


   -------------------------
   --  parallel_bulk_run  --
   -------------------------
   procedure parallel_bulk_run (num_builders : builders; logs : dim_handlers)
   is
      subtype cycle_count is Natural range 1 .. 9;
      instructions   : dim_instruction   := (others => port_match_failed);
      builder_states : dim_builder_state := (others => idle);
      cntcycle       : cycle_count       := cycle_count'First;
      run_complete   : Boolean           := False;
      available      : Positive          := Integer (num_builders);
      target         : port_id;
      all_idle       : Boolean;
      cntskip        : Natural;
      sumdata        : DPY.summary_rec;

      task type build (builder : builders);
      task body build
      is
         type Rand_Draw is range 1 .. 20;
         package Rand20 is new Ada.Numerics.Discrete_Random (Rand_Draw);
         seed : Rand20.Generator;
         build_result : Boolean;
      begin
         if builder <= num_builders then
            if not curses_support then
               TIO.Put_Line (CYC.elapsed_now & " => [" &
                               JT.zeropad (Integer (builder), 2) &
                               "] Builder launched");
            end if;
            loop
               exit when builder_states (builder) = shutdown;
               if builder_states (builder) = tasked then
                  builder_states (builder) := busy;

                  REP.launch_slave (id => builder);
                  build_result := CYC.build_package
                    (id => builder, sequence_id => instructions (builder));
                  REP.destroy_slave (id => builder);
                  if build_result then
                     builder_states (builder) := done_success;
                  else
                     builder_states (builder) := done_failure;
                  end if;
               else
                  --  idle or done-(failure|success), just wait a bit
                  delay 0.1;
               end if;
            end loop;
            if not curses_support then
               TIO.Put_Line (CYC.elapsed_now & " => [" &
                               JT.zeropad (Integer (builder), 2) &
                               "]          Shutting down");
            end if;
         end if;
      end build;

      builder_01 : build (builder => 1);
      builder_02 : build (builder => 2);
      builder_03 : build (builder => 3);
      builder_04 : build (builder => 4);
      builder_05 : build (builder => 5);
      builder_06 : build (builder => 6);
      builder_07 : build (builder => 7);
      builder_08 : build (builder => 8);
      builder_09 : build (builder => 9);
      builder_10 : build (builder => 10);
      builder_11 : build (builder => 11);
      builder_12 : build (builder => 12);
      builder_13 : build (builder => 13);
      builder_14 : build (builder => 14);
      builder_15 : build (builder => 15);
      builder_16 : build (builder => 16);
      builder_17 : build (builder => 17);
      builder_18 : build (builder => 18);
      builder_19 : build (builder => 19);
      builder_20 : build (builder => 20);
      builder_21 : build (builder => 21);
      builder_22 : build (builder => 22);
      builder_23 : build (builder => 23);
      builder_24 : build (builder => 24);
      builder_25 : build (builder => 25);
      builder_26 : build (builder => 26);
      builder_27 : build (builder => 27);
      builder_28 : build (builder => 28);
      builder_29 : build (builder => 29);
      builder_30 : build (builder => 30);
      builder_31 : build (builder => 31);
      builder_32 : build (builder => 32);
      builder_33 : build (builder => 33);
      builder_34 : build (builder => 34);
      builder_35 : build (builder => 35);
      builder_36 : build (builder => 36);
      builder_37 : build (builder => 37);
      builder_38 : build (builder => 38);
      builder_39 : build (builder => 39);
      builder_40 : build (builder => 40);
      builder_41 : build (builder => 41);
      builder_42 : build (builder => 42);
      builder_43 : build (builder => 43);
      builder_44 : build (builder => 44);
      builder_45 : build (builder => 45);
      builder_46 : build (builder => 46);
      builder_47 : build (builder => 47);
      builder_48 : build (builder => 48);
      builder_49 : build (builder => 49);
      builder_50 : build (builder => 50);
      builder_51 : build (builder => 51);
      builder_52 : build (builder => 52);
      builder_53 : build (builder => 53);
      builder_54 : build (builder => 54);
      builder_55 : build (builder => 55);
      builder_56 : build (builder => 56);
      builder_57 : build (builder => 57);
      builder_58 : build (builder => 58);
      builder_59 : build (builder => 59);
      builder_60 : build (builder => 60);
      builder_61 : build (builder => 61);
      builder_62 : build (builder => 62);
      builder_63 : build (builder => 63);
      builder_64 : build (builder => 64);

   begin
      loop
         all_idle := True;
         for slave in 1 .. num_builders loop
            case builder_states (slave) is
               when busy | tasked =>
                  all_idle := False;
               when shutdown =>
                  null;
               when idle =>
                  if run_complete then
                     builder_states (slave) := shutdown;
                  else
                     target := top_buildable_port;
                     if target = port_match_failed then
                        if nothing_left (num_builders) then
                           run_complete := True;
                           builder_states (slave) := shutdown;
                        else
                           if shutdown_recommended (available) then
                              builder_states (slave) := shutdown;
                              available := available - 1;
                           end if;
                        end if;
                     else
                        lock_package (target);
                        instructions (slave) := target;
                        builder_states (slave) := tasked;
                     end if;
                  end if;
               when done_success | done_failure =>
                  all_idle := False;
                  if builder_states (slave) = done_success then
                     if curses_support then
                        DPY.insert_history
                          (assemble_HR (slave, instructions (slave),
                           "success "));
                     else
                        TIO.Put_Line (CYC.elapsed_now & " => [" &
                                        JT.zeropad (Integer (slave), 2) &
                                        "] " & CYC.elapsed_build (slave) &
                                        " Success " &
                                        port_name (instructions (slave)));
                     end if;
                     run_hook (pkg_success, "RESULT=success ORIGIN=" &
                                 port_name (instructions (slave)) & " PKGNAME="
                               & package_name (instructions (slave)) & " ");
                     cascade_successful_build (instructions (slave));
                     bld_counter (success) := bld_counter (success) + 1;
                     TIO.Put_Line (logs (success), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)));
                     TIO.Put_Line (logs (total), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)) &
                                     " success");
                  else
                     TIO.Put_Line (logs (total), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)) &
                                     " FAILED!");
                     cascade_failed_build (instructions (slave), cntskip, logs);
                     bld_counter (skipped) := bld_counter (skipped) + cntskip;
                     bld_counter (failure) := bld_counter (failure) + 1;
                     TIO.Put_Line (logs (total), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)) &
                                     " failure skips:" & JT.int2str (cntskip));
                     TIO.Put_Line (logs (failure), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)) &
                                     " (skipped" & cntskip'Img & ")");
                     if curses_support then
                        DPY.insert_history
                          (assemble_HR (slave, instructions (slave),
                           "failure "));
                     else
                        TIO.Put_Line (CYC.elapsed_now & " => [" &
                                        JT.zeropad (Integer (slave), 2) &
                                        "] " & CYC.elapsed_build (slave) &
                                        " Failure " &
                                        port_name (instructions (slave)));
                     end if;
                     run_hook (pkg_failure, "RESULT=failure ORIGIN=" &
                                 port_name (instructions (slave)) & " PKGNAME="
                               & package_name (instructions (slave)) & " ");
                  end if;
                  instructions (slave) := port_match_failed;
                  if run_complete then
                     builder_states (slave) := shutdown;
                     DPY.insert_history (assemble_HR (slave, 0, "shutdown"));
                  else
                     builder_states (slave) := idle;
                  end if;
            end case;
         end loop;
         exit when run_complete and all_idle;
         if cntcycle = cycle_count'Last then
            cntcycle := cycle_count'First;
            TIO.Flush (logs (success));
            TIO.Flush (logs (failure));
            TIO.Flush (logs (skipped));
            TIO.Flush (logs (total));
            if curses_support then
               sumdata.Initially := bld_counter (total);
               sumdata.Built     := bld_counter (success);
               sumdata.Failed    := bld_counter (failure);
               sumdata.Ignored   := bld_counter (ignored);
               sumdata.Skipped   := bld_counter (skipped);
               sumdata.elapsed   := CYC.elapsed_now;
               sumdata.swap      := get_swap_status;
               sumdata.load      := get_instant_load;
               sumdata.pkg_hour  := hourly_build_rate;
               sumdata.impulse   := impulse_rate;
               DPY.summarize (sumdata);

               for b in builders'First .. num_builders loop
                  if builder_states (b) = shutdown then
                     DPY.update_builder (CYC.builder_status (b, True, False));
                  elsif builder_states (b) = idle then
                     DPY.update_builder (CYC.builder_status (b, False, True));
                  else
                     CYC.set_log_lines (b);
                     DPY.update_builder (CYC.builder_status (b));
                  end if;
               end loop;
               DPY.refresh_builder_window;
               DPY.refresh_history_window;
            end if;
         else
            cntcycle := cntcycle + 1;
         end if;
         delay 0.10;
      end loop;
      if PM.configuration.avec_ncurses and then curses_support
      then
         DPY.terminate_monitor;
      end if;
      run_hook (run_end, "PORTS_BUILT=" & JT.int2str (bld_counter (success)) &
                  " PORTS_FAILED=" & JT.int2str (bld_counter (failure)) &
                  " PORTS_IGNORED=" & JT.int2str (bld_counter (ignored)) &
                  " PORTS_SKIPPED=" & JT.int2str (bld_counter (skipped)));
   end parallel_bulk_run;


   --------------------
   --  lock_package  --
   --------------------
   procedure lock_package (id : port_id) is
   begin
      if id /= port_match_failed then
         all_ports (id).work_locked := True;
      end if;
   end lock_package;


   ----------------------------
   --  cascade_failed_build  --
   ----------------------------
   procedure cascade_failed_build (id : port_id; numskipped : out Natural;
                                   logs : dim_handlers)
   is
      purged  : PortScan.port_id;
      culprit : constant String := port_name (id);
   begin
      numskipped := 0;
      loop
         purged := skip_next_reverse_dependency (id);
         exit when purged = port_match_failed;
         if skip_verified (purged) then
            numskipped := numskipped + 1;
            TIO.Put_Line (logs (total), "           Skipped: " &
                            port_name (purged));
            TIO.Put_Line (logs (skipped), port_name (purged) &
                            " by " & culprit);
            DPY.insert_history (assemble_HR (1, purged, "skipped "));
            run_hook (pkg_skipped, "RESULT=skipped ORIGIN=" & port_name (purged)
                      & " PKGNAME=" & package_name (purged) & " ");
         end if;
      end loop;
      unlist_port (id);
   end cascade_failed_build;


   --------------------------------
   --  cascade_successful_build  --
   --------------------------------
   procedure cascade_successful_build (id : port_id)
   is
      procedure cycle (cursor : block_crate.Cursor);
      procedure cycle (cursor : block_crate.Cursor)
      is
         target : constant port_index := block_crate.Element (cursor);
      begin
         if all_ports (target).blocked_by.Contains (Key => id) then
            all_ports (target).blocked_by.Delete (Key => id);
         else
            raise seek_failure
              with port_name (target) & " was expected to be blocked by " &
              port_name (id);
         end if;
      end cycle;
   begin
      all_ports (id).blocks.Iterate (cycle'Access);
      delete_rank (id);
   end cascade_successful_build;


   --------------------------
   --  top_buildable_port  --
   --------------------------
   function top_buildable_port return port_id
   is
      list_len : constant Integer := Integer (rank_queue.Length);
      cursor   : ranking_crate.Cursor;
      QR       : queue_record;
      result   : port_id := port_match_failed;
   begin
      if list_len = 0 then
         return result;
      end if;
      cursor := rank_queue.First;
      for k in 1 .. list_len loop
         QR := ranking_crate.Element (Position => cursor);
         if not all_ports (QR.ap_index).work_locked and then
           all_ports (QR.ap_index).blocked_by.Is_Empty
         then
            result := QR.ap_index;
            exit;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      return result;
   end top_buildable_port;


   ----------------------------
   --  shutdown_recommended  --
   ----------------------------
   function shutdown_recommended (active_builders : Positive) return Boolean
   is
      list_len : constant Natural  := Integer (rank_queue.Length);
      list_max : constant Positive := 2 * active_builders;
      num_wait : Natural := 0;
      cursor   : ranking_crate.Cursor;
      QR       : queue_record;
   begin
      if list_len = 0 or else list_len >= list_max then
         return False;
      end if;
      cursor := rank_queue.First;
      for k in 1 .. list_len loop
         QR := ranking_crate.Element (Position => cursor);
         if not all_ports (QR.ap_index).work_locked then
            num_wait := num_wait + 1;
            if num_wait >= active_builders then
               return False;
            end if;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      return True;
   end shutdown_recommended;


   --------------------
   --  nothing_left  --
   --------------------
   function nothing_left (num_builders : builders) return Boolean
   is
      list_len : constant Integer := Integer (rank_queue.Length);
   begin
      return list_len = 0;
   end nothing_left;


   ------------------
   --  rank_arrow  --
   ------------------
   function rank_arrow (id : port_id) return ranking_crate.Cursor
   is
      rscore : constant port_index := all_ports (id).reverse_score;
      seek_target : constant queue_record := (ap_index      => id,
                                              reverse_score => rscore);
   begin
      return rank_queue.Find (seek_target);
   end rank_arrow;


   -------------------
   --  delete_rank  --
   -------------------
   procedure delete_rank (id : port_id)
   is
      rank_cursor : ranking_crate.Cursor := rank_arrow (id);
      use type ranking_crate.Cursor;
   begin
      if rank_cursor /= ranking_crate.No_Element then
         rank_queue.Delete (Position => rank_cursor);
      end if;
   end delete_rank;


   --------------------
   --  still_ranked  --
   --------------------
   function still_ranked (id : port_id) return Boolean
   is
      rank_cursor : ranking_crate.Cursor := rank_arrow (id);
      use type ranking_crate.Cursor;
   begin
      return rank_cursor /= ranking_crate.No_Element;
   end still_ranked;


   ------------------------
   --  integrity_intact  --
   ------------------------
   function integrity_intact return Boolean
   is
      procedure check_dep (cursor : block_crate.Cursor);
      procedure check_rank (cursor : ranking_crate.Cursor);

      intact : Boolean := True;
      procedure check_dep (cursor : block_crate.Cursor)
      is
         did : constant port_index := block_crate.Element (cursor);
      begin
         if not still_ranked (did) then
            intact := False;
         end if;
      end check_dep;

      procedure check_rank (cursor : ranking_crate.Cursor)
      is
         QR : constant queue_record := ranking_crate.Element (cursor);
      begin
         if intact then
            all_ports (QR.ap_index).blocked_by.Iterate (check_dep'Access);
         end if;
      end check_rank;
   begin
      rank_queue.Iterate (check_rank'Access);
      return intact;
   end integrity_intact;


   ---------------------
   --  skip_verified  --
   ---------------------
   function skip_verified (id : port_id) return Boolean is
   begin
      if id = port_match_failed then
         return False;
      end if;
      return not all_ports (id).unlist_failed;
   end skip_verified;


   --------------------
   --  queue_length  --
   --------------------
   function queue_length return Integer is
   begin
      return Integer (rank_queue.Length);
   end queue_length;


   -------------------
   --  unlist_port  --
   -------------------
   procedure unlist_port (id : port_id) is
   begin
      if id = port_match_failed then
         return;
      end if;
      if still_ranked (id) then
         delete_rank (id);
      else
         --  don't raise exception.  Since we don't prune all_reverse as
         --  we go, there's no guarantee the reverse dependency hasn't already
         --  been removed (e.g. when it is a common reverse dep)
         all_ports (id).unlist_failed := True;
      end if;
   end unlist_port;


   ------------------------------------
   --  skip_next_reverse_dependency  --
   ------------------------------------
   function skip_next_reverse_dependency (pinnacle : port_id) return port_id
   is
      rev_cursor : block_crate.Cursor;
      next_dep : port_index;
   begin
      if all_ports (pinnacle).all_reverse.Is_Empty then
         return port_match_failed;
      end if;
      rev_cursor := all_ports (pinnacle).all_reverse.First;
      next_dep := block_crate.Element (rev_cursor);
      unlist_port (id => next_dep);
      all_ports (pinnacle).all_reverse.Delete (rev_cursor);

      return next_dep;
   end skip_next_reverse_dependency;


   ---------------------
   --  ignore_reason  --
   ---------------------
   function ignore_reason (id : port_id) return String is
   begin
      if id = port_match_failed or else
        id > last_port
      then
         return "Invalid port ID";
      end if;
      return JT.USS (all_ports (id).ignore_reason);
   end ignore_reason;


   -------------------------
   --  next_ignored_port  --
   -------------------------
   function next_ignored_port return port_id
   is
      list_len : constant Integer := Integer (rank_queue.Length);
      cursor   : ranking_crate.Cursor;
      QR       : queue_record;
      result   : port_id := port_match_failed;
   begin
      if list_len = 0 then
         return result;
      end if;
      cursor := rank_queue.First;
      for k in 1 .. list_len loop
         QR := ranking_crate.Element (Position => cursor);
         if all_ports (QR.ap_index).ignored then
            result := QR.ap_index;
            DPY.insert_history (assemble_HR (1, QR.ap_index, "ignored "));
            run_hook (pkg_ignored, "RESULT=ignored ORIGIN=" &
                        port_name (QR.ap_index) & " PKGNAME="
                      & package_name (QR.ap_index) & " ");
            exit;
         end if;
         cursor := ranking_crate.Next (Position => cursor);
      end loop;
      return result;
   end next_ignored_port;


   -----------------
   --  port_name  --
   -----------------
   function port_name (id : port_id) return String is
   begin
      if id = port_match_failed or else
        id > last_port
      then
         return "Invalid port ID";
      end if;
      return get_catport (all_ports (id));
   end port_name;


   -----------------------
   --  get_swap_status  --
   -----------------------
   function get_swap_status return Float
   is
      command : String := "/usr/sbin/swapinfo -k";
      comres  : JT.Text;
      topline : JT.Text;
      crlen1  : Natural;
      crlen2  : Natural;

      blocks_total : Natural := 0;
      blocks_used  : Natural := 0;
   begin
      comres := CYC.generic_system_command (command);
      --  Throw first line away, e.g "Device 1K-blocks Used  Avail ..."
      JT.nextline (lineblock => comres, firstline => topline);
      crlen1 := JT.SU.Length (comres);
      loop
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         declare
            subs         : GSS.Slice_Set;
            opts_found   : GSS.Slice_Number;
            use type GSS.Slice_Number;
         begin
            GSS.Create (S          => subs,
                        From       => JT.USS (topline),
                        Separators => " ",
                        Mode       => GSS.Multiple);
            opts_found := GSS.Slice_Count (S => subs);
            if opts_found >= 3 then
               blocks_total := blocks_total +
                               Natural'Value (GSS.Slice (subs, 2));
               blocks_used  := blocks_used +
                               Natural'Value (GSS.Slice (subs, 3));
            end if;
         end;
      end loop;
      if blocks_total = 0 then
         return 100.0;
      else
         return 100.0 * Float (blocks_used) / Float (blocks_total);
      end if;
   exception
      when others => return 0.0;
   end get_swap_status;


   ------------------------
   --  get_instant_load  --
   ------------------------
   function get_instant_load return Float
   is
      command : String := "/sbin/sysctl vm.loadavg";
      comres  : JT.Text;
   begin
      comres := CYC.generic_system_command (command);
      declare
         stripped : constant String := JT.SU.Slice
           (Source => comres, Low => 15, High => JT.SU.Length (comres));
         instant  : constant String := JT.part_1 (stripped, " ");
      begin
         return Float'Value (instant);
      end;
   exception
      when others => return 0.0;
   end get_instant_load;


   -------------------------
   --  hourly_build_rate  --
   -------------------------
   function hourly_build_rate return Natural
   is
      pkg_that_count : constant Natural := bld_counter (success) +
                                           bld_counter (failure);
   begin
      return CYC.get_packages_per_hour (pkg_that_count, start_time);
   end hourly_build_rate;


   --------------------
   --  impulse_rate  --
   --------------------
   function impulse_rate return Natural
   is
      pkg_that_count : constant Natural := bld_counter (success) +
                                           bld_counter (failure);
      pkg_diff : Natural;
      result   : Natural;
   begin
      if impulse_counter = impulse_range'Last then
         impulse_counter := impulse_range'First;
      else
         impulse_counter := impulse_counter + 1;
      end if;
      if impulse_data (impulse_counter).virgin then
         impulse_data (impulse_counter).hack     := CAL.Clock;
         impulse_data (impulse_counter).packages := pkg_that_count;
         impulse_data (impulse_counter).virgin   := False;

         return CYC.get_packages_per_hour (pkg_that_count, start_time);
      end if;
      pkg_diff := pkg_that_count - impulse_data (impulse_counter).packages;
      result   := CYC.get_packages_per_hour
                   (packages_done => pkg_diff,
                    from_when     => impulse_data (impulse_counter).hack);
      impulse_data (impulse_counter).hack     := CAL.Clock;
      impulse_data (impulse_counter).packages := pkg_that_count;

      return result;
   end impulse_rate;


   -------------------
   --  assemble_HR  --
   -------------------
   function assemble_HR (slave : builders; pid : port_id; action : String)
                         return DPY.history_rec
   is
      HR : DPY.history_rec;
      catport : String := port_name (pid);
   begin
      HR.id := slave;
      HR.slavid      := JT.zeropad (Integer (slave), 2);
      HR.established := True;
      HR.action      := action;
      HR.origin      := (others => ' ');
      HR.run_elapsed := CYC.elapsed_now;
      if action = "shutdown " then
         HR.pkg_elapsed := "00:00:00";
      else
         HR.pkg_elapsed := CYC.elapsed_build (slave);
         if catport'Length > 43 then
            HR.origin (1 .. 42) := catport (1 .. 42);
            HR.origin (43) := LAT.Asterisk;
         else
            HR.origin (1 .. catport'Length) := catport;
         end if;
      end if;
      return HR;
   end assemble_HR;


   --------------------------
   --  file_is_executable  --
   --------------------------
   function file_is_executable (filename : String) return Boolean
   is
      command : constant String := "/usr/bin/file -b " &
        "-e ascii -e encoding -e tar -e compress " & filename;
      comres  : JT.Text;
      crlast  : Natural;
   begin
      comres := CYC.generic_system_command (command);
      crlast := JT.SU.Length (comres) - 1;
      if crlast > 18 and then JT.SU.Slice (comres, 1, 16) = "symbolic link to"
      then
         declare
            target   : constant String := JT.SU.Slice (comres, 18, crlast);
            fixedtgt : constant String (1 .. target'Length) := target;
         begin
            if fixedtgt (1) = '/' then
               return file_is_executable (fixedtgt);
            else
               return file_is_executable
                 (AD.Containing_Directory (filename) & "/" & fixedtgt);
            end if;
         end;
      else
         return JT.contains (comres, "executable");
      end if;
   end file_is_executable;


   ------------------------
   --  initialize_hooks  --
   ------------------------
   procedure initialize_hooks is
   begin
      for hook in hook_type'Range loop
         declare
            script : constant String := JT.USS (hook_location (hook));
         begin
            active_hook (hook) := AD.Exists (script) and then
              file_is_executable (script);
         end;
      end loop;
      run_hook (run_start, "PORTS_QUEUED=" & JT.int2str (queue_length) & " ");
   end initialize_hooks;


   ----------------
   --  run_hook  --
   ----------------
   procedure run_hook (hook : hook_type; envvar_list : String)
   is
      function nvpair (name : String; value : JT.Text) return String;
      function nvpair (name : String; value : JT.Text) return String is
      begin
         return name & LAT.Equals_Sign & LAT.Quotation &
           JT.USS (value) & LAT.Quotation & LAT.Space;
      end nvpair;
      common_env : constant String :=
        nvpair ("PROFILE", PM.configuration.profile) &
        nvpair ("DIR_PACKAGES", PM.configuration.dir_packages) &
        nvpair ("DIR_REPOSITORY", PM.configuration.dir_repository) &
        nvpair ("DIR_PORTS", PM.configuration.dir_portsdir) &
        nvpair ("DIR_OPTIONS", PM.configuration.dir_options) &
        nvpair ("DIR_DISTFILES", PM.configuration.dir_distfiles) &
        nvpair ("DIR_LOGS", PM.configuration.dir_logs) &
        nvpair ("DIR_BUILDBASE", PM.configuration.dir_buildbase);
      command : constant String := "/usr/bin/env " & common_env &
        envvar_list & " " & JT.USS (hook_location (hook));
   begin
      if not active_hook (hook) then
         return;
      end if;
      if CYC.external_command (command) then
         null;
      end if;
   end run_hook;


   --------------------
   --  package_name  --
   --------------------
   function package_name (id : port_id) return String is
   begin
      if id = port_match_failed or else
        id > last_port
      then
         return "Invalid port ID";
      end if;
      declare
         fullname : constant String := JT.USS (all_ports (id).package_name);
      begin
         return fullname (1 .. fullname'Length - 4);
      end;
   end package_name;


end PortScan.Ops;
