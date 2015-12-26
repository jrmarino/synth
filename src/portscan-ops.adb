--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Numerics.Discrete_Random;
with PortScan.Buildcycle;
with Replicant;

package body PortScan.Ops is

   package REP renames Replicant;
   package CYC renames PortScan.Buildcycle;


   -------------------------
   --  parallel_bulk_run  --
   -------------------------
   procedure parallel_bulk_run (num_builders : builders; logs : dim_handlers)
   is
      instructions   : dim_instruction   := (others => port_match_failed);
      builder_states : dim_builder_state := (others => idle);
      run_complete   : Boolean           := False;
      available      : Positive          := Integer (num_builders);
      target         : port_id;
      all_idle       : Boolean;
      cntskip        : Natural;

      task type build (builder : builders);
      task body build
      is
         type Rand_Draw is range 1 .. 20;
         package Rand20 is new Ada.Numerics.Discrete_Random (Rand_Draw);
         seed : Rand20.Generator;
         build_result : Boolean;
      begin
         if builder <= num_builders then
            TIO.Put_Line ("Starting Builder" & builder'Img);
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
            TIO.Put_Line ("Builder" & builder'Img & " shutting down");
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
                     TIO.Put_Line ("Built [" & slave'Img & "] " &
                                     port_name (instructions (slave)));
                     cascade_successful_build (instructions (slave));
                     bld_counter (success) := bld_counter (success) + 1;
                     TIO.Put_Line (logs (success), CYC.elapsed_now & " " &
                                      port_name (instructions (slave)));
                     TIO.Put_Line (logs (total), CYC.elapsed_now & " " &
                                     port_name (instructions (slave)) &
                                     " success");
                  else
                     cascade_failed_build (instructions (slave), cntskip, logs);
                     bld_counter (skipped) := bld_counter (skipped) + cntskip;
                     bld_counter (failure) := bld_counter (failure) + 1;
                     TIO.Put_Line (logs (total), CYC.elapsed_now &
                                     port_name (instructions (slave)) &
                                     " FAILED! skipped:" &
                                     JT.int2str (cntskip));
                     TIO.Put_Line (logs (failure), CYC.elapsed_now & " " &
                                   port_name (instructions (slave)) &
                                     " (skipped" & cntskip'Img & ")");
                  end if;
                  instructions (slave) := port_match_failed;
                  if run_complete then
                     builder_states (slave) := shutdown;
                  else
                     builder_states (slave) := idle;
                  end if;
            end case;
         end loop;
         exit when run_complete and all_idle;
         delay 0.15;
      end loop;
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

end PortScan.Ops;
