--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Display;

package PortScan.Ops is

   package DPY renames Display;

   function port_name (id : port_id) return String;
   function next_ignored_port return port_id;
   function ignore_reason (id : port_id) return String;
   function queue_length return Integer;
   function skip_verified (id : port_id) return Boolean;

   --  Returns true if every port in the queue has all of ports listed in the
   --  blocks and blocked_by containers are all also present in the queue
   function integrity_intact return Boolean;

   --  This removes the first reverse dependency port from all_ports that is
   --  found the complete reverse deps list and return the port_id of the
   --  deleted port.  If the list is empty, return port_match_failed instead.
   function skip_next_reverse_dependency (pinnacle : port_id) return port_id;

   --  removes processed port from the ranking queue.
   procedure unlist_port (id : port_id);

   --  Returns the highly priority buildable port
   function top_buildable_port return port_id;

   --  The port build succeeded, so remove the "blocked_by" designation
   --  for all the immediate reverse dependencies.
   --  Remove the port from the queue when this is done.
   procedure cascade_successful_build (id : port_id);

   --  The port build failed, so set all reverse dependences as skipped
   --  Remove the port from the queue when this is done.
   procedure cascade_failed_build (id : port_id; numskipped : out Natural;
                                   logs : dim_handlers);

   --  Kick off bulk run using the given number of builders
   --  The rank_queue and all_ports must be already set up (it's recommended
   --  To eliminate the ignored ports and subsequent skips first.
   procedure parallel_bulk_run (num_builders : builders; logs : dim_handlers);

   --  Before starting to build a port, lock it.  This is required for
   --  parallel building.
   procedure lock_package (id : port_id);

   --  Kicks off curses or sets color support off.  Do it before
   --  calling parallel_bulk_run.
   procedure initialize_display (num_builders : builders);

   --  Unconditionally copies web assets to <log directory/report directory
   --  It also provides an initial summary.json data file just the report has something to load
   procedure initialize_web_report (num_builders : builders);

   --  Removes ??_history.json files from previous runs
   procedure delete_existing_web_history_files;

   --  Call before executing sanity check.  It checks the present of build
   --  hooks at the synth_conf location and caches the results.
   --  It also fires off the first hook (run_start)
   procedure initialize_hooks;

   --  Exposed for pilot which eliminated ignored ports during the sanity check
   procedure record_history_ignored
     (elapsed   : String;
      origin    : String;
      reason    : String;
      skips     : Natural);

private

   --  History log entries average less than 200 bytes.  Allot more than twice this amount.
   kfile_unit_maxsize : constant Positive := 512;

    --  Each history segment is limited to this many log lines
   kfile_units_limit : constant Positive := 500;

   subtype impulse_range is Integer range 1 .. 500;
   subtype kfile_content  is String (1 .. kfile_unit_maxsize * kfile_units_limit);

   type progress_history is
      record
         segment       : Natural := 0;
         segment_count : Natural := 0;
         log_entry     : Natural := 0;
         last_index    : Natural := 0;
         last_written  : Natural := 0;
         content       : kfile_content;
      end record;

   type impulse_rec is
      record
         hack     : CAL.Time;
         packages : Natural := 0;
         virgin   : Boolean := True;
      end record;
   type hook_type         is (run_start, run_end, pkg_success, pkg_failure,
                             pkg_skipped, pkg_ignored);
   type machine_state     is (idle, tasked, busy, done_failure, done_success,
                              shutdown);
   type dim_instruction   is array (builders) of port_id;
   type dim_builder_state is array (builders) of machine_state;
   type dim_impulse       is array (impulse_range) of impulse_rec;
   type dim_hooks         is array (hook_type) of Boolean;
   type dim_hooksloc      is array (hook_type) of JT.Text;

   history         : progress_history;
   impulse_counter : impulse_range := impulse_range'Last;
   impulse_data    : dim_impulse;
   curses_support  : Boolean;
   active_hook     : dim_hooks := (False, False, False, False, False, False);
   hook_location   : constant dim_hooksloc :=
                     (JT.SUS (PM.synth_confdir & "/hook_run_start"),
                      JT.SUS (PM.synth_confdir & "/hook_run_end"),
                      JT.SUS (PM.synth_confdir & "/hook_pkg_success"),
                      JT.SUS (PM.synth_confdir & "/hook_pkg_failure"),
                      JT.SUS (PM.synth_confdir & "/hook_pkg_skipped"),
                      JT.SUS (PM.synth_confdir & "/hook_pkg_ignored"));

   function  nothing_left (num_builders : builders) return Boolean;
   function  shutdown_recommended (active_builders : Positive) return Boolean;
   function  still_ranked (id : port_id) return Boolean;
   function  rank_arrow (id : port_id) return ranking_crate.Cursor;
   function  package_name (id : port_id) return String;
   function  get_swap_status return Float;
   function  get_instant_load return Float;
   function  hourly_build_rate return Natural;
   function  impulse_rate return Natural;
   function  assemble_HR (slave : builders; pid : port_id;
                          action : DPY.history_action)
                          return DPY.history_rec;
   function  file_is_executable (filename : String) return Boolean;
   function  nv (name, value : String) return String;
   function  nv (name : String; value : Integer) return String;

   procedure delete_rank (id : port_id);
   procedure run_hook (hook : hook_type; envvar_list : String);
   procedure check_history_segment_capacity;
   procedure handle_first_history_entry;

   procedure write_summary_json
     (active            : Boolean;
      states            : dim_builder_state;
      num_builders      : builders;
      num_history_files : Natural);

   procedure write_history_json;

   procedure assimulate_substring
     (history   : in out progress_history;
      substring : String);

   procedure record_history_built
     (elapsed   : String;
      slave_id  : builders;
      origin    : String;
      duration  : String);

   procedure record_history_failed
     (elapsed   : String;
      slave_id  : builders;
      origin    : String;
      duration  : String;
      die_phase : String;
      skips     : Natural);

   procedure record_history_skipped
     (elapsed   : String;
      origin    : String;
      reason    : String);

end PortScan.Ops;
