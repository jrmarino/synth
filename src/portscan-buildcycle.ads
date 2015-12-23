--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar;
with JohnnyText;

package PortScan.Buildcycle is

   package AC renames Ada.Calendar;

   cycle_log_error : exception;
   cycle_cmd_error : exception;

   procedure initialize (test_mode : Boolean);

--     procedure exec_phase_run_depends     (id : builders);
--     procedure exec_phase_stage           (id : builders);
--     procedure exec_phase_package         (id : builders);


   function build_package (id : builders; sequence_id : port_id) return Boolean;

private

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : AC.Time;
         tail_time  : AC.Time;
         log_handle : aliased TIO.File_Type;
      end record;

   type phases is (check_sanity, pkg_depends, fetch_depends, fetch, checksum,
                   extract_depends, extract, patch_depends, patch,
                   build_depends, lib_depends, configure, build);
   type dim_trackers is array (builders) of trackrec;

   trackers  : dim_trackers;
   uname_mrv : JT.Text;
   testing   : Boolean;

   procedure initialize_log (id : builders; sequence_id : port_id);
   procedure finalize_log   (id : builders);

   function  exec_phase_check_sanity    (id : builders) return Boolean;
   function  exec_phase_generic (id : builders; phase : String) return Boolean;
   function  exec_phase_depends (id : builders; phase : String) return Boolean;

   function  timestamp      (hack : AC.Time) return String;
   function  generic_system_command (command : String) return JT.Text;

   function  get_environment (id : builders) return String;
   function  get_root (id : builders) return String;
   function  get_options_configuration (id : builders) return String;
   procedure set_uname_mrv;
   function  split_collection (line : JT.Text; title : String) return String;
   procedure dump_port_variables (id : builders);
   procedure nextline (lineblock, firstline : out JT.Text);
   function  log_name (sid : port_id) return String;
   function  dump_file (filename : String) return String;
   function  dump_make_conf (id : builders) return String;
   function  log_section (title : String; header : Boolean) return String;
   procedure log_phase_end (id : builders);
   procedure log_phase_begin (phase : String; id : builders);
   function  generic_execute (id : builders; command : String) return Boolean;
   function  exec_phase (id : builders; phase : String; phaseenv : String := "";
                         depends_phase : Boolean := False)
                         return Boolean;

end PortScan.Buildcycle;
