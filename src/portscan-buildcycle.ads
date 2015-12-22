--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar;
with JohnnyText;

package PortScan.Buildcycle is

   package AC renames Ada.Calendar;

   cycle_log_error : exception;
   cycle_cmd_error : exception;

   procedure initialize;

   --  procedure builder_execute (id : builders; command : String);
--     function  builder_execute (id : builders; command : String) return String;
--
--     procedure exec_phase_check_sanity    (id : builders);
--     procedure exec_phase_pkg_depends     (id : builders);
--     procedure exec_phase_fetch_depends   (id : builders);
--     procedure exec_phase_fetch           (id : builders);
--     procedure exec_phase_checksum        (id : builders);
--     procedure exec_phase_extract_depends (id : builders);
--     procedure exec_phase_extract         (id : builders);
--     procedure exec_phase_patch_depends   (id : builders);
--     procedure exec_phase_patch           (id : builders);
--     procedure exec_phase_build_depends   (id : builders);
--     procedure exec_phase_lib_depends     (id : builders);
--     procedure exec_phase_configure       (id : builders);
--     procedure exec_phase_build           (id : builders);
--     procedure exec_phase_run_depends     (id : builders);
--     procedure exec_phase_stage           (id : builders);
--     procedure exec_phase_package         (id : builders);

   procedure initialize_log (id : builders; sequence_id : port_id);
   procedure finalize_log   (id : builders);

private

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : AC.Time;
         tail_time  : AC.Time;
         log_handle : aliased TIO.File_Type;
      end record;

   type dim_trackers is array (builders) of trackrec;

   trackers  : dim_trackers;
   uname_mrv : JT.Text;


   function  timestamp      (hack : AC.Time) return String;
   function  generic_system_command (command : String) return JT.Text;

   function  get_environment (id : builders) return String;
   function  get_root (id : builders) return String;
   function  get_options_configuration (id : builders) return String;
   procedure set_uname_mrv;
   function  split_collection (line : JT.Text; title : String) return String;
   procedure dump_port_variables (id : builders);
   procedure nextline (lineblock, firstline : out JT.Text);
   function  dump_file (filename : String) return String;
   function  dump_make_conf (id : builders) return String;
   function  log_section (title : String; header : Boolean) return String;

end PortScan.Buildcycle;
