--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Calendar;
with JohnnyText;

package PortScan.Buildcycle is

   package AC renames Ada.Calendar;

   cycle_log_error : exception;
   cycle_cmd_error : exception;

   procedure initialize (test_mode : Boolean);
   function build_package (id : builders; sequence_id : port_id) return Boolean;

private

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : AC.Time;
         tail_time  : AC.Time;
         log_handle : aliased TIO.File_Type;
         dynlink    : string_crate.Vector;
      end record;

   type phases is (check_sanity, pkg_depends, fetch_depends, fetch, checksum,
                   extract_depends, extract, patch_depends, patch,
                   build_depends, lib_depends, configure, build, run_depends,
                   stage, check_plist, pkg_package, install_mtree, install,
                   deinstall);
   type dim_trackers is array (builders) of trackrec;

   trackers  : dim_trackers;
   uname_mrv : JT.Text;
   testing   : Boolean;

   chroot    : constant String := "/usr/sbin/chroot ";

   procedure initialize_log (id : builders; sequence_id : port_id);
   procedure finalize_log   (id : builders);

   function  exec_phase_generic (id : builders; phase : String) return Boolean;
   function  exec_phase_depends (id : builders; phase : String) return Boolean;
   function  exec_phase_deinstall (id : builders) return Boolean;

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
                         depends_phase : Boolean := False;
                         skip_header   : Boolean := False)
                         return Boolean;
   function  dynamically_linked (base, filename : String) return Boolean;
   procedure stack_linked_libraries (id : builders; base, filename : String);
   procedure log_linked_libraries (id : builders);

end PortScan.Buildcycle;
