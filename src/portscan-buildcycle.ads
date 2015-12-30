--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with JohnnyText;

package PortScan.Buildcycle is

   cycle_log_error : exception;
   cycle_cmd_error : exception;

   procedure initialize (test_mode : Boolean);
   function build_package (id : builders; sequence_id : port_id) return Boolean;

   --  Expose for overall build log
   function timestamp (hack : CAL.Time) return String;
   function log_duration (start, stop : CAL.Time) return String;
   function elapsed_now return String;
   function elapsed_build (id : builders) return String;

   --  The actual command to build a local repository (Returns True on success)
   function build_repository (id : builders) return Boolean;

   --  Allows other packages to call external commands (e.g. Pilot)
   --  Returns "True" on success
   function external_command (command : String) return Boolean;

   --  Was private, but expose so Pilot can use it.
   function generic_system_command (command : String) return JT.Text;

   --  temporary
   function tempstatus (id : builders) return String;

   --  records the current length of the build log.
   procedure set_log_lines (id : builders);

private

   type phases is (check_sanity, pkg_depends, fetch_depends, fetch, checksum,
                   extract_depends, extract, patch_depends, patch,
                   build_depends, lib_depends, configure, build, run_depends,
                   stage, check_plist, pkg_package, install_mtree, install,
                   deinstall);

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : CAL.Time;
         tail_time  : CAL.Time;
         log_handle : aliased TIO.File_Type;
         dynlink    : string_crate.Vector;
         phase      : phases;
         loglines   : Natural;
      end record;

   type dim_trackers is array (builders) of trackrec;

   trackers  : dim_trackers;
   uname_mrv : JT.Text;
   testing   : Boolean;

   chroot    : constant String := "/usr/sbin/chroot ";
   uselog    : constant Boolean := True;

   procedure initialize_log (id : builders);
   procedure finalize_log   (id : builders);

   function  exec_phase_generic (id : builders; phase : String) return Boolean;
   function  exec_phase_depends (id : builders; phase : String) return Boolean;
   function  exec_phase_deinstall (id : builders) return Boolean;

   function  get_environment (id : builders) return String;
   function  get_root (id : builders) return String;
   function  get_options_configuration (id : builders) return String;
   procedure set_uname_mrv;
   function  split_collection (line : JT.Text; title : String) return String;
   function  get_port_variables (id : builders) return JT.Text;
   procedure dump_port_variables (id : builders; content : JT.Text);
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
   function  elapsed_HH_MM_SS (start, stop : CAL.Time) return String;
   function  environment_override return String;
   function  phase2str (phase : phases) return String;
   function  format_loglines (numlines : Natural) return String;

   --  Install pkg-static in specific builder (Returns True on success)
   function install_pkg8 (id : builders) return Boolean;




end PortScan.Buildcycle;
