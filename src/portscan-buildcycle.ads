--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with JohnnyText;
with Display;

package PortScan.Buildcycle is

   cycle_log_error : exception;
   cycle_cmd_error : exception;

   type phases is (check_sanity, pkg_depends, fetch_depends, fetch, checksum,
                   extract_depends, extract, patch_depends, patch,
                   build_depends, lib_depends, configure, build, run_depends,
                   stage, check_plist, pkg_package, install_mtree, install,
                   deinstall);

   procedure initialize (test_mode : Boolean; jail_env : JT.Text);
   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : phases  := fetch) return Boolean;

   --  Expose for overall build log
   function timestamp (hack : CAL.Time) return String;
   function log_duration (start, stop : CAL.Time) return String;
   function elapsed_now return String;
   function elapsed_build (id : builders) return String;

   --  Was private, but expose so Pilot can use it.
   function generic_system_command (command : String) return JT.Text;

   --  Simple time calculation
   function get_packages_per_hour (packages_done : Natural;
                                   from_when : CAL.Time)
                                   return Natural;

   --  Compile status of builder for the curses display
   function builder_status (id : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec;

   --  records the current length of the build log.
   procedure set_log_lines (id : builders);

   --  If the afterphase string matches a legal phase name then that phase
   --  is returned, otherwise the value of check-sanity is returned.  Allowed
   --  phases are: extract/patch/configure/build/stage/install/deinstall.
   --  check-sanity is considered a negative response
   --  stage includes check-plist
   function valid_test_phase (afterphase : String) return phases;

private

   type execution_limit is range 1 .. 720;

   type trackrec is
      record
         seq_id     : port_id;
         head_time  : CAL.Time;
         tail_time  : CAL.Time;
         log_handle : aliased TIO.File_Type;
         dynlink    : string_crate.Vector;
         phase      : phases;
         loglines   : Natural := 0;
      end record;

   type dim_trackers is array (builders) of trackrec;

   trackers  : dim_trackers;
   uname_mrv : JT.Text;
   customenv : JT.Text;
   slave_env : JT.Text;
   testing   : Boolean;
   lock_localbase : Boolean;

   chroot    : constant String := "/usr/sbin/chroot ";
   uselog    : constant Boolean := True;

   discerr   : constant String := "Discovery error";

   function  initialize_log (id : builders) return Boolean;
   procedure finalize_log   (id : builders);

   function  exec_phase_generic (id : builders; phase : phases) return Boolean;
   function  exec_phase_depends (id : builders; phase : phases) return Boolean;
   function  exec_phase_deinstall (id : builders) return Boolean;
   function  exec_phase_build (id : builders) return Boolean;

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
   function  generic_execute (id : builders; command : String;
                              dogbite : out Boolean;
                              time_limit : execution_limit) return Boolean;
   function  exec_phase (id : builders; phase : phases;
                         time_limit    : execution_limit;
                         phaseenv      : String := "";
                         depends_phase : Boolean := False;
                         skip_header   : Boolean := False;
                         skip_footer   : Boolean := False)
                         return Boolean;
   procedure stack_linked_libraries (id : builders; base, filename : String);
   procedure log_linked_libraries (id : builders);
   procedure mark_file_system (id : builders; action : String);
   procedure interact_with_builder (id : builders);
   procedure obtain_custom_environment;
   function  dynamically_linked (base, filename : String) return Boolean;
   function  elapsed_HH_MM_SS (start, stop : CAL.Time) return String;
   function  environment_override (enable_tty : Boolean := False) return String;
   function  phase2str (phase : phases) return String;
   function  format_loglines (numlines : Natural) return String;
   function  max_time_without_output (phase : phases) return execution_limit;
   function  timeout_multiplier_x10 return Positive;
   function  detect_leftovers_and_MIA (id : builders; action : String;
                                       description : String) return Boolean;

   --  This is designed to be used twice; once with lock=True and again
   --  with lock=False.  It's a diagnostic mechanism and effectively sets
   --  /usr/local inside a slave as read-only
   procedure set_localbase_protection (id : builders; lock : Boolean);

end PortScan.Buildcycle;
