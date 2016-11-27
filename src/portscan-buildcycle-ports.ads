--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Buildcycle.Ports is

   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : String  := "") return Boolean;

   --  Compile status of builder for the curses display
   function builder_status (id : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec;

   --  Expose for build log
   function last_build_phase (id : builders) return String;

private

   type phases is (check_sanity, pkg_depends, fetch_depends, fetch, checksum,
                   extract_depends, extract, patch_depends, patch,
                   build_depends, lib_depends, configure, build, run_depends,
                   stage, check_plist, pkg_package, install_mtree, install,
                   deinstall);

   type dim_phase_trackers is array (builders) of phases;

   phase_trackers : dim_phase_trackers;

   --  If the afterphase string matches a legal phase name then that phase
   --  is returned, otherwise the value of check-sanity is returned.  Allowed
   --  phases are: extract/patch/configure/build/stage/install/deinstall.
   --  check-sanity is considered a negative response
   --  stage includes check-plist
   function valid_test_phase (afterphase : String) return phases;

   function  exec_phase (id : builders; phase : phases;
                         time_limit    : execution_limit;
                         phaseenv      : String := "";
                         depends_phase : Boolean := False;
                         skip_header   : Boolean := False;
                         skip_footer   : Boolean := False)
                         return Boolean;

   function  exec_phase_generic (id : builders; phase : phases) return Boolean;
   function  exec_phase_depends (id : builders; phase : phases) return Boolean;
   function  exec_phase_deinstall (id : builders) return Boolean;
   function  exec_phase_build (id : builders) return Boolean;
   function  phase2str (phase : phases) return String;
   function  max_time_without_output (phase : phases) return execution_limit;

end PortScan.Buildcycle.Ports;
