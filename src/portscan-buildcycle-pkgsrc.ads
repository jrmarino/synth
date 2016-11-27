--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package PortScan.Buildcycle.Pkgsrc is

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

   type phases is (bootstrap_depends, fetch, checksum, depends, tools,
                   extract, patch, wrapper, configure, build, test,
                   stage_install, create_package, package_install,
                   deinstall);

   type dim_phase_trackers is array (builders) of phases;

   phase_trackers : dim_phase_trackers;

   function exec_phase (id : builders; phase : phases;
                        time_limit    : execution_limit;
                        phaseenv      : String := "";
                        skip_header   : Boolean := False;
                        skip_footer   : Boolean := False)
                        return Boolean;

   function exec_phase_generic (id : builders; phase : phases) return Boolean;
   function exec_phase_deinstall (id : builders) return Boolean;
   function exec_phase_build (id : builders) return Boolean;
   function phase2str (phase : phases) return String;
   function valid_test_phase (afterphase : String) return phases;
   function max_time_without_output (phase : phases) return execution_limit;

end PortScan.Buildcycle.Pkgsrc;
