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

   --  Returns "True" when afterphase string matches a legal phase name.
   --  Allowed phases: extract/patch/configure/build/stage/install/deinstall
   function valid_test_phase (afterphase : String) return Boolean;

   --  Expose for build log
   function last_build_phase (id : builders) return String;

private

end PortScan.Buildcycle.Pkgsrc;
