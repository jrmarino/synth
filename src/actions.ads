--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;   use Definitions;

package Actions is

   menu_error : exception;

   --  output of "synth version"
   procedure print_version;

   --  output of "synth help"
   procedure print_help;

   --  Interactive configuration menu
   procedure launch_configure_menu (num_cores : cpu_range);

private

   function generic_execute (command : String) return Boolean;
   procedure clear_screen;

end Actions;
