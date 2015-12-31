--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Terminal_Interface.Curses;

with Definitions;  use Definitions;

package Display is

   package TIC renames Terminal_Interface.Curses;

   type summary_rec is
      record
         Initially : Natural;
         Built     : Natural;
         Failed    : Natural;
         Ignored   : Natural;
         Skipped   : Natural;
         elapsed   : String (1 .. 8);
         impulse   : Natural;
         load      : Float;
         swap      : Float;
      end record;

   --  Initialize the curses screen.
   --  Returns False if no color support (curses not used at all)
   function launch_monitor (num_builders : builders) return Boolean;

   --  The build is done, return to the console
   procedure terminate_monitor;

   --  prints the summary header
   procedure summarize (data : summary_rec);

private

   type builder_palette is array (builders) of TIC.Color_Pair;

   app_width     : TIC.Column_Count := 80;
   zone_summary  : TIC.Window;
   zone_builders : TIC.Window;
   zone_actions  : TIC.Window;
   viewheight    : TIC.Line_Count;

   c_standard    : TIC.Color_Pair;
   c_builder     : builder_palette;
   c_success     : TIC.Color_Pair;
   c_failure     : TIC.Color_Pair;
   c_ignored     : TIC.Color_Pair;
   c_skipped     : TIC.Color_Pair;
   c_sumlabel    : TIC.Color_Pair;
   c_builderbar  : TIC.Color_Pair;
   c_elapsed     : TIC.Color_Pair;

   bright        : constant TIC.Character_Attribute_Set :=
                            (Bold_Character   => True, others => False);


   procedure launch_summary_zone;
   procedure launch_builders_zone (num_builders : builders);
   procedure launch_actions_zone (num_builders : builders);

   function inc (X : TIC.Line_Position; by : Integer) return TIC.Line_Position;


end Display;
