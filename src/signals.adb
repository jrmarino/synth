--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with Ada.Characters.Latin_1;

package body Signals is

   package TIO renames Ada.Text_IO;
   package LAT renames Ada.Characters.Latin_1;

   -----------------------------------
   --  graceful_shutdown_requested  --
   -----------------------------------
   function graceful_shutdown_requested return Boolean
   is
      caught_char : Character;
      got_one     : Boolean;
   begin
      TIO.Get_Immediate (Item => caught_char, Available => got_one);
      --  Although the variable is called control_c, it's capital Q that
      --  we are catching.  It was the ESCAPE key after control-C, but that
      --  one was getting triggered by terminal ANSI codes.  Before Q it was
      --  Control-Q, but that only worked during the ncurses display.
      --  Apparently raw tty mode is required for control code detection.
      --  Capital-Q is documented, but Control-Q still works (during ncurses)
      if got_one then
         if caught_char = 'Q' or else caught_char = LAT.DC1 then
            control_c_break := True;
         end if;
      end if;
      return control_c_break;
   exception
      when others => return control_c_break;
   end graceful_shutdown_requested;


   ---------------------------------------
   --  immediate_termination_requested  --
   ---------------------------------------
   function immediate_termination_requested return Boolean is
   begin
      return seriously_break;
   end immediate_termination_requested;


--     ----------------------
--     --  Signal_Handler  --
--     ----------------------
--     protected body Signal_Handler is
--
--        -------------------------
--        --  capture_control_c  --
--        -------------------------
--        procedure capture_control_c is
--        begin
--           if control_c_break then
--              seriously_break := True;
--           else
--              control_c_break := True;
--           end if;
--        end capture_control_c;
--
--     end Signal_Handler;

end Signals;
