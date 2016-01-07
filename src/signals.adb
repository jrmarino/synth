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
      if got_one and then caught_char = LAT.ESC then
         control_c_break := True;
      end if;
      return control_c_break;
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
