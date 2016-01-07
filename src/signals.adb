--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Signals is

   -----------------------------------
   --  graceful_shutdown_requested  --
   -----------------------------------
   function graceful_shutdown_requested return Boolean is
   begin
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
