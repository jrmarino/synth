--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Interrupts.Names;

package Signals is

   pragma Unreserve_All_Interrupts;

   package BRK renames Ada.Interrupts;

   --  Returns True if Interrupt signal (control-C) has been detected
   function graceful_shutdown_requested return Boolean;

   --  Returns True if a second Interrupt signal has been detected
   function immediate_termination_requested return Boolean;

private

   protected Signal_Handler is

      procedure capture_control_c;
      pragma Attach_Handler (capture_control_c, BRK.Names.SIGINT);

      --  procedure capture_terminate;
      --  pragma Attach_Handler (capture_terminate, BRK.Names.SIGTERM);

   end Signal_Handler;

   control_c_break : Boolean := False;
   seriously_break : Boolean := False;

end Signals;
