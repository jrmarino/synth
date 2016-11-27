--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body PortScan.Buildcycle.Pkgsrc is

   ---------------------
   --  build_package  --
   ---------------------
   function build_package (id          : builders;
                           sequence_id : port_id;
                           interactive : Boolean := False;
                           interphase  : String  := "") return Boolean
   is
   begin
      return False;
   end build_package;


   ----------------------
   --  builder_status  --
   ----------------------
   function builder_status (id : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec
   is
      result   : Display.builder_rec;
   begin
      --  123456789 123456789 123456789 123456789 1234
      --   SL  elapsed   phase              lines  origin
      --   01  00:00:00  extract-depends  9999999  www/joe

      result.id       := id;
      result.slavid   := JT.zeropad (Natural (id), 2);
      result.LLines   := (others => ' ');
      result.phase    := (others => ' ');
      result.origin   := (others => ' ');
      result.shutdown := False;
      result.idle     := False;

      if shutdown then
         --  Overrides "idle" if both Shutdown and Idle are True
         result.Elapsed  := "Shutdown";
         result.shutdown := True;
         return result;
      end if;
      if idle then
         result.Elapsed := "Idle    ";
         result.idle    := True;
         return result;
      end if;

      declare
--         phasestr : constant String := phase2str (phase_trackers (id));
         catport  : constant String :=
           get_catport (all_ports (trackers (id).seq_id));
         numlines : constant String := format_loglines (trackers (id).loglines);
         linehead : constant Natural := 8 - numlines'Length;
      begin
         result.Elapsed := elapsed_HH_MM_SS (start => trackers (id).head_time,
                                             stop  => CAL.Clock);
         result.LLines (linehead .. 7) := numlines;
--         result.phase  (1 .. phasestr'Length) := phasestr;

         if catport'Length > 37 then
            result.origin (1 .. 36) := catport (1 .. 36);
            result.origin (37) := LAT.Asterisk;
         else
            result.origin (1 .. catport'Length) := catport;
         end if;
      end;
      return result;
   end builder_status;


   ------------------------
   --  valid_test_phase  --
   ------------------------
   function valid_test_phase (afterphase : String) return Boolean is
   begin
      return False;
   end valid_test_phase;


   ------------------------
   --  last_build_phase  --
   ------------------------
   function last_build_phase (id : builders) return String is
   begin
      --      return phase2str (phase => phase_trackers (id));
      return "";
   end last_build_phase;


end PortScan.Buildcycle.Pkgsrc;
