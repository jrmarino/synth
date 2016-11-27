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
         phasestr : constant String := phase2str (phase_trackers (id));
         catport  : constant String :=
           get_catport (all_ports (trackers (id).seq_id));
         numlines : constant String := format_loglines (trackers (id).loglines);
         linehead : constant Natural := 8 - numlines'Length;
      begin
         result.Elapsed := elapsed_HH_MM_SS (start => trackers (id).head_time,
                                             stop  => CAL.Clock);
         result.LLines (linehead .. 7) := numlines;
         result.phase  (1 .. phasestr'Length) := phasestr;

         if catport'Length > 37 then
            result.origin (1 .. 36) := catport (1 .. 36);
            result.origin (37) := LAT.Asterisk;
         else
            result.origin (1 .. catport'Length) := catport;
         end if;
      end;
      return result;
   end builder_status;


    -----------------
   --  phase2str  --
   -----------------
   function phase2str (phase : phases) return String is
   begin
      case phase is
         when bootstrap_depends => return "bootstrap-depends";
         when fetch             => return "fetch";
         when checksum          => return "checksum";
         when depends           => return "depends";
         when tools             => return "tools";
         when extract           => return "extract";
         when patch             => return "patch";
         when wrapper           => return "wrapper";
         when configure         => return "configure";
         when build             => return "build";
         when test              => return "test";
         when stage_install     => return "stage-install";
         when create_package    => return "create-package";
         when package_install   => return "package-install";
         when deinstall         => return "deinstall";
      end case;
   end phase2str;


   ---------------------------
   --  valid_test_phase #1  --
   ---------------------------
   function valid_test_phase (afterphase : String) return phases is
   begin
      if afterphase = "extract" then
         return extract;
      elsif afterphase = "patch" then
         return patch;
      elsif afterphase = "configure" then
         return configure;
      elsif afterphase = "build" then
         return build;
      elsif afterphase = "stage" then
         return stage_install;
      elsif afterphase = "install" then
         return package_install;
      elsif afterphase = "deinstall" then
         return deinstall;
      else
         return bootstrap_depends;
      end if;
   end valid_test_phase;


   ------------------------
   --  last_build_phase  --
   ------------------------
   function last_build_phase (id : builders) return String is
   begin
      return phase2str (phase => phase_trackers (id));
   end last_build_phase;


   -------------------------------
   --  max_time_without_output  --
   -------------------------------
   function max_time_without_output (phase : phases) return execution_limit
   is
      base : Integer;
   begin
      case phase is
         when bootstrap_depends => base := 3;
         when fetch | checksum  => return 480;
         when depends           => base := 6;
         when tools             => base := 5;
         when extract           => base := 20;
         when patch             => base := 3;
         when wrapper           => base := 3;
         when configure         => base := 15;
         when build             => base := 25;
         when test              => base := 10;
         when stage_install     => base := 20;
         when create_package    => base := 80;
         when package_install   => base := 10;
         when deinstall         => base := 10;
      end case;
      declare
         multiplier_x10 : constant Positive := timeout_multiplier_x10;
      begin
         return execution_limit (base * multiplier_x10 / 10);
      end;
   end max_time_without_output;


end PortScan.Buildcycle.Pkgsrc;
