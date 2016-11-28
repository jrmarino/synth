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
      R : Boolean;
      break_phase : constant phases := valid_test_phase (interphase);
   begin
      trackers (id).seq_id := sequence_id;
      trackers (id).loglines := 0;
      if uselog then
         if not initialize_log (id) then
            finalize_log (id);
            return False;
         end if;
      end if;
      for phase in phases'Range loop
         phase_trackers (id) := phase;

         case phase is
            when fetch | checksum | tools | extract | patch | wrapper |
                 create_package =>
               R := exec_phase_generic (id, phase);

            when bootstrap_depends | depends =>
               R := exec_phase_generic (id, phase);

            when configure =>
               if testing then
                  if lock_localbase then
                     set_localbase_protection (id, True);
                  end if;
                  mark_file_system (id, "preconfig");
               end if;
               R := exec_phase_generic (id, phase);

            when build =>
               R := exec_phase_build (id);

            when stage_install =>
               if testing then
                  mark_file_system (id, "prestage");
               end if;
               R := exec_phase_generic (id, phase);

            when test | package_install =>
               if testing then
                  R := exec_phase_generic (id, phase);
               end if;

            when deinstall =>
               if testing then
                  R := exec_phase_deinstall (id);
               end if;
         end case;
         exit when R = False;
         exit when interactive and then phase = break_phase;
      end loop;
      if uselog then
         finalize_log (id);
      end if;
      if interactive then
         interact_with_builder (id);
      end if;
      return R;
   end build_package;


   ------------------
   --  exec_phase  --
   ------------------
   function exec_phase (id : builders; phase : phases;
                        time_limit    : execution_limit;
                        phaseenv      : String := "";
                        skip_header   : Boolean := False;
                        skip_footer   : Boolean := False)
                        return Boolean
   is
      root       : constant String := get_root (id);
      pid        : port_id := trackers (id).seq_id;
      catport    : constant String := get_catport (all_ports (pid));
      result     : Boolean;
      timed_out  : Boolean;
   begin
      --  Nasty, we have to switch open and close the log file for each
      --  phase because we have to switch between File_Type and File
      --  Descriptors.  I can't find a safe way to get the File Descriptor
      --  out of the File type.

      if uselog then
         if not skip_header then
            log_phase_begin (phase2str (phase), id);
         end if;
         TIO.Close (trackers (id).log_handle);
      end if;

      declare
         command : constant String := chroot & root & environment_override &
           phaseenv & chroot_make_program & " -C " & dir_ports & "/" &
           catport & " " & phase2str (phase);
      begin
         result := generic_execute (id, command, timed_out, time_limit);
      end;

      --  Reopen the log.  I guess we can leave off the exception check
      --  since it's been passing before

      if uselog then
         TIO.Open (File => trackers (id).log_handle,
                   Mode => TIO.Append_File,
                   Name => log_name (trackers (id).seq_id));
         if timed_out then
            TIO.Put_Line (trackers (id).log_handle,
                          "###  Watchdog killed runaway process!  (no activity for" &
                            time_limit'Img & " minutes)  ###");
         end if;
         if not skip_footer then
            log_phase_end (id);
         end if;
      end if;

      return result;
   end exec_phase;


   --------------------------
   --  exec_phase_generic  --
   --------------------------
   function exec_phase_generic (id : builders; phase : phases) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (phase);
   begin
      return exec_phase (id => id, phase => phase, time_limit => time_limit);
   end exec_phase_generic;


   ------------------------
   --  exec_phase_build  --
   ------------------------
   function exec_phase_build (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (build);
      passed : Boolean;
   begin
      passed := exec_phase (id          => id,
                            phase       => build,
                            time_limit  => time_limit,
                            skip_header => False,
                            skip_footer => True);
      if testing and then passed then
         if lock_localbase then
            set_localbase_protection (id, False);
         end if;
         passed := detect_leftovers_and_MIA
           (id, "preconfig", "between port configure and build");
      end if;
      if uselog then
         log_phase_end (id);
      end if;
      return passed;
   end exec_phase_build;



   ----------------------------
   --  exec_phase_deinstall  --
   ----------------------------
   function exec_phase_deinstall (id : builders) return Boolean
   is
      time_limit : execution_limit := max_time_without_output (deinstall);
      result     : Boolean;
   begin
      --  This is only run during "testing" so assume that.
      if uselog then
         log_phase_begin (phase2str (deinstall), id);
         log_linked_libraries (id);
      end if;
      result := exec_phase (id          => id,
                            phase       => deinstall,
                            time_limit  => time_limit,
                            skip_header => True,
                            skip_footer => True);
      if not result then
         if uselog then
            log_phase_end (id);
         end if;
         return False;
      end if;
      if uselog then
         result := detect_leftovers_and_MIA
           (id, "prestage", "between staging and package deinstallation");
         log_phase_end (id);
      end if;
      return result;
   end exec_phase_deinstall;


   ----------------------
   --  builder_status  --
   ----------------------
   function builder_status (id : builders;
                            shutdown : Boolean := False;
                            idle     : Boolean := False)
                            return Display.builder_rec
   is
      phasestr : constant String := phase2str (phase_trackers (id));
   begin
      return builder_status_core (id       => id,
                                  shutdown => shutdown,
                                  idle     => idle,
                                  phasestr => phasestr);
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
