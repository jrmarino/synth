--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Actions;
with PortScan;
with PortScan.Pilot;
with Parameters;

procedure synth
is
   type mandate_type is (unset, status, help, configure, version, up_system,
                         up_repo, purge, everything, build, install, force,
                         just_build, test);

   mandate : mandate_type := unset;

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;
   package ACT renames Actions;
   package PIL renames PortScan.Pilot;

begin

   if CLI.Argument_Count = 0 then
      ACT.print_version;
      return;
   end if;

   declare
      first  : constant String := CLI.Argument (1);
      comerr : constant String := "Synth command error: ";
      badcfg : constant String := "Synth error: configuration failed to load.";
   begin
      if first = "help" then
         mandate := help;
      elsif first = "status" then
         mandate := status;
      elsif first = "version" then
         mandate := version;
      elsif first = "configure" then
         mandate := configure;
      elsif first = "install" then
         mandate := install;
      elsif first = "build" then
         mandate := build;
      elsif first = "force" then
         mandate := force;
      elsif first = "just-build" then
         mandate := just_build;
      elsif first = "upgrade-system" then
         mandate := up_system;
      elsif first = "rebuild-repository" then
         mandate := up_repo;
      elsif first = "purge-distfiles" then
         mandate := purge;
      elsif first = "everything" then
         mandate := everything;
      elsif first = "test" then
         mandate := test;
      end if;

      if CLI.Argument_Count > 1 then
         case mandate is
            when unset =>
               ACT.print_version;
               TIO.Put_Line (comerr & "'" & first &
                               "' is not a valid keyword.");
               return;
            when help | configure | version | up_repo | up_system | purge |
                 everything =>
               ACT.print_version;
               TIO.Put_Line (comerr & "'" & first &
                               "' keyword uses no arguments.");
               return;
            when others => null;
         end case;

         PortScan.set_cores;
         if not Parameters.load_configuration (PortScan.cores_available) then
            TIO.Put_Line (badcfg);
            return;
         end if;

         if not PIL.store_origins then
            --  error messages emitted by store_origins, just exit now
            return;
         end if;

         ----------------------------------
         --  Multiple argument commands  --
         ----------------------------------
         case mandate is
            when help | configure | version | up_repo | up_system | purge |
                 everything | unset =>
               --  Handled above.  Don't use "others" here;
               --  we don't want to disable full coverage
               null;
            when status =>
               TIO.Put_Line ("multi-arg STATUS to be implemented ...");
               return;
            when just_build =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
               end if;
               return;
            when build =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.verify_desire_to_rebuild_repository and then
                    PIL.rebuild_local_respository and then
                    PIL.verify_desire_to_install_packages
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
               return;
            when force =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail (delete_first => True)
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.verify_desire_to_rebuild_repository and then
                    PIL.rebuild_local_respository and then
                    PIL.verify_desire_to_install_packages
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
               return;
            when install =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.rebuild_local_respository
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
               return;
            when test =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail (delete_first => True)
               then
                  PIL.perform_bulk_run (testmode => True);
               end if;
               return;
         end case;

      else
         --------------------------------
         --  Single argument commands  --
         --------------------------------
         case mandate is
            when build | force | just_build | install | test =>
               ACT.print_version;
               TIO.Put_Line (comerr & "'" & first &
                               "' requires at least one argument.");
               return;
            when version =>
               ACT.print_version;
               return;
            when help =>
               ACT.print_help;
               return;
            when unset =>
               ACT.print_version;
               TIO.Put_Line (comerr & "'" & first &
                               "' is not a valid keyword.");
               return;
            when others => null;
         end case;

         PortScan.set_cores;
         if not Parameters.load_configuration (PortScan.cores_available) then
            TIO.Put_Line (badcfg);
            return;
         end if;

         case mandate is
            when build | just_build | install | test | version | help |
                 force | unset =>
               --  Handled above.  Don't use "others" here;
               --  we don't want to disable full coverage
               null;
            when configure =>
               ACT.launch_configure_menu (PortScan.cores_available);
               return;
            when status =>
               TIO.Put_Line ("single STATUS to be implemented ...");
               return;
            when up_system =>
               if PIL.write_pkg_repos_configuration_file then
                  PIL.upgrade_system_everything;
               end if;
               return;
            when up_repo =>
               PIL.upgrade_system_everything (skip_installation => True);
               return;
            when purge =>
               PIL.purge_distfiles;
               return;
            when everything =>
               TIO.Put_Line ("EVERYTHING to be implemented ...");
               --  run PKG.clean_repository instead of limited sanity check
               --  don't run it again before repos building
               return;
         end case;
      end if;
   end;

end synth;
