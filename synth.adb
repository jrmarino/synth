--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: License.txt

with Ada.Command_Line;
with Ada.Text_IO;
with Actions;
with PortScan.Pilot;
with Parameters;

procedure synth
is
   type mandate_type is (unset, status, help, configure, version, up_system,
                         prep_system, purge, everything, build, install, force,
                         just_build, test, status_everything, gen_repo);

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
      badcfg : constant String := "Configuration failed to load.";
      regjoe : constant String := "Only the root user can execute that.";
      holdon : constant String := "Synth is already running on this system.";
      badmnt : constant String := "Builder mounts detected; attempting to " &
                                  "remove them automatically ...";
      badwrk : constant String := "Old work directories detected; attempting " &
                                  "to remove them automatically ...";
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
      elsif first = "prepare-system" then
         mandate := prep_system;
      elsif first = "rebuild-repository" then
         mandate := gen_repo;
      elsif first = "purge-distfiles" then
         mandate := purge;
      elsif first = "everything" then
         mandate := everything;
      elsif first = "status-everything" then
         mandate := status_everything;
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
            when help | configure | version | prep_system | up_system | purge |
                 everything | status_everything =>
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

         if not Parameters.all_paths_valid then
            return;
         end if;

         if not PIL.store_origins then
            --  error messages emitted by store_origins, just exit now
            return;
         end if;

         if PIL.insufficient_privileges then
            TIO.Put_Line (regjoe);
            return;
         end if;

         if PIL.already_running then
            TIO.Put_Line (holdon);
            return;
         end if;

         if PIL.previous_run_mounts_detected then
            TIO.Put_Line (badmnt);
            if not PIL.old_mounts_successfully_removed then
               return;
            end if;
         end if;

         if PIL.previous_realfs_work_detected then
            TIO.Put_Line (badwrk);
            if not PIL.old_realfs_work_successfully_removed then
               return;
            end if;
         end if;

         if PIL.synthexec_missing then
            return;
         end if;

         PIL.create_pidfile;

         ----------------------------------
         --  Multiple argument commands  --
         ----------------------------------
         case mandate is
            when help | configure | version | prep_system | up_system | purge |
                 everything | status_everything | gen_repo | unset =>
               --  Handled above.  Don't use "others" here;
               --  we don't want to disable full coverage
               null;
            when status =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => False) and then
                 PIL.sanity_check_then_prefail (delete_first => False,
                                                dry_run => True)
               then
                  PIL.display_results_of_dry_run;
               end if;
            when just_build =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => False) and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
               end if;
            when build =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => False) and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.verify_desire_to_rebuild_repository and then
                    PIL.rebuild_local_respository and then
                    PIL.verify_desire_to_install_packages and then
                    PIL.write_pkg_repos_configuration_file
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
            when force =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => False) and then
                 PIL.sanity_check_then_prefail (delete_first => True)
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.verify_desire_to_rebuild_repository and then
                    PIL.rebuild_local_respository and then
                    PIL.verify_desire_to_install_packages and then
                    PIL.write_pkg_repos_configuration_file
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
            when install =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => False) and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.rebuild_local_respository and then
                    PIL.write_pkg_repos_configuration_file
                  then
                     PIL.upgrade_system_exactly;
                  end if;
               end if;
            when test =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports (testmode => True) and then
                 PIL.sanity_check_then_prefail (delete_first => True)
               then
                  if PIL.interact_with_single_builder then
                     PIL.bulk_run_then_interact_with_final_port;
                  else
                     PIL.perform_bulk_run (testmode => True);
                  end if;
               end if;
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


         if PIL.insufficient_privileges then
            TIO.Put_Line (regjoe);
            return;
         end if;

         if PIL.already_running then
            TIO.Put_Line (holdon);
            return;
         end if;

         PortScan.set_cores;
         if not Parameters.load_configuration (PortScan.cores_available) then
            TIO.Put_Line (badcfg);
            return;
         end if;

         if not (mandate = configure) and then not Parameters.all_paths_valid
         then
            return;
         end if;

         if PIL.previous_run_mounts_detected then
            TIO.Put_Line (badmnt);
            if not PIL.old_mounts_successfully_removed then
               return;
            end if;
         end if;

         if PIL.previous_realfs_work_detected then
            TIO.Put_Line (badwrk);
            if not PIL.old_realfs_work_successfully_removed then
               return;
            end if;
         end if;

         if PIL.synthexec_missing then
            return;
         end if;

         PIL.create_pidfile;

         case mandate is
            when build | just_build | install | test | version | help |
                 force | unset =>
               --  Handled above.  Don't use "others" here;
               --  we don't want to disable full coverage
               null;
            when configure =>
               ACT.launch_configure_menu (PortScan.cores_available);
            when status =>
               PIL.upgrade_system_everything (skip_installation => True,
                                              dry_run           => True);
            when up_system =>
               if PIL.write_pkg_repos_configuration_file then
                  PIL.upgrade_system_everything;
               end if;
            when prep_system =>
               PIL.upgrade_system_everything (skip_installation => True);
            when gen_repo =>
               if PIL.rebuild_local_respository then
                  null;
               end if;
            when purge =>
               PIL.purge_distfiles;
            when everything =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.fully_scan_ports_tree and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.rebuild_local_respository (use_full_scan => False) then
                     null;
                  end if;
               end if;
            when status_everything =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.fully_scan_ports_tree and then
                 PIL.sanity_check_then_prefail (delete_first => False,
                                                dry_run => True)
               then
                  PIL.display_results_of_dry_run;
               end if;
         end case;
      end if;
   end;

   PIL.destroy_pidfile;

end synth;
