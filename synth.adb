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
      elsif first = "update-repository" then
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
                  if PIL.verify_desire_to_rebuild_repository then
                     PIL.rebuild_local_respository;
                     if PIL.verify_desire_to_install_packages then
                        PIL.install_new_packages_to_live_system;
                     end if;
                  end if;
               end if;
               return;
            when force =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail (delete_first => True)
               then
                  PIL.perform_bulk_run (testmode => False);
                  if PIL.verify_desire_to_rebuild_repository then
                     PIL.rebuild_local_respository;
                     if PIL.verify_desire_to_install_packages then
                        PIL.install_new_packages_to_live_system;
                     end if;
                  end if;
               end if;
               return;
            when install =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => False);
                  PIL.rebuild_local_respository;
                  PIL.install_new_packages_to_live_system;
               end if;
               return;
            when test =>
               if PIL.build_pkg8_as_necessary and then
                 PIL.scan_stack_of_single_ports and then
                 PIL.sanity_check_then_prefail
               then
                  PIL.perform_bulk_run (testmode => True);
               end if;
               return;
         end case;

      else
         --  We have exactly one argument
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
               TIO.Put_Line ("UPGRADE-SYSTEM to be implemented ...");
               return;
            when up_repo =>
               TIO.Put_Line ("UPDATE_REPO to be implemented ...");
               return;
            when purge =>
               TIO.Put_Line ("PURGE to be implemented ...");
               return;
            when everything =>
               TIO.Put_Line ("EVERYTHING to be implemented ...");
               return;
         end case;
      end if;
   end;



--     pid : PortScan.port_id;
--     good_scan : Boolean;
--     pkg_good  : Boolean;
--     package T   renames Ada.Text_IO;
--     package OPS renames PortScan.Ops;
--     use type PortScan.port_id;
--
--  begin
--
--
--     Replicant.initialize;
--  --     Replicant.launch_slave (3);
--  --     Replicant.launch_slave (12);
--  --     delay 35.0;
--  --     Replicant.destroy_slave (3);
--  --     Replicant.destroy_slave (12);
--  --   return;
--
--     --  needs to read environment or make -C <anyport> -V PORTSDIR
--  --     good_scan := PortScan.scan_entire_ports_tree (portsdir => "/usr/xports");
--
--     good_scan := PortScan.scan_single_port
--       (portsdir => USS (Parameters.configuration.dir_portsdir),
--        catport => "ports-mgmt/pkg",
--        repository => USS (Parameters.configuration.dir_repository));
--
--     if good_scan then
--        PortScan.set_build_priority;
--     else
--        T.Put_Line ("pkg(8) scan failure, exiting");
--        Replicant.finalize;
--        return;
--     end if;
--
--     PortScan.Packages.limited_sanity_check
--       (repository => USS (Parameters.configuration.dir_repository));
--
--     if not PortScan.Packages.queue_is_empty then
--        PortScan.Buildcycle.initialize (False);
--        pid := PortScan.Ops.top_buildable_port;
--        T.Put_Line ("Rebuilding pkg(8) First ...");
--
--        Replicant.launch_slave (id => 1);
--        pkg_good := PortScan.Buildcycle.build_package
--          (id => 1, sequence_id => pid);
--        Replicant.destroy_slave (id => 1);
--        if not pkg_good then
--           T.Put_Line ("Failed to build pkg(8), exiting");
--           Replicant.finalize;
--           return;
--        end if;
--     end if;
--
--     PortScan.reset_ports_tree;
--
--     good_scan := PortScan.scan_single_port
--       (portsdir => USS (Parameters.configuration.dir_portsdir),
--        catport => "editors/joe",
--        repository => USS (Parameters.configuration.dir_repository));
--
--
--
--  --     good_scan := PortScan.scan_single_port
--  --       (portsdir => USS (Parameters.configuration.dir_portsdir),
--  --        catport => "mail/thunderbird",
--  --        repository => USS (Parameters.configuration.dir_repository));
--
--     if good_scan then
--        PortScan.set_build_priority;
--     else
--        Replicant.finalize;
--        return;
--     end if;
--
--
--     PortScan.Packages.limited_sanity_check
--       (repository => USS (Parameters.configuration.dir_repository));
--
--     --  return;
--
--     T.Put_Line ("");
--     T.Put_Line ("Initial Queue length is" & OPS.queue_length'Img);
--     loop
--        pid := OPS.next_ignored_port;
--        if pid = PortScan.port_match_failed then
--           exit;
--        end if;
--        T.Put_Line (OPS.port_name (pid) & " has been ignored: " &
--                    (OPS.ignore_reason (pid)));
--        OPS.cascade_failed_build (pid);
--     end loop;
--     T.Put_Line ("Final Queue length is" & OPS.queue_length'Img);
--     if PortScan.Packages.queue_is_empty then
--        T.Put_Line ("Everything is Ok, there's nothing to do.");
--     else
--        if OPS.integrity_intact then
--           T.Put_Line ("Integrity intact");
--        else
--           T.Put_Line ("Integrity lost !!!!");
--        end if;
--
--        T.Put_Line ("Starting build simulation");
--        PortScan.Buildcycle.initialize (True);
--        OPS.parallel_bulk_run
--          (num_builders => Parameters.configuration.num_builders);
--     end if;
--     Replicant.finalize;
--
--
--  --     PortScan.release_ports_tree;

end synth;
