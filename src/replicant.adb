--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Directories;
with Ada.Exceptions;
with GNAT.OS_Lib;

with Parameters;
with JohnnyText;

--  remove
with Ada.Text_IO;

package body Replicant is

   package JT  renames JohnnyText;
   package PM  renames Parameters;
   package AD  renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package OSL renames GNAT.OS_Lib;

   package TIO renames Ada.Text_IO;


   -------------------
   --  mount_point  --
   -------------------
   function location (master_mount : String; point : folder) return String is
   begin
      case point is
         when bin       => return master_mount & root_bin;
         when sbin      => return master_mount & root_sbin;
         when usr       => return master_mount & root_usr;
         when lib       => return master_mount & root_lib;
         when dev       => return master_mount & root_dev;
         when etc       => return master_mount & root_etc;
         when tmp       => return master_mount & root_tmp;
         when var       => return master_mount & root_var;
         when xports    => return master_mount & root_xports;
         when options   => return master_mount & root_options;
         when libexec   => return master_mount & root_libexec;
         when packages  => return master_mount & root_packages;
         when distfiles => return master_mount & root_distfiles;
         when wrkdirs   => return master_mount & root_wrkdirs;
         when ccache    => return master_mount & root_x2_ccache;
      end case;
   end location;


   ------------------------
   --  get_master_mount  --
   ------------------------
   function get_master_mount return String is
   begin
      return JT.USS (PM.configuration.dir_buildbase) & "/" & reference_base;
   end get_master_mount;


   -----------------------
   --  get_slave_mount  --
   -----------------------
   function get_slave_mount  (id : builders) return String is
   begin
      return JT.USS (PM.configuration.dir_buildbase) & "/" & slave_name (id);
   end get_slave_mount;


   ------------------------------------
   --  construct_live_system_master  --
   ------------------------------------
   procedure construct_live_system_master
   is
      master_mount : constant String := get_master_mount;
      cf_xports    : constant String := JT.USS (PM.configuration.dir_portsdir);
      cf_options   : constant String := JT.USS (PM.configuration.dir_options);
      cf_packages  : constant String := JT.USS (PM.configuration.dir_packages);
      cf_distfiles : constant String := JT.USS (PM.configuration.dir_distfiles);
      opsys        : nullfs_flavor   := dragonfly;
   begin
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         opsys := freebsd;
      end if;
      flavor := opsys;

      for mnt in folder'Range loop
         forge_directory (location (master_mount, mnt));
      end loop;

      mount_nullfs (root_bin,     location (master_mount, bin));
      mount_nullfs (root_sbin,    location (master_mount, sbin));
      mount_nullfs (root_lib,     location (master_mount, lib));
      mount_nullfs (root_libexec, location (master_mount, libexec));
      mount_nullfs (root_usr,     location (master_mount, usr));
      mount_nullfs (cf_xports,    location (master_mount, xports));
      mount_nullfs (cf_options,   location (master_mount, options));
      mount_nullfs (cf_packages,  location (master_mount, packages));
      mount_nullfs (cf_distfiles, location (master_mount, distfiles));

   exception
      when hiccup : others => EX.Reraise_Occurrence (hiccup);
   end construct_live_system_master;


   --------------------
   --  mount_nullfs  --
   --------------------
   procedure mount_nullfs (target, mount_point : String;
                           mode : mount_mode := readonly)
   is
      cmd_freebsd   : constant String := "/sbin/mount_nullfs";
      cmd_dragonfly : constant String := "/sbin/mount_null";
      command       : JT.Text;
      Args          : OSL.Argument_List_Access;
      Exit_Status   : Integer;
   begin
      if not AD.Exists (target) then
         raise scenario_unexpected with
           "mount target " & target & " does not exist";
      end if;

      case flavor is
         when freebsd   => command := JT.SUS (cmd_freebsd);
         when dragonfly => command := JT.SUS (cmd_dragonfly);
         when unknown   =>
            raise scenario_unexpected with
              "Mounting on unknown operating system";
      end case;
      case mode is
         when readonly  => JT.SU.Append (command, " -o ro");
         when readwrite => null;
      end case;
      JT.SU.Append (command, " " & target);
      JT.SU.Append (command, " " & mount_point);

      Args := OSL.Argument_String_To_List (JT.USS (command));
      Exit_Status := OSL.Spawn (Program_Name => Args (Args'First).all,
                                Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);

      if Exit_Status /= 0 then
         raise scenario_unexpected with
           JT.USS (command) & " => failed with code" & Exit_Status'Img;
      end if;
   end mount_nullfs;


   ---------------
   --  unmount  --
   ---------------
   procedure unmount (device_or_node : String)
   is
      command     : constant String := "/sbin/umount " & device_or_node;
      Args        : OSL.Argument_List_Access;
      Exit_Status : Integer;
   begin
      Args := OSL.Argument_String_To_List (command);
      Exit_Status := OSL.Spawn (Program_Name => Args (Args'First).all,
                                Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      if Exit_Status /= 0 then
         raise scenario_unexpected with
           command & " => failed with code" & Exit_Status'Img;
      end if;
   end unmount;


   -----------------------
   --  forge_directory  --
   -----------------------
   procedure forge_directory (target : String) is
   begin
      AD.Create_Path (New_Directory => target);
   exception
      when failed : others =>
         TIO.Put_Line (EX.Exception_Information (failed));
         raise scenario_unexpected with
           "failed to create " & target & " directory";
   end forge_directory;


   ------------------------------------
   --  take_down_live_system_master  --
   ------------------------------------
   procedure take_down_live_system_master
   is
      procedure hardened_umount (node : String);
      procedure remove_master_mount;

      master_mount : constant String := get_master_mount;
--        ccache_target : constant String := JT.USS (PM.configuration.dir_ccache);
      goodsofar     : Boolean := True;

      procedure hardened_umount (node : String) is
      begin
         unmount (device_or_node => node);
      exception
         when others =>
            goodsofar := False;
            TIO.Put_Line (node & " Failed to unmount");
      end hardened_umount;
      procedure remove_master_mount is
      begin
         if goodsofar then
            AD.Delete_Tree (master_mount);
         end if;
      exception
         when others =>
            TIO.Put_Line ("Failed to remove " & master_mount);
      end remove_master_mount;
   begin
      for mnt in subfolder'Range loop
         hardened_umount (location (master_mount, mnt));
      end loop;
--        if AD.Exists (ccache_target) then
--           hardened_umount (ccache_target);
--        end if;

      remove_master_mount;
   end take_down_live_system_master;


   -------------------
   --  mount_tmpfs  --
   -------------------
   procedure mount_tmpfs  (mount_point : String; max_size_M : Natural := 0)
   is
      cmd_freebsd   : constant String := "/sbin/mount";
      cmd_dragonfly : constant String := "/sbin/mount_tmpfs";
      command       : JT.Text;
      Args          : OSL.Argument_List_Access;
      Exit_Status   : Integer;
   begin
      case flavor is
         when freebsd   => command := JT.SUS (cmd_freebsd);
         when dragonfly => command := JT.SUS (cmd_dragonfly);
         when unknown   =>
            raise scenario_unexpected with
              "Mounting on unknown operating system";
      end case;
      if max_size_M > 0 then
         JT.SU.Append (command, " -o size=" & JT.trim (max_size_M'Img) & "M");
      end if;
      JT.SU.Append (command, " tmpfs " & mount_point);

      Args := OSL.Argument_String_To_List (JT.USS (command));
      Exit_Status := OSL.Spawn (Program_Name => Args (Args'First).all,
                                Args => Args (Args'First + 1 .. Args'Last));
      OSL.Free (Args);
      if Exit_Status /= 0 then
         raise scenario_unexpected with
           JT.USS (command) & " => failed with code" & Exit_Status'Img;
      end if;
   end mount_tmpfs;


   ------------------
   --  get_suffix  --
   ------------------
   function slave_name (id : builders) return String
   is
      id_image     : constant String := Integer (id)'Img;
      suffix       : String := "SL00";
   begin
      if id < 10 then
         suffix (4) := id_image (2);
      else
         suffix (3 .. 4) := id_image (2 .. 3);
      end if;
      return suffix;
   end slave_name;


   --------------------
   --  launch_slave  --
   --------------------
   procedure launch_slave  (id : builders)
   is
      master_mount : constant String := get_master_mount;
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
   begin
      forge_directory (slave_base);
      mount_nullfs (master_mount, slave_base);

      mount_tmpfs (location (slave_base, tmp), 200);
      mount_tmpfs (location (slave_base, etc),  12);
      mount_tmpfs (location (slave_base, var), 200);

      if PM.configuration.tmpfs_workdir then
         mount_tmpfs (location (slave_base, wrkdirs), 12 * 1024);
      else
         forge_directory (slave_work);
         mount_nullfs (slave_work, location (slave_base, wrkdirs), readwrite);
      end if;

      if PM.configuration.tmpfs_localbase then
         mount_tmpfs (slave_base & root_localbase, 12 * 1024);
      else
         forge_directory (slave_local);
         mount_nullfs (slave_local, slave_base & root_localbase, readwrite);
      end if;

      --  write etc files now
      --  write var stuff?

   end launch_slave;


   ---------------------
   --  destroy_slave  --
   ---------------------
   procedure destroy_slave (id : builders)
   is
      master_mount : constant String := get_master_mount;
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
   begin
      unmount (slave_base & root_localbase);
      if not PM.configuration.tmpfs_localbase then
         AD.Delete_Tree (slave_base & root_localbase);
      end if;

      unmount (location (slave_base, wrkdirs));
      if PM.configuration.tmpfs_workdir then
         AD.Delete_Tree (location (slave_base, wrkdirs));
      end if;

      unmount (location (slave_base, tmp));
      unmount (location (slave_base, etc));
      unmount (location (slave_base, var));
      unmount (slave_base);

   end destroy_slave;

end Replicant;
