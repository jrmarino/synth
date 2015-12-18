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
   function location (mount_base : String; point : folder) return String is
   begin
      case point is
         when bin         => return mount_base & root_bin;
         when sbin        => return mount_base & root_sbin;
         when usr_bin     => return mount_base & root_usr_bin;
         when usr_include => return mount_base & root_usr_include;
         when usr_lib     => return mount_base & root_usr_lib;
         when usr_libdata => return mount_base & root_usr_libdata;
         when usr_libexec => return mount_base & root_usr_libexec;
         when usr_local   => return mount_base & root_localbase;
         when usr_sbin    => return mount_base & root_usr_sbin;
         when usr_share   => return mount_base & root_usr_share;
         when usr_src     => return mount_base & root_usr_src;
         when lib         => return mount_base & root_lib;
         when dev         => return mount_base & root_dev;
         when etc         => return mount_base & root_etc;
         when tmp         => return mount_base & root_tmp;
         when var         => return mount_base & root_var;
         when home        => return mount_base & root_home;
         when proc        => return mount_base & root_proc;
         when root        => return mount_base & root_root;
         when xports      => return mount_base & root_xports;
         when options     => return mount_base & root_options;
         when libexec     => return mount_base & root_libexec;
         when packages    => return mount_base & root_packages;
         when distfiles   => return mount_base & root_distfiles;
         when wrkdirs     => return mount_base & root_wrkdirs;
         when ccache      => return mount_base & root_ccache;
      end case;
   end location;


   --------------------
   --  mount_target  --
   --------------------
   function mount_target (point : folder) return String is
   begin
      case point is
         when xports    => return JT.USS (PM.configuration.dir_portsdir);
         when options   => return JT.USS (PM.configuration.dir_options);
         when packages  => return JT.USS (PM.configuration.dir_packages);
         when distfiles => return JT.USS (PM.configuration.dir_distfiles);
         when ccache    => return JT.USS (PM.configuration.dir_ccache);
         when others    => return "ERROR";
      end case;
   end mount_target;


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


   ------------------------
   --  construct_system  --
   ------------------------
   procedure construct_system
   is
       opsys : nullfs_flavor   := dragonfly;
   begin
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         opsys := freebsd;
      end if;
      flavor := opsys;

      launch_slave (1);
      delay 35.0;
   end construct_system;


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
      if not AD.Exists (mount_point) then
         raise scenario_unexpected with
           "mount point " & mount_point & " does not exist";
      end if;
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


   ------------------------
   --  take_down_system  --
   ------------------------
   procedure take_down_system is
   begin
      destroy_slave (1);
   end take_down_system;


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
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
   begin
      forge_directory (slave_base);
      mount_tmpfs (slave_base);

      for mnt in folder'Range loop
         forge_directory (location (slave_base, mnt));
      end loop;

      for mnt in subfolder'Range loop
         mount_nullfs (target      => location ("", mnt),
                       mount_point => location (slave_base, mnt));

      end loop;

      --  TODO: Lock home
      --  TODO: Lock root
      --  TODO: Add symbolic link between /sys and /usr/src/sys
      --  TODO: set up /etc
      --  TODO: set up /var
      --  TODO: populate /dev


      mount_nullfs (mount_target (xports),    location (slave_base, xports));
      mount_nullfs (mount_target (options),   location (slave_base, options));
      mount_nullfs (mount_target (packages),  location (slave_base, packages));
      mount_nullfs (mount_target (distfiles), location (slave_base, distfiles));

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

      if AD.Exists (root_usr_src) then
         mount_nullfs (root_usr_src, location (slave_base, usr_src));
      end if;

      if AD.Exists (mount_target (ccache)) then
         mount_nullfs (mount_target (ccache), location (slave_base, ccache));
      end if;

   exception
      when hiccup : others => EX.Reraise_Occurrence (hiccup);
   end launch_slave;


   ---------------------
   --  destroy_slave  --
   ---------------------
   procedure destroy_slave (id : builders)
   is
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

      if AD.Exists (root_usr_src) then
         unmount (location (slave_base, usr_src));
      end if;

      if AD.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache));
      end if;

      unmount (location (slave_base, xports));
      unmount (location (slave_base, options));
      unmount (location (slave_base, packages));
      unmount (location (slave_base, distfiles));

      for mnt in subfolder'Range loop
         unmount (location (slave_base, mnt));
      end loop;

      unmount (slave_base);
      AD.Delete_Tree (slave_base);

   exception
      when hiccup : others => EX.Reraise_Occurrence (hiccup);
   end destroy_slave;

end Replicant;
