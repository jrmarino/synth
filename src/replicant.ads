--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;   use Definitions;

package Replicant is

   scenario_unexpected : exception;

--  private

   type mount_mode is (readonly, readwrite);
   type nullfs_flavor is (unknown, freebsd, dragonfly);
   type folder is (bin, sbin, lib, usr, xports, options, libexec, packages,
                   distfiles, dev, etc, tmp, var, wrkdirs, ccache);
   subtype subfolder is folder range bin .. distfiles;

   LiveBase       : constant String := "LiveBase";
   root_bin       : constant String := "/bin";
   root_sbin      : constant String := "/sbin";
   root_usr       : constant String := "/usr";
   root_lib       : constant String := "/lib";
   root_dev       : constant String := "/dev";
   root_etc       : constant String := "/etc";
   root_tmp       : constant String := "/tmp";
   root_var       : constant String := "/var";
   root_xports    : constant String := "/xports";
   root_options   : constant String := "/options";
   root_libexec   : constant String := "/libexec";
   root_wrkdirs   : constant String := "/wrkdirs";
   root_packages  : constant String := "/packages";
   root_distfiles : constant String := "/distfiles";
   root_x2_ccache : constant String := "/root/ccache";
   root_localbase : constant String := "/usr/local";

   livesys_prefix : constant String := "LS";
   flavor         : nullfs_flavor   := unknown;

   --  This builds the reference "master" to which all the builders mount
   --  This is the first major step in a bulk build
   procedure construct_live_system_master;

   --  This deconstructors the reference master and it's the last step.
   procedure take_down_live_system_master;

   --  Throws exception if mount attempt was unsuccessful
   procedure mount_nullfs (target, mount_point : String;
                           mode : mount_mode := readonly);

   --  Throws exception if mount attempt was unsuccessful
   procedure mount_tmpfs  (mount_point : String; max_size_M : Natural := 0);

   --  Throws exception if unmount attempt was unsuccessful
   procedure unmount (device_or_node : String);

   --  Throws exception if directory was not successfully created
   procedure forge_directory (target : String);

   --  Return the full path of the mount point
   function location (master_mount : String; point : folder) return String;

   --  Query configuration to determine the master mount
   function get_master_mount (base : String) return String;

   --  returns "_XX" where XX is a zero-padded integer (01 .. 32)
   function slave_name (id : builders) return String;

   procedure launch_slave  (id : builders; base : String := LiveBase);
   procedure destroy_slave (id : builders; base : String := LiveBase);

end Replicant;
