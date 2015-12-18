--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Definitions;   use Definitions;

package Replicant is

   scenario_unexpected : exception;

--  private

   type mount_mode is (readonly, readwrite);
   type nullfs_flavor is (unknown, freebsd, dragonfly);
   type folder is (bin, sbin, lib, libexec,
                   usr_bin,
                   usr_include,
                   usr_lib,
                   usr_libdata,
                   usr_libexec,
                   usr_sbin,
                   usr_share,
                   xports, options, packages, distfiles,
                   dev, etc, home, proc, root, tmp, var, wrkdirs,
                   usr_local, usr_src, ccache);
   subtype subfolder is folder range bin .. usr_share;

   --  home and root need to be set readonly
   reference_base   : constant String := "Base";
   root_bin         : constant String := "/bin";
   root_sbin        : constant String := "/sbin";
   root_usr_bin     : constant String := "/usr/bin";
   root_usr_include : constant String := "/usr/include";
   root_usr_lib     : constant String := "/usr/lib";
   root_usr_libdata : constant String := "/usr/libdata";
   root_usr_libexec : constant String := "/usr/libexec";
   root_usr_sbin    : constant String := "/usr/sbin";
   root_usr_share   : constant String := "/usr/share";
   root_usr_src     : constant String := "/usr/src";
   root_dev         : constant String := "/dev";
   root_etc         : constant String := "/etc";
   root_lib         : constant String := "/lib";
   root_tmp         : constant String := "/tmp";
   root_var         : constant String := "/var";
   root_home        : constant String := "/home";
   root_root        : constant String := "/root";
   root_proc        : constant String := "/proc";
   root_xports      : constant String := "/xports";
   root_options     : constant String := "/options";
   root_libexec     : constant String := "/libexec";
   root_wrkdirs     : constant String := "/construction";
   root_packages    : constant String := "/packages";
   root_distfiles   : constant String := "/distfiles";
   root_ccache      : constant String := "/ccache";
   root_localbase   : constant String := "/usr/local";

   flavor         : nullfs_flavor   := unknown;

   --  This builds the reference "master" to which all the builders mount
   --  This is the first major step in a bulk build
   procedure construct_system;

   --  This deconstructors the reference master and it's the last step.
   procedure take_down_system;

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
   function location (mount_base : String; point : folder) return String;
   function mount_target (point : folder) return String;

   --  Query configuration to determine the master mount
   function get_master_mount return String;
   function get_slave_mount (id : builders) return String;

   --  returns "_XX" where XX is a zero-padded integer (01 .. 32)
   function slave_name (id : builders) return String;

   procedure launch_slave  (id : builders);
   procedure destroy_slave (id : builders);

end Replicant;
