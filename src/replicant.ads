--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with JohnnyText;
with Definitions;   use Definitions;

package Replicant is

   package JT  renames JohnnyText;
   package TIO renames Ada.Text_IO;

   scenario_unexpected : exception;

   --  For every single port to be built, the build need to first be created
   --  and then destroyed when the build is complete.
   procedure launch_slave  (id : builders);
   procedure destroy_slave (id : builders);

   --  This procedure needs to be run once.
   --  It basically sets the operating system "flavor" which affects the
   --  mount command spawning.  It also creates the password database
   procedure initialize;

   --  This removes the password database
   procedure finalize;

   --  Returns True if any mounts are detected (used by pilot)
   function synth_mounts_exist return Boolean;

   --  Returns True if the attempt to clear mounts is successful.
   function clear_existing_mounts return Boolean;

private

   type mount_mode is (readonly, readwrite);
   type nullfs_flavor is (unknown, freebsd, dragonfly);
   type folder_operation is (lock, unlock);
   type folder is (bin, sbin, lib, libexec,
                   usr_bin,
                   usr_include,
                   usr_lib,
                   usr_libdata,
                   usr_libexec,
                   usr_sbin,
                   usr_share,
                   xports, options, packages, distfiles,
                   dev, etc, etc_mtree, home, proc, root, tmp, var, wrkdirs,
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
   root_etc_mtree   : constant String := "/etc/mtree";
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

   --  returns "SLXX" where XX is a zero-padded integer (01 .. 32)
   function slave_name (id : builders) return String;

   --  locks and unlocks folders, even from root
   procedure folder_access (path : String; operation : folder_operation);

   --  self explanatory
   procedure create_symlink (destination, symbolic_link : String);

   --  generic command, throws exception if exit code is not 0
   procedure execute (command : String);
   procedure silent_exec (command : String);
   function  internal_system_command (command : String) return JT.Text;

   --  create slave's /var directory tree.  Path should be an empty directory.
   procedure populate_var_folder (path : String);
   procedure populate_localbase  (path : String);

   --  create /etc/make.conf in slave
   procedure create_make_conf (path_to_etc : String);

   --  create /etc/passwd (and databases) to define system users
   procedure create_passwd (path_to_etc : String);

   --  create /etc/group to define root user
   procedure create_group      (path_to_etc : String);
   procedure create_base_group (path_to_mm  : String);

   --  copy host's /etc/resolv.conf to slave
   procedure copy_resolv_conf (path_to_etc : String);

   --  copy host's /etc/mtree files to slave
   procedure copy_mtree_files (path_to_mtree : String);

   --  mount the devices
   procedure mount_devices (path_to_dev : String);

   --  execute ldconfig as last action of slave creation
   procedure execute_ldconfig (id : builders);

   --  Used for per-profile make.conf fragments (if they exist)
   procedure concatenate_makeconf (makeconf_handle : TIO.File_Type);

end Replicant;
