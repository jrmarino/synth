--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Text_IO;
with JohnnyText;
with Definitions;   use Definitions;

private with Ada.Characters.Latin_1;
private with Ada.Directories;
private with Parameters;
private with Unix;

package Replicant is

   package JT  renames JohnnyText;
   package TIO renames Ada.Text_IO;

   scenario_unexpected : exception;

   type slave_options is record
      need_procfs    : Boolean := False;
      need_linprocfs : Boolean := False;
   end record;

   type package_abi is record
      calculated_abi      : JT.Text;
      calculated_alt_abi  : JT.Text;
      calc_abi_noarch     : JT.Text;
      calc_alt_abi_noarch : JT.Text;
   end record;

   --  For every single port to be built, the build need to first be created
   --  and then destroyed when the build is complete.
   procedure launch_slave  (id : builders; opts : slave_options);
   procedure destroy_slave (id : builders; opts : slave_options);

   --  This needs to be run as soon as the configuration profile is loaded,
   --  env before the "initialize" function
   procedure set_platform;

   --  This procedure needs to be run once.
   --  It basically sets the operating system "flavor" which affects the
   --  mount command spawning.  It also creates the password database
   procedure initialize (testmode : Boolean; num_cores : cpu_range);

   --  This removes the password database
   procedure finalize;

   --  Returns True if any mounts are detected (used by pilot)
   function synth_mounts_exist return Boolean;

   --  Returns True if any _work/_localbase dirs are detected (used by pilot)
   function disk_workareas_exist return Boolean;

   --  Returns True if the attempt to clear mounts is successful.
   function clear_existing_mounts return Boolean;

   --  Returns True if the attempt to remove the disk work areas is successful
   function clear_existing_workareas return Boolean;

   --  The actual command to build a local repository (Returns True on success)
   function build_repository (id : builders; sign_command : String := "")
                              return Boolean;

   --  Returns all the UNAME_x environment variables
   --  They will be passed to the buildcycle package
   function jail_environment return JT.Text;

   --  On FreeBSD, if "/boot" exists but "/boot/modules" does not, return True
   --  This is a pre-run validity check
   function boot_modules_directory_missing return Boolean;

private

   package PM  renames Parameters;
   package AD  renames Ada.Directories;
   package LAT renames Ada.Characters.Latin_1;

   type mount_mode is (readonly, readwrite);
   type flavors is (unknown, freebsd, dragonfly, netbsd, linux, solaris);
   type folder_operation is (lock, unlock);
   type folder is (bin, sbin, lib, libexec,
                   usr_bin,
                   usr_include,
                   usr_lib,
                   usr_libdata,
                   usr_libexec,
                   usr_sbin,
                   usr_share,
                   usr_lib32, xports, options, packages, distfiles,
                   dev, etc, etc_default, etc_mtree, etc_rcd, home, linux,
                   proc, root, tmp, var, wrkdirs, usr_local, usr_src, ccache,
                   boot);
   subtype subfolder is folder range bin .. usr_share;
   subtype filearch is String (1 .. 11);

   --  home and root need to be set readonly
   reference_base   : constant String := "Base";
   root_bin         : constant String := "/bin";
   root_sbin        : constant String := "/sbin";
   root_usr_bin     : constant String := "/usr/bin";
   root_usr_include : constant String := "/usr/include";
   root_usr_lib     : constant String := "/usr/lib";
   root_usr_lib32   : constant String := "/usr/lib32";
   root_usr_libdata : constant String := "/usr/libdata";
   root_usr_libexec : constant String := "/usr/libexec";
   root_usr_sbin    : constant String := "/usr/sbin";
   root_usr_share   : constant String := "/usr/share";
   root_usr_src     : constant String := "/usr/src";
   root_dev         : constant String := "/dev";
   root_etc         : constant String := "/etc";
   root_etc_default : constant String := "/etc/defaults";
   root_etc_mtree   : constant String := "/etc/mtree";
   root_etc_rcd     : constant String := "/etc/rc.d";
   root_lib         : constant String := "/lib";
   root_tmp         : constant String := "/tmp";
   root_var         : constant String := "/var";
   root_home        : constant String := "/home";
   root_boot        : constant String := "/boot";
   root_kmodules    : constant String := "/boot/modules";
   root_lmodules    : constant String := "/boot/modules.local";
   root_root        : constant String := "/root";
   root_proc        : constant String := "/proc";
   root_linux       : constant String := "/compat/linux";
   root_linproc     : constant String := "/compat/linux/proc";
   root_xports      : constant String := "/xports";
   root_options     : constant String := "/options";
   root_libexec     : constant String := "/libexec";
   root_wrkdirs     : constant String := "/construction";
   root_packages    : constant String := "/packages";
   root_distfiles   : constant String := "/distfiles";
   root_ccache      : constant String := "/ccache";
   root_localbase   : constant String := "/usr/local";
   chroot           : constant String := "/usr/sbin/chroot ";

   platform_type    : flavors   := unknown;
   smp_cores        : cpu_range := cpu_range'First;
   support_locks    : Boolean;
   developer_mode   : Boolean;
   abn_log_ready    : Boolean;
   builder_env      : JT.Text;
   abnormal_log     : TIO.File_Type;

   abnormal_cmd_logname : constant String := "05_abnormal_command_output.log";

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

   --  create /etc/make.conf in slave
   procedure create_make_conf (path_to_etc : String);

   --  create /etc/passwd (and databases) to define system users
   procedure create_passwd      (path_to_etc : String);
   procedure create_base_passwd (path_to_mm  : String);

   --  create /etc/group to define root user
   procedure create_group      (path_to_etc : String);
   procedure create_base_group (path_to_mm  : String);

   --  copy host's /etc/resolv.conf to slave
   procedure copy_resolv_conf (path_to_etc : String);

   --  copy host's /etc/mtree files to slave
   procedure copy_mtree_files (path_to_mtree : String);

   --  copy host's conf defaults
   procedure copy_rc_default (path_to_etc : String);

   --  create minimal /etc/services
   procedure create_etc_services (path_to_etc : String);

   --  create a dummy fstab for linux packages (looks for linprocfs)
   procedure create_etc_fstab (path_to_etc : String);

   --  mount the devices
   procedure mount_devices (path_to_dev : String);
   procedure unmount_devices (path_to_dev : String);

   --  execute ldconfig as last action of slave creation
   procedure execute_ldconfig (id : builders);

   --  Used for per-profile make.conf fragments (if they exist)
   procedure concatenate_makeconf (makeconf_handle : TIO.File_Type;
                                   target_name : String);

   --  Wrapper for rm -rf <directory>
   procedure annihilate_directory_tree (tree : String);

   --  This is only done for FreeBSD.  For DragonFly, it's a null-op
   procedure mount_linprocfs (mount_point : String);

   --  It turns out at least one major port uses procfs (gnustep)
   procedure mount_procfs (path_to_proc : String);
   procedure unmount_procfs (path_to_proc : String);

   --  Used to generic mtree exclusion files
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type);
   procedure write_preinstall_section (mtreefile : TIO.File_Type);
   procedure create_mtree_exc_preconfig (path_to_mm : String);
   procedure create_mtree_exc_preinst (path_to_mm : String);

   --  capture unexpected output while setting up builders (e.g. mount)
   procedure start_abnormal_logging;
   procedure stop_abnormal_logging;

   --  Generic directory copy utility (ordinary files only)
   function copy_directory_contents (src_directory : String;
                                     tgt_directory : String;
                                     pattern       : String) return Boolean;

end Replicant;
