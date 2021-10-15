--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Replicant.Platform;
with Ada.Containers.Vectors;
with Ada.Exceptions;
with GNAT.OS_Lib;

package body Replicant is

   package AC  renames Ada.Containers;
   package EX  renames Ada.Exceptions;
   package OSL renames GNAT.OS_Lib;
   package PLT renames Replicant.Platform;


   -------------------
   --  mount_point  --
   -------------------
   function location (mount_base : String; point : folder) return String is
   begin
      case point is
         when bin         => return mount_base & root_bin;
         when sbin        => return mount_base & root_sbin;
         when usr_bin     => return mount_base & root_usr_bin;
         when usr_games   => return mount_base & root_usr_games;
         when usr_include => return mount_base & root_usr_include;
         when usr_lib     => return mount_base & root_usr_lib;
         when usr_lib32   => return mount_base & root_usr_lib32;
         when usr_libdata => return mount_base & root_usr_libdata;
         when usr_libexec => return mount_base & root_usr_libexec;
         when usr_local   => return mount_base & root_localbase;
         when usr_sbin    => return mount_base & root_usr_sbin;
         when usr_share   => return mount_base & root_usr_share;
         when usr_src     => return mount_base & root_usr_src;
         when usr_x11r7   => return mount_base & root_X11R7;
         when lib         => return mount_base & root_lib;
         when dev         => return mount_base & root_dev;
         when etc         => return mount_base & root_etc;
         when etc_default => return mount_base & root_etc_default;
         when etc_mtree   => return mount_base & root_etc_mtree;
         when etc_rcd     => return mount_base & root_etc_rcd;
         when tmp         => return mount_base & root_tmp;
         when var         => return mount_base & root_var;
         when home        => return mount_base & root_home;
         when proc        => return mount_base & root_proc;
         when linux       => return mount_base & root_linux;
         when boot        => return mount_base & root_boot;
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


   ------------------------------
   --  procedure set_platform  --
   ------------------------------
   procedure set_platform is
   begin
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         platform_type := freebsd;
      elsif JT.equivalent (PM.configuration.operating_sys, "NetBSD") then
         platform_type := netbsd;
      elsif JT.equivalent (PM.configuration.operating_sys, "Linux") then
        platform_type := linux;
      elsif JT.equivalent (PM.configuration.operating_sys, "SunOS") then
         platform_type := solaris;
      else
         platform_type := dragonfly;
      end if;
   end set_platform;

   ------------------
   --  initialize  --
   ------------------
   procedure initialize (testmode : Boolean; num_cores : cpu_range)
   is
      mm      : constant String := get_master_mount;
      etcmp   : constant String := "/etc/master.passwd";
      command : constant String := "/usr/sbin/pwd_mkdb -p -d ";
   begin
      smp_cores := num_cores;
      developer_mode := testmode;
      support_locks := testmode and then Unix.env_variable_defined ("LOCK");

      start_abnormal_logging;

      if AD.Exists (mm) then
         annihilate_directory_tree (mm);
      end if;

      AD.Create_Path (mm & "/etc");
      case platform_type is
         when dragonfly |
              netbsd    |
              freebsd   => create_base_passwd (mm);
         when linux     => null;  -- master.passwd not used
         when solaris   => null;  -- master.passwd not used
         when unknown   => null;
      end case;
      create_base_group (mm);
      case platform_type is
         when dragonfly | freebsd =>
            execute (command & mm & "/etc " & mm & etcmp);
         when netbsd =>
            execute (command & mm & " " & mm & etcmp);
         when others => null;
      end case;
      PLT.cache_port_variables (mm);
      create_mtree_exc_preinst (mm);
      create_mtree_exc_preconfig (mm);

   end initialize;


   ----------------
   --  finalize  --
   ----------------
   procedure finalize
   is
      mm : constant String := get_master_mount;
   begin
      if AD.Exists (mm) then
         annihilate_directory_tree (mm);
      end if;
      stop_abnormal_logging;
   end finalize;


   --------------------
   --  mount_nullfs  --
   --------------------
   procedure mount_nullfs (target, mount_point : String;
                           mode : mount_mode := readonly)
   is
      cmd_freebsd   : constant String := "/sbin/mount_nullfs";
      cmd_dragonfly : constant String := "/sbin/mount_null";
      cmd_solaris   : constant String := "/usr/sbin/mount -F lofs";
      cmd_linux     : constant String := "/usr/bin/mount --bind";
      command       : JT.Text;
   begin
      if not AD.Exists (mount_point) then
         raise scenario_unexpected with
           "mount point " & mount_point & " does not exist";
      end if;
      if not AD.Exists (target) then
         raise scenario_unexpected with
           "mount target " & target & " does not exist";
      end if;

      case platform_type is
         when freebsd   => command := JT.SUS (cmd_freebsd);
         when dragonfly |
              netbsd    => command := JT.SUS (cmd_dragonfly);
         when solaris   => command := JT.SUS (cmd_solaris);
         when linux     => command := JT.SUS (cmd_linux);
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
      execute (JT.USS (command));
   end mount_nullfs;


   -----------------------
   --  mount_linprocfs  --
   -----------------------
   procedure mount_linprocfs (mount_point : String)
   is
      cmd_freebsd : constant String :=
        "/sbin/mount -t linprocfs linproc " & mount_point;
      cmd_netbsd  : constant String :=
        "/sbin/mount_procfs -o linux procfs " & mount_point;
   begin
      --  DragonFly has lost it's Linux Emulation capability.
      --  FreeBSD has it for both amd64 and i386
      --  We should return if FreeBSD arch is not amd64 or i386, but synth
      --  will not run on any other arches at the moment, so we don't have
      --  to check (and we don't have that information yet anyway)
      case platform_type is
         when freebsd   => execute (cmd_freebsd);
         when netbsd    => execute (cmd_netbsd);
         when dragonfly => null;
         when solaris   => null;
         when linux     => null;
         when unknown   => null;
      end case;
   end mount_linprocfs;


   ------------------------------
   --  set_mount_as_read_only  --
   ------------------------------
   procedure set_mount_as_read_only (mount_point : String)
   is
      --  synth only supports BSD these days
      command : constant String := "/sbin/umount -u -o ro " & mount_point;
   begin
      execute (command);
   exception
      when others =>
         TIO.Put_Line (abnormal_log, "Failed to set mount to read-only mode: " & mount_point);
   end set_mount_as_read_only;


   ---------------
   --  unmount  --
   ---------------
   procedure unmount (device_or_node : String)
   is
      bsd_command : constant String := "/sbin/umount -f " & device_or_node;
      sol_command : constant String := "/usr/sbin/umount " & device_or_node;
      lin_command : constant String := "/usr/bin/umount " & device_or_node;
   begin
      --  failure to unmount causes stderr squawks which messes up curses display
      --  Just log it and ignore for now (Add robustness later)
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    => execute (bsd_command);
         when linux     => execute (lin_command);
         when solaris   => execute (sol_command);
         when unknown   => null;
      end case;
   exception
      when others =>
         TIO.Put_Line (abnormal_log, "Failed to umount " & device_or_node);
         TIO.Put_Line (abnormal_log, "Resetting " & device_or_node & " to read-only mode");
         set_mount_as_read_only (device_or_node);
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


   -------------------
   --  mount_tmpfs  --
   -------------------
   procedure mount_tmpfs  (mount_point : String; max_size_M : Natural := 0)
   is
      cmd_freebsd   : constant String := "/sbin/mount -t tmpfs";
      cmd_dragonfly : constant String := "/sbin/mount_tmpfs";
      cmd_solaris   : constant String := "/sbin/mount -F tmpfs";
      command       : JT.Text;
   begin
      case platform_type is
         when freebsd   |
              netbsd    |
              linux     => command := JT.SUS (cmd_freebsd);
         when dragonfly => command := JT.SUS (cmd_dragonfly);
         when solaris   => command := JT.SUS (cmd_solaris);
         when unknown   =>
            raise scenario_unexpected with
              "Mounting on unknown operating system";
      end case;
      if max_size_M > 0 then
         JT.SU.Append (command, " -o size=" & JT.trim (max_size_M'Img) & "M");
      end if;
      case platform_type is
         when solaris   => JT.SU.Append (command, " swap " & mount_point);
         when freebsd   |
              dragonfly |
              netbsd    |
              linux     => JT.SU.Append (command, " tmpfs " & mount_point);
         when unknown   => null;
      end case;
      execute (JT.USS (command));
   end mount_tmpfs;


   ---------------------
   --  mount_devices  --
   ---------------------
   procedure mount_devices (path_to_dev : String)
   is
      bsd_command : constant String :=
        "/sbin/mount -t devfs devfs " & path_to_dev;
      lin_command : constant String :=
        "/usr/bin/mount --bind /dev " & path_to_dev;
   begin
      case platform_type is
         when dragonfly |
              freebsd   => execute (bsd_command);
         when linux     => execute (lin_command);
         when netbsd    => mount_nullfs (target      => "/dev",
                                         mount_point => path_to_dev);
         when solaris   => null;
         when unknown   => null;
      end case;
   end mount_devices;


   -----------------------
   --  unmount_devices  --
   -----------------------
   procedure unmount_devices (path_to_dev : String) is
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              linux     => unmount (path_to_dev);
         when netbsd    => unmount (path_to_dev);
         when solaris   => null;
         when unknown   => null;
      end case;
   end unmount_devices;


   --------------------
   --  mount_procfs  --
   --------------------
   procedure mount_procfs (path_to_proc : String)
   is
      bsd_command : constant String :=
        "/sbin/mount -t procfs proc " & path_to_proc;
      net_command : constant String :=
        "/sbin/mount_procfs /proc " & path_to_proc;
      lin_command : constant String :=
        "/usr/bin/mount --bind /proc " & path_to_proc;
   begin
      case platform_type is
         when dragonfly |
              freebsd   => execute (bsd_command);
         when netbsd    => execute (net_command);
         when linux     => execute (lin_command);
         when solaris   => null;
         when unknown   => null;
      end case;
   end mount_procfs;


   ---------------------
   --  umount_procfs  --
   ---------------------
   procedure unmount_procfs (path_to_proc : String) is
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    |
              linux     => unmount (path_to_proc);
         when solaris   => null;
         when unknown   => null;
      end case;
   end unmount_procfs;


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


   ---------------------
   --  folder_access  --
   ---------------------
   procedure folder_access (path : String; operation : folder_operation)
   is
      cmd_fallback  : constant String := "/bin/chmod";
      fback_lock    : constant String := " 555 ";
      fback_unlock  : constant String := " 755 ";

      command       : JT.Text := JT.SUS (cmd_fallback);
   begin
      if not AD.Exists (path) then
         return;
      end if;

      case operation is
         when lock   => JT.SU.Append (command, fback_lock & path);
         when unlock => JT.SU.Append (command, fback_unlock & path);
      end case;

      execute (JT.USS (command));
   end folder_access;


   ----------------------
   --  create_symlink  --
   ----------------------
   procedure create_symlink (destination, symbolic_link : String)
   is
      bsd_command : constant String := "/bin/ln -s ";
      lin_command : constant String := "/usr/bin/ln -s ";
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    |
              solaris   => execute (bsd_command & destination & " " & symbolic_link);
         when linux     => execute (lin_command & destination & " " & symbolic_link);
         when unknown   =>
            raise scenario_unexpected with
              "Executing ln on unknown operating system";
      end case;
   end create_symlink;


   ---------------------------
   --  populate_var_folder  --
   ---------------------------
   procedure populate_var_folder (path : String)
   is
      bsd_command : constant String := "/usr/sbin/mtree -p " & path &
        " -f /etc/mtree/BSD.var.dist -deqU";
      net_command : constant String := "/usr/sbin/mtree -p " & path &
        " -f /etc/mtree/special -deqU";
   begin
      case platform_type is
         when dragonfly |
              freebsd   => silent_exec (bsd_command);
         when netbsd    => silent_exec (net_command);
         when linux     => null;
         when solaris   => null;
         when unknown   => null;
      end case;
   end populate_var_folder;


   ---------------
   --  execute  --
   ---------------
   procedure execute (command : String)
   is
      Exit_Status : Integer;
      output : JT.Text := Unix.piped_command (command, Exit_Status);
   begin
      if abn_log_ready and then not JT.IsBlank (output) then
         TIO.Put_Line (abnormal_log, JT.USS (output));
      end if;
      if Exit_Status /= 0 then
         raise scenario_unexpected with
           command & " => failed with code" & Exit_Status'Img;
      end if;
   end execute;


   -------------------
   --  silent_exec  --
   -------------------
   procedure silent_exec (command : String)
   is
      cmd_output : JT.Text;
      success : Boolean := Unix.piped_mute_command (command, cmd_output);
   begin
      if not success then
         if abn_log_ready and then not JT.IsBlank (cmd_output) then
            TIO.Put_Line (abnormal_log, "piped_mute_command failure:");
            TIO.Put_Line (abnormal_log, JT.USS (cmd_output));
         end if;
         raise scenario_unexpected with
           command & " => failed (exit code not 0)";
      end if;
   end silent_exec;


   ------------------------------
   -- internal_system_command  --
   ------------------------------
   function internal_system_command (command : String) return JT.Text
   is
      content : JT.Text;
      status  : Integer;
   begin
      content := Unix.piped_command (command, status);
      if status /= 0 then
         raise scenario_unexpected with "cmd: " & command &
           " (return code =" & status'Img & ")";
      end if;
      return content;
   end internal_system_command;


   -------------------------
   --  create_base_group  --
   -------------------------
   procedure create_base_group (path_to_mm : String)
   is
      subtype sysgroup is String (1 .. 8);
      type groupset is array (1 .. 52) of sysgroup;
      users       : constant groupset :=
        ("wheel   ", "daemon  ", "kmem    ", "sys     ",
         "tty     ", "operator", "mail    ", "bin     ",
         "news    ", "man     ", "games   ", "staff   ",
         "sshd    ", "smmsp   ", "mailnull", "guest   ",
         "bind    ", "proxy   ", "authpf  ", "_pflogd ",
         "unbound ", "ftp     ", "video   ", "hast    ",
         "uucp    ", "xten    ", "dialer  ", "network ",
         "_sdpd   ", "_dhcp   ", "www     ", "vknet   ",
         "nogroup ", "nobody  ",
         --  Unique to NetBSD
         "wsrc    ", "maildrop", "postfix ", "named   ",
         "ntpd    ", "_rwhod  ", "_proxy  ", "_timedc ",
         "_httpd  ", "_mdnsd  ", "_tests  ", "_tcpdump",
         "_tss    ", "_gpio   ", "_rtadvd ", "_unbound",
         "utmp    ", "users   ");
      group       : TIO.File_Type;
      live_file   : TIO.File_Type;
      keepit      : Boolean;
      target      : constant String  := path_to_mm & "/group";
      live_origin : constant String  := "/etc/group";
   begin
      TIO.Open   (File => live_file, Mode => TIO.In_File, Name => live_origin);
      TIO.Create (File => group, Mode => TIO.Out_File, Name => target);
      while not TIO.End_Of_File (live_file) loop
         keepit := False;
         declare
            line : String := TIO.Get_Line (live_file);
         begin
            for grpindex in groupset'Range loop
               declare
                  grpcolon : String := JT.trim (users (grpindex)) & ":";
               begin
                  if grpcolon'Length <= line'Length then
                     if grpcolon = line (1 .. grpcolon'Last) then
                        keepit := True;
                        exit;
                     end if;
                  end if;
               end;
            end loop;
            if keepit then
               TIO.Put_Line (group, line);
            end if;
         end;
      end loop;
      TIO.Close (live_file);
      TIO.Close (group);
   end create_base_group;


   --------------------------
   --  create_base_passwd  --
   --------------------------
   procedure create_base_passwd (path_to_mm  : String)
   is
      subtype syspasswd is String (1 .. 10);
      type passwdset is array (1 .. 41) of syspasswd;
      users       : constant passwdset :=
        ("root      ", "toor      ", "daemon    ", "operator  ",
         "bin       ", "tty       ", "kmem      ", "mail      ",
         "games     ", "news      ", "man       ", "sshd      ",
         "smmsp     ", "mailnull  ", "bind      ", "unbound   ",
         "proxy     ", "_pflogd   ", "_dhcp     ", "uucp      ",
         "xten      ", "pop       ", "auditdistd", "_sdpd     ",
         "www       ", "_ypldap   ", "hast      ", "nobody    ",
         --  Unique to NetBSD
         "postfix   ", "named     ", "ntpd      ", "_rwhod    ",
         "_proxy    ", "_timedc   ", "_httpd    ", "_mdnsd    ",
         "_tests    ", "_tcpdump  ", "_tss      ", "_rtadvd   ",
         "_unbound  ");
      masterpwd   : TIO.File_Type;
      live_file   : TIO.File_Type;
      keepit      : Boolean;
      live_origin : constant String  := "/etc/master.passwd";
      target      : constant String  := path_to_mm & live_origin;
   begin
      TIO.Open   (File => live_file, Mode => TIO.In_File, Name => live_origin);
      TIO.Create (File => masterpwd, Mode => TIO.Out_File, Name => target);
      while not TIO.End_Of_File (live_file) loop
         keepit := False;
         declare
            line : String := TIO.Get_Line (live_file);
         begin
            for pwdindex in passwdset'Range loop
               declare
                  pwdcolon : String := JT.trim (users (pwdindex)) & ":";
               begin
                  if pwdcolon'Length <= line'Length then
                     if pwdcolon = line (1 .. pwdcolon'Last) then
                        keepit := True;
                        exit;
                     end if;
                  end if;
               end;
            end loop;
            if keepit then
               TIO.Put_Line (masterpwd, line);
            end if;
         end;
      end loop;
      TIO.Close (live_file);
      TIO.Close (masterpwd);
   end create_base_passwd;


   --------------------
   --  create_group  --
   --------------------
   procedure create_group (path_to_etc : String)
   is
      mm    : constant String := get_master_mount;
      group : constant String := "/group";
   begin
      AD.Copy_File (Source_Name => mm & group,
                    Target_Name => path_to_etc & group);
   end create_group;


   ---------------------
   --  create_passwd  --
   ---------------------
   procedure create_passwd (path_to_etc : String)
   is
      mmetc  : constant String := get_master_mount & "/etc";
      maspwd : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
   begin
      AD.Copy_File (Source_Name => mmetc & passwd,
                    Target_Name => path_to_etc & passwd);
      AD.Copy_File (Source_Name => mmetc & maspwd,
                    Target_Name => path_to_etc & maspwd);
      AD.Copy_File (Source_Name => mmetc & spwd,
                    Target_Name => path_to_etc & spwd);
      AD.Copy_File (Source_Name => mmetc & pwd,
                    Target_Name => path_to_etc & pwd);
   end create_passwd;


   ------------------------
   --  copy_mtree_files  --
   ------------------------
   procedure copy_mtree_files (path_to_mtree : String)
   is
      mtree : constant String := "/etc/mtree";
      root  : constant String := "/BSD.root.dist";
      usr   : constant String := "/BSD.usr.dist";
      var   : constant String := "/BSD.var.dist";
      spec  : constant String := "/special";
   begin
      case platform_type is
         when dragonfly | freebsd =>
            AD.Copy_File (Source_Name => mtree & root,
                          Target_Name => path_to_mtree & root);
            AD.Copy_File (Source_Name => mtree & usr,
                          Target_Name => path_to_mtree & usr);
            AD.Copy_File (Source_Name => mtree & var,
                          Target_Name => path_to_mtree & var);
         when netbsd =>
            AD.Copy_File (Source_Name => mtree & spec,
                          Target_Name => path_to_mtree & spec);
         when solaris => null;
         when linux   => null;
         when unknown => null;
      end case;
   end copy_mtree_files;


   ------------------------
   --  create_make_conf  --
   ------------------------
   procedure create_make_conf (path_to_etc : String; skip_cwrappers : Boolean)
   is
      makeconf  : TIO.File_Type;
      profilemc : constant String := PM.synth_confdir & "/" &
                  JT.USS (PM.configuration.profile) & "-make.conf";
      varcache  : constant String := get_master_mount & "/varcache.conf";
      profile   : constant String := JT.USS (PM.configuration.profile);
      mjnum     : constant Integer := Integer (PM.configuration.jobs_limit);
   begin
      case software_framework is
         when ports_collection =>
            TIO.Create (File => makeconf,
                        Mode => TIO.Out_File,
                        Name => path_to_etc & "/make.conf");

            TIO.Put_Line
              (makeconf,
                 "SYNTHPROFILE=" & profile & LAT.LF &
                 "USE_PACKAGE_DEPENDS_ONLY=yes" & LAT.LF &
                 "PACKAGE_BUILDING=yes" & LAT.LF &
                 "BATCH=yes" & LAT.LF &
                 "PKG_CREATE_VERBOSE=yes" & LAT.LF &
                 "PORTSDIR=/xports" & LAT.LF &
                 "DISTDIR=/distfiles" & LAT.LF &
                 "WRKDIRPREFIX=/construction" & LAT.LF &
                 "PORT_DBDIR=/options" & LAT.LF &
                 "PACKAGES=/packages" & LAT.LF &
                 "MAKE_JOBS_NUMBER_LIMIT=" & JT.int2str (mjnum));

            if developer_mode then
               TIO.Put_Line (makeconf, "DEVELOPER=1");
            end if;
            if AD.Exists (JT.USS (PM.configuration.dir_ccache)) then
               TIO.Put_Line (makeconf, "WITH_CCACHE_BUILD=yes");
               TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
            end if;
         when pkgsrc =>
            TIO.Create (File => makeconf,
                        Mode => TIO.Out_File,
                        Name => path_to_etc & "/mk.conf");

            --  Note there is no equivalent for PORT_DBDIR
            --  Custom options must be set in <profile>-make.conf
            TIO.Put_Line
              (makeconf,
                 "SYNTHPROFILE=" & profile & LAT.LF &
                 "USE_PACKAGE_DEPENDS_ONLY=yes" & LAT.LF &
                 "PACKAGE_BUILDING=yes" & LAT.LF &
                 "ALLOW_VULNERABLE_PACKAGES=yes" & LAT.LF &
                 "PKG_CREATE_VERBOSE=yes" & LAT.LF &
                 "PKG_FORMAT=pkgng" & LAT.LF &
                 "PKGSRCDIR=/xports" & LAT.LF &
                 "DISTDIR=/distfiles" & LAT.LF &
                 "WRKOBJDIR=/construction" & LAT.LF &
                 "PACKAGES=/packages" & LAT.LF &
                 "MAKE_JOBS=" & JT.int2str (mjnum));

            if skip_cwrappers then
               TIO.Put_Line (makeconf, "USE_CWRAPPERS=no");
            end if;
            if developer_mode then
               TIO.Put_Line (makeconf, "PKG_DEVELOPER=yes");
            end if;
            if AD.Exists (JT.USS (PM.configuration.dir_ccache)) then
               TIO.Put_Line (makeconf, "PKGSRC_COMPILER=ccache gcc");
               TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
            end if;
      end case;

      concatenate_makeconf (makeconf, profilemc);
      concatenate_makeconf (makeconf, varcache);

      TIO.Close (makeconf);

   end create_make_conf;


   ------------------------
   --  copy_resolv_conf  --
   ------------------------
   procedure copy_resolv_conf (path_to_etc : String)
   is
      original : constant String := "/etc/resolv.conf";
   begin
      if not AD.Exists (original) then
         return;
      end if;
      AD.Copy_File (Source_Name => original,
                    Target_Name => path_to_etc & "/resolv.conf");
   end copy_resolv_conf;


   -----------------------
   --  copy_rc_default  --
   -----------------------
   procedure copy_rc_default (path_to_etc : String)
   is
      rc_default : constant String := "/defaults/rc.conf";
      etc_rcconf : constant String := "/etc" & rc_default;
   begin
      if not AD.Exists (etc_rcconf) then
         return;
      end if;
      AD.Copy_File (Source_Name => etc_rcconf,
                    Target_Name => path_to_etc & rc_default);
   end copy_rc_default;


   -------------------
   --  copy_etc_rc  --
   -------------------
   procedure copy_etc_rcsubr (path_to_etc : String)
   is
      rcsubr     : constant String := "/rc.subr";
      etc_rcsubr : constant String := "/etc" & rcsubr;
   begin
      if not AD.Exists (etc_rcsubr) then
         return;
      end if;
      AD.Copy_File (Source_Name => etc_rcsubr,
                    Target_Name => path_to_etc & rcsubr);
   end copy_etc_rcsubr;


   ---------------------
   --  copy_ldconfig  --
   ---------------------
   procedure copy_ldconfig (path_to_etc : String)
   is
      ldconfig     : constant String := "/rc.d/ldconfig";
      etc_ldconfig : constant String := "/etc" & ldconfig;
   begin
      if not AD.Exists (etc_ldconfig) then
         return;
      end if;
      AD.Copy_File (Source_Name => etc_ldconfig,
                    Target_Name => path_to_etc & ldconfig,
                    Form => "mode=copy,preserve=all_attributes");
   end copy_ldconfig;


   ---------------------------
   --  create_etc_services  --
   ---------------------------
   procedure create_etc_services (path_to_etc : String)
   is
      svcfile : TIO.File_Type;
   begin
      TIO.Create (File => svcfile,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/services");
      TIO.Put_Line (svcfile,
                      "ftp    21/tcp" & LAT.LF &
                      "ftp    21/udp" & LAT.LF &
                      "ssh    22/tcp" & LAT.LF &
                      "ssh    22/udp" & LAT.LF &
                      "http   80/tcp" & LAT.LF &
                      "http   80/udp" & LAT.LF &
                      "https 443/tcp" & LAT.LF &
                      "https 443/udp" & LAT.LF);
      TIO.Close (svcfile);
   end create_etc_services;


   ------------------------
   --  create_etc_fstab  --
   ------------------------
   procedure create_etc_fstab (path_to_etc : String)
   is
      fstab : TIO.File_Type;
   begin
      TIO.Create (File => fstab,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/fstab");
      case platform_type is
         when dragonfly | freebsd =>
            TIO.Put_Line (fstab, "linproc /usr/compat/proc linprocfs rw 0 0");
         when netbsd =>
            TIO.Put_Line (fstab, "procfs /emul/linux/proc procfs ro,linux 0 0");
         when linux | solaris => null;
         when unknown => null;
      end case;
      TIO.Close (fstab);
   end create_etc_fstab;


   -------------------------
   --  create_etc_shells  --
   -------------------------
   procedure create_etc_shells (path_to_etc : String)
   is
      shells : TIO.File_Type;
   begin
      TIO.Create (File => shells,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/shells");
      case platform_type is
         when dragonfly | freebsd =>
            TIO.Put_Line (shells,
                            "/bin/sh" & LAT.LF &
                            "/bin/csh" & LAT.LF &
                            "/bin/tcsh" & LAT.LF);
         when netbsd =>
            TIO.Put_Line (shells,
                            "/bin/sh" & LAT.LF &
                            "/bin/csh" & LAT.LF &
                            "/bin/ksh" & LAT.LF);
         when linux =>
            TIO.Put_Line (shells,
                            "/bin/sh" & LAT.LF &
                            "/bin/bash" & LAT.LF &
                            "/sbin/nologin" & LAT.LF &
                            "/usr/bin/sh" & LAT.LF &
                            "/usr/bin/bash" & LAT.LF &
                            "/usr/sbin/nologin" & LAT.LF);
         when solaris =>
            TIO.Put_Line (shells,
                            "/bin/bash" & LAT.LF &
                            "/bin/csh" & LAT.LF &
                            "/bin/ksh" & LAT.LF &
                            "/bin/ksh93" & LAT.LF &
                            "/bin/sh" & LAT.LF &
                            "/bin/tcsh" & LAT.LF &
                            "/bin/zsh" & LAT.LF &
                            "/sbin/sh" & LAT.LF &
                            "/usr/bin/bash" & LAT.LF &
                            "/usr/bin/csh" & LAT.LF &
                            "/usr/bin/ksh" & LAT.LF &
                            "/usr/bin/ksh93" & LAT.LF &
                            "/usr/bin/sh" & LAT.LF &
                            "/usr/bin/tcsh" & LAT.LF &
                            "/usr/bin/zsh");
         when unknown => null;
      end case;
      TIO.Close (shells);
   end create_etc_shells;


   ------------------------
   --  execute_ldconfig  --
   ------------------------
   procedure execute_ldconfig (id : builders)
   is
      smount      : constant String := get_slave_mount (id);
      bsd_command : constant String := chroot & smount &
                                       " /sbin/ldconfig -m /lib /usr/lib";
      lin_command : constant String := chroot & smount &
                                       " /usr/sbin/ldconfig /lib /usr/lib";
   begin
      case platform_type is
         when dragonfly | freebsd => execute (bsd_command);
         when linux => execute (lin_command);
         when netbsd | solaris => null;
         when unknown => null;
      end case;
   end execute_ldconfig;


   -------------------------------
   --  copy_directory_contents  --
   -------------------------------
   function copy_directory_contents (src_directory : String;
                                     tgt_directory : String;
                                     pattern       : String) return Boolean
   is
      Search  : AD.Search_Type;
      Dir_Ent : AD.Directory_Entry_Type;
      Filter  : constant AD.Filter_Type := (AD.Ordinary_File => True,
                                            AD.Special_File  => False,
                                            AD.Directory     => False);
   begin
      if not AD.Exists (src_directory) then
         return False;
      end if;
      AD.Create_Path (tgt_directory);
      AD.Start_Search (Search    => Search,
                       Directory => src_directory,
                       Filter    => Filter,
                       Pattern   => pattern);
      while AD.More_Entries (Search => Search) loop
         AD.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         AD.Copy_File
           (Source_Name => src_directory & "/" & AD.Simple_Name (Dir_Ent),
            Target_Name => tgt_directory & "/" & AD.Simple_Name (Dir_Ent));
      end loop;
      return True;
   exception
      when others => return False;
   end copy_directory_contents;


   ------------------------
   --  build_repository  --
   ------------------------
   function build_repository (id : builders; sign_command : String := "")
                              return Boolean
   is
      smount  : constant String := get_slave_mount (id);
      command : constant String := chroot & smount & " " &
                root_localbase & "/sbin/pkg-static repo /packages";
      sc_cmd  : constant String := host_pkg8 & " repo " & smount &
                "/packages signing_command: ";
      key_loc : constant String := "/etc/repo.key";
      use_key : constant Boolean := AD.Exists (smount & key_loc);
      use_cmd : constant Boolean := not JT.IsBlank (sign_command);
   begin
      if not PLT.standalone_pkg8_install (id) then
         TIO.Put_Line ("Failed to install pkg-static in builder" & id'Img);
         return False;
      end if;
      if use_key then
         silent_exec (command & " " & key_loc);
      elsif use_cmd then
         silent_exec (sc_cmd & sign_command);
      else
         silent_exec (command);
      end if;
      return True;
   exception
      when quepaso : others =>
         TIO.Put_Line (EX.Exception_Message (quepaso));
         return False;
   end build_repository;


   ---------------------------------
   --  annihilate_directory_tree  --
   ---------------------------------
   procedure annihilate_directory_tree (tree : String)
   is
      command : constant String := "/bin/rm -rf " & tree;
   begin
      silent_exec (command);
   exception
      when others => null;
   end annihilate_directory_tree;


   --------------------
   --  launch_slave  --
   --------------------
   procedure launch_slave  (id : builders; opts : slave_options)
   is
      function clean_mount_point (point : folder) return String;
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
      slave_linux  : constant String := slave_base & "_linux";
      dir_system   : constant String := JT.USS (PM.configuration.dir_system);
      live_system  : constant Boolean := (dir_system = "/");

      function clean_mount_point (point : folder) return String is
      begin
         if live_system then
            return location ("", point);
         else
            return location (dir_system, point);
         end if;
      end clean_mount_point;
   begin
      forge_directory (slave_base);
      mount_tmpfs (slave_base);

      for mnt in folder'Range loop
         forge_directory (location (slave_base, mnt));
      end loop;

      for mnt in subfolder'Range loop
         mount_nullfs (target      => clean_mount_point (mnt),
                       mount_point => location (slave_base, mnt));
      end loop;

      folder_access (location (slave_base, home), lock);
      folder_access (location (slave_base, root), lock);

      mount_nullfs (mount_target (xports),    location (slave_base, xports));
      mount_nullfs (mount_target (options),   location (slave_base, options));
      mount_nullfs (mount_target (packages),  location (slave_base, packages),
                    mode => readwrite);
      mount_nullfs (mount_target (distfiles), location (slave_base, distfiles),
                    mode => readwrite);

      if PM.configuration.tmpfs_workdir then
         --  The mount cap of 16Gb was removed; rust was the latest port to bust it
         mount_tmpfs (location (slave_base, wrkdirs));
      else
         forge_directory (slave_work);
         mount_nullfs (slave_work, location (slave_base, wrkdirs), readwrite);
      end if;

      if not support_locks and then PM.configuration.tmpfs_localbase then
         mount_tmpfs (slave_base & root_localbase, 12 * 1024);
      else
         forge_directory (slave_local);
         mount_nullfs (slave_local, slave_base & root_localbase, readwrite);
      end if;

      if opts.need_procfs then
         mount_procfs (path_to_proc => location (slave_base, proc));
      end if;

      --  special platform handling
      case platform_type is
         when dragonfly =>
            declare
               bootdir : String := clean_mount_point (boot);
            begin
               if AD.Exists (bootdir) then
                  mount_nullfs (target      => bootdir,
                                mount_point => location (slave_base, boot));
                  mount_tmpfs (slave_base & root_lmodules, 100);
               end if;
            end;
            declare
               games : String := clean_mount_point (usr_games);
            begin
               if AD.Exists (games) then
                  mount_nullfs
                    (target      => games,
                     mount_point => location (slave_base, usr_games));
               end if;
            end;
         when freebsd =>
            if opts.need_linprocfs then
               if PM.configuration.tmpfs_localbase then
                  mount_tmpfs (slave_base & root_linux, 12 * 1024);
               else
                  forge_directory (slave_linux);
                  mount_nullfs (target      => slave_linux,
                                mount_point => slave_base & root_linux,
                                mode        => readwrite);
               end if;
               forge_directory (slave_base & root_linproc);
               mount_linprocfs (mount_point => slave_base & root_linproc);
            end if;
            declare
               lib32 : String := clean_mount_point (usr_lib32);
            begin
               if AD.Exists (lib32) then
                  mount_nullfs
                    (target      => lib32,
                     mount_point => location (slave_base, usr_lib32));
               end if;
            end;
            declare
               bootdir : String := clean_mount_point (boot);
            begin
               if AD.Exists (bootdir) then
                  mount_nullfs (target      => bootdir,
                                mount_point => location (slave_base, boot));
                  mount_tmpfs (slave_base & root_boot_fw, 100);
                  mount_tmpfs (slave_base & root_kmodules, 100);
               end if;
            end;
            declare
               games : String := clean_mount_point (usr_games);
            begin
               if AD.Exists (games) then
                  mount_nullfs
                    (target      => games,
                     mount_point => location (slave_base, usr_games));
               end if;
            end;
         when netbsd  =>
            declare
               x11_dir : String := clean_mount_point (usr_x11r7);
            begin
               if AD.Exists (x11_dir) then
                  mount_nullfs
                    (target      => x11_dir,
                     mount_point => location (slave_base, usr_x11r7));
               end if;
            end;
         when linux   => null;  -- for now
         when solaris => null;  -- for now
         when unknown => null;
      end case;

      declare
         srcdir : String := clean_mount_point (usr_src);
      begin
         if AD.Exists (srcdir) then
            mount_nullfs (srcdir, location (slave_base, usr_src));
            if AD.Exists (srcdir & "/sys") then
               create_symlink (destination   => "usr/src/sys",
                               symbolic_link => slave_base & "/sys");
            end if;
         end if;
      end;

      if AD.Exists (mount_target (ccache)) then
         mount_nullfs (mount_target (ccache), location (slave_base, ccache),
                       mode => readwrite);
      end if;

      mount_devices (location (slave_base, dev));

      populate_var_folder (location (slave_base, var));
      copy_mtree_files    (location (slave_base, etc_mtree));
      copy_rc_default     (location (slave_base, etc));
      copy_resolv_conf    (location (slave_base, etc));
      create_make_conf    (location (slave_base, etc), opts.skip_cwrappers);
      create_passwd       (location (slave_base, etc));
      create_group        (location (slave_base, etc));
      create_etc_services (location (slave_base, etc));
      create_etc_shells   (location (slave_base, etc));
      create_etc_fstab    (location (slave_base, etc));
      copy_etc_rcsubr     (location (slave_base, etc));
      copy_ldconfig       (location (slave_base, etc));

      execute_ldconfig (id);

   exception
      when hiccup : others => EX.Reraise_Occurrence (hiccup);
   end launch_slave;


   ---------------------
   --  destroy_slave  --
   ---------------------
   procedure destroy_slave (id : builders; opts : slave_options)
   is
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
      slave_linux  : constant String := slave_base & "_linux";
      dir_system   : constant String := JT.USS (PM.configuration.dir_system);
   begin
      unmount (slave_base & root_localbase);
      if support_locks or else not PM.configuration.tmpfs_localbase then
         --  We can't use AD.Delete_Tree because it skips directories
         --  starting with "." (pretty useless then)
         annihilate_directory_tree (slave_local);
      end if;

      unmount (location (slave_base, wrkdirs));
      if not PM.configuration.tmpfs_workdir then
         annihilate_directory_tree (slave_work);
      end if;

      if AD.Exists (location (dir_system, usr_src)) then
         unmount (location (slave_base, usr_src));
      end if;

      if AD.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache));
      end if;

      case platform_type is
         when dragonfly =>
            if AD.Exists (location (dir_system, boot)) then
               unmount (slave_base & root_lmodules);
               unmount (location (slave_base, boot));
            end if;
            if AD.Exists (location (dir_system, usr_games)) then
               unmount (location (slave_base, usr_games));
            end if;
         when freebsd =>
            if opts.need_linprocfs then
               unmount (slave_base & root_linproc);
               unmount (slave_base & root_linux);
               if not PM.configuration.tmpfs_localbase then
                  annihilate_directory_tree (slave_linux);
               end if;
            end if;
            if AD.Exists (location (dir_system, usr_lib32)) then
               unmount (location (slave_base, usr_lib32));
            end if;
            if AD.Exists (location (dir_system, boot)) then
               unmount (slave_base & root_kmodules);
               unmount (slave_base & root_boot_fw);
               unmount (location (slave_base, boot));
            end if;
            if AD.Exists (location (dir_system, usr_games)) then
               unmount (location (slave_base, usr_games));
            end if;
         when netbsd  =>
            if AD.Exists (location (dir_system, usr_x11r7)) then
               unmount (location (slave_base, usr_x11r7));
            end if;
         when linux   => null;
         when solaris => null;
         when unknown => null;
      end case;

      if opts.need_procfs then
         unmount_procfs (location (slave_base, proc));
      end if;

      unmount_devices (location (slave_base, dev));

      unmount (location (slave_base, xports));
      unmount (location (slave_base, options));
      unmount (location (slave_base, packages));
      unmount (location (slave_base, distfiles));

      for mnt in subfolder'Range loop
         unmount (location (slave_base, mnt));
      end loop;

      unmount (slave_base);
      annihilate_directory_tree (slave_base);

   exception
      when hiccup : others => EX.Reraise_Occurrence (hiccup);
   end destroy_slave;


   --------------------------
   --  synth_mounts_exist  --
   --------------------------
   function synth_mounts_exist return Boolean
   is
      buildbase   : constant String := JT.USS (PM.configuration.dir_buildbase);
      comres      : JT.Text;
      topline     : JT.Text;
      crlen1      : Natural;
      crlen2      : Natural;
   begin
      comres := internal_system_command (PLT.df_command);
      crlen1 := JT.SU.Length (comres);
      loop
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if JT.contains (topline, buildbase) then
            return True;
         end if;
      end loop;
      return False;
   exception
      when others =>
         return True;
   end synth_mounts_exist;


   -----------------------------
   --  clear_existing_mounts  --
   -----------------------------
   function clear_existing_mounts return Boolean
   is
      package crate is new AC.Vectors (Index_Type   => Positive,
                                       Element_Type => JT.Text,
                                       "="          => JT.SU."=");
      procedure annihilate (cursor : crate.Cursor);

      buildbase : constant String := JT.USS (PM.configuration.dir_buildbase);
      comres    : JT.Text;
      topline   : JT.Text;
      crlen1    : Natural;
      crlen2    : Natural;
      mindex    : Natural;
      mlength   : Natural;
      mpoints   : crate.Vector;

      procedure annihilate (cursor : crate.Cursor)
      is
         mountpoint : constant String := JT.USS (crate.Element (cursor));
      begin
         unmount (mountpoint);
         if AD.Exists (mountpoint) then
            AD.Delete_Directory (mountpoint);
         end if;
      exception
         when others => null;
      end annihilate;
   begin
      comres := internal_system_command (PLT.df_command);
      crlen1 := JT.SU.Length (comres);
      loop
         JT.nextline (lineblock => comres, firstline => topline);
         crlen2 := JT.SU.Length (comres);
         exit when crlen1 = crlen2;
         crlen1 := crlen2;
         if JT.contains (topline, buildbase) then
            mindex  := JT.SU.Index (topline, buildbase);
            mlength := JT.SU.Length (topline);
            mpoints.Append (JT.SUS (JT.SU.Slice (topline, mindex, mlength)));
         end if;
      end loop;

      mpoints.Reverse_Iterate (Process => annihilate'Access);

      if synth_mounts_exist then
         return False;
      end if;

      --  No need to remove empty dirs, the upcoming run will do that.
      return True;
   end clear_existing_mounts;


   ----------------------------
   --  disk_workareas_exist  --
   ----------------------------
   function disk_workareas_exist return Boolean
   is
      Search    : AD.Search_Type;
      buildbase : constant String := JT.USS (PM.configuration.dir_buildbase);
      result    : Boolean := False;
   begin
      if not AD.Exists (buildbase) then
         return False;
      end if;
      AD.Start_Search (Search    => Search,
                       Directory => buildbase,
                       Filter    => (AD.Directory => True, others => False),
                       Pattern   => "SL*_*");

      result := AD.More_Entries (Search => Search);
      return result;
   end disk_workareas_exist;


   --------------------------------
   --  clear_existing_workareas  --
   --------------------------------
   function clear_existing_workareas return Boolean
   is
      Search    : AD.Search_Type;
      Dir_Ent   : AD.Directory_Entry_Type;
      buildbase : constant String := JT.USS (PM.configuration.dir_buildbase);
   begin
      AD.Start_Search (Search    => Search,
                       Directory => buildbase,
                       Filter    => (AD.Directory => True, others => False),
                       Pattern   => "SL*_*");
      while AD.More_Entries (Search => Search) loop
         AD.Get_Next_Entry (Search => Search, Directory_Entry => Dir_Ent);
         declare
            target : constant String := buildbase & "/" &
                                        AD.Simple_Name (Dir_Ent);
         begin
            annihilate_directory_tree (target);
         end;
      end loop;
      return True;
   exception
      when others => return False;
   end clear_existing_workareas;


   ----------------------------
   --  concatenate_makeconf  --
   ----------------------------
   procedure concatenate_makeconf (makeconf_handle : TIO.File_Type;
                                   target_name : String)
   is
      fragment : TIO.File_Type;
   begin
      if AD.Exists (target_name) then
         TIO.Open (File => fragment, Mode => TIO.In_File, Name => target_name);
         while not TIO.End_Of_File (fragment) loop
            declare
               Line : String := TIO.Get_Line (fragment);
            begin
               TIO.Put_Line (makeconf_handle, Line);
            end;
         end loop;
         TIO.Close (fragment);
      end if;
   exception
      when others => null;
   end concatenate_makeconf;




   ---------------------------------------
   --  write_common_mtree_exclude_base  --
   ---------------------------------------
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type) is
   begin
      TIO.Put_Line
        (mtreefile,
           "./bin" & LAT.LF
         & "./boot" & LAT.LF
         & "./ccache" & LAT.LF
         & "./compat/linux/proc" & LAT.LF
         & "./construction" & LAT.LF
         & "./dev" & LAT.LF
         & "./distfiles" & LAT.LF
         & "./lib" & LAT.LF
         & "./libexec" & LAT.LF
         & "./home" & LAT.LF
         & "./options" & LAT.LF
         & "./packages" & LAT.LF
         & "./proc" & LAT.LF
         & "./root" & LAT.LF
         & "./sbin" & LAT.LF
         & "./tmp" & LAT.LF
         & "./usr/bin" & LAT.LF
         & "./usr/include" & LAT.LF
         & "./usr/lib" & LAT.LF
         & "./usr/lib32" & LAT.LF
         & "./usr/libdata" & LAT.LF
         & "./usr/libexec" & LAT.LF
         & "./usr/sbin" & LAT.LF
         & "./usr/share" & LAT.LF
         & "./usr/src" & LAT.LF
         & "./var/db/fontconfig" & LAT.LF
         & "./var/run" & LAT.LF
         & "./var/tmp" & LAT.LF
         & "./xports"
        );
   end write_common_mtree_exclude_base;


   --------------------------------
   --  write_preinstall_section  --
   --------------------------------
   procedure write_preinstall_section (mtreefile : TIO.File_Type) is
   begin
      case software_framework is
         when ports_collection =>
            TIO.Put_Line
              (mtreefile,
                 "./etc/group" & LAT.LF
               & "./etc/make.conf" & LAT.LF
               & "./etc/make.conf.bak" & LAT.LF
               & "./etc/make.nxb.conf" & LAT.LF
               & "./etc/master.passwd" & LAT.LF
               & "./etc/passwd" & LAT.LF
               & "./etc/pwd.db" & LAT.LF
               & "./etc/shells" & LAT.LF
               & "./etc/spwd.db" & LAT.LF
               & "./var/db" & LAT.LF
               & "./var/log" & LAT.LF
               & "./var/mail" & LAT.LF
               & "./var/spool" & LAT.LF
               & "./var/tmp" & LAT.LF
               & "./usr/local/etc/gconf/gconf.xml.defaults/%gconf-tree*.xml" & LAT.LF
               & "./usr/local/lib/gio/modules/giomodule.cache" & LAT.LF
               & "./usr/local/info/dir" & LAT.LF
               & "./usr/local/info" & LAT.LF
               & "./usr/local/*/info/dir" & LAT.LF
               & "./usr/local/*/info" & LAT.LF
               & "./usr/local/*/ls-R" & LAT.LF
               & "./usr/local/share/octave/octave_packages" & LAT.LF
               & "./usr/local/share/xml/catalog.ports"
              );
         when pkgsrc =>
            TIO.Put_Line
              (mtreefile,
                 "./etc/group" & LAT.LF
               & "./etc/mk.conf" & LAT.LF
               & "./etc/master.passwd" & LAT.LF
               & "./etc/passwd" & LAT.LF
               & "./etc/pwd.db" & LAT.LF
               & "./etc/shells" & LAT.LF
               & "./etc/spwd.db" & LAT.LF
               & "./var/db" & LAT.LF
               & "./var/log" & LAT.LF
               & "./var/mail" & LAT.LF
               & "./var/spool" & LAT.LF
               & "./var/tmp" & LAT.LF
               & "./usr/pkg/etc/gconf/gconf.xml.defaults/%gconf-tree*.xml" & LAT.LF
               & "./usr/pkg/lib/gio/modules/giomodule.cache" & LAT.LF
               & "./usr/pkg/info/dir" & LAT.LF
               & "./usr/pkg/info" & LAT.LF
               & "./usr/pkg/*/info/dir" & LAT.LF
               & "./usr/pkg/*/info" & LAT.LF
               & "./usr/pkg/*/ls-R" & LAT.LF
               & "./usr/pkg/share/xml/catalog.ports"
              );
      end case;
   end write_preinstall_section;



   --------------------------------
   --  create_mtree_exc_preinst  --
   --------------------------------
   procedure create_mtree_exc_preinst (path_to_mm : String)
   is
      mtreefile : TIO.File_Type;
      filename  : constant String := path_to_mm & "/mtree.prestage.exclude";
   begin
      TIO.Create (File => mtreefile, Mode => TIO.Out_File, Name => filename);
      write_common_mtree_exclude_base (mtreefile);
      write_preinstall_section (mtreefile);
      TIO.Close (mtreefile);
   end create_mtree_exc_preinst;


   ----------------------------------
   --  create_mtree_exc_preconfig  --
   ----------------------------------
   procedure create_mtree_exc_preconfig (path_to_mm : String)
   is
      mtreefile : TIO.File_Type;
      filename  : constant String := path_to_mm & "/mtree.preconfig.exclude";
   begin
      TIO.Create (File => mtreefile, Mode => TIO.Out_File, Name => filename);
      write_common_mtree_exclude_base (mtreefile);
      TIO.Close (mtreefile);
   end create_mtree_exc_preconfig;


   ------------------------
   --  jail_environment  --
   ------------------------
   function jail_environment return JT.Text is
   begin
      return builder_env;
   end jail_environment;


   --------------------------------------
   --  boot_modules_directory_missing  --
   --------------------------------------
   function boot_modules_directory_missing return Boolean is
   begin
      if JT.equivalent (PM.configuration.operating_sys, "DragonFly") then
         declare
            sroot   : constant String := JT.USS (PM.configuration.dir_system);
            bootdir : constant String := sroot & root_boot;
            modsdir : constant String := sroot & root_lmodules;
         begin
            if AD.Exists (bootdir) and then not AD.Exists (modsdir) then
               return True;
            end if;
         end;
      end if;
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         declare
            sroot   : constant String := JT.USS (PM.configuration.dir_system);
            bootdir : constant String := sroot & root_boot;
            modsdir : constant String := sroot & root_kmodules;
         begin
            if AD.Exists (bootdir) and then not AD.Exists (modsdir) then
               return True;
            end if;
         end;
      end if;
      return False;
   end boot_modules_directory_missing;


   ------------------------------
   --  start_abnormal_logging  --
   ------------------------------
   procedure start_abnormal_logging
   is
      logpath : constant String := JT.USS (PM.configuration.dir_logs)
        & "/" & abnormal_cmd_logname;
   begin
      if AD.Exists (logpath) then
         AD.Delete_File (logpath);
      end if;
      TIO.Create (File => abnormal_log,
                  Mode => TIO.Out_File,
                  Name => logpath);
      abn_log_ready := True;
      exception
      when others => abn_log_ready := False;
   end start_abnormal_logging;


   -----------------------------
   --  stop_abnormal_logging  --
   -----------------------------
   procedure stop_abnormal_logging is
   begin
      if abn_log_ready then
         TIO.Close (abnormal_log);
      end if;
   end stop_abnormal_logging;

end Replicant;
