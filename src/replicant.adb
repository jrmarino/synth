--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Characters.Latin_1;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with GNAT.OS_Lib;
with Util.Streams.Pipes;
with Util.Streams.Buffered;
with Util.Processes;

with Parameters;

package body Replicant is

   package PM  renames Parameters;
   package AC  renames Ada.Containers;
   package AD  renames Ada.Directories;
   package EX  renames Ada.Exceptions;
   package OSL renames GNAT.OS_Lib;
   package STR renames Util.Streams;
   package LAT renames Ada.Characters.Latin_1;


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
         when usr_lib32   => return mount_base & root_usr_lib32;
         when usr_libdata => return mount_base & root_usr_libdata;
         when usr_libexec => return mount_base & root_usr_libexec;
         when usr_local   => return mount_base & root_localbase;
         when usr_sbin    => return mount_base & root_usr_sbin;
         when usr_share   => return mount_base & root_usr_share;
         when usr_src     => return mount_base & root_usr_src;
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


   ------------------
   --  initialize  --
   ------------------
   procedure initialize (testmode : Boolean; num_cores : cpu_range)
   is
      opsys   : nullfs_flavor   := dragonfly;
      mm      : constant String := get_master_mount;
      maspas  : constant String := "/master.passwd";
      etcmp   : constant String := "/etc" & maspas;
      command : constant String := "/usr/sbin/pwd_mkdb -p -d " & mm & " " &
                                   mm & maspas;
   begin
      smp_cores := num_cores;
      developer_mode := testmode;
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         opsys := freebsd;
      end if;
      flavor := opsys;

      if AD.Exists (mm) then
         annihilate_directory_tree (mm);
      end if;

      AD.Create_Path (mm);
      AD.Copy_File (etcmp, mm & maspas);
      execute (command);
      create_base_group (mm);
      cache_port_variables (mm);
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
   end finalize;


   --------------------
   --  mount_nullfs  --
   --------------------
   procedure mount_nullfs (target, mount_point : String;
                           mode : mount_mode := readonly)
   is
      cmd_freebsd   : constant String := "/sbin/mount_nullfs";
      cmd_dragonfly : constant String := "/sbin/mount_null";
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
      execute (JT.USS (command));
   end mount_nullfs;


   -----------------------
   --  mount_linprocfs  --
   -----------------------
   procedure mount_linprocfs (mount_point : String)
   is
      cmd_freebsd   : constant String := "/sbin/mount -t linprocfs linproc " &
                                          mount_point;
   begin
      --  DragonFly has lost it's Linux Emulation capability.
      --  FreeBSD has it for both amd64 and i386
      --  We should return if FreeBSD arch is not amd64 or i386, but synth
      --  will not run on any other arches at the moment, so we don't have
      --  to check (and we don't have that information yet anyway)
      if flavor = freebsd then
         execute (cmd_freebsd);
      end if;
   end mount_linprocfs;


   ---------------
   --  unmount  --
   ---------------
   procedure unmount (device_or_node : String)
   is
      command : constant String := "/sbin/umount " & device_or_node;
   begin
      --  failure to unmount causes stderr squawks which messes up curses
      --  display.  Just ignore for now (Add robustness later)
      silent_exec (command);
   exception
      when others => null;  -- silently fail
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
      command       : JT.Text;
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
      execute (JT.USS (command));
   end mount_tmpfs;


   ---------------------
   --  mount_devices  --
   ---------------------
   procedure mount_devices (path_to_dev : String)
   is
      command : constant String := "/sbin/mount -t devfs devfs " & path_to_dev;
   begin
      execute (command);
   end mount_devices;


   --------------------
   --  mount_procfs  --
   --------------------
   procedure mount_procfs (path_to_proc : String)
   is
      command : constant String := "/sbin/mount -t procfs proc " & path_to_proc;
   begin
      execute (command);
   end mount_procfs;


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
      cmd_freebsd   : constant String := "/bin/chflags";
      cmd_dragonfly : constant String := "/usr/bin/chflags";
      flag_lock     : constant String := " schg ";
      flag_unlock   : constant String := " noschg ";
      command       : JT.Text;
   begin
      if not AD.Exists (path) then
         raise scenario_unexpected with
           "chflags: " & path & " path does not exist";
      end if;
      case flavor is
         when freebsd   => command := JT.SUS (cmd_freebsd);
         when dragonfly => command := JT.SUS (cmd_dragonfly);
         when unknown   =>
            raise scenario_unexpected with
              "Executing cflags on unknown operating system";
      end case;
      case operation is
         when lock   => JT.SU.Append (command, flag_lock & path);
         when unlock => JT.SU.Append (command, flag_unlock & path);
      end case;
      execute (JT.USS (command));
   end folder_access;


   ----------------------
   --  create_symlink  --
   ----------------------
   procedure create_symlink (destination, symbolic_link : String)
   is
      command : constant String :=
                         "/bin/ln -s " & destination & " " & symbolic_link;
   begin
      execute (command);
   end create_symlink;


   ---------------------------
   --  populate_var_folder  --
   ---------------------------
   procedure populate_var_folder (path : String)
   is
      command : constant String := "/usr/sbin/mtree -p " & path &
        " -f /etc/mtree/BSD.var.dist -deqU";
   begin
      silent_exec (command);
   end populate_var_folder;


   ---------------
   --  execute  --
   ---------------
   procedure execute (command : String)
   is
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
   end execute;


   -------------------
   --  silent_exec  --
   -------------------
   procedure silent_exec (command : String)
   is
      pipe        : aliased STR.Pipes.Pipe_Stream;
      buffer      : STR.Buffered.Buffered_Stream;
      Exit_Status : Integer;
   begin
      pipe.Open (Command => command, Mode => Util.Processes.READ_ALL);
      pipe.Close;
      Exit_Status := pipe.Get_Exit_Status;
      if Exit_Status /= 0 then
         raise scenario_unexpected with
           command & " => failed with code" & Exit_Status'Img;
      end if;
   end silent_exec;


   ------------------------------
   -- internal_system_command  --
   ------------------------------
   function internal_system_command (command : String) return JT.Text
   is
      pipe    : aliased STR.Pipes.Pipe_Stream;
      buffer  : STR.Buffered.Buffered_Stream;
      content : JT.Text;
      status  : Integer;
   begin
      pipe.Open (Command => command, Mode => Util.Processes.READ_ALL);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;
      status := pipe.Get_Exit_Status;
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
      type groupset is array (1 .. 34) of sysgroup;
      users       : constant groupset :=
        ("wheel   ", "daemon  ", "kmem    ", "sys     ",
         "tty     ", "operator", "mail    ", "bin     ",
         "news    ", "man     ", "games   ", "staff   ",
         "sshd    ", "smmsp   ", "mailnull", "guest   ",
         "bind    ", "proxy   ", "authpf  ", "_pflogd ",
         "unbound ", "ftp     ", "video   ", "hast    ",
         "uucp    ", "xten    ", "dialer  ", "network ",
         "_sdpd   ", "_dhcp   ", "www     ", "vknet   ",
         "nogroup ", "nobody  ");
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
      mm     : constant String := get_master_mount;
      maspwd : constant String := "/master.passwd";
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
   begin
      AD.Copy_File (Source_Name => mm & passwd,
                    Target_Name => path_to_etc & passwd);
      AD.Copy_File (Source_Name => mm & maspwd,
                    Target_Name => path_to_etc & maspwd);
      AD.Copy_File (Source_Name => mm & spwd,
                    Target_Name => path_to_etc & spwd);
      AD.Copy_File (Source_Name => mm & pwd,
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
   begin
      AD.Copy_File (Source_Name => mtree & root,
                    Target_Name => path_to_mtree & root);
      AD.Copy_File (Source_Name => mtree & usr,
                    Target_Name => path_to_mtree & usr);
      AD.Copy_File (Source_Name => mtree & var,
                    Target_Name => path_to_mtree & var);
   end copy_mtree_files;


   ------------------------
   --  create_make_conf  --
   ------------------------
   procedure create_make_conf (path_to_etc : String)
   is
      makeconf  : TIO.File_Type;
      profilemc : constant String := PM.synth_confdir & "/" &
                  JT.USS (PM.configuration.profile) & "-make.conf";
      varcache  : constant String := get_master_mount & "/varcache.conf";
   begin
      TIO.Create (File => makeconf,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/make.conf");

      TIO.Put_Line (makeconf, "USE_PACKAGE_DEPENDS=yes");
      TIO.Put_Line (makeconf, "PACKAGE_BUILDING=yes");
      TIO.Put_Line (makeconf, "BATCH=yes");
      TIO.Put_Line (makeconf, "PKG_CREATE_VERBOSE=yes");
      TIO.Put_Line (makeconf, "PORTSDIR=/xports");
      TIO.Put_Line (makeconf, "DISTDIR=/distfiles");
      TIO.Put_Line (makeconf, "WRKDIRPREFIX=/construction");
      TIO.Put_Line (makeconf, "PORT_DBDIR=/options");
      TIO.Put_Line (makeconf, "PACKAGES=/packages");
      TIO.Put_Line (makeconf, "MAKE_JOBS_NUMBER_LIMIT=" &
                      (JT.trim (PM.configuration.jobs_limit'Img)));

      if developer_mode then
         TIO.Put_Line (makeconf, "DEVELOPER=1");
      end if;
      if AD.Exists (JT.USS (PM.configuration.dir_ccache)) then
         TIO.Put_Line (makeconf, "WITH_CCACHE_BUILD=yes");
         TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
      end if;

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
      TIO.Put_Line (fstab, "linproc /usr/compat/proc linprocfs rw 0 0");
      TIO.Close (fstab);
   end create_etc_fstab;


   ------------------------
   --  execute_ldconfig  --
   ------------------------
   procedure execute_ldconfig (id : builders)
   is
      smount  : constant String := get_slave_mount (id);
      command : constant String := chroot & smount &
                                   " /sbin/ldconfig -m /lib /usr/lib";
   begin
      execute (command);
   end execute_ldconfig;


   -------------------------------
   --  standalone_pkg8_install  --
   -------------------------------
   function standalone_pkg8_install (id : builders) return Boolean
   is
      smount  : constant String := get_slave_mount (id);
      taropts : constant String := "-C / */pkg-static";
      command : constant String := chroot & smount &
        " /usr/bin/tar -xf /packages/Latest/pkg.txz " & taropts;
   begin
      silent_exec (command);
      return True;
   exception
      when others => return False;
   end standalone_pkg8_install;


   ------------------------
   --  build_repository  --
   ------------------------
   function build_repository (id : builders) return Boolean
   is
      smount  : constant String := get_slave_mount (id);
      command : constant String := chroot & smount & " " &
        host_localbase & "/sbin/pkg-static repo /packages";
   begin
      if not standalone_pkg8_install (id) then
         TIO.Put_Line ("Failed to install pkg-static in builder" & id'Img);
         return False;
      end if;
      silent_exec (command);
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

      if opts.need_procfs then
         mount_procfs (path_to_proc => location (slave_base, proc));
      end if;

      if flavor = freebsd then
         if opts.need_linprocfs then
            if PM.configuration.tmpfs_localbase then
               mount_tmpfs (slave_base & root_linux, 12 * 1024);
            else
               forge_directory (slave_linux);
               mount_nullfs (slave_linux, slave_base & root_linux, readwrite);
            end if;
            forge_directory (slave_base & root_linproc);
            mount_linprocfs (mount_point => slave_base & root_linproc);
         end if;
         declare
            lib32 : String := clean_mount_point (usr_lib32);
         begin
            if AD.Exists (lib32) then
               mount_nullfs (target      => lib32,
                             mount_point => location (slave_base, usr_lib32));
            end if;
         end;
      end if;

      if AD.Exists (root_usr_src) then
         mount_nullfs (root_usr_src, location (slave_base, usr_src));
         if AD.Exists (root_usr_src & "/sys") then
            create_symlink (destination   => "usr/src/sys",
                            symbolic_link => slave_base & "/sys");
         end if;
      end if;

      if AD.Exists (mount_target (ccache)) then
         mount_nullfs (mount_target (ccache), location (slave_base, ccache),
                       mode => readwrite);
      end if;

      mount_devices (location (slave_base, dev));

      populate_var_folder (location (slave_base, var));
      copy_mtree_files    (location (slave_base, etc_mtree));
      copy_rc_default     (location (slave_base, etc));
      copy_resolv_conf    (location (slave_base, etc));
      create_make_conf    (location (slave_base, etc));
      create_passwd       (location (slave_base, etc));
      create_group        (location (slave_base, etc));
      create_etc_services (location (slave_base, etc));
      create_etc_fstab    (location (slave_base, etc));

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
      if not PM.configuration.tmpfs_localbase then
         --  We can't use AD.Delete_Tree because it skips directories
         --  starting with "." (pretty useless then)
         annihilate_directory_tree (slave_local);
      end if;

      unmount (location (slave_base, wrkdirs));
      if not PM.configuration.tmpfs_workdir then
         annihilate_directory_tree (slave_work);
      end if;

      if AD.Exists (root_usr_src) then
         unmount (location (slave_base, usr_src));
      end if;

      if AD.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache));
      end if;

      if flavor = freebsd then
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
      end if;

      if opts.need_procfs then
         unmount (location (slave_base, proc));
      end if;

      unmount (location (slave_base, dev));
      unmount (location (slave_base, xports));
      unmount (location (slave_base, options));
      unmount (location (slave_base, packages));
      unmount (location (slave_base, distfiles));

      for mnt in subfolder'Range loop
         unmount (location (slave_base, mnt));
      end loop;

      folder_access (location (slave_base, home), unlock);
      folder_access (location (slave_base, root), unlock);
      folder_access (location (slave_base, var) & "/empty", unlock);

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
      buildbase : constant String := JT.USS (PM.configuration.dir_buildbase);
      command   : constant String := "/bin/df -h";
      comres    : JT.Text;
      topline   : JT.Text;
      crlen1    : Natural;
      crlen2    : Natural;
   begin
      comres := internal_system_command (command);
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
      package sorter is new crate.Generic_Sorting ("<" => JT.SU."<");
      procedure annihilate (cursor : crate.Cursor);

      buildbase : constant String := JT.USS (PM.configuration.dir_buildbase);
      command1  : constant String := "/bin/df -h";
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
      comres := internal_system_command (command1);
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

      sorter.Sort (Container => mpoints);
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


   ----------------------------
   --  cache_port_variables  --
   ----------------------------
   procedure cache_port_variables (path_to_mm : String)
   is
      function create_OSRELEASE (OSRELEASE : String) return String;
      OSVER    : constant String := get_osversion_from_param_header;
      ARCH     : constant String := get_arch_from_bourne_shell;
      portsdir : constant String := JT.USS (PM.configuration.dir_portsdir);
      fullport : constant String := portsdir & "/ports-mgmt/pkg";
      command  : constant String :=
                 "/usr/bin/make __MAKE_CONF=/dev/null -C " & fullport &
                 " -VHAVE_COMPAT_IA32_KERN -VCONFIGURE_MAX_CMD_LEN";
      pipe     : aliased STR.Pipes.Pipe_Stream;
      buffer   : STR.Buffered.Buffered_Stream;
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;
      vconf    : TIO.File_Type;

      type result_range is range 1 .. 2;

      function create_OSRELEASE (OSRELEASE : String) return String
      is
         --  FreeBSD OSVERSION is 6 or 7 digits
         --          OSVERSION [M]MNNPPP
         --  DragonFly OSVERSION is 6 digits
         --            OSVERSION MNNNPP
         len : constant Natural := OSRELEASE'Length;
         FL  : constant Natural := len - 4;
         OSR : constant String (1 .. len) := OSRELEASE;
         MM  : String (1 .. 2) := "  ";
         PP  : String (1 .. 2) := "  ";
      begin
         if len < 6 then
            return "1.0-SYNTH";
         end if;
         if len = 6 then
            MM (2) := OSR (1);
         else
            MM := OSR (1 .. 2);
         end if;
         if flavor = dragonfly then
            if OSR (3) = '0' then
               PP (2) := OSR (4);
            else
               PP := OSR (3 .. 4);
            end if;
         else
            if OSR (FL) = '0' then
               PP (2) := OSR (FL + 1);
            else
               PP := OSR (FL .. FL + 1);
            end if;
         end if;
         return JT.trim (MM) & "." & JT.trim (PP) & "-SYNTH";
      end create_OSRELEASE;

   begin
      pipe.Open (Command => command);
      buffer.Initialize (Output => null,
                         Input  => pipe'Unchecked_Access,
                         Size   => 4096);
      buffer.Read (Into => content);
      pipe.Close;

      status := pipe.Get_Exit_Status;
      if status /= 0 then
         raise scenario_unexpected with
           "cache_port_variables: return code =" & status'Img;
      end if;

      TIO.Create (File => vconf,
                  Mode => TIO.Out_File,
                  Name => path_to_mm & "/varcache.conf");

      for k in result_range loop
         JT.nextline (lineblock => content, firstline => topline);
         declare
            value : constant String := JT.USS (topline);
         begin
            case k is
               when 1 => TIO.Put_Line (vconf, "HAVE_COMPAT_IA32_KERN=" & value);
               when 2 => TIO.Put_Line (vconf, "CONFIGURE_MAX_CMD_LEN=" & value);
            end case;
         end;
      end loop;
      TIO.Put_Line (vconf, "_SMP_CPUS=" & JT.int2str (Integer (smp_cores)));
      TIO.Put_Line (vconf, "UID=0");
      TIO.Put_Line (vconf, "ARCH=" & ARCH);
      TIO.Put (vconf, "OPSYS=");
      case flavor is
         when freebsd   => TIO.Put_Line (vconf, "FreeBSD");
                           TIO.Put_Line (vconf, "OSVERSION=" & OSVER);
         when dragonfly => TIO.Put_Line (vconf, "DragonFly");
                           TIO.Put_Line (vconf, "DFLYVERSION=" & OSVER);
                           TIO.Put_Line (vconf, "OSVERSION=9999999");
         when unknown   => TIO.Put_Line (vconf, "Unknown");
      end case;
      TIO.Put_Line (vconf, "_OSRELEASE=" & create_OSRELEASE (OSVER));
      TIO.Close (vconf);
   end cache_port_variables;


   ---------------------------------------
   --  write_common_mtree_exclude_base  --
   ---------------------------------------
   procedure write_common_mtree_exclude_base (mtreefile : TIO.File_Type) is
   begin
      TIO.Put_Line
        (mtreefile,
           "./bin" & LAT.LF
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
         & "./xports"
        );
   end write_common_mtree_exclude_base;


   --------------------------------
   --  write_preinstall_section  --
   --------------------------------
   procedure write_preinstall_section (mtreefile : TIO.File_Type) is
   begin
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
         & "./var/db/freebsd-update" & LAT.LF
         & "./var/db/pkg" & LAT.LF
         & "./var/log" & LAT.LF
         & "./var/mail" & LAT.LF
         & "./var/run" & LAT.LF
         & "./var/tmp" & LAT.LF
         & "./usr/local/etc/gconf/gconf.xml.defaults/%gconf-tree*.xml" & LAT.LF
         & "./usr/local/lib/gio/modules/giomodule.cache" & LAT.LF
         & "./usr/local/info/dir" & LAT.LF
         & "./usr/local/*/info/dir" & LAT.LF
         & "./usr/local/*/ls-R" & LAT.LF
         & "./usr/local/share/octave/octave_packages" & LAT.LF
         & "./usr/local/share/xml/catalog.ports"
        );
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


   ---------------------------------------
   --  get_osversion_from_param_header  --
   ---------------------------------------
   function get_osversion_from_param_header return String
   is
      function get_pattern return String;
      function get_pattern return String
      is
         DFVER  : constant String := "#define __DragonFly_version ";
         FBVER  : constant String := "#define __FreeBSD_version ";
         BADVER : constant String := "#define __Unknown_version ";
      begin
         case flavor is
            when freebsd   => return FBVER;
            when dragonfly => return DFVER;
            when unknown   => return BADVER;
         end case;
      end get_pattern;

      header  : TIO.File_Type;
      badres  : constant String := "100000";
      pattern : constant String := get_pattern;
      paramh  : constant String := JT.USS (PM.configuration.dir_system) &
                                   "usr/include/sys/param.h";
   begin
      TIO.Open (File => header, Mode => TIO.In_File, Name => paramh);
      while not TIO.End_Of_File (header) loop
         declare
            Line : constant String := TIO.Get_Line (header);
         begin
            if JT.contains (Line, pattern) then
               declare
                  OSVER : constant String := JT.part_2 (Line, pattern);
                  len   : constant Natural := OSVER'Length;
               begin
                  exit when len < 7;
                  TIO.Close (header);
                  case OSVER (OSVER'First + 6) is
                     when '0' .. '9' =>
                        return JT.trim (OSVER (OSVER'First .. OSVER'First + 6));
                     when others =>
                        return JT.trim (OSVER (OSVER'First .. OSVER'First + 5));
                  end case;
               end;
            end if;
         end;
      end loop;
      TIO.Close (header);
      return badres;
   exception
      when others =>
         if TIO.Is_Open (header) then
            TIO.Close (header);
         end if;
         return badres;
   end get_osversion_from_param_header;


   ----------------------------------
   --  get_arch_from_bourne_shell  --
   ----------------------------------
   function get_arch_from_bourne_shell return String
   is
      command : constant String := "/usr/bin/file -b " &
                JT.USS (PM.configuration.dir_system) & "/bin/sh";
      badarch : constant String := "BADARCH";
      comres  : JT.Text;
   begin
      comres := internal_system_command (command);
      declare
         unlen    : constant Natural := JT.SU.Length (comres) - 1;
         fileinfo : constant String := JT.USS (comres)(1 .. unlen);
         arch     : constant String (1 .. 11) :=
                    fileinfo (fileinfo'First + 27 .. fileinfo'First + 37);
      begin
         if arch (1 .. 6) = "x86-64" then
            case flavor is
               when freebsd   => return "amd64";
               when dragonfly => return "x86_64";
               when unknown   => return badarch;
            end case;
         elsif arch = "Intel 80386" then
            return "i386";
         else
            return badarch;
         end if;
      end;
   exception
      when others =>
         return badarch;
   end get_arch_from_bourne_shell;

end Replicant;
