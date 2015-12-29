--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
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
         when etc_mtree   => return mount_base & root_etc_mtree;
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


   ------------------
   --  initialize  --
   ------------------
   procedure initialize
   is
      opsys   : nullfs_flavor   := dragonfly;
      mm      : constant String := get_master_mount;
      maspas  : constant String := "/master.passwd";
      etcmp   : constant String := "/etc" & maspas;
      command : constant String := "/usr/sbin/pwd_mkdb -p -d " & mm & " " &
                                   mm & maspas;
   begin
      if JT.equivalent (PM.configuration.operating_sys, "FreeBSD") then
         opsys := freebsd;
      end if;
      flavor := opsys;

      if AD.Exists (mm) then
         AD.Delete_Tree (mm);
      end if;

      AD.Create_Path (mm);
      AD.Copy_File (etcmp, mm & maspas);
      execute (command);
      create_base_group (mm);

   end initialize;


   ----------------
   --  finalize  --
   ----------------
   procedure finalize
   is
      mm : constant String := get_master_mount;
   begin
      if AD.Exists (mm) then
         AD.Delete_Tree (mm);
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


   ---------------
   --  unmount  --
   ---------------
   procedure unmount (device_or_node : String)
   is
      command : constant String := "/sbin/umount " & device_or_node;
   begin
      execute (command);
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
      cmd_freebsd   : constant String := "/sbin/mount";
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


   --------------------------
   --  populate_localbase  --
   --------------------------
   procedure populate_localbase  (path : String)
   is
      command : constant String := "/usr/sbin/mtree -p " & path &
        " -f /etc/mtree/BSD.local.dist -deqU";
   begin
      silent_exec (command);
   end populate_localbase;


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
            if line'Length > sysgroup'Length then
               for grpindex in groupset'Range loop
                  declare
                     grpcolon : String := JT.trim (users (grpindex)) & ":";
                  begin
                     if grpcolon = line (1 .. grpcolon'Last) then
                        keepit := True;
                        exit;
                     end if;
                  end;
               end loop;
               if keepit then
                  TIO.Put_Line (group, line);
               end if;
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
      passwd : constant String := "/passwd";
      spwd   : constant String := "/spwd.db";
      pwd    : constant String := "/pwd.db";
   begin
      AD.Copy_File (Source_Name => mm & passwd,
                    Target_Name => path_to_etc & passwd);
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
   begin
      TIO.Create (File => makeconf,
                  Mode => TIO.Out_File,
                  Name => path_to_etc & "/make.conf");

      TIO.Put_Line (makeconf, "USE_PACKAGE_DEPENDS=yes");
      TIO.Put_Line (makeconf, "PACKAGE_BUILDING=yes");
      TIO.Put_Line (makeconf, "BATCH=yes");
      TIO.Put_Line (makeconf, "NO_BACKUP=yes");
      TIO.Put_Line (makeconf, "PKG_CREATE_VERBOSE=yes");
      TIO.Put_Line (makeconf, "PORTSDIR=/xports");
      TIO.Put_Line (makeconf, "DISTDIR=/distfiles");
      TIO.Put_Line (makeconf, "WRKDIRPREFIX=/construction");
      TIO.Put_Line (makeconf, "PORT_DBDIR=/options");
      TIO.Put_Line (makeconf, "PACKAGES=/packages");
      TIO.Put_Line (makeconf, "MAKE_JOBS_NUMBER_LIMIT=" &
                      (JT.trim (PM.configuration.jobs_limit'Img)));

      if AD.Exists (JT.USS (PM.configuration.dir_ccache)) then
         TIO.Put_Line (makeconf, "WITH_CCACHE_BUILD=yes");
         TIO.Put_Line (makeconf, "CCACHE_DIR=/ccache");
      end if;

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


   ------------------------
   --  execute_ldconfig  --
   ------------------------
   procedure execute_ldconfig (id : builders)
   is
      smount  : constant String := get_slave_mount (id);
      command : constant String := "/usr/sbin/chroot " & smount &
                                   " /sbin/ldconfig -m /lib /usr/lib";
   begin
      execute (command);
   end execute_ldconfig;


   --------------------
   --  launch_slave  --
   --------------------
   procedure launch_slave  (id : builders)
   is
      slave_base   : constant String := get_slave_mount (id);
      slave_work   : constant String := slave_base & "_work";
      slave_local  : constant String := slave_base & "_localbase";
      dir_system   : constant String := JT.USS (PM.configuration.dir_system);
      live_system  : constant Boolean := (dir_system = "/");
   begin
      forge_directory (slave_base);
      mount_tmpfs (slave_base);

      for mnt in folder'Range loop
         forge_directory (location (slave_base, mnt));
      end loop;

      for mnt in subfolder'Range loop
         if live_system then
            mount_nullfs (target      => location ("", mnt),
                          mount_point => location (slave_base, mnt));
         else
            mount_nullfs (target      => location (dir_system, mnt),
                          mount_point => location (slave_base, mnt));
         end if;
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
      populate_localbase  (location (slave_base, usr_local));
      copy_mtree_files    (location (slave_base, etc_mtree));
      copy_resolv_conf    (location (slave_base, etc));
      create_make_conf    (location (slave_base, etc));
      create_passwd       (location (slave_base, etc));
      create_group        (location (slave_base, etc));

      execute_ldconfig (id);

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
         AD.Delete_Tree (slave_local);
      end if;

      unmount (location (slave_base, wrkdirs));
      if not PM.configuration.tmpfs_workdir then
         AD.Delete_Tree (slave_work);
      end if;

      if AD.Exists (root_usr_src) then
         unmount (location (slave_base, usr_src));
      end if;

      if AD.Exists (mount_target (ccache)) then
         unmount (location (slave_base, ccache));
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
      AD.Delete_Tree (slave_base);

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

      procedure annihilate (cursor : crate.Cursor) is
      begin
         TIO.Put_Line (JT.USS (crate.Element (cursor)));
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

      --  don't forget to delete buildbase !!
      return False;
   end clear_existing_mounts;

end Replicant;
