--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package body Replicant.Platform is

   ------------------
   --  df_command  --
   ------------------
   function df_command return String is
   begin
      case platform_type is
         when dragonfly |
              freebsd   |
              netbsd    => return "/bin/df -h";
         when solaris   => return "/usr/sbin/df -h";
         when linux     => return "/usr/bin/df -h";
         when unknown   => return "skip";
      end case;
   end df_command;


   -------------------------
   --  file_type_command  --
   -------------------------
   function file_type_command return String
   is
      bsd_command : constant String := "/usr/bin/file -b " &
                    JT.USS (PM.configuration.dir_system) & "/bin/sh";
      lin_command : constant String := "/usr/bin/file -b " &
                    JT.USS (PM.configuration.dir_system) & "/usr/bin/bash";
      sol_command : constant String := "/usr/bin/file " &
                    JT.USS (PM.configuration.dir_system) & "/usr/sbin/sh";
   begin
      case platform_type is
         when freebsd | dragonfly | netbsd =>
            return bsd_command;
         when linux =>
            return lin_command;
         when solaris =>
            return sol_command;
         when unknown =>
            return "bad file_type_command invocation";
      end case;
   end file_type_command;


   --------------------------
   --  file_is_executable  --
   --------------------------
   function file_is_executable (filename : String) return Boolean
   is
      command : constant String := "/usr/bin/file -b -L " &
        "-e ascii -e encoding -e tar -e compress " &
        LAT.Quotation & filename & LAT.Quotation;
      sol_cmd : constant String := "/usr/bin/file " &
        LAT.Quotation & filename & LAT.Quotation;
      comres  : JT.Text;
   begin
      case platform_type is
         when dragonfly | freebsd | netbsd | linux =>
            comres := internal_system_command (command);
         when solaris =>
            comres := internal_system_command (sol_cmd);
         when unknown =>
            return False;
      end case;
      return JT.contains (comres, "executable");
   exception
      when others => return False;
   end file_is_executable;


   --------------------------
   --  dynamically_linked  --
   --------------------------
   function dynamically_linked (base, filename : String) return Boolean
   is
      command : String := chroot & base & " /usr/bin/file -b -L " &
        "-e ascii -e encoding -e tar -e compress " &
        LAT.Quotation & filename & LAT.Quotation;
      sol_cmd : constant String := "/usr/bin/file " &
        LAT.Quotation & filename & LAT.Quotation;
      comres  : JT.Text;
   begin
      case platform_type is
         when dragonfly | freebsd | netbsd | linux =>
            comres := internal_system_command (command);
         when solaris =>
            comres := internal_system_command (sol_cmd);
         when unknown =>
            return False;
      end case;
      return JT.contains (comres, "dynamically linked");
   exception
      when others => return False;
   end dynamically_linked;


   -----------------------------------
   --  isolate_arch_from_file_type  --
   -----------------------------------
   function isolate_arch_from_file_type (fileinfo : String) return filearch
   is
      --   DF: ELF 64-bit LSB executable, x86-64
      --   FB: ELF 64-bit LSB executable, x86-64
      --   FB: ELF 32-bit LSB executable, Intel 80386
      --   NB: ELF 64-bit LSB executable, x86-64
      --    L: ELF 64-bit LSB executable, x86-64
      --  OPN: ELF 64-bit LSB shared object, x86-64, version 1 (FreeBSD), ...

      fragment : constant String := JT.trim (JT.specific_field (fileinfo, 2, ","));
      answer   : filearch := (others => ' ');
   begin
      if fragment'Length > filearch'Length then
         answer := fragment (fragment'First .. fragment'First + filearch'Length - 1);
      else
         answer (answer'First .. answer'First + fragment'Length - 1) := fragment;
      end if;
      return answer;
   end isolate_arch_from_file_type;


   ----------------------------------
   --  get_arch_from_bourne_shell  --
   ----------------------------------
   function get_arch_from_bourne_shell return String
   is
      function translate_arch (arch : filearch) return String;
      badarch : constant String := "BADARCH";
      comres  : JT.Text;
      arch    : filearch;

      function translate_arch (arch : filearch) return String is
      begin
         if arch (arch'First .. arch'First + 5) = "x86-64" or else
           arch (arch'First .. arch'First + 4) = "AMD64"
         then
            case platform_type is
               when freebsd   => return "amd64";
               when netbsd    => return "amd64";
               when dragonfly => return "x86_64";
               when linux     => return "x86_64";
               when solaris   => return "x86_64";
               when unknown   => return badarch;
            end case;
         elsif arch = "ARM aarch64" then
            return "aarch64";
         elsif arch = "ARM        " then
            return "armv6";
         elsif arch = "Intel 80386" then
            return "i386";
         else
            return badarch;
         end if;
      end translate_arch;
   begin
      comres := internal_system_command (file_type_command);
      arch   := isolate_arch_from_file_type (JT.USS (comres));
      return translate_arch (arch);
   exception
      when others =>
         return badarch;
   end get_arch_from_bourne_shell;


   --------------------------------------
   --  determine_package_architecture  --
   --------------------------------------
   function determine_package_architecture return package_abi
   is
      function newsuffix (arch : filearch) return String;
      function suffix    (arch : filearch) return String;
      function get_major (fileinfo : String; OS : String) return String;
      function even      (fileinfo : String) return String;

      command : constant String := file_type_command;
      res     : package_abi;
      arch    : filearch;
      UN      : JT.Text;

      function suffix (arch : filearch) return String is
      begin
         if arch (arch'First .. arch'First + 5) = "x86-64" or else
           arch (arch'First .. arch'First + 4) = "AMD64"
         then
            return "x86:64";
         elsif arch = "Intel 80386" then
            return "x86:32";
         elsif arch = "ARM aarch64" then
            return "aarch64:64";
         elsif arch = "ARM        " then
            return "armv6:32:el:eabi:softfp";
         else
            return "unknown:" & arch;
         end if;
      end suffix;

      function newsuffix (arch : filearch) return String is
      begin
         if arch (arch'First .. arch'First + 5) = "x86-64" or else
           arch (arch'First .. arch'First + 4) = "AMD64"
         then
            return "amd64";
         elsif arch = "Intel 80386" then
            return "i386";
         elsif arch = "ARM aarch64" then
            return "arm64";
         elsif arch = "ARM        " then
            return "armv6";
         else
            return "unknown:" & arch;
         end if;
      end newsuffix;

      function even (fileinfo : String) return String
      is
         --  DF  4.5-DEVELOPMENT: ... DragonFly 4.0.501
         --  DF 4.10-RELEASE    : ... DragonFly 4.0.1000
         --  DF 4.11-DEVELOPMENT: ... DragonFly 4.0.1102
         --
         --  Alternative future format (file version 2.0)
         --  DFV 400702: ... DragonFly 4.7.2
         --  DFV 401117: ..  DragonFly 4.11.17
         rest  : constant String := JT.part_2 (fileinfo, "DragonFly ");
         major : constant String := JT.part_1 (rest, ".");
         rest2 : constant String := JT.part_2 (rest, ".");
         part2 : constant String := JT.part_1 (rest2, ".");
         rest3 : constant String := JT.part_2 (rest2, ".");
         part3 : constant String := JT.part_1 (rest3, ",");
         minor : String (1 .. 2) := "00";
         point : Character;
      begin
         if part2 = "0" then
            --  version format in October 2016
            declare
               mvers : String (1 .. 4) := "0000";
               lenp3 : constant Natural := part3'Length;
            begin
               mvers (mvers'Last - lenp3 + 1 .. mvers'Last) := part3;
               minor := mvers (1 .. 2);
            end;
         else
            --  Alternative future format (file version 2.0)
            declare
               lenp2 : constant Natural := part2'Length;
            begin
               minor (minor'Last - lenp2 + 1 .. minor'Last) := part2;
            end;
         end if;

         point := minor (2);
         case point is
            when '1' => minor (2) := '2';
            when '3' => minor (2) := '4';
            when '5' => minor (2) := '6';
            when '7' => minor (2) := '8';
            when '9' => minor (2) := '0';
                        minor (1) := Character'Val (Character'Pos (minor (1)) + 1);
            when others => null;
         end case;
         if minor (1) = '0' then
            return major & "." & minor (2);
         else
            return major & "." & minor (1 .. 2);
         end if;

      end even;

      function get_major (fileinfo : String; OS : String) return String
      is
         --  FreeBSD 10.2, stripped
         --  FreeBSD 11.0 (1100093), stripped
         --  NetBSD 7.0.1, not stripped
         rest  : constant String := JT.part_2 (fileinfo, OS);
         major : constant String := JT.part_1 (rest, ".");
      begin
         return major;
      end get_major;

   begin
      UN := internal_system_command (file_type_command);
      arch   := isolate_arch_from_file_type (JT.USS (UN));
      case platform_type is
         when dragonfly =>
            declare
               dfly    : constant String := "dragonfly:";
               release : constant String := even (JT.USS (UN));
            begin
               res.calculated_abi := JT.SUS (dfly);
               JT.SU.Append (res.calculated_abi, release & ":");
               res.calc_abi_noarch := res.calculated_abi;
               JT.SU.Append (res.calculated_abi, suffix (arch));
               JT.SU.Append (res.calc_abi_noarch, "*");
               res.calculated_alt_abi  := res.calculated_abi;
               res.calc_alt_abi_noarch := res.calc_abi_noarch;
            end;
         when freebsd =>
            declare
               fbsd1   : constant String := "FreeBSD:";
               fbsd2   : constant String := "freebsd:";
               release : constant String := get_major (JT.USS (UN),
                                                       "FreeBSD ");
            begin
               res.calculated_abi     := JT.SUS (fbsd1);
               res.calculated_alt_abi := JT.SUS (fbsd2);
               JT.SU.Append (res.calculated_abi, release & ":");
               JT.SU.Append (res.calculated_alt_abi, release & ":");
               res.calc_abi_noarch     := res.calculated_abi;
               res.calc_alt_abi_noarch := res.calculated_alt_abi;
               JT.SU.Append (res.calculated_abi, newsuffix (arch));
               JT.SU.Append (res.calculated_alt_abi, suffix (arch));
               JT.SU.Append (res.calc_abi_noarch, "*");
               JT.SU.Append (res.calc_alt_abi_noarch, "*");
            end;
         when netbsd =>
            declare
               net1     : constant String := "NetBSD:";
               net2     : constant String := "netbsd:";
               release  : constant String := get_major (JT.USS (UN),
                                                        "NetBSD ");
            begin
               res.calculated_abi     := JT.SUS (net1);
               res.calculated_alt_abi := JT.SUS (net2);
               JT.SU.Append (res.calculated_abi, release & ":");
               JT.SU.Append (res.calculated_alt_abi, release & ":");
               res.calc_abi_noarch     := res.calculated_abi;
               res.calc_alt_abi_noarch := res.calculated_alt_abi;
               JT.SU.Append (res.calculated_abi, newsuffix (arch));
               JT.SU.Append (res.calculated_alt_abi, suffix (arch));
               JT.SU.Append (res.calc_abi_noarch, "*");
               JT.SU.Append (res.calc_alt_abi_noarch, "*");
            end;
         when linux   => null;  --  TBD (check ABI first)
         when solaris => null;  --  TBD (check ABI first)
         when unknown => null;
      end case;
      return res;
   end determine_package_architecture;


   ------------------------
   --  swapinfo_command  --
   ------------------------
   function swapinfo_command return String is
   begin
      case platform_type is
         when dragonfly | freebsd =>
            return "/usr/sbin/swapinfo -k";
         when netbsd =>
            return "/sbin/swapctl -lk";
         when linux =>
            return "/usr/sbin/swapon --bytes --show=NAME,SIZE,USED,PRIO";
         when solaris =>
            return "/usr/sbin/swap -l";
         when unknown =>
            return "";
      end case;
   end swapinfo_command;


   -------------------------
   --  interactive_shell  --
   -------------------------
   function interactive_shell return String is
   begin
      case platform_type is
         when dragonfly | freebsd =>
            return "/bin/tcsh";
         when netbsd =>
            return "/bin/sh";
         when linux =>
            return "/bin/bash";
         when solaris =>
            return "TBD";
         when unknown =>
            return "DONTCARE";
      end case;
   end interactive_shell;


   -----------------
   --  load_core  --
   -----------------
   function load_core (instant_load : Boolean) return Float
   is
      function probe_load return String;

      ----------------- 123456789-123456789-123456789-
      --  DFLY/FreeBSD: vm.loadavg: { 0.00 0.00 0.00 }
      --  NetBSD:       vm.loadavg: 0.00 0.00 0.00
      --  Linux:        0.00 0.01 0.05 3/382 15409
      --  Solaris:      [~42 chars]load average: 0.01, 0.01, 0.01

      zero : constant Float := 0.0;
      lo   : Integer;

      function probe_load return String
      is
         bsd  : constant String := "/usr/bin/env LANG=C /sbin/sysctl vm.loadavg";
         lin  : constant String := "/bin/cat /proc/loadavg";
         sol  : constant String := "/usr/bin/uptime";
      begin
         case platform_type is
            when dragonfly | freebsd =>
               lo := 14;
               return JT.USS (internal_system_command (bsd));
            when netbsd =>
               lo := 12;
               return JT.USS (internal_system_command (bsd));
            when linux =>
               lo := 0;
               return JT.USS (internal_system_command (lin));
            when solaris =>
               return JT.USS (internal_system_command (sol));
            when unknown =>
               return "";
         end case;
      end probe_load;

      comres : constant String := probe_load;
   begin
      case platform_type is
         when dragonfly | freebsd | netbsd | linux =>
            declare
               stripped : constant String := comres (comres'First + lo .. comres'Last);
            begin
               if instant_load then
                  return Float'Value (JT.specific_field (stripped, 1, " "));
               else
                  return Float'Value (JT.specific_field (stripped, 2, " "));
               end if;
            end;
         when solaris =>
            declare
               stripped : constant String := JT.part_2 (comres, "load average: ");
            begin
               if instant_load then
                  return Float'Value (JT.specific_field (stripped, 1, ", "));
               else
                  return Float'Value (JT.specific_field (stripped, 2, ", "));
               end if;
            end;
         when unknown => return zero;
      end case;
   exception
      when others => return zero;
   end load_core;


   ------------------------
   --  get_instant_load  --
   ------------------------
   function get_instant_load return Float is
   begin
      return load_core (instant_load => True);
   end get_instant_load;


   -------------------------
   --  get_5_minute_load  --
   -------------------------
   function get_5_minute_load return Float is
   begin
      return load_core (instant_load => False);
   end get_5_minute_load;


   -----------------------
   --  get_number_cpus  --
   -----------------------
   function get_number_cpus return Positive
   is
      --  Chicken/Egg issue.
      --  Platform type is not available.  This function is called before
      --  The profile loading which requires the number of cpus as an argument.
      --  Therefore, we need two commands, the first being getting the OPSYS
      --  through the uname -s command.

      type opsys is (FreeFly, NetBSD, Linux, Solaris, Unsupported);
      uname   : constant String := "/usr/bin/uname -s";
      bsd_cmd : constant String := "/sbin/sysctl hw.ncpu";
      lin_cmd : constant String := "/usr/bin/nproc";
      sol_cmd : constant String := "/usr/sbin/psrinfo -pv";
      thissys : opsys;
      comres  : JT.Text;
      status  : Integer;
      start   : Positive;
   begin
      comres := Unix.piped_command (uname, status);
      if status /= 0 then
         return 1;
      end if;

      declare
         resstr    : String := JT.USS (comres);
         opsys_str : String := resstr (resstr'First .. resstr'Last - 1);
      begin
         if opsys_str = "FreeBSD" then
            thissys := FreeFly;
         elsif opsys_str = "DragonFly" then
            thissys := FreeFly;
         elsif opsys_str = "NetBSD" then
               thissys := NetBSD;
         elsif opsys_str = "Linux" then
            thissys := Linux;
         elsif opsys_str = "SunOS" then
            thissys := Solaris;
         else
            thissys := Unsupported;
         end if;
      end;

      --  DF/Free: expected output: "hw.ncpu: C" where C is integer
      --  NetBSD:  expected output: "hw.ncpu = C"
      --  Linux:   expected output: "C"
      --  Solaris: expected output:
      --    The physical processor has 64 virtual processors (0-63)
      --      UltraSPARC-T2+ (cpuid 0 clock 1165 MHz)
      --    The physical processor has 64 virtual processors (64-127)
      --      UltraSPARC-T2+ (cpuid 64 clock 1165 MHz)
      case thissys is
         when FreeFly =>
            start := 10;
            comres := Unix.piped_command (bsd_cmd, status);
         when NetBSD =>
            start := 11;
            comres := Unix.piped_command (bsd_cmd, status);
         when Linux   =>
            start := 1;
            comres := Unix.piped_command (lin_cmd, status);
         when Solaris =>
            start := 1;
            comres := Unix.piped_command (sol_cmd, status);
            --  garbage (incomplete).  See src/parameters.adb#L686 off ravenadm for rest
         when Unsupported =>
            return 1;
      end case;

      if status /= 0 then
         return 1;
      end if;
      declare
         resstr : String := JT.USS (comres);
         ncpu   : String := resstr (start .. resstr'Last - 1);
         number : Positive := Integer'Value (ncpu);
      begin
         return number;
      exception
         when others => return 1;
      end;
   end get_number_cpus;


   ------------------------------
   --  set_file_as_executable  --
   ------------------------------
   procedure set_file_as_executable (fullpath : String)
   is
      --  supported by all platforms
      command : constant String := "/bin/chmod 755 " & fullpath;
   begin
      silent_exec (command);
   exception
      when others => null;
   end set_file_as_executable;


   -------------------------------
   --  standalone_pkg8_install  --
   -------------------------------
   function standalone_pkg8_install (id : builders) return Boolean
   is
      smount  : constant String := get_slave_mount (id);
      taropt1 : constant String := "*/pkg-static";
      taropt2 : constant String := "*/pkg-static */pkgng_admin";
      command : constant String := chroot & smount &
        " /usr/bin/tar -x --xz -f /packages/Latest/pkg.txz -C / ";
      install_make : constant String := chroot & smount &
        " /usr/pkg/sbin/pkg-static add -A /packages/Latest/bmake.txz";
   begin
      case software_framework is
         when ports_collection =>
            silent_exec (command & taropt1);
         when pkgsrc =>
            silent_exec (command & taropt2);
            silent_exec (install_make);
      end case;
      return True;
   exception
      when others => return False;
   end standalone_pkg8_install;


   ------------------------------
   --  host_pkgsrc_mk_install  --
   ------------------------------
   function host_pkgsrc_mk_install (id : builders) return Boolean
   is
      smount  : constant String := get_slave_mount (id);
      src_dir : constant String := host_localbase & "/share/mk";
      tgt_dir : constant String := smount & root_localbase & "/share/mk";
   begin
      return copy_directory_contents (src_dir, tgt_dir, "*.mk");
   end host_pkgsrc_mk_install;


   ---------------------------------
   --  host_pkgsrc_bmake_install  --
   ---------------------------------
   function host_pkgsrc_bmake_install (id : builders) return Boolean
   is
      smount      : constant String := get_slave_mount (id);
      host_bmake  : constant String := host_localbase & "/bin/bmake";
      slave_path  : constant String := smount & root_localbase & "/bin";
      slave_bmake : constant String := slave_path & "/bmake";
   begin
      if not AD.Exists (host_bmake) then
         return False;
      end if;
      AD.Create_Path (slave_path);
      AD.Copy_File (Source_Name => host_bmake,
                    Target_Name => slave_bmake);
      set_file_as_executable (slave_bmake);
      return True;
   exception
      when others => return False;
   end host_pkgsrc_bmake_install;


   ----------------------------------
   --  host_pkgsrc_digest_install  --
   ----------------------------------
   function host_pkgsrc_digest_install (id : builders) return Boolean
   is
      smount       : String := get_slave_mount (id);
      host_digest  : String := host_localbase & "/bin/digest";
      slave_digest : String := smount & root_localbase & "/bin/digest";
   begin
      if not AD.Exists (host_digest) then
         return False;
      end if;
      AD.Copy_File (Source_Name => host_digest,
                    Target_Name => slave_digest);
      set_file_as_executable (slave_digest);
      return True;
   exception
      when others => return False;
   end host_pkgsrc_digest_install;


   --------------------------------
   --  host_pkgsrc_pkg8_install  --
   --------------------------------
   function host_pkgsrc_pkg8_install (id : builders) return Boolean
   is
      smount      : constant String := get_slave_mount (id);
      host_pkgst  : constant String := host_localbase & "/sbin/pkg-static";
      host_admin  : constant String := host_localbase & "/sbin/pkgng_admin";
      slave_path  : constant String := smount & root_localbase & "/sbin";
      slave_pkg   : constant String := slave_path & "/pkg-static";
      slave_admin : constant String := slave_path & "/pkgng_admin";
   begin
      if not AD.Exists (host_pkgst) or else not AD.Exists (host_admin) then
         return False;
      end if;
      AD.Create_Path (slave_path);
      AD.Copy_File (Source_Name => host_pkgst,
                    Target_Name => slave_pkg);
      AD.Copy_File (Source_Name => host_admin,
                    Target_Name => slave_admin);
      set_file_as_executable (slave_pkg);
      set_file_as_executable (slave_admin);
      return True;
   exception
      when others => return False;
   end host_pkgsrc_pkg8_install;


   ----------------------------
   --  cache_port_variables  --
   ----------------------------
   procedure cache_port_variables (path_to_mm : String)
   is
      function create_OSRELEASE (OSRELEASE : String) return String;
      procedure write_if_defined (varname, value : String);
      OSVER    : constant String := get_osversion_from_param_header;
      ARCH     : constant String := get_arch_from_bourne_shell;
      portsdir : constant String := JT.USS (PM.configuration.dir_portsdir);
      fullport : constant String := portsdir & "/ports-mgmt/pkg";
      command  : constant String :=
        host_make & " __MAKE_CONF=/dev/null -C " & fullport &
        " USES=" & LAT.Quotation & "python compiler:features objc" & LAT.Quotation &
        " GNU_CONFIGURE=1 USE_JAVA=1 USE_LINUX=1" &
        " -VHAVE_COMPAT_IA32_KERN" &
        " -VCONFIGURE_MAX_CMD_LEN" &
        " -V_PERL5_FROM_BIN" &
        " -V_CCVERSION_921dbbb2" &
        " -V_CXXINTERNAL_acaad9ca" &
        " -V_OBJC_CCVERSION_921dbbb2" &
        " -VCC_OUTPUT_921dbbb2_58173849" &  --  c89
        " -VCC_OUTPUT_921dbbb2_9bdba57c" &  --  c99
        " -VCC_OUTPUT_921dbbb2_6a4fe7f5" &  --  c11
        " -VCC_OUTPUT_921dbbb2_6bcac02b" &  --  gnu89
        " -VCC_OUTPUT_921dbbb2_67d20829" &  --  gnu99
        " -VCC_OUTPUT_921dbbb2_bfa62e83" &  --  gnu11
        " -VCC_OUTPUT_921dbbb2_f0b4d593" &  --  c++98
        " -VCC_OUTPUT_921dbbb2_308abb44" &  --  c++0x
        " -VCC_OUTPUT_921dbbb2_f00456e5" &  --  c++11
        " -VCC_OUTPUT_921dbbb2_65ad290d" &  --  c++14
        " -VCC_OUTPUT_921dbbb2_b2657cc3" &  --  gnu++98
        " -VCC_OUTPUT_921dbbb2_380987f7";   --  gnu++11
      content  : JT.Text;
      topline  : JT.Text;
      status   : Integer;
      vconf    : TIO.File_Type;

      type result_range is range 1 .. 18;

      function create_OSRELEASE (OSRELEASE : String) return String
      is
         --  FreeBSD   OSVERSION is 6 or 7 digits
         --            OSVERSION [M]MNNPPP
         --  DragonFly OSVERSION is 6 digits
         --            OSVERSION MNNNPP
         --  NetBSD    OSVERSION is 9 or 10 digits
         --            OSVERSION [M]MNNrrPP00
         len : constant Natural := OSRELEASE'Length;
         OSR : constant String (1 .. len) := OSRELEASE;
         MM  : String (1 .. 2) := "  ";
         NN  : String (1 .. 2) := "  ";
         PP  : String (1 .. 2) := "  ";
         FL  : Natural;
         one_digit : Boolean := True;
      begin
         if len < 6 then
            return "1.0-SYNTH";
         end if;
         case platform_type is
            when dragonfly =>
               MM (2) := OSR (1);
               FL := 3;
            when freebsd =>
               if len > 6 then
                  one_digit := False;
               end if;
               FL := len - 4;
            when netbsd =>
               if len > 9 then
                  one_digit := False;
               end if;
               FL := len - 7;
               if OSR (FL + 4) = '0' then
                  PP (2) := OSR (FL + 5);
               else
                  PP := OSR (FL + 4 .. FL + 5);
               end if;
            when unknown => null;
            when linux | solaris => null;  --  TBD
         end case;
         if one_digit then
            MM (2) := OSR (1);
         else
            MM := OSR (1 .. 2);
         end if;
         if OSR (FL) = '0' then
            NN (2) := OSR (FL + 1);
         else
            NN := OSR (FL .. FL + 1);
         end if;
         case software_framework is
            when ports_collection =>
               return JT.trim (MM) & "." & JT.trim (NN) & "-SYNTH";
            when pkgsrc =>
               case platform_type is
                  when netbsd =>
                     return JT.trim (MM) & "." & JT.trim (NN) & "." &
                       JT.trim (PP);
                  when others =>
                     return JT.trim (MM) & "." & JT.trim (NN);
               end case;
         end case;
      end create_OSRELEASE;

      procedure write_if_defined (varname, value : String) is
      begin
         if value /= "" then
            TIO.Put_Line (vconf, varname & "=" & value);
         end if;
      end write_if_defined;

      release : constant String := create_OSRELEASE (OSVER);

   begin
      builder_env := JT.blank;

      TIO.Create (File => vconf,
                  Mode => TIO.Out_File,
                  Name => path_to_mm & "/varcache.conf");

      --  framework specific parts
      case software_framework is
         when ports_collection =>
            content := Unix.piped_command (command, status);
            if status = 0 then
               for k in result_range loop
                  JT.nextline (lineblock => content, firstline => topline);
                  declare
                     value : constant String := JT.USS (topline);
                  begin
                     case k is
                        when  1 => TIO.Put_Line (vconf, "HAVE_COMPAT_IA32_KERN=" & value);
                        when  2 => TIO.Put_Line (vconf, "CONFIGURE_MAX_CMD_LEN=" & value);
                        when  3 => TIO.Put_Line (vconf, "_PERL5_FROM_BIN=" & value);
                        when  4 => write_if_defined ("_CCVERSION_921dbbb2", value);
                        when  5 => write_if_defined ("_CXXINTERNAL_acaad9ca", value);
                        when  6 => write_if_defined ("_OBJC_CCVERSION_921dbbb2", value);
                        when  7 => write_if_defined ("CC_OUTPUT_921dbbb2_58173849", value);
                        when  8 => write_if_defined ("CC_OUTPUT_921dbbb2_9bdba57c", value);
                        when  9 => write_if_defined ("CC_OUTPUT_921dbbb2_6a4fe7f5", value);
                        when 10 => write_if_defined ("CC_OUTPUT_921dbbb2_6bcac02b", value);
                        when 11 => write_if_defined ("CC_OUTPUT_921dbbb2_67d20829", value);
                        when 12 => write_if_defined ("CC_OUTPUT_921dbbb2_bfa62e83", value);
                        when 13 => write_if_defined ("CC_OUTPUT_921dbbb2_f0b4d593", value);
                        when 14 => write_if_defined ("CC_OUTPUT_921dbbb2_308abb44", value);
                        when 15 => write_if_defined ("CC_OUTPUT_921dbbb2_f00456e5", value);
                        when 16 => write_if_defined ("CC_OUTPUT_921dbbb2_65ad290d", value);
                        when 17 => write_if_defined ("CC_OUTPUT_921dbbb2_b2657cc3", value);
                        when 18 => write_if_defined ("CC_OUTPUT_921dbbb2_380987f7", value);
                     end case;
                  end;
               end loop;
            end if;
            TIO.Put_Line (vconf, "_ALTCCVERSION_921dbbb2=none");
            TIO.Put_Line (vconf, "_OBJC_ALTCCVERSION_921dbbb2=none");
            TIO.Put_Line (vconf, "_SMP_CPUS=" & JT.int2str (Integer (smp_cores)));
            TIO.Put_Line (vconf, "UID=0");
            TIO.Put_Line (vconf, "ARCH=" & ARCH);
            case platform_type is
               when freebsd =>
                  TIO.Put_Line (vconf, "OPSYS=FreeBSD");
                  TIO.Put_Line (vconf, "OSVERSION=" & OSVER);
               when dragonfly =>
                  TIO.Put_Line (vconf, "OPSYS=DragonFly");
                  TIO.Put_Line (vconf, "DFLYVERSION=" & OSVER);
                  TIO.Put_Line (vconf, "OSVERSION=9999999");
               when netbsd | linux | solaris => null;
               when unknown => null;
            end case;
            TIO.Put_Line (vconf, "OSREL=" & release (1 .. release'Last - 6));
            TIO.Put_Line (vconf, "_OSRELEASE=" & release);
            TIO.Put_Line (vconf, "PYTHONBASE=/usr/local");
            TIO.Put_Line (vconf, "_PKG_CHECKED=1");

         when pkgsrc =>
            TIO.Put_Line (vconf, "OS_VERSION= " & release);
            TIO.Put_Line (vconf, "HOST_MACHINE_ARCH= " & ARCH);
            case platform_type is
               when freebsd =>
                  TIO.Put_Line
                    (vconf,
                       "OPSYS= FreeBSD" & LAT.LF &
                       "LOWER_OPSYS= freebsd" & LAT.LF &
                       "MAKEFLAGS= OPSYS=FreeBSD");
               when dragonfly =>
                  TIO.Put_Line
                    (vconf,
                       "OPSYS= DragonFly" & LAT.LF &
                       "LOWER_OPSYS= dragonfly" & LAT.LF &
                       "MAKEFLAGS= OPSYS=DragonFly");
               when netbsd =>
                  TIO.Put_Line
                    (vconf,
                       "OPSYS= NetBSD" & LAT.LF &
                       "LOWER_OPSYS= netbsd" & LAT.LF &
                       "MAKEFLAGS= OPSYS=NetBSD");
               when linux =>
                  TIO.Put_Line
                    (vconf,
                       "OPSYS= Linux" & LAT.LF &
                       "LOWER_OPSYS= linux" & LAT.LF &
                       "MAKEFLAGS= OPSYS=Linux");
               when solaris =>
                  TIO.Put_Line
                    (vconf,
                       "OPSYS= SunOS" & LAT.LF &
                       "LOWER_OPSYS= solaris" & LAT.LF &
                       "MAKEFLAGS= OPSYS=SunOS");
               when unknown => null;
            end case;
            TIO.Put_Line
              (vconf,
                 "MAKEFLAGS+= OS_VERSION=" & release & LAT.LF &
                 "MAKEFLAGS+= HOST_MACHINE_ARCH=" & ARCH & LAT.LF &
                 "MAKEFLAGS+= _PKGSRCDIR=/xports");
      end case;
      TIO.Close (vconf);

      case platform_type is
         when freebsd =>
            JT.SU.Append (builder_env, "UNAME_s=FreeBSD " &
                            "UNAME_v=FreeBSD\ " & release);
         when dragonfly =>
            JT.SU.Append (builder_env, "UNAME_s=DragonFly " &
                            "UNAME_v=DragonFly\ " & release);
         when netbsd =>
            JT.SU.Append (builder_env, "UNAME_s=NetBSD " &
                            "UNAME_v=NetBSD\ " & release);
         when linux =>
            JT.SU.Append (builder_env, "UNAME_s=Linux " &
                            "UNAME_v=Linux\ " & release);
         when solaris =>
            JT.SU.Append (builder_env, "UNAME_s=SunOS " &
                            "UNAME_v=SunOS\ " & release);
         when unknown => null;
      end case;

      --  The last entry of builder_env must be a blank space
      JT.SU.Append (builder_env, " UNAME_p=" & ARCH);
      JT.SU.Append (builder_env, " UNAME_m=" & ARCH);
      JT.SU.Append (builder_env, " UNAME_r=" & release & " ");

   end cache_port_variables;


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
         NBVER  : constant String := "#define" & LAT.HT &
                                     "__NetBSD_Version__" & LAT.HT;
         BADVER : constant String := "#define __Unknown_version ";
      begin
         case platform_type is
            when freebsd   => return FBVER;
            when dragonfly => return DFVER;
            when netbsd    => return NBVER;
            when linux     => return BADVER; -- TBD
            when solaris   => return BADVER; -- TBD
            when unknown   => return BADVER;
         end case;
      end get_pattern;

      header  : TIO.File_Type;
      badres  : constant String := "100000";
      pattern : constant String := get_pattern;
      paramh  : constant String := JT.USS (PM.configuration.dir_system) &
                                   "/usr/include/sys/param.h";
   begin
      TIO.Open (File => header, Mode => TIO.In_File, Name => paramh);
      while not TIO.End_Of_File (header) loop
         declare
            Line : constant String := TIO.Get_Line (header);
         begin
            if JT.contains (Line, pattern) then
               declare
                  OSVER : constant String :=
                          JT.trim (JT.part_2 (Line, pattern));
                  len   : constant Natural := OSVER'Length;
                  final : Integer;
               begin
                  exit when len < 7;
                  TIO.Close (header);
                  final := OSVER'First + 5;
                  for x in final + 1 .. OSVER'Last loop
                     case OSVER (x) is
                        when '0' .. '9' => final := x;
                        when others => return OSVER (OSVER'First .. final);
                     end case;
                  end loop;
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

end Replicant.Platform;
