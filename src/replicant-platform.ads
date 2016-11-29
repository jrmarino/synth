--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Replicant.Platform is

   --  returns platform-specific df command
   function df_command return String;

   --  platform-specific version of file command
   function file_type_command return String;

   --  Calculate both types of package ABI as a function of platform
   function determine_package_architecture return package_abi;

   --  Return platform-specific command for swapinfo
   function swapinfo_command return String;

   --  Return load averages (platform specific)
   function get_instant_load return Float;
   function get_5_minute_load return Float;

   --  Return value of ncpu (from syctl query)
   function get_number_cpus return Positive;

   --  Return true if file is executable (platform-specific)
   function file_is_executable (filename : String) return Boolean;

   --  Return true if file is dynamically linked (platform-specific)
   function dynamically_linked (base, filename : String) return Boolean;

   --  In order to do scanning in a clean environment prior to the true build
   --  Returns True on success
   function standalone_pkg8_install (id : builders) return Boolean;

   --  Required for building first pkg(8) and bmake(8) for pkgsrc
   --  They are just copies of hosts versions
   function host_pkgsrc_mk_install (id : builders) return Boolean;
   function host_pkgsrc_bmake_install (id : builders) return Boolean;
   function host_pkgsrc_digest_install (id : builders) return Boolean;
   function host_pkgsrc_pkg8_install (id : builders) return Boolean;

   --  Cache variables that spawn to get populated to extended make.conf
   procedure cache_port_variables (path_to_mm : String);

private

   function isolate_arch_from_file_type (fileinfo : String) return filearch;

   --  Derived from /usr/bin/file -b <slave>/bin/sh
   function get_arch_from_bourne_shell return String;

   --  Get OSVERSION from <sys/param.h>
   function get_osversion_from_param_header return String;

   --  common logic for instant and 5-minute load average
   function load_core (instant_load : Boolean) return Float;

   --  equivalent to chmod 755 <fullpath>
   procedure set_file_as_executable (fullpath : String);

end Replicant.Platform;
