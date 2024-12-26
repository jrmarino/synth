--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   synth_version_major : constant String := "3";
   synth_version_minor : constant String := "05";
   copyright_years     : constant String := "2015-2024";
   host_localbase      : constant String := "/usr/local";
   host_make           : constant String := "/usr/bin/make";
   host_pkg8           : constant String := host_localbase & "/sbin/pkg";
   host_bmake          : constant String := host_localbase & "/bin/bmake";
   host_make_program   : constant String := host_make;
   chroot_make         : constant String := "/usr/bin/make";
   chroot_bmake        : constant String := "/usr/pkg/bin/bmake -m /usr/pkg/share/mk";
   chroot_make_program : constant String := chroot_make;
   jobs_per_cpu        : constant := 2;

   type cpu_range is range 1 .. 64;
   type scanners  is range cpu_range'First .. cpu_range'Last;
   type builders  is range cpu_range'First .. cpu_range'Last * jobs_per_cpu;
   type package_system is (ports_collection, pkgsrc);

   software_framework : constant package_system := ports_collection;

   --  Notes for tailoring Synth.  Use sed to:
   --  1. Modify host_localbase to value of LOCALBASE
   --  2. Change software_framework to "pkgsrc" for pkgsrc version
   --  3. Change host_make_program to "host_bmake" for Non-NetBSD pkgsrc platforms
   --  4. Change chroot_make_program to "chroot_bmake" for pkgsrc version
   --  5. On replicant.ads, change "/usr/local" to "/usr/pkg" on pkgsrc

end Definitions;
