--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   synth_version_major : constant String := "0";
   synth_version_minor : constant String := "99";
   copyright_years     : constant String := "2015-2016";
   host_localbase      : constant String := "/usr/local";
   host_pkg8           : constant String := host_localbase & "/sbin/pkg";
   jobs_per_cpu        : constant := 2;

   type cpu_range is range 1 .. 32;
   type scanners  is range cpu_range'First .. cpu_range'Last;
   type builders  is range cpu_range'First .. cpu_range'Last * jobs_per_cpu;

end Definitions;
