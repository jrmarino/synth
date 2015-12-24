--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   synth_version_major : constant := 0;
   synth_version_minor : constant := 9;
   synth_version_point : constant := 0;

   jobs_per_cpu  : constant := 2;

   type cpu_range is range 1 .. 32;
   type scanners  is range 1 .. cpu_range'Last;
   type builders  is range 1 .. cpu_range'Last * jobs_per_cpu;

end Definitions;
