--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

package Definitions is

   pragma Pure;

   jobs_per_cpu : constant := 2;

   type cpu_range is range 1 .. 32;
   type scanners  is range 1 .. cpu_range'Last;
   type builders  is range 1 .. cpu_range'Last * jobs_per_cpu;

end Definitions;
