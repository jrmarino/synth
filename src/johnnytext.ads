--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Unbounded;
with Ada.Strings;

package JohnnyText is

   package AS  renames Ada.Strings;
   package SU  renames Ada.Strings.Unbounded;
   subtype Text is SU.Unbounded_String;
   blank : constant Text := SU.Null_Unbounded_String;

   --  converters : Text <==> String
   function USS (US : Text)   return String;
   function SUS (S  : String) return Text;

   --  True if the string is zero length
   function IsBlank (US : Text)   return Boolean;
   function IsBlank (S  : String) return Boolean;

   --  True if strings are identical
   function equivalent (A, B : Text) return Boolean;
   function equivalent (A : Text; B : String) return Boolean;

   --  Trim both sides
   function trim (US : Text) return Text;
   function trim (S : String) return String;

   --  unpadded numeric image
   function int2str  (A : Integer) return String;
   function int2text (A : Integer) return Text;

   --  convert boolean to lowercase string
   function bool2str  (A : Boolean) return String;
   function bool2text (A : Boolean) return Text;

end JohnnyText;
