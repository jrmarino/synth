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

   --  Return first line of block of lines (line is removed from block)
   procedure nextline (lineblock, firstline : out Text);

   --  shorthand for index
   function contains (S : String; fragment : String) return Boolean;
   function contains (US : Text; fragment : String) return Boolean;

   --  Return half of a string split by separator
   function part_1 (S : String; separator : String := "/") return String;
   function part_2 (S : String; separator : String := "/") return String;

   --  Replace a single character with another single character (first found)
   function replace (S : String; reject, shiny : Character) return String;

   --  Numeric image with left-padded zeros
   function zeropad (N : Natural; places : Positive) return String;

    --  Returns number of instances of a given character in a given string
   function count_char (S : String; focus : Character) return Natural;

   --  Search entire string S for focus character and replace all instances with substring
   function replace_char (S : String; focus : Character; substring : String) return String;

end JohnnyText;
