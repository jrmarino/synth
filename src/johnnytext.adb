--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Strings.Fixed;

package body JohnnyText is

   -----------
   --  USS  --
   -----------
   function USS (US : Text) return String is
   begin
         return SU.To_String (US);
   end USS;


   -----------
   --  SUS  --
   -----------
   function SUS (S : String) return Text is
   begin
      return SU.To_Unbounded_String (S);
   end SUS;


   -----------------
   --  IsBlank #1 --
   -----------------
   function IsBlank (US : Text)   return Boolean is
   begin
      return SU.Length (US) = 0;
   end IsBlank;


   -----------------
   --  IsBlank #2 --
   -----------------
   function IsBlank (S  : String) return Boolean is
   begin
      return S'Length = 0;
   end IsBlank;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A, B : Text) return Boolean
   is
      use type Text;
   begin
      return A = B;
   end equivalent;


   ------------------
   --  equivalent  --
   ------------------
   function equivalent (A : Text; B : String) return Boolean
   is
      AS : constant String := USS (A);
   begin
      return AS = B;
   end equivalent;


   --------------
   --  trim #1 --
   --------------
   function trim (US : Text) return Text is
   begin
      return SU.Trim (US, AS.Both);
   end trim;


   --------------
   --  trim #2 --
   --------------
   function trim (S : String) return String is
   begin
      return AS.Fixed.Trim (S, AS.Both);
   end trim;

   ---------------
   --  int2str  --
   ---------------
   function int2str  (A : Integer) return String
   is
      raw : constant String := A'Img;
      len : constant Natural := raw'Length;
   begin
      if A < 0 then
         return raw;
      else
         return raw (2 .. len);
      end if;
   end int2str;


   ----------------
   --  int2text  --
   ----------------
   function int2text (A : Integer) return Text is
   begin
      return SUS (int2str (A));
   end int2text;


   ----------------
   --  bool2str  --
   ----------------
   function bool2str  (A : Boolean) return String is
   begin
      if A then
         return "true";
      end if;
      return "false";
   end bool2str;


   -----------------
   --  bool2text  --
   -----------------
   function bool2text (A : Boolean) return Text is
   begin
      return SUS (bool2str (A));
   end bool2text;


   ----------------
   --  nextline  --
   ----------------
   procedure nextline (lineblock, firstline : out Text)
   is
      CR_loc : Natural;
      CR : constant String (1 .. 1) := (1 => Character'Val (10));
   begin
      CR_loc := SU.Index (Source => lineblock, Pattern => CR);
      if CR_loc = 0 then
         firstline := lineblock;
         return;
      end if;
      firstline := SUS (SU.Slice
                        (Source => lineblock, Low => 1, High => CR_loc - 1));
      SU.Delete (Source => lineblock, From => 1, Through => CR_loc);
   end nextline;


   --------------------
   --  contains  #1  --
   --------------------
   function contains (S : String; fragment : String) return Boolean is
   begin
      return (AS.Fixed.Index (Source => S, Pattern => fragment) > 0);
   end contains;


   --------------------
   --  contains  #2  --
   --------------------
   function contains (US : Text; fragment : String) return Boolean is
   begin
      return (SU.Index (Source => US, Pattern => fragment) > 0);
   end contains;


   --------------
   --  part_1  --
   --------------
   function part_1 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (S'First .. slash - 1);
   end part_1;


   --------------
   --  part_2  --
   --------------
   function part_2 (S : String; separator : String := "/") return String
   is
      slash : Integer := AS.Fixed.Index (S, separator);
   begin
      if slash = 0 then
         return S;
      end if;
      return S (slash + separator'Length .. S'Last);
   end part_2;


   ---------------
   --  replace  --
   ---------------
   function replace (S : String; reject, shiny : Character) return String
   is
      rejectstr : constant String (1 .. 1) := (1 => reject);
      focus     : constant Natural :=
                           AS.Fixed.Index (Source => S, Pattern => rejectstr);
      returnstr : String := S;
   begin
      if focus > 0 then
        returnstr (focus) := shiny;
      end if;
      return returnstr;
   end replace;


   ---------------
   --  zeropad  --
   ---------------
   function zeropad (N : Natural; places : Positive) return String
   is
      template : String (1 .. places) := (others => '0');
      myimage  : constant String := trim (N'Img);
      startpos : constant Natural := 1 + places - myimage'Length;
   begin
      template (startpos .. places) := myimage;
      return template;
   end zeropad;


   ------------------
   --  count_char  --
   ------------------
   function count_char (S : String; focus : Character) return Natural
   is
      result : Natural := 0;
   begin
      for x in S'Range loop
         if S (x) = focus then
            result := result + 1;
         end if;
      end loop;
      return result;
   end count_char;


   --------------------
   --  replace_char  --
   --------------------
   function replace_char (S : String; focus : Character; substring : String) return String
   is
      num_to_replace : constant Natural := count_char (S, focus);
   begin
      if num_to_replace = 0 then
         return S;
      end if;

      declare
         ssm1   : constant Natural := substring'Length - 1;
         strlen : constant Natural := S'Length + (num_to_replace * ssm1);
         product : String (1 .. strlen);
         ndx : Positive := 1;
      begin
         for x in S'Range loop
            if S (x) = focus then
               product (ndx .. ndx + ssm1) := substring;
               ndx := ndx + substring'Length;
            else
               product (ndx) := S (x);
               ndx := ndx + 1;
            end if;
         end loop;
         return product;
      end;
   end replace_char;

end JohnnyText;
