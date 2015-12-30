--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with JohnnyText;

package body Display is

   package JT renames JohnnyText;

   ----------------------
   --  launch_monitor  --
   ----------------------
   function launch_monitor (num_builders : builders) return Boolean is
   begin
      TIC.Init_Screen;
      TIC.Set_Echo_Mode (False);
      TIC.Set_Raw_Mode (True);
      TIC.Set_Cbreak_Mode (True);
      if not TIC.Has_Colors then
         return False;
      end if;

      TIC.Start_Color;
      TIC.Init_Pair (TIC.Color_Pair (1), TIC.White,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (2), TIC.Green,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (3), TIC.Red,    TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (4), TIC.Yellow, TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (5), TIC.Black,  TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (6), TIC.Cyan,   TIC.Black);
      TIC.Init_Pair (TIC.Color_Pair (7), TIC.White,  TIC.Blue);

      c_standard    := TIC.Color_Pair (1);
      c_success     := TIC.Color_Pair (2);
      c_failure     := TIC.Color_Pair (3);
      c_ignored     := TIC.Color_Pair (4);
      c_skipped     := TIC.Color_Pair (5);
      c_sumlabel    := TIC.Color_Pair (6);
      c_builderbar  := TIC.Color_Pair (7);

      launch_summary_zone;
      launch_builders_zone (num_builders);
      launch_actions_zone (num_builders);
      return True;
   end launch_monitor;


   -------------------------
   --  terminate_monitor  --
   -------------------------
   procedure terminate_monitor is
   begin
      TIC.Delete (Win => zone_summary);
      TIC.Delete (Win => zone_builders);
      TIC.Delete (Win => zone_actions);
      TIC.End_Windows;
   end terminate_monitor;


   ---------------------------
   --  launch_summary_zone  --
   ---------------------------
   procedure launch_summary_zone is
   begin
      zone_summary := TIC.Create (
                      Number_Of_Lines       => 2,
                      Number_Of_Columns     => 80,
                      First_Line_Position   => 0,
                      First_Column_Position => 0);
   end launch_summary_zone;


   ----------------------------
   --  launch_builders_zone  --
   ----------------------------
   procedure launch_builders_zone (num_builders : builders)
   is
      hghtint : constant Integer := 2 + Integer (num_builders);
      height  : constant TIC.Line_Position := TIC.Line_Position (hghtint);
   begin
      zone_builders := TIC.Create (Number_Of_Lines       => height,
                                   Number_Of_Columns     => 80,
                                   First_Line_Position   => 2,
                                   First_Column_Position => 0);
   end launch_builders_zone;


   ---------------------------
   --  launch_actions_zone  --
   ---------------------------
   procedure launch_actions_zone (num_builders : builders)
   is
      consumed   : constant Integer := Integer (num_builders) + 2 + 2;
      difference : constant Integer := 0 - consumed;
      viewheight : constant TIC.Line_Position := inc (TIC.Lines, difference);
      viewpos    : constant TIC.Line_Position := TIC.Line_Position (consumed);
   begin
      zone_actions := TIC.Create (Number_Of_Lines       => viewheight,
                                  Number_Of_Columns     => 80,
                                  First_Line_Position   => viewpos,
                                  First_Column_Position => 0);
   end launch_actions_zone;


   -----------
   --  inc  --
   -----------
   function inc (X : TIC.Line_Position; by : Integer) return TIC.Line_Position
   is
      use type TIC.Line_Position;
   begin
      return X + TIC.Line_Position (by);
   end inc;


   -----------------
   --  summarize  --
   -----------------
   procedure summarize (data : summary_rec)
   is
      function pad (S : String; amount : Positive := 5) return String;
      function fmtpc (f : Float; percent : Boolean) return String;
      line1 : String := "Total 00000   Built 00000  Ignored 00000  " &
                        "Load  0.00  Pkg/hour 0000  Elapsed";
      line2 : String := " Left 00000  Failed 00000  skipped 00000  " &
                        "swap  0.0%   Impulse 0000  00:00:00";
      remaining : constant Integer := data.Initially - data.Built -
        data.Failed - data.Ignored - data.Skipped;

      function pad (S : String; amount : Positive := 5) return String
      is
         result : String (1 .. amount) := (others => ' ');
         slen   : constant Natural := S'Length;
         start  : constant Natural := 1 + amount - slen;
      begin
         result (start .. amount) := S;
         return result;
      end pad;
      function fmtpc (f : Float; percent : Boolean) return String
      is
         type loadtype is delta 0.01 digits 3;
         result : String (1 .. 5) := (others => ' ');
         raw1   : constant loadtype := loadtype (f);
         raw2   : constant String := raw1'Img;
         rlen   : constant Natural := raw2'Length;
         start  : constant Natural := 6 - rlen;
      begin
         result (start .. 5) := raw2;
         if percent then
            result (5) := '%';
         end if;
         return result;
      end fmtpc;

      L1F1 : constant String := pad (JT.int2str (data.Initially));
      L1F2 : constant String := pad (JT.int2str (data.Built));
      L1F3 : constant String := pad (JT.int2str (data.Ignored));
      L2F1 : constant String := pad (JT.int2str (remaining));
      L2F2 : constant String := pad (JT.int2str (data.Failed));
      L2F3 : constant String := pad (JT.int2str (data.Skipped));
      L2F5 : constant String := pad (JT.int2str (data.impulse), 4);
      L1F4 : constant String := fmtpc (data.load, False);
      L2F4 : constant String := fmtpc (data.swap, True);

   begin
      line1  (7 .. 11) := L1F1;
      line1 (21 .. 25) := L1F2;
      line1 (36 .. 40) := L1F3;
      line2 (48 .. 52) := L1F4;
      line2  (7 .. 11) := L2F1;
      line2 (21 .. 25) := L2F2;
      line2 (36 .. 40) := L2F3;
      line2 (48 .. 52) := L2F4;
      line2 (64 .. 67) := L2F5;
      line2 (70 .. 77) := data.elapsed;

      TIC.Set_Character_Attributes (Win => zone_summary,
                                    Attr => TIC.Normal_Video,
                                    Color => TIC.Color_Pair (c_standard));

      TIC.Move_Cursor (Win => zone_summary, Line => 0, Column => 0);
      TIC.Add (Win => zone_summary, Str => line1);
      TIC.Move_Cursor (Win => zone_summary, Line => 1, Column => 0);
      TIC.Add (Win => zone_summary, Str => line2);
      TIC.Refresh;
   end summarize;


end Display;
