--  This file is covered by the Internet Software Consortium (ISC) License
--  Reference: ../License.txt

with Ada.Command_Line;

package body PortScan.Pilot is

   package CLI renames Ada.Command_Line;

   ---------------------
   --  store_origins  --
   ---------------------
   function store_origins return Boolean
   is
   begin
      if CLI.Argument_Count <= 1 then
         return False;
      end if;
      portlist.Clear;
      if CLI.Argument_Count = 2 then
         --  Check if this is a file
         if AD.Exists (CLI.Argument (2)) then
            return valid_file (CLI.Argument (2));
         end if;
         if valid_catport (catport => CLI.Argument (2)) then
            portlist.Insert (JT.SUS (CLI.Argument (2)), port_id (2));
            return True;
         else
            TIO.Put_Line (badport & CLI.Argument (2));
            return False;
         end if;
      end if;
      for k in 2 .. CLI.Argument_Count loop
         if valid_catport (catport => CLI.Argument (k)) then
            portlist.Insert (JT.SUS (CLI.Argument (k)), port_id (k));
         else
            TIO.Put_Line (badport & "'" & CLI.Argument (k) & "'" & k'Img);
            return False;
         end if;
      end loop;
      return True;
   end store_origins;


   ------------------
   --  valid_file  --
   ------------------
   function valid_file (path : String) return Boolean
   is
      handle : TIO.File_Type;
      good   : Boolean;
      total  : Natural := 0;
   begin
      TIO.Open (File => handle, Mode => TIO.In_File, Name => path);
      good := True;
      while not TIO.End_Of_File (handle) loop
         declare
            line : constant String := JT.trim (TIO.Get_Line (handle));
         begin
            if not JT.IsBlank (line) then
               if valid_catport (line) then
                  portlist.Insert (JT.SUS (line), port_id (total));
                  total := total + 1;
               else
                  TIO.Put_Line (badport & line);
                  good := False;
                  exit;
               end if;
            end if;
         end;
      end loop;
      TIO.Close (handle);
      return (total > 0) and then good;
   exception
      when others => return False;
   end valid_file;


   ---------------------
   --  valid_catport  --
   ---------------------
   function valid_catport (catport : String) return Boolean
   is
      use type AD.File_Kind;
   begin
      if JT.contains (catport, "/") then
         declare
            cat   : constant String := JT.part_1 (catport);
            port  : constant String := JT.part_2 (catport);
            path1 : constant String := JT.USS (PM.configuration.dir_portsdir) &
                                       "/" & cat;
            fpath : constant String := path1 & "/" & port;
            alpha : constant Character := cat (1);
         begin
            if not AD.Exists (path1) then
               return False;
            end if;

            if alpha in 'A' .. 'Z' then
               return False;
            end if;

            if JT.contains (port, "/") then
               return False;
            end if;

            if not AD.Exists (fpath) then
               return False;
            end if;

            if AD.Kind (fpath) = AD.Directory then
               return True;
            end if;
         end;
      end if;
      return False;
   end valid_catport;

end PortScan.Pilot;
