with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;
with GNAT.Current_Exception;
with Emulator_8080.Processor;
with Emulator_8080.Disassembler;

procedure Emulator_Main is
   package Rom_IO is new Ada.Sequential_IO(Element_Type => Emulator_8080.Byte_Type);

   Rom_Directory_Path : constant String := Ada.Directories.Current_Directory & "/rom/";
   Rom_File_Size : constant Natural :=
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.h")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.g")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.f")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.e"));

   Rom_File : Rom_IO.File_Type;
   Rom_File_Content : Emulator_8080.Byte_Array_Type( 0 .. Rom_File_Size);
   Rom_Byte_Index : Natural := 0;
   Processor : Emulator_8080.Processor.Processor_Type;

   procedure Read_Rom_File(Rom_File_Path : in String) is
      Current_Rom_Byte : Emulator_8080.Byte_Type := 0;
   begin
      Rom_IO.Open(File => Rom_File, Mode => Rom_IO.In_File, Name => Rom_File_Path);
      while not Rom_IO.End_Of_File(Rom_File) loop
         Rom_IO.Read(File => Rom_File, Item => Current_Rom_Byte);
         Rom_File_Content(Rom_Byte_Index) := Current_Rom_Byte;
         Rom_Byte_Index := Rom_Byte_Index + 1;
      end loop;
      Rom_IO.Close(Rom_File);
   end Read_Rom_File;

   procedure Write_Invaders_Rom(Rom_File_Path : in String) is
   begin
      Rom_IO.Open(File => Rom_File, Mode => Rom_IO.Out_File, Name => Rom_File_Path);
      for I in Rom_File_Content'Range loop
         Rom_IO.Write(File => Rom_File, Item => Rom_File_Content(I));
      end loop;
      Rom_IO.Close(Rom_File);
   end Write_Invaders_Rom;

begin
   Ada.Text_IO.Put_Line("Starting emulator_8080.");
   Ada.Text_IO.Put_Line("Reading Rom...");
   Read_Rom_File(Rom_Directory_Path & "invaders.h");
   Read_Rom_File(Rom_Directory_Path & "invaders.g");
   Read_Rom_File(Rom_Directory_Path & "invaders.f");
   Read_Rom_File(Rom_Directory_Path & "invaders.e");

   Write_Invaders_Rom(Rom_Directory_Path & "invaders.rom");

   Ada.Text_IO.Put_Line("File read successfully.");
   Ada.Text_IO.Put_Line("--> File size:  " & Rom_File_Size'Img);
   Ada.Text_IO.Put_Line("--> last Index: " & Rom_Byte_Index'Img);

   Ada.Text_IO.Put_Line("Initializing CPU");
   Processor := Emulator_8080.Processor.Initialize(Rom_File_Content);
   Ada.Text_IO.Put_Line("Running emulation...");
   Emulator_8080.Disassembler.Read_Rom(Execution_Mode => Emulator_8080.Disassembler.Execute_And_Print,
                                       Processor      => Processor);
exception
   when others =>
      Ada.Text_IO.Put_Line("EXCEPTION CAUGHT IN MAIN");
      Ada.Text_IO.Put_Line(GNAT.Current_Exception.Exception_Information);
end Emulator_Main;
