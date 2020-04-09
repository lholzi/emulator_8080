with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;
with Emulator_8080.Processor;
with Emulator_8080.Disassembler;

procedure Emulator_Main is
   package Rom_IO is new Ada.Sequential_IO(Element_Type => Emulator_8080.Byte_Type);
   package Rom_Text_IO is new Ada.Text_IO.Integer_IO(Emulator_8080.Byte_Type);

   Rom_Directory_Path : constant String := "/home/lholzi/Schreibtisch/Projects/emulator_8080/rom/";
   Rom_File_Size : constant Natural :=
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.h")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.g")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.f")) +
       Natural(Ada.Directories.Size(Rom_Directory_Path & "invaders.e"));

   Rom_File : Rom_IO.File_Type;
   Rom_File_Content : Emulator_8080.Byte_Array_Type( 0 .. Rom_File_Size);
   Processor : Emulator_8080.Processor.Processor_Type;
   Rom_Byte_Index : Natural := 0;

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

begin
   Read_Rom_File(Rom_Directory_Path & "invaders.h");
   Read_Rom_File(Rom_Directory_Path & "invaders.g");
   Read_Rom_File(Rom_Directory_Path & "invaders.f");
   Read_Rom_File(Rom_Directory_Path & "invaders.e");
   Ada.Text_IO.Put_Line("File size: " & Rom_File_Size'Img);
   Ada.Text_IO.Put_Line("last Index: " & Rom_Byte_Index'Img);
   Emulator_8080.Disassembler.Read_Rom(Rom_Bytes => Rom_File_Content,
                                       Processor => Processor);
end Emulator_Main;
