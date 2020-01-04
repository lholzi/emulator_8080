with Ada.Text_IO;
package body Emulator_8080.Processor is

   procedure NOP is
   begin
      null;--Ada.Text_IO.Put_Line("NOP");
   end NOP;


   procedure LXI_BxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Emulator_8080.Processor.Processor_Type) is
   begin
      Processor.B := Byte_3;
      Processor.C := Byte_2;
   end LXI_BxD16;

   procedure STAX_B(Processor : in out Processor_Type) is
      C : constant Byte_Pair_Type := (High_Order_Byte => Processor.C,
                                      Low_Order_Byte => Processor.B);
      BC : constant Address_Type := Convert_To_Address(C);
   begin
      Processor.RAM(BC) := Processor.A;
      Ada.Text_IO.Put_Line("STAX");
   end Stax_B;

   procedure INX_B(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("INX_B NOT YET IMPLEMENTED");
   end INX_B;

   procedure INR_B(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.B + 1;
   end INR_B;

   procedure DCR_B(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.B - 1;
   end DCR_B;

   procedure MVI_BxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.B := Byte_2;
   end MVI_BxD8;

   procedure RLC(Processor : in out Processor_Type) is
      use Interfaces;
      Tmp : constant Unsigned_8 := Unsigned_8(Processor.A);
      Prev_Bit_7 : constant Unsigned_8 := Shift_Right(Tmp, 7);
      Result : constant Unsigned_8 := Shift_Left(Tmp, 1) or Prev_Bit_7;
   begin
      --TODO SET CARRY?
      Processor.A := Register_Type(Tmp);
   end RLC;

   procedure DAD_B(Processor : in out Processor_Type) is
   begin

   end DAD_B;

   procedure Unimplemented_Instruction is
   begin
      null;--Ada.Text_IO.Put_Line("Not yet implemented");
   end Unimplemented_Instruction;
end Emulator_8080.Processor;
