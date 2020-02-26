with Ada.Text_IO;
with GNAT.Source_Info;
with GNAT.Current_Exception;


package body Emulator_8080.Processor is

   procedure Print_Exception(Throwing_Function, Exception_Cause : in String) is
   begin
      ADa.Text_IO.Put(Throwing_Function & " threw exception-> ");
      Ada.Text_IO.Put_Line(Exception_Cause);
   end Print_Exception;

   procedure NOP is
   begin
      null;--Ada.Text_IO.Put_Line("NOP");
   end NOP;


   procedure LXI_BxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Processor_Type) is
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
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Stax_B;

   procedure INX_B(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("INX_B NOT YET IMPLEMENTED");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INX_B;

   procedure INR_B(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.B + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_B;

   procedure DCR_B(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.B - 1;
   exception
     when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_B;

   procedure MVI_BxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.B := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_BxD8;

   procedure RLC(Processor : in out Processor_Type) is
      use Interfaces;
      Tmp : constant Unsigned_8 := Unsigned_8(Processor.A);
      Prev_Bit_7 : constant Unsigned_8 := Shift_Right(Tmp, 7);
      Result : constant Unsigned_8 := Shift_Left(Tmp, 1) or Prev_Bit_7;
   begin
      --TODO SET CARRY?
      Processor.A := Register_Type(Tmp);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RLC;

   procedure DAD_B(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                        Low_Order_Byte  => Processor.L));
      BC : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.B,
                                                        Low_Order_Byte  => Processor.C));

      Result : constant Concatenated_Register_Type := HL + BC;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DAD_B;

   procedure LDAX_B(Processor : in out Processor_Type) is
      Adress_Byte_Pair : constant Byte_Pair_Type := (High_Order_Byte => Processor.B,
                                                     Low_Order_Byte  => Processor.C);
      Adress : constant Address_Type := Convert_To_Address(Adress_Byte_Pair);
      Value : constant Byte_Type := Processor.RAM(Adress);
      --Adress : constant Address_Type := Convert
   begin
      Processor.A := Value;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LDAX_B;

   procedure Unimplemented_Instruction is
   begin
      null;--Ada.Text_IO.Put_Line("Not yet implemented");
   exception
     when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Unimplemented_Instruction;
end Emulator_8080.Processor;
