with Ada.Text_IO;
with GNAT.Source_Info;
with GNAT.Current_Exception;
with Ada.Text_IO;

package body Emulator_8080.Processor is

   procedure Print_Exception(Throwing_Function, Exception_Cause : in String) is
   begin
      ADa.Text_IO.Put(Throwing_Function & " threw exception-> ");
      Ada.Text_IO.Put_Line(Exception_Cause);
   end Print_Exception;

   function Initialize(Rom : in Byte_Array_Type) return Processor_Type is
      Processor : Processor_Type;
      Counter : Address_Type := Processor.Memory'First;
   begin
      for I in Rom'Range loop
         Processor.Memory(Counter) := Rom(I);
         Counter := Counter + 1;
      end loop;
      Ada.Text_IO.Put_Line("Counter: " & Counter'Img);
      return Processor;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
         return Processor;
   end Initialize;

   procedure NOP is
   begin
      null;--Ada.Text_IO.Put_Line("NOP");
   end NOP;

   procedure LXI_BxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Processor_Type) is
   begin
      Processor.B := Byte_3;
      Processor.C := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LXI_BxD16;

   procedure STAX_B(Processor : in out Processor_Type) is
      C : constant Byte_Pair_Type := (High_Order_Byte => Processor.C,
                                      Low_Order_Byte => Processor.B);
      BC : constant Address_Type := Convert_To_Address(C);
   begin
      Processor.Memory(BC) := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Stax_B;

   procedure INX_B(Processor : in out Processor_Type) is
      use Interfaces;
      BC : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.B,
                                                         Low_Order_Byte  => Processor.C));
      Result : constant Concatenated_Register_Type := BC + 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(BC);
   begin
      Processor.B := Converted_Result.High_Order_Byte;
      Processor.C := Converted_Result.High_Order_Byte;
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
      Value : constant Byte_Type := Processor.Memory(Adress);
      --Adress : constant Address_Type := Convert
   begin
      Processor.A := Value;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LDAX_B;

   procedure DCX_B(Processor : in out Processor_Type) is
      use Interfaces;
      BC : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.B,
                                                         Low_Order_Byte  => Processor.C));
      Result : constant Concatenated_Register_Type := BC - 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.B := Converted_Result.High_Order_Byte;
      Processor.C := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCX_B;

   procedure INR_C(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.C + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_C;

   procedure DCR_C(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.C - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_C;

   procedure MVI_CxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.C := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_CxD8;


   procedure RRC(Processor : in out Processor_Type) is
      use Interfaces;
      Tmp : constant Unsigned_8 := Unsigned_8(Processor.A);
      Prev_Bit_0 : constant Unsigned_8 := Shift_Right(Shift_Left(Tmp, 7), 7);
      Result : constant Unsigned_8 := Shift_Left(Tmp, 1) or Prev_Bit_0;
   begin
      --TODO SET CARRY?
      Processor.A := Register_Type(Tmp);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RRC;


   procedure LXI_DxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Processor_Type) is
   begin
      Processor.D := Byte_3;
      Processor.E := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LXI_DxD16;

   procedure STAX_D(Processor : in out Processor_Type) is
      C : constant Byte_Pair_Type := (High_Order_Byte => Processor.D,
                                      Low_Order_Byte => Processor.E);
      DE : constant Address_Type := Convert_To_Address(C);
   begin
      Processor.Memory(DE) := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Stax_D;

   procedure INX_D(Processor : in out Processor_Type) is
      use Interfaces;
      DE : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.D,
                                                         Low_Order_Byte  => Processor.E));
      Result : constant Concatenated_Register_Type := DE + 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(DE);
   begin
      Processor.D := Converted_Result.High_Order_Byte;
      Processor.E := Converted_Result.High_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INX_D;

   procedure INR_D(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.D + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_D;

   procedure DCR_D(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.D - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_D;

   procedure MVI_DxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.D := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_DxD8;

   procedure RAL(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("Procedure RAL not yet implemented");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RAL;

   procedure DAD_D(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                        Low_Order_Byte  => Processor.L));
      DE : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.D,
                                                        Low_Order_Byte  => Processor.E));

      Result : constant Concatenated_Register_Type := HL + DE;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DAD_D;

   procedure LDAX_D(Processor : in out Processor_Type) is
      Adress_Byte_Pair : constant Byte_Pair_Type := (High_Order_Byte => Processor.D,
                                                     Low_Order_Byte  => Processor.E);
      Adress : constant Address_Type := Convert_To_Address(Adress_Byte_Pair);
      Value : constant Byte_Type := Processor.Memory(Adress);
      --Adress : constant Address_Type := Convert
   begin
      Processor.A := Value;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LDAX_D;

   procedure DCX_D(Processor : in out Processor_Type) is
      use Interfaces;
      DE : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.D,
                                                         Low_Order_Byte  => Processor.E));
      Result : constant Concatenated_Register_Type := DE - 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.D := Converted_Result.High_Order_Byte;
      Processor.E := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCX_D;

   procedure INR_E(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.E + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_E;

   procedure DCR_E(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.E - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_E;

   procedure MVI_ExD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.E := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_ExD8;

   procedure RAR(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("Procedure RAR not yet implemented");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RAR;

   procedure LXI_HxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Processor_Type) is
   begin
      Processor.H := Byte_3;
      Processor.L := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LXI_HxD16;

   procedure SHLD_Adr(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("Procedure SHLD_Adr not yet implemented");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SHLD_Adr;

   procedure INX_H(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                         Low_Order_Byte  => Processor.L));
      Result : constant Concatenated_Register_Type := HL + 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(HL);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.High_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INX_H;

   procedure INR_H(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.H + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_H;

   procedure DCR_H(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.H - 1;
   exception
     when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_H;

   procedure MVI_HxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.H := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_HxD8;

   procedure Unimplemented_Instruction is
   begin
      null;--Ada.Text_IO.Put_Line("Not yet implemented");
   exception
     when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Unimplemented_Instruction;

end Emulator_8080.Processor;
