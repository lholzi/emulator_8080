with Ada.Text_IO;
with GNAT.Source_Info;
with GNAT.Current_Exception;
with Ada.Text_IO;

package body Emulator_8080.Processor is

   procedure Print_Exception(Throwing_Function, Exception_Cause : in String) is
   begin
      Ada.Text_IO.Put(Throwing_Function & " threw exception -> ");
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

   procedure Set_Zero_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type) is
      use Interfaces;
   begin
      if (Value and 16#ff#) = 0 then
         Processor.Zero_Flag := Set;
      else
         Processor.Zero_Flag := Not_Set;
      end if;
   end Set_Zero_Flag_If_Applicable;

   procedure Set_Sign_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type) is
      use Interfaces;
   begin
      if (Value and 16#80#) = 16#80# then
         Processor.Sign_Flag := Set;
      else
         Processor.Sign_Flag := Not_Set;
      end if;
   end Set_Sign_Flag_If_Applicable;

   procedure Set_Carry_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type) is
      use Interfaces;
   begin
      if Value > 16#ff# then
         Processor.Carry_Flag := Set;
      else
         Processor.Carry_Flag := Not_Set;
      end if;
   end Set_Carry_Flag_If_Applicable;

   procedure Add(Summand : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_16 := Unsigned_16(Processor.A) + Unsigned_16(Summand);
   begin
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Add;

   procedure Add_With_Carry(Summand : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : Unsigned_16 := Unsigned_16(Processor.A) + Unsigned_16(Summand);
      Carry_Summand : Unsigned_16 := 0;
   begin
      if(Processor.Carry_Flag = Set) then Carry_Summand := 1; end if;
      Result := Result + Carry_Summand;
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Add_With_Carry;

   procedure Sub(Subtrahend : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_16 := Unsigned_16(Processor.A) - Unsigned_16(Subtrahend);
   begin
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Sub;

   procedure Sub_With_Carry(Subtrahend : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : Unsigned_16 := Unsigned_16(Processor.A) - Unsigned_16(Subtrahend);
      Carry_Subtrahend : Unsigned_16 := 0;
   begin
      if(Processor.Carry_Flag = Set) then Carry_Subtrahend := 1; end if;
      Result := Result - Carry_Subtrahend;
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Sub_With_Carry;

   procedure And_A(Value : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_16 := Unsigned_16(Processor.A) and Unsigned_16(Value);
   begin
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end And_A;

   procedure Xor_A(Value : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_16 := Unsigned_16(Processor.A) xor Unsigned_16(Value);
   begin
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Xor_A;

   procedure Or_A(Value : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_16 := Unsigned_16(Processor.A) or Unsigned_16(Value);
   begin
      Set_Zero_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Sign_Flag_If_Applicable(Value => Result, Processor => Processor);
      Set_Carry_Flag_If_Applicable(Value => Result, Processor => Processor);
      Processor.A := Register_Type(Result and 16#ff#);
   end Or_A;

   procedure Compare_A(Value : in Register_Type; Processor : in out Processor_Type) is
      use Interfaces;
      Result : constant Unsigned_8 := Unsigned_8(Processor.A) - Unsigned_8(Value);
   begin
      if Result = 0 then
         Processor.Zero_Flag := Set;
      end if;

      if (16#80# = (Result and 16#80#)) then
         Processor.Sign_Flag := Set;
      end if;
      --TODO PARITY CHECK
      if Processor.A < Value then
         Processor.Carry_Flag := Set;
      end if;
   end Compare_A;

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
      Processor.C := Converted_Result.Low_Order_Byte;
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
      Processor.L := Converted_Result.Low_Order_Byte;
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

   procedure DAA(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("Special function DAA");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DAA;

   procedure DAD_H(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                         Low_Order_Byte  => Processor.L));
      Result : constant Concatenated_Register_Type := HL + HL;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(HL);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DAD_H;

   procedure LHLD(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_2,
                                                                            Low_Order_Byte  => Byte_3));
      L_Value : constant Byte_Type := Processor.Memory(Address);
      H_Value : constant Byte_Type := Processor.Memory(Address + 1);
   begin
      Processor.L := L_Value;
      Processor.H := H_Value;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LHLD;

   procedure DCX_H(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                         Low_Order_Byte  => Processor.L));
      Result : constant Concatenated_Register_Type := HL - 1;
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCX_H;

   procedure INR_L(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.L + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_L;

   procedure DCR_L(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.L - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_L;

   procedure MVI_LxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type) is
   begin
      Processor.L := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_LxD8;

   procedure CMA(Processor : in out Processor_Type) is
      use Interfaces;
      A : constant Interfaces.Unsigned_8 := Interfaces.Unsigned_8(Processor.A);
   begin
      Processor.A := Byte_Type(A xor A);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMA;

   procedure LXI_SPxD16(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      Processor.Stack_Pointer := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_3,
                                                                          Low_Order_Byte  => Byte_2));
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LXI_SPxD16;

   procedure STA(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_3,
                                                                            Low_Order_Byte  => Byte_2));
   begin
      Processor.Memory(Address) := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end STA;

   procedure INX_SP(Processor : in out Processor_Type) is
      use Interfaces;
   begin
      Processor.Stack_Pointer := Processor.Stack_Pointer + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INX_SP;

   procedure INR_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.Memory(Address) + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_M;

   procedure DCR_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.Memory(Address) - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_M;

   procedure MVI_MxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_MxD8;

   procedure STC(Processor : in out Processor_Type) is
   begin
      Processor.Carry_Flag := Set;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end STC;

   procedure DAD_SP(Processor : in out Processor_Type) is
      use Interfaces;
      HL : constant Concatenated_Register_Type :=
        Convert_To_Concatenated_Register(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                        Low_Order_Byte  => Processor.L));

      Result : constant Concatenated_Register_Type := HL + Unsigned_16(Processor.Stack_Pointer);
      Converted_Result : constant Byte_Pair_Type := Convert_To_Byte_Pair(Result);
   begin
      Processor.H := Converted_Result.High_Order_Byte;
      Processor.L := Converted_Result.Low_Order_Byte;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DAD_SP;

   procedure LDA(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_3,
                                                                            Low_Order_Byte  => Byte_2));
   begin
      Processor.A := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end LDA;

   procedure DCX_SP(Processor : in out Processor_Type) is
      use Interfaces;
   begin
      Processor.Stack_Pointer := Processor.Stack_Pointer - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCX_SP;

   procedure INR_A(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.A + 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end INR_A;

   procedure DCR_A(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.A - 1;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end DCR_A;

   procedure MVI_AxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      Processor.A := Byte_2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MVI_AxD8;

   procedure CMC(Processor : in out Processor_Type) is
   begin
      if Processor.Carry_Flag = Set then
         Processor.Carry_Flag := Not_Set;
      else
         Processor.Carry_Flag := Set;
      end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMC;

   procedure MOV_BxB(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxB;

   procedure MOV_BxC(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxC;

     procedure MOV_BxD(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxD;

   procedure MOV_BxE(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxE;

   procedure MOV_BxH(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxH;

     procedure MOV_BxL(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxL;

   procedure MOV_BxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.B := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxM;

   procedure MOV_BxA(Processor : in out Processor_Type) is
   begin
      Processor.B := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_BxA;

   procedure MOV_CxB(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxB;

   procedure MOV_CxC(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxC;

     procedure MOV_CxD(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxD;

   procedure MOV_CxE(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxE;

   procedure MOV_CxH(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxH;

     procedure MOV_CxL(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxL;

   procedure MOV_CxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.C := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxM;

   procedure MOV_CxA(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_CxA;

   procedure MOV_DxB(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxB;

   procedure MOV_DxC(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxC;

   procedure MOV_DxD(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxD;

   procedure MOV_DxE(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxE;

   procedure MOV_DxH(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxH;

   procedure MOV_DxL(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxL;

   procedure MOV_DxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.D := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxM;

   procedure MOV_DxA(Processor : in out Processor_Type) is
   begin
      Processor.D := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_DxA;

   procedure MOV_ExB(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExB;

   procedure MOV_ExC(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExC;

   procedure MOV_ExD(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExD;

   procedure MOV_ExE(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExE;

   procedure MOV_ExH(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExH;

   procedure MOV_ExL(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExL;

   procedure MOV_ExM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.E := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExM;

   procedure MOV_ExA(Processor : in out Processor_Type) is
   begin
      Processor.E := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_ExA;

   procedure MOV_HxB(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxB;

   procedure MOV_HxC(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxC;

   procedure MOV_HxD(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxD;

   procedure MOV_HxE(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxE;

   procedure MOV_HxH(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxH;

   procedure MOV_HxL(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxL;

   procedure MOV_HxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.H := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxM;

   procedure MOV_HxA(Processor : in out Processor_Type) is
   begin
      Processor.H := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_HxA;

   procedure MOV_LxB(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxB;

   procedure MOV_LxC(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxC;

   procedure MOV_LxD(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxD;

   procedure MOV_LxE(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxE;

   procedure MOV_LxH(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxH;

   procedure MOV_LxL(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxL;

   procedure MOV_LxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.L := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxM;

   procedure MOV_LxA(Processor : in out Processor_Type) is
   begin
      Processor.L := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_LxA;

   procedure MOV_MxB(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxB;

      procedure MOV_MxC(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxC;

   procedure MOV_MxD(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxD;

   procedure MOV_MxE(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxE;

   procedure MOV_MxH(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxH;

   procedure MOV_MxL(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxL;

   procedure HLT(Processor : in out Processor_Type) is
   begin
      Ada.Text_IO.Put_Line("Special function HLT");
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end HLT;

   procedure MOV_MxA(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.Memory(Address) := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_MxA;

   procedure MOV_AxB(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.B;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxB;

   procedure MOV_AxC(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.C;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxC;

   procedure MOV_AxD(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.D;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxD;

   procedure MOV_AxE(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.E;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxE;

   procedure MOV_AxH(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.H;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxH;

   procedure MOV_AxL(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.L;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxL;

   procedure MOV_AxM(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Processor.A := Processor.Memory(Address);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxM;

   procedure MOV_AxA(Processor : in out Processor_Type) is
   begin
      Processor.A := Processor.A;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end MOV_AxA;

   procedure ADD_B(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.B,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_B;

   procedure ADD_C(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.C,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_C;

   procedure ADD_D(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.D,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_D;

   procedure ADD_E(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.E,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_E;

   procedure ADD_H(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.H,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_H;

   procedure ADD_L(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.L,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_L;

   procedure ADD_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Add(Summand   => Processor.Memory(Address),
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_M;

   procedure ADD_A(Processor : in out Processor_Type) is
   begin
      Add(Summand   => Processor.A,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADD_A;

   procedure ADC_B(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.B,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_B;

   procedure ADC_C(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.C,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_C;

   procedure ADC_D(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.D,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_D;

   procedure ADC_E(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.E,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_E;

   procedure ADC_H(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.H,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_H;

   procedure ADC_L(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.L,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_L;

   procedure ADC_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Add_With_Carry(Summand   => Processor.Memory(Address),
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_M;

   procedure ADC_A(Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Processor.A,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADC_A;

   procedure SUB_B(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.B,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_B;

   procedure SUB_C(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.C,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_C;

   procedure SUB_D(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.D,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_D;

   procedure SUB_E(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.E,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_E;

   procedure SUB_H(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.H,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_H;

   procedure SUB_L(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.L,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_L;

   procedure SUB_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Sub(Subtrahend => Processor.Memory(Address),
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_M;

   procedure SUB_A(Processor : in out Processor_Type) is
   begin
      Sub(Subtrahend => Processor.A,
          Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SUB_A;

   procedure SBB_B(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.B,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_B;

   procedure SBB_C(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.C,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_C;

   procedure SBB_D(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.D,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_D;

   procedure SBB_E(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.E,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_E;

   procedure SBB_H(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.H,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_H;

   procedure SBB_L(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.L,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_L;

   procedure SBB_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Sub_With_Carry(Subtrahend => Processor.Memory(Address),
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_M;

   procedure SBB_A(Processor : in out Processor_Type) is
   begin
      Sub_With_Carry(Subtrahend => Processor.A,
                     Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end SBB_A;

   procedure ANA_B(Processor : in out Processor_Type) is
   begin
      And_A(Value      => Processor.B,
            Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_B;

   procedure ANA_C(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.C,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_C;

   procedure ANA_D(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.D,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_D;

   procedure ANA_E(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.E,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_E;

   procedure ANA_H(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.H,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_H;

   procedure ANA_L(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.L,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_L;

   procedure ANA_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      And_A(Value     => Processor.Memory(Address),
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_M;

   procedure ANA_A(Processor : in out Processor_Type) is
   begin
      And_A(Value     => Processor.A,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ANA_A;

   procedure XRA_B(Processor : in out Processor_Type) is
   begin
      Xor_A(Value      => Processor.B,
            Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_B;

   procedure XRA_C(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.C,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_C;

   procedure XRA_D(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.D,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_D;

   procedure XRA_E(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.E,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_E;

   procedure XRA_H(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.H,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_H;

   procedure XRA_L(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.L,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_L;

   procedure XRA_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Xor_A(Value     => Processor.Memory(Address),
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_M;

   procedure XRA_A(Processor : in out Processor_Type) is
   begin
      Xor_A(Value     => Processor.A,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end XRA_A;

   procedure ORA_B(Processor : in out Processor_Type) is
   begin
      Or_A(Value      => Processor.B,
           Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_B;

   procedure ORA_C(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.C,
           Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_C;

   procedure ORA_D(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.D,
           Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_D;

   procedure ORA_E(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.E,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_E;

   procedure ORA_H(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.H,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_H;

   procedure ORA_L(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.L,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_L;

   procedure ORA_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Or_A(Value     => Processor.Memory(Address),
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_M;

   procedure ORA_A(Processor : in out Processor_Type) is
   begin
      Or_A(Value     => Processor.A,
            Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ORA_A;

   procedure CMP_B(Processor : in out Processor_Type) is
   begin
      Compare_A(Value      => Processor.B,
                Processor  => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_B;

   procedure CMP_C(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.C,
                Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_C;

   procedure CMP_D(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.D,
                Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_D;

   procedure CMP_E(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.E,
                Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_E;

   procedure CMP_H(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.H,
               Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_H;

   procedure CMP_L(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.L,
                Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_L;

   procedure CMP_M(Processor : in out Processor_Type) is
      Address : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.H,
                                                                            Low_Order_Byte  => Processor.L));
   begin
      Or_A(Value     => Processor.Memory(Address),
           Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_M;

   procedure CMP_A(Processor : in out Processor_Type) is
   begin
      Compare_A(Value     => Processor.A,
                Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CMP_A;

   procedure RNZ(Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Not_Set then
         RET(Processor);
      end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RNZ;

   procedure POP_B(Processor : in out Processor_Type) is
   begin
      Processor.C := Processor.Memory(Processor.Stack_Pointer);
      Processor.B := Processor.Memory(Processor.Stack_Pointer + 1);
      Processor.Stack_Pointer := Processor.Stack_Pointer + 2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end POP_B;

   procedure JNZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Not_Set then
         JMP(Byte_2    => Byte_2,
             Byte_3    => Byte_3,
             Processor => Processor);
       end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end JNZ;

   procedure JMP(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
      PC : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_3,
                                                                            Low_Order_Byte  => Byte_2));
   begin
      Ada.Text_IO.Put_Line("JMP REACHED:" & PC'Img);
      Processor.Program_Counter := PC;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end JMP;

   procedure CNZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Not_Set then
         CALL(Byte_2    => Byte_2,
              Byte_3    => Byte_3,
              Processor => Processor);
      end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CNZ;

   procedure PUSH_B(Processor : in out Processor_Type) is
   begin
      Processor.Memory(Processor.Stack_Pointer - 2) := Processor.C;
      Processor.Memory(Processor.Stack_Pointer - 1) := Processor.B;
      Processor.Stack_Pointer := Processor.Stack_Pointer - 2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end PUSH_B;

   procedure ADI_D8(Byte_2 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      Add(Summand   => Byte_2,
          Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ADI_D8;

   procedure RST_0(Processor : in out Processor_Type) is
   begin
      CALL(Byte_2    => 0,
           Byte_3    => 0,
           Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RST_0;

   procedure RZ(Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Set then
         RET(Processor);
       end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RZ;

   procedure RET(Processor : in out Processor_Type) is
      use Interfaces;
      PC : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Processor.Memory(Processor.Stack_Pointer + 1),
                                                                       Low_Order_Byte  => Processor.Memory(Processor.Stack_Pointer)));
   begin
      Processor.Program_Counter := PC;
      Processor.Stack_Pointer := Processor.Stack_Pointer + 2;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RET;

   procedure JZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Set then
         JMP(Byte_2    => Byte_2,
             Byte_3    => Byte_3,
             Processor => Processor);
       end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end JZ;

   procedure CZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      if Processor.Zero_Flag = Set then
         CALL(Byte_2    => Byte_2,
             Byte_3    => Byte_3,
             Processor => Processor);
       end if;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CZ;

   procedure CALL(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type) is
      SP_Values : constant Byte_Pair_Type := Convert_To_Byte_Pair(Processor.Stack_Pointer);
      PC : constant Address_Type := Convert_To_Address(Byte_Pair_Type'(High_Order_Byte => Byte_3,
                                                                            Low_Order_Byte  => Byte_2));
   begin
      Processor.Memory(Processor.Stack_Pointer - 1) := SP_Values.High_Order_Byte;
      Processor.Memory(Processor.Stack_Pointer - 2) := SP_Values.Low_Order_Byte;
      Processor.Stack_Pointer := Processor.Stack_Pointer - 2;
      Processor.Program_Counter := PC;
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end CALL;

   procedure ACI_D8(Byte_2 : in Byte_Type; Processor : in out Processor_Type) is
   begin
      Add_With_Carry(Summand   => Byte_2,
                     Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end ACI_D8;

   procedure RST_1(Processor : in out Processor_Type) is
   begin
      CALL(Byte_2    => 0,
           Byte_3    => 0,
           Processor => Processor);
   exception
      when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end RST_1;

   procedure Unimplemented_Instruction is
   begin
      null;--Ada.Text_IO.Put_Line("Not yet implemented");
   exception
     when others =>
         Print_Exception(Throwing_Function => GNAT.Source_Info.Enclosing_Entity,
                         Exception_Cause   => GNAT.Current_Exception.Exception_Information);
   end Unimplemented_Instruction;

end Emulator_8080.Processor;
