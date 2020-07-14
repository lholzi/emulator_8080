private with Unchecked_Conversion;
with Interfaces;

package Emulator_8080.Processor is
   subtype Register_Type is Emulator_8080.Byte_Type;
   type Address_Type is new Natural range 0 .. 16#FFFF#;
   subtype Rom_Address_Type is Address_Type range 0 .. 16#1FFF#;
   type Flag_Type is (Not_Set, Set) with Size => 1;
   type Memory_Type is array (Address_Type) of Byte_Type;
   type Processor_Type is record
      A : Register_Type := 0;
      B : Register_Type := 0;
      C : Register_Type := 0;
      D : Register_Type := 0;
      E : Register_Type := 0;
      H : Register_Type := 0;
      L : Register_Type := 0;

      Sign_Flag : Flag_Type := Not_Set;
      Zero_Flag : Flag_Type := Not_Set;
      Parity_Flag : Flag_Type := Not_Set;
      Carry_Flag : Flag_Type := Not_Set;
      Auxillary_Carry : Flag_Type := Not_Set;

      Memory : Memory_Type := (others => 0);
      Program_Counter : Address_Type := 0;
      Stack_Pointer : Address_Type := Address_Type'Last;
   end record;

   function Initialize(Rom : in Byte_Array_Type) return Processor_Type;
   procedure NOP;
   procedure LXI_BxD16(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure STAX_B(Processor : in out Processor_Type);
   procedure INX_B(Processor : in out Processor_Type);
   procedure INR_B(Processor : in out Processor_Type);
   procedure DCR_B(Processor : in out Processor_Type);
   procedure MVI_BxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure RLC(Processor : in out Processor_Type);
   --
   procedure DAD_B(Processor : in out Processor_Type);
   procedure LDAX_B(Processor : in out Processor_Type);
   procedure DCX_B(Processor : in out Processor_Type);
   procedure INR_C(Processor : in out Processor_Type);
   procedure DCR_C(Processor : in out Processor_Type);
   procedure MVI_CxD8(Byte_2 : in Byte_Type; Processor: in out Processor_Type);
   procedure RRC(Processor : in out Processor_Type);
   --
   procedure LXI_DxD16(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure STAX_D(Processor : in out Processor_Type);
   procedure INX_D(Processor : in out Processor_Type);
   procedure INR_D(Processor : in out Processor_Type);
   procedure DCR_D(Processor : in out Processor_Type);
   procedure MVI_DxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure RAL(Processor : in out Processor_Type);
   --
   procedure DAD_D(Processor : in out Processor_Type);
   procedure LDAX_D(Processor : in out Processor_Type);
   procedure DCX_D(Processor : in out Processor_Type);
   procedure INR_E(Processor : in out Processor_Type);
   procedure DCR_E(Processor : in out Processor_Type);
   procedure MVI_ExD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure RAR(Processor : in out Processor_Type);
   --
   procedure LXI_HxD16(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure SHLD_Adr(Processor : in out Processor_Type);
   procedure INX_H(Processor : in out Processor_Type);
   procedure INR_H(Processor : in out Processor_Type);
   procedure DCR_H(Processor : in out Processor_Type);
   procedure MVI_HxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure DAA(Processor : in out Processor_Type);
   --
   procedure DAD_H(Processor : in out Processor_Type);
   procedure LHLD(Byte_2, Byte_3 : in Byte_Type; Processor: in out Processor_Type);
   procedure DCX_H(Processor : in out Processor_Type);
   procedure INR_L(Processor : in out Processor_Type);
   procedure DCR_L(Processor : in out Processor_Type);
   procedure MVI_LxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure CMA(Processor : in out Processor_Type);
   --
   procedure LXI_SPxD16(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure STA(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure INX_SP(Processor : in out Processor_Type);
   procedure INR_M(Processor : in out Processor_Type);
   procedure DCR_M(Processor : in out Processor_Type);
   procedure MVI_MxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure STC(Processor : in out Processor_Type);
   --
   procedure DAD_SP(Processor : in out Processor_Type);
   procedure LDA(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure DCX_SP(Processor : in out Processor_Type);
   procedure INR_A(Processor : in out Processor_Type);
   procedure DCR_A(Processor : in out Processor_Type);
   procedure MVI_AxD8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure CMC(Processor : in out Processor_Type);
   --
   procedure MOV_BxB(Processor : in out Processor_Type);
   procedure MOV_BxC(Processor : in out Processor_Type);
   procedure MOV_BxD(Processor : in out Processor_Type);
   procedure MOV_BxE(Processor : in out Processor_Type);
   procedure MOV_BxH(Processor : in out Processor_Type);
   procedure MOV_BxL(Processor : in out Processor_Type);
   procedure MOV_BxM(Processor : in out Processor_Type);
   procedure MOV_BxA(Processor : in out Processor_Type);
   --
   procedure MOV_CxB(Processor : in out Processor_Type);
   procedure MOV_CxC(Processor : in out Processor_Type);
   procedure MOV_CxD(Processor : in out Processor_Type);
   procedure MOV_CxE(Processor : in out Processor_Type);
   procedure MOV_CxH(Processor : in out Processor_Type);
   procedure MOV_CxL(Processor : in out Processor_Type);
   procedure MOV_CxM(Processor : in out Processor_Type);
   procedure MOV_CxA(Processor : in out Processor_Type);
   --
   procedure MOV_DxB(Processor : in out Processor_Type);
   procedure MOV_DxC(Processor : in out Processor_Type);
   procedure MOV_DxD(Processor : in out Processor_Type);
   procedure MOV_DxE(Processor : in out Processor_Type);
   procedure MOV_DxH(Processor : in out Processor_Type);
   procedure MOV_DxL(Processor : in out Processor_Type);
   procedure MOV_DxM(Processor : in out Processor_Type);
   procedure MOV_DxA(Processor : in out Processor_Type);
   --
   procedure MOV_ExB(Processor : in out Processor_Type);
   procedure MOV_ExC(Processor : in out Processor_Type);
   procedure MOV_ExD(Processor : in out Processor_Type);
   procedure MOV_ExE(Processor : in out Processor_Type);
   procedure MOV_ExH(Processor : in out Processor_Type);
   procedure MOV_ExL(Processor : in out Processor_Type);
   procedure MOV_ExM(Processor : in out Processor_Type);
   procedure MOV_ExA(Processor : in out Processor_Type);
   --
   procedure MOV_HxB(Processor : in out Processor_Type);
   procedure MOV_HxC(Processor : in out Processor_Type);
   procedure MOV_HxD(Processor : in out Processor_Type);
   procedure MOV_HxE(Processor : in out Processor_Type);
   procedure MOV_HxH(Processor : in out Processor_Type);
   procedure MOV_HxL(Processor : in out Processor_Type);
   procedure MOV_HxM(Processor : in out Processor_Type);
   procedure MOV_HxA(Processor : in out Processor_Type);
   --
   procedure MOV_LxB(Processor : in out Processor_Type);
   procedure MOV_LxC(Processor : in out Processor_Type);
   procedure MOV_LxD(Processor : in out Processor_Type);
   procedure MOV_LxE(Processor : in out Processor_Type);
   procedure MOV_LxH(Processor : in out Processor_Type);
   procedure MOV_LxL(Processor : in out Processor_Type);
   procedure MOV_LxM(Processor : in out Processor_Type);
   procedure MOV_LxA(Processor : in out Processor_Type);
   --
   procedure MOV_MxB(Processor : in out Processor_Type);
   procedure MOV_MxC(Processor : in out Processor_Type);
   procedure MOV_MxD(Processor : in out Processor_Type);
   procedure MOV_MxE(Processor : in out Processor_Type);
   procedure MOV_MxH(Processor : in out Processor_Type);
   procedure MOV_MxL(Processor : in out Processor_Type);
   procedure HLT(Processor : in out Processor_Type);
   --
   procedure MOV_MxA(Processor : in out Processor_Type);
   procedure MOV_AxB(Processor : in out Processor_Type);
   procedure MOV_AxC(Processor : in out Processor_Type);
   procedure MOV_AxD(Processor : in out Processor_Type);
   procedure MOV_AxE(Processor : in out Processor_Type);
   procedure MOV_AxH(Processor : in out Processor_Type);
   procedure MOV_AxL(Processor : in out Processor_Type);
   procedure MOV_AxM(Processor : in out Processor_Type);
   procedure MOV_AxA(Processor : in out Processor_Type);
   --
   procedure ADD_B(Processor : in out Processor_Type);
   procedure ADD_C(Processor : in out Processor_Type);
   procedure ADD_D(Processor : in out Processor_Type);
   procedure ADD_E(Processor : in out Processor_Type);
   procedure ADD_H(Processor : in out Processor_Type);
   procedure ADD_L(Processor : in out Processor_Type);
   procedure ADD_M(Processor : in out Processor_Type);
   procedure ADD_A(Processor : in out Processor_Type);
   --
   procedure ADC_B(Processor : in out Processor_Type);
   procedure ADC_C(Processor : in out Processor_Type);
   procedure ADC_D(Processor : in out Processor_Type);
   procedure ADC_E(Processor : in out Processor_Type);
   procedure ADC_H(Processor : in out Processor_Type);
   procedure ADC_L(Processor : in out Processor_Type);
   procedure ADC_M(Processor : in out Processor_Type);
   procedure ADC_A(Processor : in out Processor_Type);
   --
   procedure SUB_B(Processor : in out Processor_Type);
   procedure SUB_C(Processor : in out Processor_Type);
   procedure SUB_D(Processor : in out Processor_Type);
   procedure SUB_E(Processor : in out Processor_Type);
   procedure SUB_H(Processor : in out Processor_Type);
   procedure SUB_L(Processor : in out Processor_Type);
   procedure SUB_M(Processor : in out Processor_Type);
   procedure SUB_A(Processor : in out Processor_Type);
   --
   procedure SBB_B(Processor : in out Processor_Type);
   procedure SBB_C(Processor : in out Processor_Type);
   procedure SBB_D(Processor : in out Processor_Type);
   procedure SBB_E(Processor : in out Processor_Type);
   procedure SBB_H(Processor : in out Processor_Type);
   procedure SBB_L(Processor : in out Processor_Type);
   procedure SBB_M(Processor : in out Processor_Type);
   procedure SBB_A(Processor : in out Processor_Type);
   --
   procedure ANA_B(Processor : in out Processor_Type);
   procedure ANA_C(Processor : in out Processor_Type);
   procedure ANA_D(Processor : in out Processor_Type);
   procedure ANA_E(Processor : in out Processor_Type);
   procedure ANA_H(Processor : in out Processor_Type);
   procedure ANA_L(Processor : in out Processor_Type);
   procedure ANA_M(Processor : in out Processor_Type);
   procedure ANA_A(Processor : in out Processor_Type);
   --
   procedure XRA_B(Processor : in out Processor_Type);
   procedure XRA_C(Processor : in out Processor_Type);
   procedure XRA_D(Processor : in out Processor_Type);
   procedure XRA_E(Processor : in out Processor_Type);
   procedure XRA_H(Processor : in out Processor_Type);
   procedure XRA_L(Processor : in out Processor_Type);
   procedure XRA_M(Processor : in out Processor_Type);
   procedure XRA_A(Processor : in out Processor_Type);
   --
   procedure ORA_B(Processor : in out Processor_Type);
   procedure ORA_C(Processor : in out Processor_Type);
   procedure ORA_D(Processor : in out Processor_Type);
   procedure ORA_E(Processor : in out Processor_Type);
   procedure ORA_H(Processor : in out Processor_Type);
   procedure ORA_L(Processor : in out Processor_Type);
   procedure ORA_M(Processor : in out Processor_Type);
   procedure ORA_A(Processor : in out Processor_Type);
   --
   procedure CMP_B(Processor : in out Processor_Type);
   procedure CMP_C(Processor : in out Processor_Type);
   procedure CMP_D(Processor : in out Processor_Type);
   procedure CMP_E(Processor : in out Processor_Type);
   procedure CMP_H(Processor : in out Processor_Type);
   procedure CMP_L(Processor : in out Processor_Type);
   procedure CMP_M(Processor : in out Processor_Type);
   procedure CMP_A(Processor : in out Processor_Type);
   --
   procedure RNZ(Processor : in out Processor_Type);
   procedure POP_B(Processor : in out Processor_Type);
   procedure JNZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure JMP(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure RET(Processor : in out Processor_Type);
   procedure CNZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure PUSH_B(Processor : in out Processor_Type);
   procedure ADI_D8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure RST_0(Processor : in out Processor_Type);
   procedure RZ(Processor : in out Processor_Type);
   procedure JZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   --
   procedure CZ(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure CALL(Byte_2, Byte_3 : in Byte_Type; Processor : in out Processor_Type);
   procedure ACI_D8(Byte_2 : in Byte_Type; Processor : in out Processor_Type);
   procedure RST_1(Processor : in out Processor_Type);
   procedure RNC(Processor : in out Processor_Type);
   procedure POP_D(Processor : in out Processor_Type);


   procedure Unimplemented_Instruction;

private
   subtype Concatenated_Register_Type is Interfaces.Unsigned_16;
   type Byte_Pair_Type is record
      High_Order_Byte : Byte_Type := 0;
      Low_Order_Byte  : Byte_Type := 0;
   end record;
   for Byte_Pair_Type use record
      High_Order_Byte at 0 range 0 .. 7;
      Low_Order_Byte at 1 range 0 .. 7;
   end record;
   for Byte_Pair_Type'Size use 16;

   procedure Set_Zero_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type);
   procedure Set_Sign_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type);
   procedure Set_Carry_Flag_If_Applicable(Value : in Interfaces.Unsigned_16; Processor : in out Processor_Type);
   procedure Add(Summand : in Register_Type; Processor : in out Processor_Type);
   procedure Add_With_Carry(Summand : in Register_Type; Processor : in out Processor_Type);
   procedure Sub(Subtrahend : in Register_Type; Processor : in out Processor_Type);
   procedure Sub_With_Carry(Subtrahend : in Register_Type; Processor : in out Processor_Type);
   procedure And_A(Value : in Register_Type; Processor : in out Processor_Type);
   procedure Xor_A(Value : in Register_Type; Processor : in out Processor_Type);
   procedure Or_A(Value : in Register_Type; Processor : in out Processor_Type);
   procedure Compare_A(Value : in Register_Type; Processor : in out Processor_Type);

   function Convert_To_Concatenated_Register is new Unchecked_Conversion(Source => Byte_Pair_Type,
                                                                         Target => Concatenated_Register_Type);
   function Convert_To_Address is new Unchecked_Conversion(Source => Byte_Pair_Type,
                                                           Target => Address_Type);
   function Convert_To_Byte_Pair is new Unchecked_Conversion(Source => Address_Type,
                                                             Target => Byte_Pair_Type);
   function Convert_To_Byte_Pair is new Unchecked_Conversion(Source => Concatenated_Register_Type,
                                                             Target => Byte_Pair_Type);


end Emulator_8080.Processor;
