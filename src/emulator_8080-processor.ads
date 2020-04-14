private with Unchecked_Conversion;
private with Interfaces;

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
      Stack_Pointer : Register_Type := 0;
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

   function Convert_To_Concatenated_Register is new Unchecked_Conversion(Source => Byte_Pair_Type,
                                                                     Target => Concatenated_Register_Type);
   function Convert_To_Address is new Unchecked_Conversion(Source => Byte_Pair_Type,
                                                           Target => Address_Type);
   function Convert_To_Byte_Pair is new Unchecked_Conversion(Source => Address_Type,
                                                             Target => Byte_Pair_Type);
   function Convert_To_Byte_Pair is new Unchecked_Conversion(Source => Concatenated_Register_Type,
                                                             Target => Byte_Pair_Type);


end Emulator_8080.Processor;
