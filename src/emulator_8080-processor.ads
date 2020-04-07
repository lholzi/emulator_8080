private with Unchecked_Conversion;
private with Interfaces;

package Emulator_8080.Processor is
   subtype Register_Type is Emulator_8080.Byte_Type;
   type Address_Type is new Natural range 0 .. 16#FFFF#;
   type Flag_Type is (Not_Set, Set) with Size => 1;
   type Ram_Type is array (Address_Type) of Byte_Type;
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

      RAM : Ram_Type := (others => 0);
   end record;

   procedure NOP;
   procedure LXI_BxD16(Byte_2, Byte_3 : in Emulator_8080.Byte_Type;
                       Processor : in out Processor_Type);
   procedure STAX_B(Processor : in out Processor_Type);
   procedure INX_B(Processor : in out Processor_Type);
   procedure INR_B(Processor : in out Processor_Type);
   procedure DCR_B(Processor : in out Processor_Type);
   procedure MVI_BxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor : in out Processor_Type);
   procedure RLC(Processor : in out Processor_Type);
   procedure DAD_B(Processor : in out Processor_Type);
   procedure LDAX_B(Processor : in out Processor_Type);
   procedure DCX_B(Processor : in out Processor_Type);
   procedure INR_C(Processor : in out Processor_Type);
   procedure DCR_C(Processor : in out Processor_Type);
   procedure MVI_CxD8(Byte_2 : in Emulator_8080.Byte_Type;
                      Processor: in out Processor_Type);
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
