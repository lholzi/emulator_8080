with Emulator_8080.Processor;

package Emulator_8080.Disassembler is
   procedure Read_Rom(Rom_Bytes : in Byte_Array_Type;
                      Processor : in out Emulator_8080.Processor.Processor_Type);
end Emulator_8080.Disassembler;
