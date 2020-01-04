with Emulator_8080.Processor;
package body Emulator_8080.Disassembler is

   procedure Read_Rom(Rom_Bytes : in Byte_Array_Type;
                      Processor : in out Emulator_8080.Processor.Processor_Type) is
      Program_Counter : Natural := Rom_Bytes'First;
   begin
      while Program_Counter < Rom_Bytes'Last loop
         declare
            Current_Instruction : constant Byte_Type := Rom_Bytes(Program_Counter);
         begin
            case Current_Instruction is
               when 16#0# =>
                  Emulator_8080.Processor.NOP;
                  Program_Counter := Program_Counter + 1;
               when 16#1# =>
                  Emulator_8080.Processor.LXI_BxD16(Byte_2    => Rom_Bytes(Program_Counter + 1),
                                                    Byte_3    => Rom_Bytes(Program_Counter + 2),
                                                    Processor => Processor);
                  Program_Counter:= Program_Counter + 3;
               when 16#2# =>
                  Emulator_8080.Processor.STAX_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#3# =>
                  Emulator_8080.Processor.INX_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#4# =>
                  Emulator_8080.Processor.INR_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#5# =>
                  Emulator_8080.Processor.DCR_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#6# =>
                  Emulator_8080.Processor.MVI_BxD8(Byte_2    => Rom_Bytes(Program_Counter + 1),
                                                   Processor => Processor);
                  Program_Counter := Program_Counter + 2;
               when others =>
                  Emulator_8080.Processor.Unimplemented_Instruction;
                  Program_Counter := Program_Counter + 1;
            end case;
         end;
      end loop;
   end Read_Rom;

end Emulator_8080.Disassembler;
