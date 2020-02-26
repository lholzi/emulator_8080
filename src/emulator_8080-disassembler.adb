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
               when 16#01# =>
                  Emulator_8080.Processor.LXI_BxD16(Byte_2    => Rom_Bytes(Program_Counter + 1),
                                                    Byte_3    => Rom_Bytes(Program_Counter + 2),
                                                    Processor => Processor);
                  Program_Counter:= Program_Counter + 3;
               when 16#02# =>
                  Emulator_8080.Processor.STAX_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#03# =>
                  Emulator_8080.Processor.INX_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#04# =>
                  Emulator_8080.Processor.INR_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#05# =>
                  Emulator_8080.Processor.DCR_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#06# =>
                  Emulator_8080.Processor.MVI_BxD8(Byte_2    => Rom_Bytes(Program_Counter + 1),
                                                   Processor => Processor);
                  Program_Counter := Program_Counter + 2;
               when 16#07# =>
                  Emulator_8080.Processor.RLC(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#08# =>
                  Emulator_8080.Processor.NOP;
                  Program_Counter := Program_Counter + 1;
               when 16#09# =>
                  Emulator_8080.Processor.DAD_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when 16#0a# =>
                  Emulator_8080.Processor.LDAX_B(Processor);
                  Program_Counter := Program_Counter + 1;
               when others =>
                  Emulator_8080.Processor.Unimplemented_Instruction;
                  Program_Counter := Program_Counter + 1;
            end case;
         end;
      end loop;
   end Read_Rom;

end Emulator_8080.Disassembler;
