package body Emulator_8080.Disassembler is

   procedure Read_Rom(Processor : in out Emulator_8080.Processor.Processor_Type) is
      use Emulator_8080.Processor;
   begin
      while Processor.Program_Counter <= Emulator_8080.Processor.Rom_Address_Type'Last loop
         declare
            Current_Instruction : constant Byte_Type := Processor.Memory(Processor.Program_Counter);
         begin
            case Current_Instruction is
               when 16#0# =>
                  Emulator_8080.Processor.NOP;
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#01# =>
                  Emulator_8080.Processor.LXI_BxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                    Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                    Processor => Processor);
                  Processor.Program_Counter:= Processor.Program_Counter + 3;
               when 16#02# =>
                  Emulator_8080.Processor.STAX_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#03# =>
                  Emulator_8080.Processor.INX_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#04# =>
                  Emulator_8080.Processor.INR_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#05# =>
                  Emulator_8080.Processor.DCR_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#06# =>
                  Emulator_8080.Processor.MVI_BxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#07# =>
                  Emulator_8080.Processor.RLC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

             ------

               when 16#08# =>
                  Emulator_8080.Processor.NOP;
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#09# =>
                  Emulator_8080.Processor.DAD_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#0a# =>
                  Emulator_8080.Processor.LDAX_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#0b# =>
                  Emulator_8080.Processor.DCX_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#0c# =>
                  Emulator_8080.Processor.INR_C(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#0d# =>
                  Emulator_8080.Processor.DCR_C(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#0e# =>
                  Emulator_8080.Processor.MVI_CxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#0f# =>
                  Emulator_8080.Processor.RRC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#11# =>
                  Emulator_8080.Processor.LXI_DxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                    Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                    Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#12# =>
                  Emulator_8080.Processor.STAX_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#13# =>
                  Emulator_8080.Processor.INX_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#14# =>
                  Emulator_8080.Processor.INR_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#15# =>
                  Emulator_8080.Processor.DCR_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#16# =>
                  Emulator_8080.Processor.MVI_DxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#17# =>
                  Emulator_8080.Processor.RAL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#19# =>
                  Emulator_8080.Processor.DAD_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when others =>
                  Emulator_8080.Processor.Unimplemented_Instruction;
                  Processor.Program_Counter := Processor.Program_Counter + 1;
            end case;
         end;
      end loop;
   end Read_Rom;

end Emulator_8080.Disassembler;
