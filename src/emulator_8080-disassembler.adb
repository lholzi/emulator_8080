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
               when 16#1A# =>
                  Emulator_8080.Processor.LDAX_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#1b# =>
                  Emulator_8080.Processor.DCX_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#1c# =>
                  Emulator_8080.Processor.INR_E(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#1d# =>
                  Emulator_8080.Processor.DCR_E(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#1e# =>
                  Emulator_8080.Processor.MVI_ExD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#1f# =>
                  Emulator_8080.Processor.RAR(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

               ------

               when 16#21# =>
                  Emulator_8080.Processor.LXI_HxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                    Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                    Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#22# =>
                  Emulator_8080.Processor.SHLD_Adr(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#23# =>
                  Emulator_8080.Processor.INX_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#24# =>
                  Emulator_8080.Processor.INR_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#25# =>
                  Emulator_8080.Processor.DCR_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#26# =>
                  Emulator_8080.Processor.MVI_HxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#27# =>
                  Emulator_8080.Processor.DAA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

               ------

               when 16#29# =>
                  Emulator_8080.Processor.DAD_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#2a# =>
                  Emulator_8080.Processor.LHLD(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                               Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                               Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#2b# =>
                  Emulator_8080.Processor.DCX_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#2c# =>
                  Emulator_8080.Processor.INR_L(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#2d# =>
                  Emulator_8080.Processor.DCR_L(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#2e# =>
                  Emulator_8080.Processor.MVI_LxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#2f# =>
                  Emulator_8080.Processor.CMA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

               ------

               when 16#31# =>
                  Emulator_8080.Processor.LXI_SPxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                     Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                     Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#32# =>
                  Emulator_8080.Processor.STA(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                              Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#33# =>
                  Emulator_8080.Processor.INX_SP(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#34# =>
                  Emulator_8080.Processor.INR_M(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#35# =>
                  Emulator_8080.PRocessor.DCR_M(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#36# =>
                  Emulator_8080.Processor.MVI_MxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#37# =>
                  Emulator_8080.Processor.STC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#39# =>
                  Emulator_8080.Processor.DAD_SP(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#3a# =>
                  Emulator_8080.Processor.LDA(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                              Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 3;
               when 16#3b# =>
                  Emulator_8080.Processor.DCX_SP(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#3c# =>
                  Emulator_8080.Processor.INR_A(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#3d# =>
                  Emulator_8080.Processor.DCR_A(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#3e# =>
                  Emulator_8080.Processor.MVI_AxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                   Processor => Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 2;
               when 16#3f# =>
                  Emulator_8080.Processor.CMC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#40# =>
                  Emulator_8080.Processor.MOV_BxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#41# =>
                  Emulator_8080.Processor.MOV_BxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#42# =>
                  Emulator_8080.Processor.MOV_BxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#43# =>
                  Emulator_8080.Processor.MOV_BxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#44# =>
                  Emulator_8080.Processor.MOV_BxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#45# =>
                  Emulator_8080.Processor.MOV_BxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#46# =>
                  Emulator_8080.Processor.MOV_BxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#47# =>
                  Emulator_8080.Processor.MOV_BxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#48# =>
                  Emulator_8080.Processor.MOV_CxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#49# =>
                  Emulator_8080.Processor.MOV_CxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4a# =>
                  Emulator_8080.Processor.MOV_CxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4b# =>
                  Emulator_8080.Processor.MOV_CxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4c# =>
                  Emulator_8080.Processor.MOV_CxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4d# =>
                  Emulator_8080.Processor.MOV_CxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4e# =>
                  Emulator_8080.Processor.MOV_CxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#4f# =>
                  Emulator_8080.Processor.MOV_CxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#50# =>
                  Emulator_8080.Processor.MOV_DxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#51# =>
                  Emulator_8080.Processor.MOV_DxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#52# =>
                  Emulator_8080.Processor.MOV_DxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#53# =>
                  Emulator_8080.Processor.MOV_DxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#54# =>
                  Emulator_8080.Processor.MOV_DxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#55# =>
                  Emulator_8080.Processor.MOV_DxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#56# =>
                  Emulator_8080.Processor.MOV_DxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#57# =>
                  Emulator_8080.Processor.MOV_DxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#58# =>
                  Emulator_8080.Processor.MOV_ExB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#59# =>
                  Emulator_8080.Processor.MOV_ExC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5a# =>
                  Emulator_8080.Processor.MOV_ExD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5b# =>
                  Emulator_8080.Processor.MOV_ExE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5c# =>
                  Emulator_8080.Processor.MOV_ExH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5d# =>
                  Emulator_8080.Processor.MOV_ExL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5e# =>
                  Emulator_8080.Processor.MOV_ExM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#5f# =>
                  Emulator_8080.Processor.MOV_ExA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#60# =>
                  Emulator_8080.Processor.MOV_HxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#61# =>
                  Emulator_8080.Processor.MOV_HxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#62# =>
                  Emulator_8080.Processor.MOV_HxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#63# =>
                  Emulator_8080.Processor.MOV_HxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#64# =>
                  Emulator_8080.Processor.MOV_HxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#65# =>
                  Emulator_8080.Processor.MOV_HxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#66# =>
                  Emulator_8080.Processor.MOV_HxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#67# =>
                  Emulator_8080.Processor.MOV_HxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#68# =>
                  Emulator_8080.Processor.MOV_LxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#69# =>
                  Emulator_8080.Processor.MOV_LxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6a# =>
                  Emulator_8080.Processor.MOV_LxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6b# =>
                  Emulator_8080.Processor.MOV_LxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6c# =>
                  Emulator_8080.Processor.MOV_LxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6d# =>
                  Emulator_8080.Processor.MOV_LxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6e# =>
                  Emulator_8080.Processor.MOV_LxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#6f# =>
                  Emulator_8080.Processor.MOV_LxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#70# =>
                  Emulator_8080.Processor.MOV_MxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#71# =>
                  Emulator_8080.Processor.MOV_MxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#72# =>
                  Emulator_8080.Processor.MOV_MxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#73# =>
                  Emulator_8080.Processor.MOV_MxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#74# =>
                  Emulator_8080.Processor.MOV_MxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#75# =>
                  Emulator_8080.Processor.MOV_MxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#76# =>
                  Emulator_8080.PRocessor.HLT(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#77# =>
                  Emulator_8080.Processor.MOV_MxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#78# =>
                  Emulator_8080.Processor.MOV_AxB(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#79# =>
                  Emulator_8080.Processor.MOV_AxC(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7a# =>
                  Emulator_8080.Processor.MOV_AxD(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7b# =>
                  Emulator_8080.Processor.MOV_AxE(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7c# =>
                  Emulator_8080.Processor.MOV_AxH(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7d# =>
                  Emulator_8080.Processor.MOV_AxL(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7e# =>
                  Emulator_8080.Processor.MOV_AxM(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#7f# =>
                  Emulator_8080.Processor.MOV_AxA(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#80# =>
                  Emulator_8080.Processor.ADD_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#81# =>
                  Emulator_8080.Processor.ADD_C(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#82# =>
                  Emulator_8080.Processor.ADD_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#83# =>
                  Emulator_8080.Processor.ADD_E(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#84# =>
                  Emulator_8080.Processor.ADD_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#85# =>
                  Emulator_8080.Processor.ADD_L(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#86# =>
                  Emulator_8080.Processor.ADD_M(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#87# =>
                  Emulator_8080.Processor.ADD_A(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#88# =>
                  Emulator_8080.Processor.ADC_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#89# =>
                  Emulator_8080.Processor.ADC_C(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8a# =>
                  Emulator_8080.Processor.ADC_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8b# =>
                  Emulator_8080.Processor.ADC_E(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8c# =>
                  Emulator_8080.Processor.ADC_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8d# =>
                  Emulator_8080.Processor.ADC_L(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8e# =>
                  Emulator_8080.Processor.ADC_M(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#8f# =>
                  Emulator_8080.Processor.ADC_A(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;

              ------

               when 16#90# =>
                  Emulator_8080.Processor.SUB_B(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#91# =>
                  Emulator_8080.Processor.SUB_C(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#92# =>
                  Emulator_8080.Processor.SUB_D(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#93# =>
                  Emulator_8080.Processor.SUB_E(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#94# =>
                  Emulator_8080.Processor.SUB_H(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#95# =>
                  Emulator_8080.Processor.SUB_L(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#96# =>
                  Emulator_8080.Processor.SUB_M(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when 16#97# =>
                  Emulator_8080.Processor.SUB_A(Processor);
                  Processor.Program_Counter := Processor.Program_Counter + 1;
               when others =>
                  Emulator_8080.Processor.Unimplemented_Instruction;
                  Processor.Program_Counter := Processor.Program_Counter + 1;
            end case;
         end;
      end loop;
   end Read_Rom;

end Emulator_8080.Disassembler;
