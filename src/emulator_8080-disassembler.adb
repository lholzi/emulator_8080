with Ada.Text_IO;
with GNAT.Current_Exception;

package body Emulator_8080.Disassembler is

   procedure Read_Rom(Processor : in out Emulator_8080.Processor.Processor_Type) is
      use Emulator_8080.Processor;
      Current_Instruction : Byte_Type := 0;
   begin
      while Processor.Program_Counter <= Emulator_8080.Processor.Rom_Address_Type'Last loop
         Current_Instruction := Processor.Memory(Processor.Program_Counter);
         Ada.Text_IO.Put_Line(Current_Instruction'Img);
         case Current_Instruction is
            when 16#0# =>
               Emulator_8080.Processor.NOP(Processor);
            when 16#01# =>
               Emulator_8080.Processor.LXI_BxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                 Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                 Processor => Processor);
            when 16#02# =>
               Emulator_8080.Processor.STAX_B(Processor);
            when 16#03# =>
               Emulator_8080.Processor.INX_B(Processor);
            when 16#04# =>
               Emulator_8080.Processor.INR_B(Processor);
            when 16#05# =>
               Emulator_8080.Processor.DCR_B(Processor);
            when 16#06# =>
               Emulator_8080.Processor.MVI_BxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#07# =>
               Emulator_8080.Processor.RLC(Processor);

          ------

            when 16#08# =>
               Emulator_8080.Processor.NOP(Processor);
            when 16#09# =>
               Emulator_8080.Processor.DAD_B(Processor);
            when 16#0a# =>
               Emulator_8080.Processor.LDAX_B(Processor);
            when 16#0b# =>
               Emulator_8080.Processor.DCX_B(Processor);
            when 16#0c# =>
               Emulator_8080.Processor.INR_C(Processor);
            when 16#0d# =>
               Emulator_8080.Processor.DCR_C(Processor);
            when 16#0e# =>
               Emulator_8080.Processor.MVI_CxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#0f# =>
               Emulator_8080.Processor.RRC(Processor);

           ------

            when 16#11# =>
               Emulator_8080.Processor.LXI_DxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                 Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                 Processor => Processor);
            when 16#12# =>
               Emulator_8080.Processor.STAX_D(Processor);
            when 16#13# =>
               Emulator_8080.Processor.INX_D(Processor);
            when 16#14# =>
               Emulator_8080.Processor.INR_D(Processor);
            when 16#15# =>
               Emulator_8080.Processor.DCR_D(Processor);
            when 16#16# =>
               Emulator_8080.Processor.MVI_DxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#17# =>
               Emulator_8080.Processor.RAL(Processor);

           ------

            when 16#19# =>
               Emulator_8080.Processor.DAD_D(Processor);
            when 16#1A# =>
               Emulator_8080.Processor.LDAX_D(Processor);
            when 16#1b# =>
               Emulator_8080.Processor.DCX_D(Processor);
            when 16#1c# =>
               Emulator_8080.Processor.INR_E(Processor);
            when 16#1d# =>
               Emulator_8080.Processor.DCR_E(Processor);
            when 16#1e# =>
               Emulator_8080.Processor.MVI_ExD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#1f# =>
               Emulator_8080.Processor.RAR(Processor);

            ------

            when 16#21# =>
               Emulator_8080.Processor.LXI_HxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                 Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                 Processor => Processor);
            when 16#22# =>
               Emulator_8080.Processor.SHLD_Adr(Processor);
            when 16#23# =>
               Emulator_8080.Processor.INX_H(Processor);
            when 16#24# =>
               Emulator_8080.Processor.INR_H(Processor);
            when 16#25# =>
               Emulator_8080.Processor.DCR_H(Processor);
            when 16#26# =>
               Emulator_8080.Processor.MVI_HxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#27# =>
               Emulator_8080.Processor.DAA(Processor);

            ------

            when 16#29# =>
               Emulator_8080.Processor.DAD_H(Processor);
            when 16#2a# =>
               Emulator_8080.Processor.LHLD(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                            Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                            Processor => Processor);
            when 16#2b# =>
               Emulator_8080.Processor.DCX_H(Processor);
            when 16#2c# =>
               Emulator_8080.Processor.INR_L(Processor);
            when 16#2d# =>
               Emulator_8080.Processor.DCR_L(Processor);
            when 16#2e# =>
               Emulator_8080.Processor.MVI_LxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#2f# =>
               Emulator_8080.Processor.CMA(Processor);

            ------

            when 16#31# =>
               Emulator_8080.Processor.LXI_SPxD16(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                  Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                                  Processor => Processor);
            when 16#32# =>
               Emulator_8080.Processor.STA(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#33# =>
               Emulator_8080.Processor.INX_SP(Processor);
            when 16#34# =>
               Emulator_8080.Processor.INR_M(Processor);
            when 16#35# =>
               Emulator_8080.PRocessor.DCR_M(Processor);
            when 16#36# =>
               Emulator_8080.Processor.MVI_MxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#37# =>
               Emulator_8080.Processor.STC(Processor);

           ------

            when 16#39# =>
               Emulator_8080.Processor.DAD_SP(Processor);
            when 16#3a# =>
               Emulator_8080.Processor.LDA(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#3b# =>
               Emulator_8080.Processor.DCX_SP(Processor);
            when 16#3c# =>
               Emulator_8080.Processor.INR_A(Processor);
            when 16#3d# =>
               Emulator_8080.Processor.DCR_A(Processor);
            when 16#3e# =>
               Emulator_8080.Processor.MVI_AxD8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                                Processor => Processor);
            when 16#3f# =>
               Emulator_8080.Processor.CMC(Processor);

           ------

            when 16#40# =>
               Emulator_8080.Processor.MOV_BxB(Processor);
            when 16#41# =>
               Emulator_8080.Processor.MOV_BxC(Processor);
            when 16#42# =>
               Emulator_8080.Processor.MOV_BxD(Processor);
            when 16#43# =>
               Emulator_8080.Processor.MOV_BxE(Processor);
            when 16#44# =>
               Emulator_8080.Processor.MOV_BxH(Processor);
            when 16#45# =>
               Emulator_8080.Processor.MOV_BxL(Processor);
            when 16#46# =>
               Emulator_8080.Processor.MOV_BxM(Processor);
            when 16#47# =>
               Emulator_8080.Processor.MOV_BxA(Processor);

           ------

            when 16#48# =>
               Emulator_8080.Processor.MOV_CxB(Processor);
            when 16#49# =>
               Emulator_8080.Processor.MOV_CxC(Processor);
            when 16#4a# =>
               Emulator_8080.Processor.MOV_CxD(Processor);
            when 16#4b# =>
               Emulator_8080.Processor.MOV_CxE(Processor);
            when 16#4c# =>
               Emulator_8080.Processor.MOV_CxH(Processor);
            when 16#4d# =>
               Emulator_8080.Processor.MOV_CxL(Processor);
            when 16#4e# =>
               Emulator_8080.Processor.MOV_CxM(Processor);
            when 16#4f# =>
               Emulator_8080.Processor.MOV_CxA(Processor);

           ------

            when 16#50# =>
               Emulator_8080.Processor.MOV_DxB(Processor);
            when 16#51# =>
               Emulator_8080.Processor.MOV_DxC(Processor);
            when 16#52# =>
               Emulator_8080.Processor.MOV_DxD(Processor);
            when 16#53# =>
               Emulator_8080.Processor.MOV_DxE(Processor);
            when 16#54# =>
               Emulator_8080.Processor.MOV_DxH(Processor);
            when 16#55# =>
               Emulator_8080.Processor.MOV_DxL(Processor);
            when 16#56# =>
               Emulator_8080.Processor.MOV_DxM(Processor);
            when 16#57# =>
               Emulator_8080.Processor.MOV_DxA(Processor);

           ------

            when 16#58# =>
               Emulator_8080.Processor.MOV_ExB(Processor);
            when 16#59# =>
               Emulator_8080.Processor.MOV_ExC(Processor);
            when 16#5a# =>
               Emulator_8080.Processor.MOV_ExD(Processor);
            when 16#5b# =>
               Emulator_8080.Processor.MOV_ExE(Processor);
            when 16#5c# =>
               Emulator_8080.Processor.MOV_ExH(Processor);
            when 16#5d# =>
               Emulator_8080.Processor.MOV_ExL(Processor);
            when 16#5e# =>
               Emulator_8080.Processor.MOV_ExM(Processor);
            when 16#5f# =>
               Emulator_8080.Processor.MOV_ExA(Processor);

           ------

            when 16#60# =>
               Emulator_8080.Processor.MOV_HxB(Processor);
            when 16#61# =>
               Emulator_8080.Processor.MOV_HxC(Processor);
            when 16#62# =>
               Emulator_8080.Processor.MOV_HxD(Processor);
            when 16#63# =>
               Emulator_8080.Processor.MOV_HxE(Processor);
            when 16#64# =>
               Emulator_8080.Processor.MOV_HxH(Processor);
            when 16#65# =>
               Emulator_8080.Processor.MOV_HxL(Processor);
            when 16#66# =>
               Emulator_8080.Processor.MOV_HxM(Processor);
            when 16#67# =>
               Emulator_8080.Processor.MOV_HxA(Processor);

           ------

            when 16#68# =>
               Emulator_8080.Processor.MOV_LxB(Processor);
            when 16#69# =>
               Emulator_8080.Processor.MOV_LxC(Processor);
            when 16#6a# =>
               Emulator_8080.Processor.MOV_LxD(Processor);
            when 16#6b# =>
               Emulator_8080.Processor.MOV_LxE(Processor);
            when 16#6c# =>
               Emulator_8080.Processor.MOV_LxH(Processor);
            when 16#6d# =>
               Emulator_8080.Processor.MOV_LxL(Processor);
            when 16#6e# =>
               Emulator_8080.Processor.MOV_LxM(Processor);
            when 16#6f# =>
               Emulator_8080.Processor.MOV_LxA(Processor);

           ------

            when 16#70# =>
               Emulator_8080.Processor.MOV_MxB(Processor);
            when 16#71# =>
               Emulator_8080.Processor.MOV_MxC(Processor);
            when 16#72# =>
               Emulator_8080.Processor.MOV_MxD(Processor);
            when 16#73# =>
               Emulator_8080.Processor.MOV_MxE(Processor);
            when 16#74# =>
               Emulator_8080.Processor.MOV_MxH(Processor);
            when 16#75# =>
               Emulator_8080.Processor.MOV_MxL(Processor);
            when 16#76# =>
               Emulator_8080.PRocessor.HLT(Processor);

           ------

            when 16#77# =>
               Emulator_8080.Processor.MOV_MxA(Processor);
            when 16#78# =>
               Emulator_8080.Processor.MOV_AxB(Processor);
            when 16#79# =>
               Emulator_8080.Processor.MOV_AxC(Processor);
            when 16#7a# =>
               Emulator_8080.Processor.MOV_AxD(Processor);
            when 16#7b# =>
               Emulator_8080.Processor.MOV_AxE(Processor);
            when 16#7c# =>
               Emulator_8080.Processor.MOV_AxH(Processor);
            when 16#7d# =>
               Emulator_8080.Processor.MOV_AxL(Processor);
            when 16#7e# =>
               Emulator_8080.Processor.MOV_AxM(Processor);
            when 16#7f# =>
               Emulator_8080.Processor.MOV_AxA(Processor);

           ------

            when 16#80# =>
               Emulator_8080.Processor.ADD_B(Processor);
            when 16#81# =>
               Emulator_8080.Processor.ADD_C(Processor);
            when 16#82# =>
               Emulator_8080.Processor.ADD_D(Processor);
            when 16#83# =>
               Emulator_8080.Processor.ADD_E(Processor);
            when 16#84# =>
               Emulator_8080.Processor.ADD_H(Processor);
            when 16#85# =>
               Emulator_8080.Processor.ADD_L(Processor);
            when 16#86# =>
               Emulator_8080.Processor.ADD_M(Processor);
            when 16#87# =>
               Emulator_8080.Processor.ADD_A(Processor);

           ------

            when 16#88# =>
               Emulator_8080.Processor.ADC_B(Processor);
            when 16#89# =>
               Emulator_8080.Processor.ADC_C(Processor);
            when 16#8a# =>
               Emulator_8080.Processor.ADC_D(Processor);
            when 16#8b# =>
               Emulator_8080.Processor.ADC_E(Processor);
            when 16#8c# =>
               Emulator_8080.Processor.ADC_H(Processor);
            when 16#8d# =>
               Emulator_8080.Processor.ADC_L(Processor);
            when 16#8e# =>
               Emulator_8080.Processor.ADC_M(Processor);
            when 16#8f# =>
               Emulator_8080.Processor.ADC_A(Processor);

           ------

            when 16#90# =>
               Emulator_8080.Processor.SUB_B(Processor);
            when 16#91# =>
               Emulator_8080.Processor.SUB_C(Processor);
            when 16#92# =>
               Emulator_8080.Processor.SUB_D(Processor);
            when 16#93# =>
               Emulator_8080.Processor.SUB_E(Processor);
            when 16#94# =>
               Emulator_8080.Processor.SUB_H(Processor);
            when 16#95# =>
               Emulator_8080.Processor.SUB_L(Processor);
            when 16#96# =>
               Emulator_8080.Processor.SUB_M(Processor);
            when 16#97# =>
               Emulator_8080.Processor.SUB_A(Processor);

           ------

            when 16#98# =>
               Emulator_8080.Processor.SBB_B(Processor);
            when 16#99# =>
               Emulator_8080.Processor.SBB_C(Processor);
            when 16#9a# =>
               Emulator_8080.Processor.SBB_D(Processor);
            when 16#9b# =>
               Emulator_8080.Processor.SBB_E(Processor);
            when 16#9c# =>
               Emulator_8080.Processor.SBB_H(Processor);
            when 16#9d# =>
               Emulator_8080.Processor.SBB_L(Processor);
            when 16#9e# =>
               Emulator_8080.Processor.SBB_M(Processor);
            when 16#9f# =>
               Emulator_8080.Processor.SBB_A(Processor);

           ------

            when 16#a0# =>
               Emulator_8080.Processor.ANA_B(Processor);
            when 16#a1# =>
               Emulator_8080.Processor.ANA_C(Processor);
            when 16#a2# =>
               Emulator_8080.Processor.ANA_D(Processor);
            when 16#a3# =>
               Emulator_8080.Processor.ANA_E(Processor);
            when 16#a4# =>
               Emulator_8080.Processor.ANA_H(Processor);
            when 16#a5# =>
               Emulator_8080.Processor.ANA_L(Processor);
            when 16#a6# =>
               Emulator_8080.Processor.ANA_M(Processor);
            when 16#a7# =>
               Emulator_8080.Processor.ANA_A(Processor);

           ------

            when 16#a8# =>
               Emulator_8080.Processor.XRA_B(Processor);
            when 16#a9# =>
               Emulator_8080.Processor.XRA_C(Processor);
            when 16#aa# =>
               Emulator_8080.Processor.XRA_D(Processor);
            when 16#ab# =>
               Emulator_8080.Processor.XRA_E(Processor);
            when 16#ac# =>
               Emulator_8080.Processor.XRA_H(Processor);
            when 16#ad# =>
               Emulator_8080.Processor.XRA_L(Processor);
            when 16#ae# =>
               Emulator_8080.Processor.XRA_M(Processor);
            when 16#af# =>
               Emulator_8080.Processor.XRA_A(Processor);

           ------

            when 16#b0# =>
               Emulator_8080.Processor.ORA_B(Processor);
            when 16#b1# =>
               Emulator_8080.Processor.ORA_C(Processor);
            when 16#b2# =>
               Emulator_8080.Processor.ORA_D(Processor);
            when 16#b3# =>
               Emulator_8080.Processor.ORA_E(Processor);
            when 16#b4# =>
               Emulator_8080.Processor.ORA_H(Processor);
            when 16#b5# =>
               Emulator_8080.Processor.ORA_L(Processor);
            when 16#b6# =>
               Emulator_8080.Processor.ORA_M(Processor);
            when 16#b7# =>
               Emulator_8080.Processor.ORA_A(Processor);

           ------

            when 16#b8# =>
               Emulator_8080.Processor.CMP_B(Processor);
            when 16#b9# =>
               Emulator_8080.Processor.CMP_C(Processor);
            when 16#ba# =>
               Emulator_8080.Processor.CMP_D(Processor);
            when 16#bb# =>
               Emulator_8080.Processor.CMP_E(Processor);
            when 16#bc# =>
               Emulator_8080.Processor.CMP_H(Processor);
            when 16#bd# =>
               Emulator_8080.Processor.CMP_L(Processor);
            when 16#be# =>
               Emulator_8080.Processor.CMP_M(Processor);
            when 16#bf# =>
               Emulator_8080.Processor.CMP_A(Processor);
            when 16#c0# =>
               Emulator_8080.Processor.RNZ(Processor);
            when 16#c1# =>
               Emulator_8080.Processor.POP_B(Processor);
            when 16#c2# =>
               Emulator_8080.Processor.JNZ(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#c3# =>
               Emulator_8080.Processor.JMP(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#c4# =>
               Emulator_8080.Processor.CNZ(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#c5# =>
               Emulator_8080.Processor.PUSH_B(Processor);
            when 16#c6# =>
               Emulator_8080.Processor.ADI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#c7# =>
               Emulator_8080.Processor.RST_0(Processor);
            when 16#c8# =>
               Emulator_8080.Processor.RZ(Processor);
            when 16#c9# =>
               Emulator_8080.Processor.RET(Processor);
            when 16#ca# =>
               Emulator_8080.Processor.JZ( Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);


           ------


            when 16#cc# =>
               Emulator_8080.Processor.CZ( Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);

            when 16#cd# =>
               Emulator_8080.Processor.CALL(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                            Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                            Processor => Processor);
            when 16#ce# =>
               Emulator_8080.Processor.ACI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#cf# =>
               Emulator_8080.Processor.RST_1(Processor);


           ------

            when 16#d0# =>
               Emulator_8080.Processor.RNC(Processor);
            when 16#d1# =>
               Emulator_8080.Processor.POP_D(Processor);
            when 16#d2# =>
               Emulator_8080.Processor.JNC(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#d3# =>
               Emulator_8080.Processor.OUT_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#d4# =>
               Emulator_8080.Processor.CNC(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#d5# =>
               Emulator_8080.Processor.PUSH_D(Processor);
            when 16#d6# =>
               Emulator_8080.Processor.SUI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#d7# =>
               Emulator_8080.Processor.RST_2(Processor);
            when 16#d8# =>
               Emulator_8080.Processor.RC(Processor);


           ------


            when 16#da# =>
               Emulator_8080.Processor.JC(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#db# =>
               Emulator_8080.Processor.IN_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                             Processor => Processor);
            when 16#dc# =>
               Emulator_8080.Processor.CC(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                          Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                          Processor => Processor);


           ------


            when 16#de# =>
               Emulator_8080.Processor.SBI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#df# =>
               Emulator_8080.Processor.RST_3(Processor);
            when 16#e0# =>
               Emulator_8080.Processor.RPO(Processor);
            when 16#e1# =>
               Emulator_8080.Processor.POP_H(Processor);
            when 16#e2# =>
               Emulator_8080.Processor.JPO(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#e3# =>
               Emulator_8080.Processor.XTHL(Processor => Processor);
            when 16#e4# =>
               Emulator_8080.Processor.CPO(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#e5# =>
               Emulator_8080.Processor.PUSH_H(Processor => Processor);
            when 16#e6# =>
               Emulator_8080.Processor.ANI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                              Processor => Processor);
            when 16#e7# =>
               Emulator_8080.Processor.RST_4(Processor => Processor);
            when 16#e8# =>
               Emulator_8080.Processor.RPE(Processor => Processor);
            when 16#e9# =>
               Emulator_8080.Processor.PCHL(Processor => Processor);
            when 16#ea# =>
               Emulator_8080.Processor.JPE(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#eb# =>
               Emulator_8080.Processor.XCHG(Processor => Processor);
            when 16#ec# =>
               Emulator_8080.Processor.CPE(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                           Processor => Processor);
            when 16#ee# =>
               Emulator_8080.Processor.XRI_D8(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                           Processor => Processor);
            when 16#ef# =>
               Emulator_8080.Processor.RST_5(Processor => Processor);
            when 16#f0# =>
               Emulator_8080.Processor.RP(Processor => Processor);
            when 16#f1# =>
               Emulator_8080.Processor.POP_PSW(Processor => Processor);
            when 16#f2# =>
               Emulator_8080.Processor.JP(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                          Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                          Processor => Processor);
            when 16#f3# =>
               Emulator_8080.Processor.DI(Processor => Processor);
            when 16#f4# =>
               Emulator_8080.Processor.CP(Byte_2    => Processor.Memory(Processor.Program_Counter + 1),
                                          Byte_3    => Processor.Memory(Processor.Program_Counter + 2),
                                          Processor => Processor);
            when 16#f5# =>
               Emulator_8080.Processor.PUSH_PSW(Processor => Processor);
            when others =>
               Emulator_8080.Processor.Unimplemented_Instruction(Processor);
         end case;
      end loop;
   exception
      when others =>
         Ada.Text_IO.Put_Line("Exception was thrown while executing rom. Informations:");
         Ada.Text_IO.Put_Line("--> Instruction:     " & Current_Instruction'Img);
         Ada.Text_IO.Put_Line("--> Program_Counter: " & Processor.Program_Counter'Img);
         Ada.Text_IO.Put_Line("--> Stack_Pointer: " & Processor.Stack_Pointer'Img);
         Ada.Text_IO.Put_Line("--> Message:");
         Ada.Text_IO.Put_Line(GNAT.Current_Exception.Exception_Message);
   end Read_Rom;

end Emulator_8080.Disassembler;
