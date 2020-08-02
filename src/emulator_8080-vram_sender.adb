with Ada.Text_IO;

package body Emulator_8080.Vram_Sender is
   procedure Send_Vram(Vram : in Emulator_8080.Processor.Vram_Type) is
   begin
      Ada.Text_IO.Put_Line("TESTITEST CALLBACK!");
   end;
end Emulator_8080.Vram_Sender;
