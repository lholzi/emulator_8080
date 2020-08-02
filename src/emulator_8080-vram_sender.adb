with Ada.Text_IO;

package body Emulator_8080.Vram_Sender is
   procedure Send_Vram(Vram : in Emulator_8080.Processor.Vram_Type) is
   begin
      for I in Vram'Range loop
         if Vram(I) /= 0 then
            Ada.Text_IO.Put_Line(Vram(I)'Img);
         end if;
      end loop;
   end;
end Emulator_8080.Vram_Sender;
