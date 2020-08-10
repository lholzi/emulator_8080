with Emulator_8080.Processor;
private with GNAT.Sockets;
package Emulator_8080.Vram_Sender is
   procedure Initialize(Port : in Natural; Ip_Address : in String);
   procedure Close;
   procedure Send_Vram(Vram : in Emulator_8080.Processor.Vram_Type);

private
   Sender_Socket : GNAT.Sockets.Socket_Type;
end Emulator_8080.Vram_Sender;
