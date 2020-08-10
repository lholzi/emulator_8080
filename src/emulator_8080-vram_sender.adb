with Ada.Text_IO;
with Ada.Streams;
with GNAT.Sockets;

package body Emulator_8080.Vram_Sender is
   procedure Initialize(Port : in Natural; Ip_Address : in String) is
      Address : constant GNAT.Sockets.Sock_Addr_Type := GNAT.Sockets.Sock_Addr_Type'(Family => GNAT.Sockets.Family_Inet,
                                                                                     Addr   => GNAT.Sockets.Inet_Addr(Ip_Address),
                                                                                     Port   => GNAT.Sockets.Port_Type(Port));
   begin
      GNAT.Sockets.Create_Socket(Sender_Socket);
   end Initialize;

   procedure Close is
   begin
      GNAT.Sockets.Close_Socket(Sender_Socket);
   end Close;

   procedure Send_Vram(Vram : in Emulator_8080.Processor.Vram_Type) is
      Data : aliased Ada.Streams.Stream_Element_Array(1 .. Vram'Length);
   begin
      --Emulator_8080.Processor.Vram_Type'Write(Data'Class, Vram);
      for I in Vram'Range loop
         if Vram(I) /= 0 then
            Ada.Text_IO.Put_Line(Vram(I)'Img);
         end if;
      end loop;
   end;
end Emulator_8080.Vram_Sender;
