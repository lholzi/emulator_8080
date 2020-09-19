with Ada.Text_IO;
with Ada.Streams;
with GNAT.Sockets;

package body Emulator_8080.Vram_Sender is

   Address : GNAT.Sockets.Sock_Addr_Type;
   procedure Initialize(Port : in Natural; Ip_Address : in String) is
   begin
      Address := GNAT.Sockets.Sock_Addr_Type'(Family => GNAT.Sockets.Family_Inet,
                                              Addr   => GNAT.Sockets.Inet_Addr(Ip_Address),
                                              Port   => GNAT.Sockets.Port_Type(Port));
      GNAT.Sockets.Create_Socket(Socket => Sender_Socket,
                                 Family => GNAT.Sockets.Family_Inet,
                                 Mode   => GNAT.Sockets.Socket_Datagram);
   end Initialize;

   procedure Close is
   begin
      GNAT.Sockets.Close_Socket(Sender_Socket);
   end Close;

   procedure Send_Vram(Vram : in Emulator_8080.Processor.Vram_Type) is
      use Ada.Streams;
      Data : Ada.Streams.Stream_Element_Array(1 .. Vram'Length);
      Counter : Ada.Streams.Stream_Element_Offset := Data'First;
      Last : Ada.Streams.Stream_Element_Offset;
   begin
      for I in Vram'Range loop
         Data(Counter) := Ada.Streams.Stream_Element(Vram(I));
         Counter := Counter + 1;
      end loop;
      Gnat.Sockets.Send_Socket (Sender_Socket, Data, Last, Address);
   end;
end Emulator_8080.Vram_Sender;
