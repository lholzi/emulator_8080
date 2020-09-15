package Emulator_8080 is
   type Byte_Type is mod 256;
   for Byte_Type'Size use 8;

   type Byte_Array_Type is array(Natural range <>) of Byte_Type;
end Emulator_8080;
