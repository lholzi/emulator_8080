package Emulator_8080 is

   type Byte_Type is range 0 .. 2**8 - 1;
   for Byte_Type'Size use 8;

   type Byte_Array_Type is array(Natural range <>) of Byte_Type;
   type Ram_Type is range 0 .. 16#FFFF#;

end Emulator_8080;
