
//  Microsoft (R) .NET Framework IL Disassembler.  Version 4.6.81.0
//  Copyright (c) Microsoft Corporation.  All rights reserved.



// Metadata version: v4.0.30319
.assembly extern mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 4:0:0:0
}
.assembly SmartCalc
{
  .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilationRelaxationsAttribute::.ctor(int32) = ( 01 00 08 00 00 00 00 00 ) 
  .custom instance void [mscorlib]System.Runtime.CompilerServices.RuntimeCompatibilityAttribute::.ctor() = ( 01 00 01 00 54 02 16 57 72 61 70 4E 6F 6E 45 78   // ....T..WrapNonEx
                                                                                                             63 65 70 74 69 6F 6E 54 68 72 6F 77 73 01 )       // ceptionThrows.
  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.module SmartCalc.exe
// MVID: {B798B366-6EA2-4938-B897-79AD53D05BDD}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       // WINDOWS_CUI
.corflags 0x00000001    //  ILONLY
// Image base: 0x003C0000


// =============== CLASS MEMBERS DECLARATION ===================

.class public auto ansi beforefieldinit SmartCalc
       extends [mscorlib]System.Object
{
  .field private static literal int32 minBound = int32(0x00000001)
  .field private static literal int32 maxBound = int32(0x0000000A)
  .field private static class [mscorlib]System.Random rnd
  .method private hidebysig static int32 
          getRandomNumber() cil managed
  {
    // Code size       19 (0x13)
    .maxstack  3
    .locals init (int32 V_0)
    IL_0000:  nop
    IL_0001:  ldsfld     class [mscorlib]System.Random SmartCalc::rnd
    IL_0006:  ldc.i4.1
    IL_0007:  ldc.i4.s   11
    IL_0009:  callvirt   instance int32 [mscorlib]System.Random::Next(int32,
                                                                      int32)
    IL_000e:  stloc.0
    IL_000f:  br.s       IL_0011

    IL_0011:  ldloc.0
    IL_0012:  ret
  } // end of method SmartCalc::getRandomNumber

  .method public hidebysig static void  Main() cil managed
  {
    .entrypoint
    // Code size       159 (0x9f)
    .maxstack  4
    .locals init (string V_0,
             int32 V_1,
             int32 V_2,
             int32 V_3,
             bool V_4)
    IL_0000:  nop
    IL_0001:  ldstr      bytearray (2F 04 20 00 2D 00 20 00 38 04 3D 04 42 04 35 04   // /. .-. .8.=.B.5.
                                    3B 04 3B 04 35 04 3A 04 42 04 43 04 30 04 3B 04   // ;.;.5.:.B.C.0.;.
                                    4C 04 3D 04 4B 04 39 04 20 00 3A 04 30 04 3B 04   // L.=.K.9. .:.0.;.
                                    4C 04 3A 04 43 04 3B 04 4F 04 42 04 3E 04 40 04   // L.:.C.;.O.B.>.@.
                                    21 00 )                                           // !.
    IL_0006:  call       void [mscorlib]System.Console::WriteLine(string)
    IL_000b:  nop
    IL_000c:  ldstr      bytearray (1A 04 30 04 3A 04 20 00 42 04 35 04 31 04 4F 04   // ..0.:. .B.5.1.O.
                                    20 00 37 04 3E 04 32 04 43 04 42 04 3F 00 )       //  .7.>.2.C.B.?.
    IL_0011:  call       void [mscorlib]System.Console::WriteLine(string)
    IL_0016:  nop
    IL_0017:  call       string [mscorlib]System.Console::ReadLine()
    IL_001c:  stloc.0
    IL_001d:  br.s       IL_009a

    IL_001f:  nop
    IL_0020:  call       int32 SmartCalc::getRandomNumber()
    IL_0025:  stloc.1
    IL_0026:  call       int32 SmartCalc::getRandomNumber()
    IL_002b:  stloc.2
    IL_002c:  ldstr      bytearray (21 04 3A 04 3E 04 3B 04 4C 04 3A 04 3E 04 20 00   // !.:.>.;.L.:.>. .
                                    31 04 43 04 34 04 35 04 42 04 20 00 7B 00 31 00   // 1.C.4.5.B. .{.1.
                                    7D 00 20 00 7B 00 30 00 7D 00 20 00 7B 00 32 00   // }. .{.0.}. .{.2.
                                    7D 00 3F 00 )                                     // }.?.
    IL_0031:  ldstr      "-"
    IL_0036:  ldloc.1
    IL_0037:  box        [mscorlib]System.Int32
    IL_003c:  ldloc.2
    IL_003d:  box        [mscorlib]System.Int32
    IL_0042:  call       void [mscorlib]System.Console::WriteLine(string,
                                                                  object,
                                                                  object,
                                                                  object)
    IL_0047:  nop
    IL_0048:  br.s       IL_0057

    IL_004a:  nop
    IL_004b:  ldstr      bytearray (12 04 32 04 35 04 34 04 38 04 20 00 47 04 38 04   // ..2.5.4.8. .G.8.
                                    41 04 3B 04 3E 04 2C 00 20 00 3F 04 3E 04 36 04   // A.;.>.,. .?.>.6.
                                    30 04 3B 04 43 04 39 04 41 04 42 04 30 04 )       // 0.;.C.9.A.B.0.
    IL_0050:  call       void [mscorlib]System.Console::WriteLine(string)
    IL_0055:  nop
    IL_0056:  nop
    IL_0057:  call       string [mscorlib]System.Console::ReadLine()
    IL_005c:  ldloca.s   V_3
    IL_005e:  call       bool [mscorlib]System.Int32::TryParse(string,
                                                               int32&)
    IL_0063:  ldc.i4.0
    IL_0064:  ceq
    IL_0066:  stloc.s    V_4
    IL_0068:  ldloc.s    V_4
    IL_006a:  brtrue.s   IL_004a

    IL_006c:  ldloc.1
    IL_006d:  ldloc.2
    IL_006e:  sub
    IL_006f:  ldloc.3
    IL_0070:  ceq
    IL_0072:  ldc.i4.0
    IL_0073:  ceq
    IL_0075:  stloc.s    V_4
    IL_0077:  ldloc.s    V_4
    IL_0079:  brtrue.s   IL_008b

    IL_007b:  nop
    IL_007c:  ldstr      bytearray (12 04 35 04 40 04 3D 04 3E 04 2C 00 20 00 7B 00   // ..5.@.=.>.,. .{.
                                    30 00 7D 00 )                                     // 0.}.
    IL_0081:  ldloc.0
    IL_0082:  call       void [mscorlib]System.Console::WriteLine(string,
                                                                  object)
    IL_0087:  nop
    IL_0088:  nop
    IL_0089:  br.s       IL_0099

    IL_008b:  nop
    IL_008c:  ldstr      bytearray (7B 00 30 00 7D 00 2C 00 20 00 42 04 4B 04 20 00   // {.0.}.,. .B.K. .
                                    3D 04 35 04 20 00 3F 04 40 04 30 04 32 04 )       // =.5. .?.@.0.2.
    IL_0091:  ldloc.0
    IL_0092:  call       void [mscorlib]System.Console::WriteLine(string,
                                                                  object)
    IL_0097:  nop
    IL_0098:  nop
    IL_0099:  nop
    IL_009a:  ldc.i4.1
    IL_009b:  stloc.s    V_4
    IL_009d:  br.s       IL_001f
  } // end of method SmartCalc::Main

  .method public hidebysig specialname rtspecialname 
          instance void  .ctor() cil managed
  {
    // Code size       7 (0x7)
    .maxstack  8
    IL_0000:  ldarg.0
    IL_0001:  call       instance void [mscorlib]System.Object::.ctor()
    IL_0006:  ret
  } // end of method SmartCalc::.ctor

  .method private hidebysig specialname rtspecialname static 
          void  .cctor() cil managed
  {
    // Code size       11 (0xb)
    .maxstack  8
    IL_0000:  newobj     instance void [mscorlib]System.Random::.ctor()
    IL_0005:  stsfld     class [mscorlib]System.Random SmartCalc::rnd
    IL_000a:  ret
  } // end of method SmartCalc::.cctor

} // end of class SmartCalc


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************
