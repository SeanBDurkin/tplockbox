{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2013 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$DEFINE UNSUPPORTED_COMPILER_VERSION}
{$DEFINE DELPHI_XE8_DOWN}
{$DEFINE DELPHI_D10_Seatle_DOWN}
{$DEFINE DELPHI_D10_1Brln_DOWN}
{$DEFINE DELPHI_XE7_DOWN}
{$DEFINE DELPHI_XE6_DOWN}
{$DEFINE DELPHI_XE5_DOWN}
{$DEFINE DELPHI_XE4_DOWN}
{$DEFINE DELPHI_XE3_DOWN}
{$DEFINE DELPHI_XE2_DOWN}
{$DEFINE DELPHI_XE_DOWN}
{$DEFINE DELPHI_2010_DOWN}

{$IFDEF VER210} // RAD Studio 2010
  {$DEFINE DELPHI_2010}
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE CPUX86}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
{$ENDIF VER210}

{$IFDEF VER220} // RAD Studio XE
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$DEFINE CPUX86}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
{$ENDIF VER220}

{$IFDEF VER230} // RAD Studio XE2
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
{$ENDIF VER230}

{$IFDEF VER240} // RAD Studio XE3
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
{$ENDIF VER240}

{$IFDEF VER250} // RAD Studio XE4
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
{$ENDIF VER250}

{$IFDEF VER260} // RAD Studio XE5
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
{$ENDIF VER260}

{$IFDEF VER270} // RAD Studio XE6
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
{$ENDIF VER270}

{$IFDEF VER280} // RAD Studio XE7
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
{$ENDIF VER280}

{$IF CompilerVersion = 29.0}  // VER290, RAD Studio XE8
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE8_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
  {$UNDEF DELPHI_XE7_DOWN}
{$ENDIF}

{$IF CompilerVersion = 30.0}  // VER300, Delphi 2010 Seattle
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE8_UP}
  {$DEFINE DELPHI_D10_Seattle_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
  {$UNDEF DELPHI_XE7_DOWN}
{$ENDIF}


{$IF CompilerVersion = 31.0}  // VER310, Delphi D10.1 Berlin
  {$DEFINE DELPHI_2010_UP}
  {$DEFINE DELPHI_XE_UP}
  {$DEFINE DELPHI_XE2_UP}
  {$DEFINE DELPHI_XE3_UP}
  {$DEFINE DELPHI_XE4_UP}
  {$DEFINE DELPHI_XE5}
  {$DEFINE DELPHI_XE5_UP}
  {$DEFINE DELPHI_XE6_UP}
  {$DEFINE DELPHI_XE7_UP}
  {$DEFINE DELPHI_XE8_UP}
  {$DEFINE DELPHI_D10_Seattle_UP}
  {$DEFINE DELPHI_D10_1Brln_UP}
  {$DEFINE SUPPORTS_REGEX}
  {$UNDEF UNSUPPORTED_COMPILER_VERSION}
  {$UNDEF DELPHI_2010_DOWN}
  {$UNDEF DELPHI_XE_DOWN}
  {$UNDEF DELPHI_XE2_DOWN}
  {$UNDEF DELPHI_XE3_DOWN}
  {$UNDEF DELPHI_XE4_DOWN}
  {$UNDEF DELPHI_XE5_DOWN}
  {$UNDEF DELPHI_XE6_DOWN}
  {$UNDEF DELPHI_XE7_DOWN}
  {$UNDEF DELPHI_D10_Seattle_DOWN}
{$ENDIF}



{$IFDEF UNSUPPORTED_COMPILER_VERSION}
  {$MESSAGE ERROR 'Unsupported compiler version! Delphi 10.1 Berlin or later required.'}
{$ENDIF}


// The following section is copied from the includes for the OmniThread Library.
//  Apache license. Refer to the statement of copying permission and license agreement
//  in the OTL.

{$IF CompilerVersion >= 26}{$LEGACYIFEND ON}{$IFEND}

{$IF CompilerVersion < 19} //D2007
  {$DEFINE OTL_NeedsWindowsAPIs}
{$IFEND}

{$IF CompilerVersion >= 20} //D2009
  {$DEFINE OTL_Anonymous}
  {$DEFINE OTL_Generics}
  {$DEFINE OTL_HasInterlockedCompareExchangePointer}
{$IFEND}

{$IF CompilerVersion >= 21} //D2010
  {$DEFINE OTL_ERTTI}
  {$DEFINE OTL_DeprecatedResume}
  {$DEFINE OTL_KnowsParamCount}
  {$DEFINE OTL_HasTkPointer}
  {$DEFINE OTL_HasArrayOfT}
  {$DEFINE OTL_HasNameThreadForDebugging}
{$IFEND}

{$IF CompilerVersion >= 22} //DXE
  {$DEFINE OTL_HasThreadID}
  {$DEFINE OTL_HasTInterlocked}
  {$DEFINE OTL_GoodGenerics}
  {$DEFINE OTL_HasTThreadYield}
{$IFEND}

{$IF CompilerVersion >= 23} //DXE2
  {$DEFINE OTL_Supports64Bit}
  {$DEFINE OTL_HasSystemTypes}
  {$DEFINE OTL_HasCorrectNativeInt}
{$IFEND}

{$IF CompilerVersion >= 24} //DXE3
  {$DEFINE OTL_LongGetMethodInfo}
{$IFEND}

{$IF CompilerVersion >= 26} //DXE5
  {$DEFINE OTL_StrPasInAnsiStrings}
{$IFEND}

{$IF CompilerVersion >= 28} //DXE7
  {$DEFINE OTL_MobileSupport}
{$IFEND}



  {$ifdef CONDITIONALEXPRESSIONS}
    {$if CompilerVersion >= 25.0}
      {$LEGACYIFEND OFF}
    {$endif}
  {$endif}

  {$IFDEF CPU386} {$define CPUINTEL} {$ENDIF}
  {$IFDEF CPUX64} {$define CPUINTEL} {$ENDIF}

  {$IF (defined( CPU32BITS) and defined( CPU64BITS)) or ((not defined( CPU32BITS)) and (not defined( CPU64BITS)))}
    {$undef CPU32BITS}
    {$undef CPU64BITS}
    {$IF SizeOf( pointer) = 4}
      {$define  CPU32BITS}
    {$ENDIF}
    {$IF SizeOf( pointer) = 8}
      {$define   CPU64BITS}
    {$ENDIF}
  {$ENDIF}

{$IFDEF CPU64BITS}
  {$ALIGN 8}
{$ELSE}
  {$ALIGN 4}
{$ENDIF}

  {$IF (defined( CPUARM) and defined( CPUINTEL)) or ((not defined( CPUARM)) and (not defined( CPUINTEL)))}
    {$Message Fatal 'CPU = ?'}
  {$ENDIF}

  {$IF (defined( CPU32BITS) and defined( CPU64BITS)) or ((not defined( CPU32BITS)) and (not defined( CPU64BITS)))}
    {$Message Fatal 'Architecture = ?'}
  {$ENDIF}



