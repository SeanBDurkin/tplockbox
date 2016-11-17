unit SBD.Environment;
interface

type
TProcessorArchitecture = (
  paIntel,    // x86
  paAMD64,    // x64 (AMD or Intel)
  paIA64,     // Intel Itanium Processor Family (IPF)
  paIOS,
  paOSX,
  paAndroid,
  paLinux,
  paUnknown); // Unknown processor.

TProcessorInformation = class
  public
    class function Architecture: TProcessorArchitecture;
    class function OSVersion: string;
    class function UTC_Offset: string;
  end;


implementation


uses
  {$IFDEF MSWINDOWS}
    Windows,
 {$ENDIF}
  SysUtils;


{$IFDEF MSWINDOWS}
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9; // x64 (AMD or Intel)
  PROCESSOR_ARCHITECTURE_IA64  = 6; // Intel Itanium Processor Family (IPF)
  PROCESSOR_ARCHITECTURE_INTEL = 0; // x86
  PROCESSOR_ARCHITECTURE_UNKNOWN = $FFFF;  // Unknown processor.

  Architectures: array[ 0..2 ] of record
  FCode: word; FArch: TProcessorArchitecture end = (
   (FCode: PROCESSOR_ARCHITECTURE_INTEL; FArch: paIntel),
   (FCode: PROCESSOR_ARCHITECTURE_AMD64; FArch: paAMD64),
   (FCode: PROCESSOR_ARCHITECTURE_IA64 ; FArch: paIA64));

type
TGetNativeSystemInfo = procedure( var lpSystemInfo: TSystemInfo); stdcall;

function WinArchitecture: TProcessorArchitecture;
var
  KernelLib: integer;
  GetNativeSystemInfoProc: TGetNativeSystemInfo;
  Ok: boolean;
  SystemInfo: TSystemInfo;
  j: integer;
begin
result := paUnknown;
GetNativeSystemInfoProc := nil;
KernelLib := SafeLoadLibrary( 'Kernel32.dll');
try
  if KernelLib <> 0 then
    GetNativeSystemInfoProc := GetProcAddress(
      KernelLib, 'GetNativeSystemInfoA');
  Ok := assigned( GetNativeSystemInfoProc);
  if Ok then
    GetNativeSystemInfoProc( SystemInfo)
finally
  if KernelLib <> 0 then
    FreeLibrary( KernelLib)
  end;
if not Ok then
  windows.GetSystemInfo( SystemInfo);
for j := Low( Architectures) to High( Architectures) do
  begin
  if Architectures[j].FCode <> SystemInfo.wProcessorArchitecture then
    continue;
  result := Architectures[j].FArch;
  break
  end
end;
{$ENDIF}

class function TProcessorInformation.Architecture: TProcessorArchitecture;
begin
{$IFDEF MSWINDOWS}
  result := WinArchitecture;
{$ENDIF}
{$IFDEF IOS}
  result := paIOS;
{$ENDIF}
{$IFDEF MACOS}
  result := paOSX;
{$ENDIF}
{$IFDEF ANDROID}
  result := paAndroid;
{$ENDIF}
{$IFDEF LINUX}
  result := paLinux;
{$ENDIF}
end;

class function TProcessorInformation.OSVersion: string;
begin
{$IFDEF MSWINDOWS}
  result := Format( '%d.%d %s', [SysUtils.Win32MajorVersion,
                                 SysUtils.Win32MinorVersion,
                                 SysUtils.Win32CSDVersion]);
{$ENDIF}
{$IFDEF IOS}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF MACOS}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF ANDROID}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF LINUX}
  result := 'function not yet developed';
{$ENDIF}
end;

class function TProcessorInformation.UTC_Offset: string;
{$IFDEF MSWINDOWS}
var
  Info: TTimeZoneInformation;
{$ENDIF}

  function BiasAsString( BiasMinutes: integer): string;
  begin
  if BiasMinutes = 0 then
      result := 'Z'
    else
      begin
      result := Format( '%.2d:%.2d', [Abs( BiasMinutes) div 60, Abs( BiasMinutes) mod 60]);
      if BiasMinutes < 0 then
          result := 'UTC-' + result
        else
          result := 'UTC+' + result
      end
  end;

begin
{$IFDEF MSWINDOWS}
case GetTimeZoneInformation( Info) of
  TIME_ZONE_ID_INVALID,
  TIME_ZONE_ID_UNKNOWN:  result := '(unknown)';
  TIME_ZONE_ID_STANDARD: result := BiasAsString( Info.Bias + Info.StandardBias);
  TIME_ZONE_ID_DAYLIGHT: result := BiasAsString( Info.Bias + Info.DaylightBias);
  end;
{$ENDIF}
{$IFDEF IOS}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF MACOS}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF ANDROID}
  result := 'function not yet developed';
{$ENDIF}
{$IFDEF LINUX}
  result := 'function not yet developed';
{$ENDIF}
end;

end.
