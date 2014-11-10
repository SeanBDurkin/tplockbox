unit TPLB3.Compatibility;

interface

{$IFDEF MSWINDOWS}
uses
  Windows;
{$ENDIF}

{$IF CompilerVersion <= 18}
type
  TBytes = packed array of Byte;
{$IFEND}

// X3 and above have built-in, cross-platform atomic operations (System.Atomic*).
// On XE2 and below, fall back to the Win32 API on MSWINDOWS and non-atomic operations
// on other platforms.
{$IF CompilerVersion < 24}
  function AtomicIncrement(var Target: Integer): Integer;
  function AtomicDecrement(var Target: Integer): Integer;
{$IFEND}

implementation

{$IF CompilerVersion < 24}
  function AtomicIncrement(var Target: Integer): Integer;
  begin
    {$IFDEF MSWINDOWS}
      Result := InterlockedIncrement(Target);
    {$ELSE}
      // TODO: find a platform-specific way of doing this atomically
      Inc(Target);
    {$ENDIF}
  end;

  function AtomicDecrement(var Target: Integer): Integer;
  begin
    {$IFDEF MSWINDOWS}
      Result := InterlockedDecrement(Target);
    {$ELSE}
      // TODO: find a platform-specific way of doing this atomically
      Dec(Target);
    {$ENDIF}
  end;
{$IFEND}

end.

