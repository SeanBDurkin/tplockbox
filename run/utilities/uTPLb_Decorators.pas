unit uTPLb_Decorators;
interface

type
IControlObject = interface
  ['{420914AC-6242-417E-8D18-7B163056DA60}']
  function ControlObject: TObject;
  end;

{$IF compilerversion >= 21}
IntegerRange = class( TCustomAttribute)
  private
    FMin, FMax: Integer;

  public
    constructor Create( Min1, Max1: Integer);
    property Min : integer read FMin;
    property Max : Integer read FMax;
  end;

DesignDescription = class( TCustomAttribute)
  private
    FDescription: string;
  public
    constructor Create( const Description1: string);
    property Description: string read FDescription;
  end;
{$IFEND}


IVariableSeedSize = Interface
['{38096CBB-5ACB-43D7-826A-C21812F6E447}']
    function MinSeedByteSize: integer;
    function MaxSeedByteSize: integer;

    property Min : integer read MinSeedByteSize;
    property Max : Integer read MaxSeedByteSize;
  end;



implementation



{$IF compilerversion >= 21}
constructor IntegerRange.Create( Min1, Max1: Integer);
begin
FMin := Min1;
FMax := Max1
end;

constructor DesignDescription.Create( const Description1: string);
begin
FDescription := Description1
end;

{$IFEND}


end.
