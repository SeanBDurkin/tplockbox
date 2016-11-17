unit DUnitM.CommandLine.General;
interface
uses Types, XMLIntf, Generics.Collections, Classes;
type

IPrimitiveCommandLine = interface
  ['{C1B61925-A98F-4A2E-9FB7-9188D4663997}']
    function ApplicationName: string;
    function Parameters: TStringDynArray;
  end;

TParamKind = (pkString, pkInteger, pkFloat, pkBoolean, pkDateTime, pkDate, pkTime, pkXML);
  // pkXML is an unecrypted XML file, specified by file-name.

ICommandLineParamValue = interface
  // Can be cast into one of IFloatParamValue etc.
  ['{E6347C57-5A7A-4765-ABD0-9091D046189D}']
    function Kind: TParamKind;
    function AsString: string;
    function IsNull: boolean;
  end;

TCommandLineDict = class( TDictionary<string,ICommandLineParamValue>)
                   end;

TCommandLineReadError = (erDocumentRead, erDocumentFileDelete);
TCommandLineReadErrorSet = set of TCommandLineReadError;

// Assume that all parameters are of the form..
//   -pName:pValue
//  where:
//    1. pName in the above is a place-marker for the parameter name.
//    2. pValue in the above is a place-marker for the parameter value.
//    3. The '-' character in the above can be substited with '/', with no effect.
//    4. The ':' character in the above can be substited with '=', with no effect.
//    5. pName is non-empty.
//    6. pName is unique.
//    7. pName is case insensitive. When read back, the lowercase form will be presented.
//    8. pName must conform to a legal unprefixed XML element name (XML 1.0 standard; 5th edition) .
//          (Therefore pName may not contain either the ':' nor the '=" characters.)
//    9. There are no restrictions on pValue, and if it is a string type, it is case sensitive.
//    10. Zero or one parameter (with pName equal to DocumentName()) is the document name.
//        This is a file name of an XML document (possibly encrypted) that contains
//        Additional parameter values passed by the client.
//    11. If a parameter is present in both the document and the raw command line,
//        the value given in the raw command line shall prevail.


ICommandLine = interface
  ['{1157FD56-7FE3-49D0-8EC0-8DCC9E361361}']
    function RawForm: IPrimitiveCommandLine;
    function ParameterDocument: IXMLDocument;
    function DocumentName: string;
    function Params: TCommandLineDict;
  end;

IStringParamValue = interface( ICommandLineParamValue)
  ['{F3A1E908-CA84-426E-9ECC-6379DAD0A794}']
  end;

IFloatParamValue = interface( ICommandLineParamValue)
  ['{D0E3E94C-16A4-489F-A9A1-B5A7A980F185}']
    procedure GetValue( var Value: double; var IsDefaulted: boolean);
  end;

IIntegerParamValue = interface( ICommandLineParamValue)
  ['{75F11C08-707F-400F-B7DB-E92EE87C0CBB}']
    procedure GetValue( var Value: integer; var IsDefaulted: boolean);
  end;

IBooleanParamValue = interface( ICommandLineParamValue)
  ['{3BE46F7B-AB36-4235-8DE4-652C4198159D}']
    procedure GetValue( var Value: boolean; var IsDefaulted: boolean);
  end;

IDateTimeParamValue = interface( ICommandLineParamValue)
  ['{A41B4335-8007-4A34-A0E1-2A82E1BD7BFF}']
    procedure GetValue( var Value: TDateTime; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  end;

IDateParamValue = interface( ICommandLineParamValue)
  ['{E505EA27-78F3-40AD-BE72-D44666802E5E}']
    procedure GetValue( var Value: TDate; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  end;

ITimeParamValue = interface( ICommandLineParamValue)
  ['{1B2433B4-85FE-45C4-BFF3-9F52BCFA4AB5}']
    procedure GetValue( var Value: TTime; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  end;

IXDocValue = interface( ICommandLineParamValue)
  ['{6A4D5311-956D-4C82-98FA-7E430E5C8FC1}']
    procedure GetValue( var Value: IXMLDocument; var SourceFileName: string);
  end;

TDecisionFunc = reference to function( const Doc: IXMLDocument; NameValues: TStrings): boolean;
TDecryptor = reference to function( CipherText: TStream; NameValues: TStrings): TStream;
  // The input stream is taken from its current position.
  // The returned stream has a position of 0, and will be of type TMemoryStream.

IParameterRegistrar = interface
  ['{A64B3211-E062-489E-88D0-F6F7B8975B1D}']
    // Name == name of the parameter
    // Default == default or deemed value if the parameter is absent.
    // AllowedInRaw == Look for the parameter on the raw command line.
    // DocXPath == If not empty, and the parameter was not found on the command line, the this is the XPath expression to the value in the document.
    // IncludesTZ = If True, the default value includes a Time-Zone component.
    // TZMinutes = If IncludesTZ, the time zone component of the default value is specified in minutes as an offset to UTC.
    procedure RegisterStringParameter  ( const Name, Default, DocXPath: string; EmptyCountsAsAbsent, AllowedInRaw: boolean);
    procedure RegisterIntegerParameter ( const Name: string; Default: integer; const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterFloatParameter   ( const Name: string; Default: double;  const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterBooleanParameter ( const Name: string; Default: boolean; const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterDateTimeParameter( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterDateParameter    ( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterTimeParameter    ( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterXMLParameter     ( const Name, DocXPath: string; AllowedInRaw: boolean; const Namespaces: string);
  end;

TParameterRegistrationFragment = reference to procedure( const Registrar: IParameterRegistrar);

ICommandLineFactory = interface
  ['{E5C15EE8-3759-41DD-96A5-3B8483475CEB}']
    function Make(
      const RawForm: IPrimitiveCommandLine;
      const DocParamName: string; // Name of the command-line parameter that
                 // specifies the file name of the xml document to be read for
                 // other parameter values. This can be null.
      const NamespaceDecls: string; // xml namespaces for the DocXPath parameters passed to the ParameterRegistry.
      var   CommandLine: ICommandLine;  // Returns the manufactured ICommandLine.
      var   Errors: TCommandLineReadErrorSet; // Returns any errors
            RegistrationProc: TParameterRegistrationFragment;  // The user defines parameter defaults and xpaths here. Can be nil.
            DoDeleleDocumentFile: TDecisionFunc; // If this function exists and returns True,
                                                 //  the document file will be deleted after it is read.
                                                 // If the document is invalid, it will not be deleted.
            DocDecryptor: TDecryptor) // If assigned, it designates that the document is encrypted, and this function will decrypt it.
          : boolean; // Return True if there we no errors.
  end;


IApplicationSpecificCommandLineFactory = interface
  ['{E43FFC99-3F50-4FF2-9017-6FE97CB73BE9}']
    function Make(
      const RawForm: IPrimitiveCommandLine;
      const ProtocolReader: ICommandLineFactory;
      var   CommandLine: ICommandLine;
      var   Errors: TCommandLineReadErrorSet)
          : boolean;
  end;

function StandardPrimitiveCommandLine: IPrimitiveCommandLine;
function StandardCommandLineFactory: ICommandLineFactory;

implementation














type
ICommandLineParamValue_Internal = interface
  ['{36EB5296-7480-483B-BEA5-7774393340AC}']
    function SetFromRawCommandLine( const Value: string): boolean;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;
  end;

TCommandLineParamValue = class abstract( TInterfacedObject, ICommandLineParamValue, ICommandLineParamValue_Internal)
  protected
    FisNull: boolean;
    FAllowedInRaw: boolean;
    FDocXPath: string;
    function Kind: TParamKind;    virtual; abstract;
    function AsString: string;    virtual; abstract;
    function IsNull: boolean;     virtual;
    function SetFromRawCommandLine( const Value: string): boolean;      virtual; abstract;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    virtual;
    constructor Create( isNull, AllowedInRaw: boolean; const DocXPath: string);
  end;


TStringParamValue = class( TCommandLineParamValue, IStringParamValue)
  protected
    FEmptyCountsAsAbsent: boolean;
    FDefault: string;
    function Kind: TParamKind;    override;
    function AsString: string;    override;
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( const Default, DocXPath: string; EmptyCountsAsAbsent, AllowedInRaw: boolean);
  end;

TFloatParamValue = class( TCommandLineParamValue, IFloatParamValue)
  private
    FDefault: double;
    procedure GetValue( var Value: double; var IsDefaulted: boolean);
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( Default: double;  const DocXPath: string; AllowedInRaw: boolean);
  end;

TIntegerParamValue = class( TCommandLineParamValue, IIntegerParamValue)
  private
    FDefault: integer;
    procedure GetValue( var Value: integer; var IsDefaulted: boolean);
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( Default: integer; const DocXPath: string; AllowedInRaw: boolean);
  end;

TBooleanParamValue = class( TCommandLineParamValue, IBooleanParamValue)
  private
    FDefault: boolean;
    procedure GetValue( var Value: boolean; var IsDefaulted: boolean);
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( Default: boolean; const DocXPath: string; AllowedInRaw: boolean);
  end;

TDateTimeParamValue = class( TCommandLineParamValue, IDateTimeParamValue)
  private
    procedure GetValue( var Value: TDateTime; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  protected
    FDefault: TDateTime;
    FIncludesTZ: boolean;
    FTZMinutes: integer;
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( const DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean; TZMinutes: integer);
  end;

TDateParamValue = class( TDateTimeParamValue, IDateParamValue)
  private
    procedure GetValue( var Value: TDate; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  protected
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( const DocXPath: string; AllowedInRaw: boolean; Default: TDate; IncludesTZ: boolean; TZMinutes: integer);
  end;

TTimeParamValue = class( TCommandLineParamValue, ITimeParamValue)
  private
    procedure GetValue( var Value: TTime; var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
  protected
    function SetFromRawCommandLine( const Value: string): boolean;      override;
    function SetFromParamDocument ( const Value: IXMLNode): boolean;    override;
  public
    constructor Create( const DocXPath: string; AllowedInRaw: boolean; Default: TTime; IncludesTZ: boolean; TZMinutes: integer);
  end;

TXDocValue = class( TCommandLineParamValue, IXDocValue)
  private
    FNamespaces: string;
    procedure GetValue( var Value: IXMLDocument; var SourceFileName: string);
  public
    constructor Create( const DocXPath: string; AllowedInRaw: boolean; const Namespaces: string);
  end;

TPrimitiveCommandLine = class( TInterfacedObject, IPrimitiveCommandLine)
  private
    function ApplicationName: string;
    function Parameters: TStringDynArray;
  end;


TCommandLineFactory = class( TInterfacedObject, ICommandLineFactory)
  private
    function Make(
      const RawForm: IPrimitiveCommandLine;
      const DocParamName: string;
      const NamespaceDecls: string;
      var   CommandLine: ICommandLine;
      var   Errors: TCommandLineReadErrorSet;
            RegistrationProc: TParameterRegistrationFragment;
            DoDeleleDocumentFile: TDecisionFunc;
            DocDecryptor: TDecryptor)
          : boolean;
  end;

TParameterRegistrar = class( TInterfacedObject, IParameterRegistrar)
  private
    FParams: TCommandLineDict;
    procedure RegisterStringParameter  ( const Name, Default, DocXPath: string; EmptyCountsAsAbsent, AllowedInRaw: boolean);
    procedure RegisterIntegerParameter ( const Name: string; Default: integer; const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterFloatParameter   ( const Name: string; Default: double;  const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterBooleanParameter ( const Name: string; Default: boolean; const DocXPath: string; AllowedInRaw: boolean);
    procedure RegisterDateTimeParameter( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterDateParameter    ( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterTimeParameter    ( const Name, DocXPath: string; AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean = False; TZMinutes: integer = 0);
    procedure RegisterXMLParameter     ( const Name, DocXPath: string; AllowedInRaw: boolean; const Namespaces: string);

  public
    constructor Create( Params1: TCommandLineDict);
  end;

TCommandLine = class( TInterfacedObject, ICommandLine)
  private
    FRawForm: IPrimitiveCommandLine;
    FParamDoc: IXMLDocument;
    FDocName: string;
    FParams: TCommandLineDict;

    function RawForm: IPrimitiveCommandLine;
    function ParameterDocument: IXMLDocument;
    function DocumentName: string;
    function Params: TCommandLineDict;

  public
    constructor Create(
      const RawForm: IPrimitiveCommandLine;
      const DocParamName: string;
      const NamespaceDecls: string;
      var   Errors: TCommandLineReadErrorSet;
            RegistrationProc: TParameterRegistrationFragment;
            DoDeleleDocumentFile: TDecisionFunc;
            DocDecryptor: TDecryptor);
    destructor Destroy; override;
  end;

function StandardPrimitiveCommandLine: IPrimitiveCommandLine;
begin
result := TPrimitiveCommandLine.Create
end;

function StandardCommandLineFactory: ICommandLineFactory;
begin
result := TCommandLineFactory.Create
end;



function TPrimitiveCommandLine.ApplicationName: string;
begin
result := ParamStr( 0)
end;

function TPrimitiveCommandLine.Parameters: TStringDynArray;
begin
// TODO

end;


function TCommandLineFactory.Make(const RawForm: IPrimitiveCommandLine;
  const DocParamName, NamespaceDecls: string; var CommandLine: ICommandLine;
  var Errors: TCommandLineReadErrorSet;
  RegistrationProc: TParameterRegistrationFragment;
  DoDeleleDocumentFile: TDecisionFunc; DocDecryptor: TDecryptor): boolean;
begin
CommandLine := TCommandLine.Create(
  RawForm, DocParamName, NamespaceDecls,
  Errors, RegistrationProc, DoDeleleDocumentFile, DocDecryptor)
end;


constructor TCommandLine.Create(
  const RawForm: IPrimitiveCommandLine;
  const DocParamName, NamespaceDecls: string;
  var Errors: TCommandLineReadErrorSet;
  RegistrationProc: TParameterRegistrationFragment;
  DoDeleleDocumentFile: TDecisionFunc; DocDecryptor: TDecryptor);
var
  NameValues: TStrings;
  Reg: IParameterRegistrar;
begin
FRawForm  := RawForm;
Errors    := [];
FParamDoc := nil;
FParams   := TCommandLineDict.Create;
NameValues := TStringList.Create;
Reg := TParameterRegistrar.Create( FParams);

// TODO

NameValues.Free
end;

destructor TCommandLine.Destroy;
begin
FParams.Free;
inherited
end;

function TCommandLine.DocumentName: string;
begin
result := FDocName
end;

function TCommandLine.ParameterDocument: IXMLDocument;
begin
result := FParamDoc
end;

function TCommandLine.Params: TCommandLineDict;
begin
result := FParams
end;

function TCommandLine.RawForm: IPrimitiveCommandLine;
begin
result := FRawForm
end;

{ TParameterRegistrar }

constructor TParameterRegistrar.Create( Params1: TCommandLineDict);
begin
FParams := Params1
end;

procedure TParameterRegistrar.RegisterBooleanParameter( const Name: string;
  Default: boolean; const DocXPath: string; AllowedInRaw: boolean);
begin

end;

procedure TParameterRegistrar.RegisterDateParameter( const Name,
  DocXPath: string; AllowedInRaw: boolean; Default: TDateTime;
  IncludesTZ: boolean; TZMinutes: integer);
begin

end;

procedure TParameterRegistrar.RegisterDateTimeParameter( const Name,
  DocXPath: string; AllowedInRaw: boolean; Default: TDateTime;
  IncludesTZ: boolean; TZMinutes: integer);
begin

end;

procedure TParameterRegistrar.RegisterFloatParameter( const Name: string;
  Default: double; const DocXPath: string; AllowedInRaw: boolean);
begin

end;

procedure TParameterRegistrar.RegisterIntegerParameter( const Name: string;
  Default: integer; const DocXPath: string; AllowedInRaw: boolean);
begin

end;

procedure TParameterRegistrar.RegisterStringParameter( const Name, Default,
  DocXPath: string; EmptyCountsAsAbsent, AllowedInRaw: boolean);
begin

end;

procedure TParameterRegistrar.RegisterTimeParameter( const Name,
  DocXPath: string; AllowedInRaw: boolean; Default: TDateTime;
  IncludesTZ: boolean; TZMinutes: integer);
begin

end;

procedure TParameterRegistrar.RegisterXMLParameter( const Name, DocXPath: string;
  AllowedInRaw: boolean; const Namespaces: string);
begin

end;

{ TCommandLineParamValue }

constructor TCommandLineParamValue.Create(isNull, AllowedInRaw: boolean;
  const DocXPath: string);
begin

end;

function TCommandLineParamValue.IsNull: boolean;
begin

end;

function TCommandLineParamValue.SetFromParamDocument(
  const Value: IXMLNode): boolean;
begin

end;

{ TStringParamValue }

function TStringParamValue.AsString: string;
begin

end;

constructor TStringParamValue.Create(const Default, DocXPath: string;
  EmptyCountsAsAbsent, AllowedInRaw: boolean);
begin

end;

function TStringParamValue.Kind: TParamKind;
begin

end;

function TStringParamValue.SetFromParamDocument(const Value: IXMLNode): boolean;
begin

end;

function TStringParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TFloatParamValue }

constructor TFloatParamValue.Create(Default: double; const DocXPath: string;
  AllowedInRaw: boolean);
begin

end;

procedure TFloatParamValue.GetValue(var Value: double;
  var IsDefaulted: boolean);
begin

end;

function TFloatParamValue.SetFromParamDocument(const Value: IXMLNode): boolean;
begin

end;

function TFloatParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TIntegerParamValue }

constructor TIntegerParamValue.Create(Default: integer; const DocXPath: string;
  AllowedInRaw: boolean);
begin

end;

procedure TIntegerParamValue.GetValue(var Value: integer;
  var IsDefaulted: boolean);
begin

end;

function TIntegerParamValue.SetFromParamDocument(
  const Value: IXMLNode): boolean;
begin

end;

function TIntegerParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TBooleanParamValue }

constructor TBooleanParamValue.Create(Default: boolean; const DocXPath: string;
  AllowedInRaw: boolean);
begin

end;

procedure TBooleanParamValue.GetValue(var Value, IsDefaulted: boolean);
begin

end;

function TBooleanParamValue.SetFromParamDocument(
  const Value: IXMLNode): boolean;
begin

end;

function TBooleanParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TDateTimeParamValue }

constructor TDateTimeParamValue.Create(const DocXPath: string;
  AllowedInRaw: boolean; Default: TDateTime; IncludesTZ: boolean;
  TZMinutes: integer);
begin

end;

procedure TDateTimeParamValue.GetValue(var Value: TDateTime;
  var TZMinutes: integer; var TZIncluded, IsDefaulted: boolean);
begin

end;

function TDateTimeParamValue.SetFromParamDocument(
  const Value: IXMLNode): boolean;
begin

end;

function TDateTimeParamValue.SetFromRawCommandLine(
  const Value: string): boolean;
begin

end;

{ TDateParamValue }

constructor TDateParamValue.Create(const DocXPath: string;
  AllowedInRaw: boolean; Default: TDate; IncludesTZ: boolean;
  TZMinutes: integer);
begin

end;

procedure TDateParamValue.GetValue(var Value: TDate; var TZMinutes: integer;
  var TZIncluded, IsDefaulted: boolean);
begin

end;

function TDateParamValue.SetFromParamDocument(const Value: IXMLNode): boolean;
begin

end;

function TDateParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TTimeParamValue }

constructor TTimeParamValue.Create(const DocXPath: string;
  AllowedInRaw: boolean; Default: TTime; IncludesTZ: boolean;
  TZMinutes: integer);
begin

end;

procedure TTimeParamValue.GetValue(var Value: TTime; var TZMinutes: integer;
  var TZIncluded, IsDefaulted: boolean);
begin

end;

function TTimeParamValue.SetFromParamDocument(const Value: IXMLNode): boolean;
begin

end;

function TTimeParamValue.SetFromRawCommandLine(const Value: string): boolean;
begin

end;

{ TXDocValue }

constructor TXDocValue.Create(const DocXPath: string; AllowedInRaw: boolean;
  const Namespaces: string);
begin

end;

procedure TXDocValue.GetValue(var Value: IXMLDocument;
  var SourceFileName: string);
begin

end;

end.
