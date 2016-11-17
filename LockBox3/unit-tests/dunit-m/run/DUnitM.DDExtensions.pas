{***************************************************************************}
{                                                                           }
{        DUnit-M                                                            }
{                                                                           }
{        Copyright (C) 2015 Sean B. Durkin                                  }
{                                                                           }
{        Author: Sean B. Durkin                                             }
{        sean@seanbdurkin.id.au                                             }
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

{***************************************************************************}
{                                                                           }
{               NOTICE of DERIVATION and CHANGE                             }
{                                                                           }
{  This project is a derived work. It is dervied from the DUnitX library    }
{  created By Vincent Parrett                                               }
{  (hosted at https://github.com/VSoftTechnologies/DUnitX).                 }
{  The copyright holder for the original code is as per the following       }
{  comment block. The copyright holder for all changes in this file from    }
{  that base is as denoted following.                                       }
{                                                                           }
{        Copyright (C) 2015 Sean B. Durkin                                  }
{                                                                           }
{        Author: Sean B. Durkin                                             }
{        sean@seanbdurkin.id.au                                             }
{***************************************************************************}

// The Original DUnitX comment header was ....

{***************************************************************************}
{                                                                           }
{           DUnitX                                                          }
{                                                                           }
{           Copyright (C) 2012 Vincent Parrett                              }
{                                                                           }
{           vincent@finalbuilder.com                                        }
{           http://www.finalbuilder.com                                     }
{                                                                           }
{ Contributors : Vincent Parrett                                            }
{                Jason Smith                                                }
{                Nick Hodges                                                }
{                Nicholas Ring                                              }
{                                                                           }
{***************************************************************************}

// DUnit-M is a unit testing framework for software written in and for Delphi XE7
// or later. Some of the files from this project are either copied or derived
// from the DUnitX project.

unit DUnitM.DDExtensions;
{$I ../includes/DUnitM.inc}
interface
uses DUnitM.UnitTestFramework, Rtti
      {$ifdef MSWINDOWS}
        , XMLIntf
      {$endif}
     ;

type
{$ifdef MSWINDOWS}
ADOAttribute = class( TDataDrivenTestAttributeBase)
public
  sConnectionString: string;
  sTableName: string;
  constructor Create( const psConnectionString, psTableName: string);
  function  SignatureMatches( poTestMethod: TRttiMethod): boolean; override;
  function  AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; override;
  procedure CustomiseConnectionString( var sFinalConString, sFinalTableName: string); virtual;
end;
{$endif}

{$ifdef MSWINDOWS}
ExcelAttribute = class( ADOAttribute)
public
  constructor Create( const psTableName: string
    {$ifdef SUPPORTS_REGEX}
      ; const psConnectionOverrides: string = ''
    {$endif});
  function AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; override;
end;
{$endif}

TextFileAttribute = class( TDataDrivenTestAttributeBase)
public
  function SignatureMatches( poTestMethod: TRttiMethod): boolean; override;
  function AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; override;
end;

{$ifdef MSWINDOWS}
XMLAttribute = class( TDataDrivenTestAttributeBase)
public
  sXPath: string;
  sNamespaceDecls: string;
  constructor Create( const psXPath: string; const psNamespaceDecls: string = '');
  function SignatureMatches( poTestMethod: TRttiMethod): boolean; override;
  function AcquireFactory( ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory; override;
end;
{$endif}


implementation







uses DUnitM.TestCase, DUnitM.TestNode, TypInfo, SysUtils, Classes,
     DUnitM.RttiUtils, IOUtils, SBD.Utils.XML2
      {$ifdef SUPPORTS_REGEX}
       , RegularExpressions
      {$endif}
      {$ifdef MSWINDOWS}
       {$ifdef DELPHI_XE2_UP}
       , Data.Win.ADODB
       {$else}
       , ADODB
       {$endif}
      {$endif}
     ;


type
TDDTestCaseFactoryBase = class abstract( TInterfacedObject, IDataDrivenTestCaseFactory)
  protected
    ProcAsObject: TReflector;
    ProcMethod: TRttiMethod;
    DrivingAttribute: TDataDrivenTestAttributeBase;
    function Repeats: integer;            virtual;
    function MethodSkips: integer;        virtual;
    function Name: string;                virtual;
    function Make: ITestCase;             virtual; abstract;
    function ManagerClass: TDataDrivenTestCaseNavManagerClass; virtual; abstract;
  public
    constructor Create( Attri: TDataDrivenTestAttributeBase; ProcParentAsObject: TReflector; ProcType: TRttiMethod);  virtual;
    class function SignatureMatches( poTestMethod: TRttiMethod): boolean;   virtual; abstract;
  end;

{$ifdef MSWINDOWS}
TADOFactory = class( TDDTestCaseFactoryBase)
  protected
    function Make: ITestCase;             override;
    function ManagerClass: TDataDrivenTestCaseNavManagerClass; override;
    function MethodSkips: integer;        override;

  protected type
    TDDManager = class( TDataDrivenTestCaseNavManager)
      public
        constructor Create( Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase); override;
        function  EstimatedCount( TestBed: TObject; var CountItems: integer): boolean;   override;
        function  OpenLineItemCursor( TestBed: TObject): ITestCaseLineItemCursor;        override;
        procedure TestLineItem( TestBed: TObject; Cursor: ITestCaseLineItemCursor);      override;

      protected
        FGetFileNameMeth : TRttiMethod;
        sConnectionString: string;
        sTableName       : string;
        FAttri           : ADOAttribute;
        procedure MakeComponents(
          TestBed: TObject; var ADOCon: TADOConnection; var ADOTable: TADOTable);
      end;

  protected type
    TTestCaseLineItemCursor = class( TInterfacedObject, ITestCaseLineItemCursor)
      protected
        FADOCon: TADOConnection;
        FADOTable: TADOTable;
        FisFirst: boolean;
        function  Next: boolean;                                                           virtual;
        procedure GetLineItemDescription( RowNumber: integer; var sDescription: string);   virtual;
        function  LineItemObject: TObject;                                                 virtual;
      public
        constructor Create( ADOCon: TADOConnection; ADOTable: TADOTable);
        destructor Destroy; override;
      end;

  public
    class function SignatureMatches( poTestMethod: TRttiMethod): boolean;  override;
  end;
{$endif}

TTextFactory = class( TDDTestCaseFactoryBase)
  protected
    function Make: ITestCase;             override;
    function ManagerClass: TDataDrivenTestCaseNavManagerClass; override;
    function MethodSkips: integer;        override;

  protected type
    TDDManager = class( TDataDrivenTestCaseNavManager)
      public
        constructor Create( Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase); override;
        function  EstimatedCount( TestBed: TObject; var CountItems: integer): boolean;   override;
        function  OpenLineItemCursor( TestBed: TObject): ITestCaseLineItemCursor;        override;
        procedure TestLineItem( TestBed: TObject; Cursor: ITestCaseLineItemCursor);      override;

      protected
        FGetFileNameMeth : TRttiMethod;
        function GetTextFileName( TestBed: TObject): string;
      end;

  protected type
    TTextLineCursor = class( TInterfacedObject, ITestCaseLineItemCursor)
      protected
        FIdx: integer;
        FData: TStrings;
        FCurrent: TObject;
        function  Next: boolean;                                                           virtual;
        procedure GetLineItemDescription( RowNumber: integer; var sDescription: string);   virtual;
        function  LineItemObject: TObject;                                                 virtual;
      public
        constructor Create( const sFileName: string);
        destructor Destroy; override;
      end;

  public
    class function SignatureMatches( poTestMethod: TRttiMethod): boolean;  override;
  end;

{$ifdef MSWINDOWS}
TXMLFactory = class( TDDTestCaseFactoryBase)
  protected
    function Make: ITestCase;             override;
    function ManagerClass: TDataDrivenTestCaseNavManagerClass; override;
    function MethodSkips: integer;        override;

  protected type
    TDDManager = class( TDataDrivenTestCaseNavManager)
      public
        constructor Create( Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase); override;
        function  EstimatedCount( TestBed: TObject; var CountItems: integer): boolean;   override;
        function  OpenLineItemCursor( TestBed: TObject): ITestCaseLineItemCursor;        override;
        procedure TestLineItem( TestBed: TObject; Cursor: ITestCaseLineItemCursor);      override;

      protected
        FGetFileNameMeth : TRttiMethod;
        sXPath: string;
        sNamespaceDecls: string;
      end;

  protected type
    TNodeCursor = class( TInterfacedObject, ITestCaseLineItemCursor)
      protected
        FDoc    : IXMLDocument;
        FCursor : IEnumerator<IXMLNode>;
        FCurrent: TObject;
        function  Next: boolean;                                                           virtual;
        procedure GetLineItemDescription( RowNumber: integer; var sDescription: string);   virtual;
        function  LineItemObject: TObject;                                                 virtual;
      public
        constructor Create( const FileName1, XPath1, NamespaceDecl1: string);
        destructor Destroy; override;
      end;

  public
    class function SignatureMatches( poTestMethod: TRttiMethod): boolean;  override;
  end;
{$endif}









{$ifdef MSWINDOWS}
constructor ADOAttribute.Create( const psConnectionString, psTableName: string);
begin
sConnectionString := psConnectionString;
sTableName        := psTableName
end;

procedure ADOAttribute.CustomiseConnectionString(
  var sFinalConString, sFinalTableName: string);
begin
end;

function ADOAttribute.AcquireFactory(
  ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory;
begin
result := TADOFactory.Create( self, ProcParentAsObject as TReflector, ProcType)
end;

function ADOAttribute.SignatureMatches( poTestMethod: TRttiMethod): boolean;
begin
result := TADOFactory.SignatureMatches( poTestMethod)
end;


constructor ExcelAttribute.Create( const psTableName: string
    {$ifdef SUPPORTS_REGEX}
      ; const psConnectionOverrides: string = ''
    {$endif});
var
  ConProps: TStrings;
  {$ifdef SUPPORTS_REGEX}
    Match: TMatch;
  {$endif}
  sCon, sPropDef: string;
begin
ConProps := TStringList.Create;
ConProps.Add( 'Provider=Microsoft.ACE.OLEDB.12.0');
ConProps.Add( 'Mode=Read');
ConProps.Add( 'Extended Properties="Excel 12.0 Xml;HDR=YES"');
ConProps.Add( 'Persist Security Info=False');
{$ifdef SUPPORTS_REGEX}
if psConnectionOverrides <> '' then
  for Match in TRegex.Matches( psConnectionOverrides+';',
                               '([^;="]+)=([^;="]*|("[^"]*")+);') do
    ConProps.Values[ Match.Groups[1].Value] := Match.Groups[2].Value;
{$endif}
for sPropDef in ConProps do
  sCon := sCon + sPropDef + ';';
Delete( sCon, Length( sCon), 1);
inherited Create( sCon, psTableName)
end;

function ExcelAttribute.AcquireFactory(
  ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory;
begin
result := TADOFactory.Create( self, ProcParentAsObject as TReflector, ProcType)
end;
{$endif}



function TextFileAttribute.AcquireFactory(
  ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory;
begin
result := TTextFactory.Create( self, ProcParentAsObject as TReflector, ProcType)
end;

function TextFileAttribute.SignatureMatches( poTestMethod: TRttiMethod): boolean;
begin
result := TTextFactory.SignatureMatches( poTestMethod)
end;


{$ifdef MSWINDOWS}
constructor XMLAttribute.Create( const psXPath, psNamespaceDecls: string);
begin
sXPath          := psXPath;
sNamespaceDecls := psNamespaceDecls
end;


function XMLAttribute.AcquireFactory(
  ProcParentAsObject: TObject; ProcType: TRttiMethod): IDataDrivenTestCaseFactory;
begin
result := TXMLFactory.Create( self, ProcParentAsObject as TReflector, ProcType)
end;

function XMLAttribute.SignatureMatches( poTestMethod: TRttiMethod): boolean;
begin
result := TXMLFactory.SignatureMatches( poTestMethod)
end;
{$endif}


constructor TDDTestCaseFactoryBase.Create(
  Attri: TDataDrivenTestAttributeBase; ProcParentAsObject: TReflector; ProcType: TRttiMethod);
begin
ProcAsObject     := ProcParentAsObject;
ProcMethod       := ProcType;
DrivingAttribute := Attri
end;

function TDDTestCaseFactoryBase.MethodSkips: integer;
begin
result := 0
end;

function TDDTestCaseFactoryBase.Name: string;
begin
result := ProcMethod.Name
end;

function TDDTestCaseFactoryBase.Repeats: integer;
begin
result := 1
end;


{$ifdef MSWINDOWS}
function TADOFactory.Make: ITestCase;
begin
result := TDataDrivenTestCase.CreateCase( ManagerClass, ProcAsObject, ProcMethod, DrivingAttribute)
end;

function TADOFactory.ManagerClass: TDataDrivenTestCaseNavManagerClass;
begin
result := TDDManager
end;

function TADOFactory.MethodSkips: integer;
begin
result := 1
end;

class function TADOFactory.SignatureMatches( poTestMethod: TRttiMethod): boolean;
// Signature looks like ...
//    {$ifdef MSWINDOWS}
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.ADO('ADO-connection-string','TabName')]
//        procedure TestIt2( DataSource: TADOTable);
//        function  TestIt2FileName: string;
//    {$endif}
var
  FileMeth: TRttiMethod;
  DataSourceParam: TRttiParameter;
  ExpectedFlags: TParamFlags;
begin
result := (poTestMethod.MethodKind = mkProcedure) and
          (Length( poTestMethod.GetParameters) = 1);
if not result then exit;
DataSourceParam := poTestMethod.GetParameters[0];
{$ifdef DELPHI_XE2_UP}
  ExpectedFlags := [pfAddress];
{$else}
  ExpectedFlags := [];
{$endif}
result := (DataSourceParam.ParamType.TypeKind = tkClass) and
          (DataSourceParam.Flags = ExpectedFlags) and
          (DataSourceParam.ParamType.Handle = TypeInfo( TADOTable)) and

          NextDeclaredMethod( poTestMethod, FileMeth) and
          (FileMeth.MethodKind = mkFunction) and
          (Length( FileMeth.GetParameters) = 0) and
          (FileMeth.ReturnType.TypeKind = tkUString)
end;


constructor TADOFactory.TDDManager.Create(
  Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase);
begin
PrimitiveCreate( Parent, Method);
NextDeclaredMethod( Method, FGetFileNameMeth);  // function  TestIt2FileName: string;
FAttri := nil;
if Attribute is ADOAttribute then
  begin
  FAttri                 := ADOAttribute( Attribute);
  self.sConnectionString := FAttri.sConnectionString;
  self.sTableName        := FAttri.sTableName
  end
end;

procedure TADOFactory.TDDManager.MakeComponents(
  TestBed: TObject; var ADOCon: TADOConnection; var ADOTable: TADOTable);
var
  sCon, sTabName: string;
  sFN: string;
begin
ADOCon := TADOConnection.Create( nil);
with ADOCon do
  begin
  Attributes          := [];
  CommandTimeout      := 30;
  ConnectionTimeout   := 15;
  ConnectOptions      := coConnectUnspecified;
  CursorLocation      := clUseClient;
  IsolationLevel      := ilCursorStability;
  KeepConnection      := True;
  LoginPrompt         := False;
  Mode                := cmRead
  end;
ADOTable := TADOTable.Create( nil);
sTabName := self.sTableName;
with ADOTable do
  begin
  AutoCalcFields := False;
  CacheSize      := 1;
  CommandTimeout := 30;
  Connection     := ADOCon;
  CursorLocation := clUseClient;
  CursorType     := ctStatic;
  EnableBCD      := True;
  ExecuteOptions := [];
  LockType       := ltReadOnly;
  MarshalOptions := moMarshalAll;
  ReadOnly       := True;
  TableDirect    := not sTabName.StartsWith('[')
  end;
sCon := self.sConnectionString;
try
  sFN := FGetFileNameMeth.Invoke( TestBed, []).AsString
except
  sFN := ''
  end;
if sFN <> '' then
  begin
  if sCon <> '' then
    sCon := sCon + ';';
  sCon := sCon + 'Data Source=' + sFN
  end;
if assigned( FAttri) then
  FAttri.CustomiseConnectionString( sCon, sTabName);
ADOCon.ConnectionString := sCon;
ADOTable.TableName      := sTabName
end;

function TADOFactory.TDDManager.EstimatedCount(
  TestBed: TObject; var CountItems: integer): boolean;
var
  ADOCon: TADOConnection;
  ADOTable: TADOTable;
begin
CountItems := 0;
MakeComponents( TestBed, ADOCon, ADOTable);
try try
  ADOCon.Connected := True;
  ADOTable.Active  := True;
  ADOTable.Active  := False;
  ADOTable.Active  := True;
  if ADOTable.Active then
    CountItems := ADOTable.RecordCount;
  result := ADOTable.Active
except on E: Exception do
  result := False
  end
finally
  ADOTable.Active  := False;
  ADOCon.Connected := False;
  ADOTable.Free;
  ADOCon.Free
  end
end;

function TADOFactory.TDDManager.OpenLineItemCursor(
  TestBed: TObject): ITestCaseLineItemCursor;
var
  ADOCon: TADOConnection;
  ADOTable: TADOTable;
  Owns: boolean;
begin
result := nil;
Owns   := True;
MakeComponents( TestBed, ADOCon, ADOTable);
try try
  ADOCon.Connected := True;
  ADOTable.Active  := True;
  ADOTable.Active  := False;
  ADOTable.Active  := True;
  if ADOTable.Active then
    begin
    Owns   := False;
    result := TTestCaseLineItemCursor.Create( ADOCon, ADOTable)
    end
except on E: Exception do
  result := nil
  end
finally
  if Owns then
    begin
    ADOTable.Active  := False;
    ADOCon.Connected := False;
    ADOTable.Free;
    ADOCon.Free
    end
  end
end;

procedure TADOFactory.TDDManager.TestLineItem(
  TestBed: TObject; Cursor: ITestCaseLineItemCursor);
begin
TestMeth.Invoke( TestBed, [Cursor.LineItemObject as TADOTable])
end;
{$endif}


constructor TADOFactory.TTestCaseLineItemCursor.Create(ADOCon: TADOConnection;
  ADOTable: TADOTable);
begin
FADOCon   := ADOCon;
FADOTable := ADOTable;
FisFirst  := True
end;

destructor TADOFactory.TTestCaseLineItemCursor.Destroy;
begin
FADOTable.Free;
FADOCon.Free;
inherited
end;

procedure TADOFactory.TTestCaseLineItemCursor.GetLineItemDescription(
  RowNumber: integer; var sDescription: string);
begin
end;

function TADOFactory.TTestCaseLineItemCursor.LineItemObject: TObject;
begin
result := FADOTable
end;

function TADOFactory.TTestCaseLineItemCursor.Next: boolean;
begin
result := not FADOTable.Eof;
if not result then exit;
if FisFirst then
    FisFirst := False
  else
    begin
    FADOTable.Next;
    result := not FADOTable.Eof
    end
end;


function TTextFactory.Make: ITestCase;
begin
result := TDataDrivenTestCase.CreateCase( ManagerClass, ProcAsObject, ProcMethod, DrivingAttribute)
end;

function TTextFactory.ManagerClass: TDataDrivenTestCaseNavManagerClass;
begin
result := TDDManager
end;

function TTextFactory.MethodSkips: integer;
begin
result := 1
end;

class function TTextFactory.SignatureMatches(
  poTestMethod: TRttiMethod): boolean;
// Signature looks like ...
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.TextFile]
//        procedure TestIt4( const Line: string);
//        function  TestIt4FileName: string;
//
var
  FileMeth: TRttiMethod;
begin
result := (poTestMethod.MethodKind = mkProcedure) and
          (Length( poTestMethod.GetParameters) = 1);
if not result then exit;
with poTestMethod.GetParameters[0] do
  result := (ParamType.TypeKind = tkUString) and
            (Flags = [pfConst]) and

            NextDeclaredMethod( poTestMethod, FileMeth) and
            (FileMeth.MethodKind = mkFunction) and
            (Length( FileMeth.GetParameters) = 0) and
            (FileMeth.ReturnType.TypeKind = tkUString)
end;


constructor TTextFactory.TDDManager.Create(
  Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase);
begin
PrimitiveCreate( Parent, Method);
NextDeclaredMethod( Method, FGetFileNameMeth);  // function  TestIt2FileName: string;
end;


function TTextFactory.TDDManager.EstimatedCount(
  TestBed: TObject; var CountItems: integer): boolean;
var
  Data: TStrings;
begin
Data   := TStringList.Create;
try try
  Data.LoadFromFile( GetTextFileName( TestBed));
  CountItems := Data.Count;
  result     := True
finally
  Data.Free
  end
except
  result     := False
  end
end;

function TTextFactory.TDDManager.GetTextFileName( TestBed: TObject): string;
begin
try
  result := FGetFileNameMeth.Invoke( TestBed, []).AsString
except
  result := ''
  end
end;

type TStringWrapper = class( TAggregatedObject)
  public
    sValue: string;
  end;

function TTextFactory.TDDManager.OpenLineItemCursor(
  TestBed: TObject): ITestCaseLineItemCursor;
begin
result := TTextLineCursor.Create( GetTextFileName( TestBed))
end;

procedure TTextFactory.TDDManager.TestLineItem(
  TestBed: TObject; Cursor: ITestCaseLineItemCursor);
begin
TestMeth.Invoke( TestBed, [(Cursor.LineItemObject as TStringWrapper).sValue])
end;


constructor TTextFactory.TTextLineCursor.Create(
  const sFileName: string);
begin
FCurrent := TStringWrapper.Create( self);
FData    := TStringList.Create;
FIdx     := -1
end;

destructor TTextFactory.TTextLineCursor.Destroy;
begin
FCurrent.Free;
FData.Free;
inherited
end;

procedure TTextFactory.TTextLineCursor.GetLineItemDescription(
  RowNumber: integer; var sDescription: string);
begin
end;

function TTextFactory.TTextLineCursor.LineItemObject: TObject;
begin
result := FCurrent;
TStringWrapper( FCurrent).sValue := FData[ FIdx]
end;

function TTextFactory.TTextLineCursor.Next: boolean;
begin
result := FIdx <= (FData.Count - 2);
if result then
  Inc( FIdx)
end;










{$ifdef MSWINDOWS}
function TXMLFactory.Make: ITestCase;
begin
result := TDataDrivenTestCase.CreateCase( ManagerClass, ProcAsObject, ProcMethod, DrivingAttribute)
end;

function TXMLFactory.ManagerClass: TDataDrivenTestCaseNavManagerClass;
begin
result := TDDManager
end;

function TXMLFactory.MethodSkips: integer;
begin
result := 1
end;

class function TXMLFactory.SignatureMatches( poTestMethod: TRttiMethod): boolean;
//    {$ifdef MSWINDOWS}
//    [DataDrivenTest]
//      [DUnitM.DDExtensions.XML('XPath-expression')]
//        procedure TestIt5( const Node: IXMLNode);
//        function  TestIt5FileName: string;
//    {$endif}
var
  FileMeth: TRttiMethod;
  NodeParam: TRttiParameter;
begin
result := (poTestMethod.MethodKind = mkProcedure) and
          (Length( poTestMethod.GetParameters) = 1);
if not result then exit;
NodeParam := poTestMethod.GetParameters[0];
result := (NodeParam.ParamType.TypeKind = tkInterface) and
          (NodeParam.Flags = [pfConst, pfAddress]) and
          // Does D2010 use pfAddress for interface pointers? To be investigated.
          (NodeParam.ParamType is TRttiInterfaceType) and
          IsEqualGUID(
              TRttiInterfaceType( NodeParam.ParamType).GUID,
              IXMLNode) and

          NextDeclaredMethod( poTestMethod, FileMeth) and
          (FileMeth.MethodKind = mkFunction) and
          (Length( FileMeth.GetParameters) = 0) and
          (FileMeth.ReturnType.TypeKind = tkUString)
end;


constructor TXMLFactory.TDDManager.Create(
  Parent: TTestCase; Method: TRttiMethod; Attribute: TDataDrivenTestAttributeBase);
var
  Attri: XMLAttribute;
begin
PrimitiveCreate( Parent, Method);
NextDeclaredMethod( Method, FGetFileNameMeth);  // function  TestIt2FileName: string;
if Attribute is XMLAttribute then
  begin
  Attri                := XMLAttribute( Attribute);
  self.sXPath          := Attri.sXPath;
  self.sNamespaceDecls := Attri.sNamespaceDecls
  end
end;

function TXMLFactory.TDDManager.EstimatedCount(
  TestBed: TObject; var CountItems: integer): boolean;
begin
result := False
// When we can build an XPath 3.0 engine, we will be able to leverage
//  XPath to do the counting. For now, manually count the items.
end;

function TXMLFactory.TDDManager.OpenLineItemCursor(
  TestBed: TObject): ITestCaseLineItemCursor;
var
  sFN: string;
begin
sFN    := FGetFileNameMeth.Invoke( TestBed, []).AsString;
result := TXMLFactory.TNodeCursor.Create( sFN, sXPath, sNamespaceDecls)
end;


type TXMLNodeWrapper = class( TAggregatedObject)
  public
    FDatum: IXMLNode;
  end;

procedure TXMLFactory.TDDManager.TestLineItem(
  TestBed: TObject; Cursor: ITestCaseLineItemCursor);
var
  Datum: IXMLNode;
  Args : array of TValue;
begin
Datum := (Cursor.LineItemObject as TXMLNodeWrapper).FDatum;
SetLength( Args, 1);
Args[0] := TValue.From( Datum);
TestMeth.Invoke( TestBed, Args)
end;


constructor TXMLFactory.TNodeCursor.Create(
  const FileName1, XPath1, NamespaceDecl1: string);
begin
FDoc    := SBD.Utils.XML2.TXMLDoc.FromFile( FileName1);
if NamespaceDecl1 <> '' then
  SBD.Utils.XML2.TXMLDoc.DeclareSelectionNamespaces( FDoc, NamespaceDecl1);
FCursor := SBD.Utils.XML2.TXPath.Enum(
  SBD.Utils.XML2.TXPath.Select( FDoc.Node, XPath1)).GetEnumerator;
FCurrent := TXMLNodeWrapper.Create( self)
end;

destructor TXMLFactory.TNodeCursor.Destroy;
begin
FCurrent.Free;
FCursor := nil;
FDoc    := nil;
inherited
end;

procedure TXMLFactory.TNodeCursor.GetLineItemDescription(
  RowNumber: integer; var sDescription: string);
begin
end;

function TXMLFactory.TNodeCursor.LineItemObject: TObject;
begin
result := FCurrent;
TXMLNodeWrapper( FCurrent).FDatum := FCursor.Current
end;

function TXMLFactory.TNodeCursor.Next: boolean;
begin
result := FCursor.MoveNext
end;
{$endif}

end.
