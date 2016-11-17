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

unit DUnitM.TestCaseParameters;
interface
uses DUnitM.UnitTestFramework, DUnitM.TestNode, Rtti, SBD.Generics, SBD.Messages,
     DUnitM.MemoryMonitor, DUnitM.RunInstance;

type

TTestCaseParamProcessorStatus = (psMore, psSyntaxError, psDone);
RTestCaseParamDatum = record
    FParam: TRttiParameter;
    FActualValue: TValue;
    FSetCounter: integer;
  end;


TTestCaseParamProcessor = class
  public
    FMethod: TRttiMethod;
    FStringToParse: string;
    FParseResult: TTestCaseParamProcessorStatus;
    FParams: IList2<RTestCaseParamDatum>;
    constructor Create( Method1: TRttiMethod; const StringToParse1: string);
    constructor CreateSimple( Method1: TRttiMethod);
    procedure Parse;
    procedure Invoke( Obj: TObject);
  private
    procedure ParseOneParam;
    function  ParseName( var sParamName: string): boolean;
    function  FindParamIndex( const sParamName: string; var Idx: integer): boolean;
    function  ParseEquals: boolean;
    function  ParseOpenQuote( var sQuoteChar: string): boolean;
    function  ParseValue( const sQuoteChar: string; Idx: integer; var ParamValue: TValue): boolean;
    function  ParseCloseQuote( const sQuoteChar: string): boolean;
  end;


implementation








uses Generics.Collections, SysUtils, TypInfo, SBD.Utils.XML2, Math,
     SBD.Messages.Solution, DUnitM.TestCaseRound;

constructor TTestCaseParamProcessor.CreateSimple( Method1: TRttiMethod);
begin
FMethod := Method1;
FStringToParse := '';
FParseResult := psDone;
FParams := TList2<RTestCaseParamDatum>.Create
end;


constructor TTestCaseParamProcessor.Create(
  Method1: TRttiMethod; const StringToParse1: string);
var
  Params: TArray<TRttiParameter>;
  Param: TRttiParameter;
  ParamRec: RTestCaseParamDatum;
  idx: integer;
  RequireFlags: TParamFlags;
  RequireType: TList<PTypeInfo>;
  Ok: boolean;

begin
FMethod := Method1;
FStringToParse := StringToParse1;
FParseResult := psMore;
FParams := TList2<RTestCaseParamDatum>.Create;
Params  := FMethod.GetParameters;
Ok      := True;
RequireType  := TList<PTypeInfo>.Create;
for idx := 0 to Length( Params) - 1 do
  begin
  Param := Params[ idx];
  ParamRec.FParam := Param;
  RequireFlags := [];
  RequireType.Clear;
  case Param.ParamType.TypeKind of
    tkInteger    : RequireFlags := [];
    tkUString    : RequireFlags := [pfConst];
    tkFloat      : begin
                   RequireFlags := [];
                   RequireType.Add( TypeInfo( TDateTime));
                   RequireType.Add( TypeInfo( Double));
                   end;
    tkEnumeration: begin
                   RequireFlags := [];
                   RequireType.Add( TypeInfo( boolean))
                   end
    else           Ok := False
    end;
  Ok := Ok and (Param.Flags = RequireFlags) and
    ((RequireType.Count = 0) or (RequireType.IndexOf( Param.ParamType.Handle) <> -1));
  if not Ok then break;
  ParamRec.FSetCounter := 0;
  FParams.Add( ParamRec)
  end;
RequireType.Free;
if not Ok then
  FParseResult := psSyntaxError
end;

procedure TTestCaseParamProcessor.Invoke( Obj: TObject);
var
  Args: array of TValue;
  iParam: integer;
  Ok: Boolean;
  Kind: TTypeKind;

begin
SetLength( Args, FParams.Count);
for iParam := 0 to FParams.Count - 1 do
  begin
  Args[ iParam] := FParams[ iParam].FActualValue;
  Ok   := not FParams[ iParam].FActualValue.IsEmpty;
  Kind := FParams[ iParam].FActualValue.Kind
  end;
FMethod.Invoke( Obj, Args)
end;

procedure TTestCaseParamProcessor.Parse;
var
  ParamRec: RTestCaseParamDatum;
begin
while FParseResult = psMore do
  ParseOneParam;
for ParamRec in FParams do
  if ParamRec.FSetCounter <> 1 then
    FParseResult := psSyntaxError
end;

function TTestCaseParamProcessor.FindParamIndex(
  const sParamName: string; var Idx: integer): boolean;
var
  ParamRec: RTestCaseParamDatum;
begin
result := False;
Idx    := -1;
for ParamRec in FParams do
  begin
  Inc( Idx);
  result := SameText( ParamRec.FParam.Name, sParamName);
  if result then break
  end;
if not result then
  FParseResult := psSyntaxError
end;


procedure TTestCaseParamProcessor.ParseOneParam;
var
  ParamRec: RTestCaseParamDatum;
  Idx: integer;
  ParamValue: TValue;
  sParamName: string;
  sQuoteChar: string;
begin
if FParseResult = psSyntaxError then exit;
FStringToParse := Trim( FStringToParse);
if FStringToParse = '' then
  begin
  FParseResult := psDone;
  exit
  end;
if ParseName( sParamName) and
   FindParamIndex( sParamName, Idx) and
   ParseEquals and
   ParseOpenQuote( sQuoteChar) and
   ParseValue( sQuoteChar, Idx, ParamValue) and
   ParseCloseQuote( sQuoteChar) and
   (FParseResult <> psSyntaxError) then
  begin
  ParamRec := FParams[ Idx];
  ParamRec.FActualValue := ParamValue;
  Inc( ParamRec.FSetCounter);
  FParams[ Idx] := ParamRec
  end
end;

function TTestCaseParamProcessor.ParseOpenQuote(
  var sQuoteChar: string): boolean;
begin
sQuoteChar := Copy( FStringToParse, 1, 1);
result := (sQuoteChar = '"') or (sQuoteChar = '''');
Delete( FStringToParse, 1, 1);
if not result then
  FParseResult := psSyntaxError
end;

function TTestCaseParamProcessor.ParseCloseQuote(
  const sQuoteChar: string): boolean;
begin
result := Copy( FStringToParse, 1, 1) = sQuoteChar;
Delete( FStringToParse, 1, 1);
if not result then
  FParseResult := psSyntaxError
end;

function TTestCaseParamProcessor.ParseEquals: boolean;
begin
result := Copy( FStringToParse, 1, 1) = '=';
Delete( FStringToParse, 1, 1);
if not result then
  FParseResult := psSyntaxError
end;

function TTestCaseParamProcessor.ParseName( var sParamName: string): boolean;
var
  iCh: integer;
  ParseLen, ValueLen: integer;
begin
iCh      := 0;
ParseLen := Length( FStringToParse);
ValueLen := ParseLen;
while iCh <= (ParseLen - 1) do
  begin
  Inc( iCh);
  if Copy( FStringToParse, iCh, 1) <> '=' then continue;
  ValueLen := iCh - 1;
  break
  end;
sParamName := Copy( FStringToParse, 1, ValueLen);
Delete( FStringToParse, 1, ValueLen);
result := sParamName <> '';
if not result then
  FParseResult := psSyntaxError
end;


type
TSubparserClass = class of TSubparser;
TSubparser = class
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; virtual; abstract;
  end;

TIntegerSubparser = class( TSubparser)
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; override;
  end;

TBooleanSubparser = class( TSubparser)
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; override;
  end;

TStringSubparser = class( TSubparser)
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; override;
  end;

TDoubleSubparser = class( TSubparser)
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; override;
  end;

TDateTimeSubparser = class( TSubparser)
  public
    function Parse( const sValue: string; var ParamValue: TValue): boolean; override;
  end;


function TTestCaseParamProcessor.ParseValue(
  const sQuoteChar: string; Idx: integer; var ParamValue: TValue): boolean;
type
  RSubparserMap = record
      TypeKind: TTypeKind;
      SubParser: TSubparserClass;
    end;
const
  SubParsers: array[0..3] of RSubparserMap = (
    (TypeKind: tkInteger    ; SubParser: TIntegerSubparser),
    (TypeKind: tkFloat      ; SubParser: TDoubleSubparser),
    (TypeKind: tkUString    ; SubParser: TStringSubparser),
    (TypeKind: tkEnumeration; SubParser: TBooleanSubparser));
var
  Key: TTypeKind;
  Cls: TSubparserClass;
  I: integer;
  SubParser: TSubparser;
  sUnescaped: string;
  iCh: integer;
  QuotLen: integer;
  ParseLen: integer;
  ValueLen: integer;
begin
QuotLen := Length( sQuoteChar);
iCh := 0;
ParseLen := Length( FStringToParse);
ValueLen := ParseLen;
while iCh <= (ParseLen - 1) do
  begin
  Inc( iCh);
  if Copy( FStringToParse, iCh, QuotLen) <> sQuoteChar then continue;
  if Copy( FStringToParse, iCh + QuotLen, QuotLen) = sQuoteChar then
      Inc( iCh, 2 * QuotLen - 1)
    else
      begin
      ValueLen := iCh - 1;
      break
      end;
  end;
sUnescaped := StringReplace( Copy( FStringToParse, 1, ValueLen), sQuoteChar+sQuoteChar, sQuoteChar, [rfReplaceAll]);
Delete( FStringToParse, 1, ValueLen);
Key := FParams[ Idx].FParam.ParamType.TypeKind;
Cls := nil;
for i := Low( Subparsers) to High( Subparsers) do
  begin
  if Subparsers[i].TypeKind <> Key then Continue;
  Cls := Subparsers[i].SubParser;
  break
  end;
if (Cls = TDoubleSubparser) and (FParams[ Idx].FParam.ParamType.Handle = TypeInfo( TDateTime)) then
  Cls := TDateTimeSubparser;
SubParser := Cls.Create;
result := SubParser.Parse( sUnescaped, ParamValue);
SubParser.Free;
if not result then
  FParseResult := psSyntaxError
end;


function TIntegerSubparser.Parse(
  const sValue: string; var ParamValue: TValue): boolean;
var
  i64: int64;
  i32: integer;
begin
result := Txs_integer.Decode( sValue, i64);
i32    := integer( i64);
result := result and (int64( i32) = i64);
ParamValue := i32
end;


function TBooleanSubparser.Parse(
  const sValue: string; var ParamValue: TValue): boolean;
begin
result := sValue <> '';
ParamValue := Txs_boolean.Decode( sValue, False)
end;


function TStringSubparser.Parse(
  const sValue: string; var ParamValue: TValue): boolean;
begin
result := True;
ParamValue := sValue
end;


function TDoubleSubparser.Parse( const sValue: string;
  var ParamValue: TValue): boolean;
var
  Dub: double;
begin
result := Txs_float.Decode( sValue, Dub);
ParamValue := Dub
end;


function TDateTimeSubparser.Parse(
  const sValue: string; var ParamValue: TValue): boolean;
var
  Decoded: TDateTime;
  bIncludeTZ: boolean;
  TZMinutes: integer;
  Ok: boolean;
  X: TValue;
begin
result := Txs_dateTime.Decode( sValue, Decoded, bIncludeTZ, TZMinutes) and
          (not bIncludeTZ);
// The following commented-out line appears not be working in D2010.
// ParamValue.From<TDateTime>( Decoded);
//  So this is the work-around ...
TValue.Make( @Decoded, System.TypeInfo( TDateTime), ParamValue);
Ok := not ParamValue.IsEmpty;
end;

end.
