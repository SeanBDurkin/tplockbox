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

unit DUnitM.Assertion;
interface
uses DunitM.UnitTestFramework, Classes, DUnitM.TestNode, SysUtils;

{$I ../includes/DUnitM.inc}

type
TAssertChecker = class( TInterfacedObject, IAssert)
  private
    FCheckGateway: ICheckGateway;

  private
    {$ifdef DELPHI_XE_UP}
    FGn: TAssertGeneric;
    function Gn: TAssertGeneric;
    {$endif}

    procedure Check;
    procedure Pass;
    procedure Fail( const message : string = ''; const errorAddrs : pointer = nil);
    procedure Warn( const message : string = '');
    procedure AreEqual(const left : string; const right : string; const ignoreCase : boolean; const message : string);overload;
    procedure AreEqual(const left : string; const right : string; const message : string = '');overload;
    procedure AreEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    procedure AreEqual(const left, right : Extended; const message : string = '');overload;
    procedure AreEqual(const left, right : TClass; const message : string = '');overload;
    procedure AreEqual(const left, right : TDate; const message : string = '');overload;
    procedure AreEqual(const left, right : TDateTime; const message : string = '');overload;
    procedure AreEqual(const left, right : Integer; const message : string = '');overload;
    procedure AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');
    procedure AreNotEqual(const left : string; const right : string; const ignoreCase : boolean = true; const message : string = '');overload;
    procedure AreNotEqual(const left, right : Extended; const tolerance : Extended; const message : string = '');overload;
    procedure AreNotEqual(const left, right : TClass; const message : string = '');overload;
    procedure AreNotEqual(const left, right : TDate; const message : string = '');overload;
    procedure AreNotEqual(const left, right : TDateTime; const message : string = '');overload;
    procedure AreNotEqual(const left, right : Integer; const message : string = '');overload;
    procedure AreNotEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string = '');
    procedure AreSame(const left, right : TObject; const message : string = '');overload;
    procedure AreSame(const left, right : IInterface; const message : string = '');overload;

    procedure AreNotSame(const left, right : TObject; const message : string = '');overload;
    procedure AreNotSame(const left, right : IInterface; const message : string = '');overload;

    procedure IsTrue ( const condition : boolean; const message : string = '');
    procedure IsFalse( const condition : boolean; const message : string = '');
    procedure IsNull(const condition : TObject; const message : string = '');overload;
    procedure IsNull(const condition : Pointer; const message : string = '');overload;
    procedure IsNull(const condition : IInterface; const message : string = '');overload;
    procedure IsNull(const condition : Variant; const message : string = '');overload;

    procedure IsNotNull(const condition : TObject; const message : string = '');overload;
    procedure IsNotNull(const condition : Pointer; const message : string = '');overload;
    procedure IsNotNull(const condition : IInterface; const message : string = '');overload;
    procedure IsNotNull(const condition : Variant; const message : string = '');overload;

    procedure IsEmpty(const value : string; const message : string = '');overload;
    procedure IsEmpty(const value : Variant; const message : string = '');overload;
    procedure IsEmpty(const value : TStrings; const message : string = '');overload;
    procedure IsEmpty(const value : TList; const message : string = '');overload;
    procedure IsEmpty(const value : IInterfaceList; const message : string = '');overload;

    procedure IsNotEmpty(const value : string; const message : string = '');overload;
    procedure IsNotEmpty(const value : Variant; const message : string = '');overload;
    procedure IsNotEmpty(const value : TStrings; const message : string = '');overload;
    procedure IsNotEmpty(const value : TList; const message : string = '');overload;
    procedure IsNotEmpty(const value : IInterfaceList; const message : string = '');overload;

    procedure WillRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    procedure WillRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    procedure WillNotRaise(const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    procedure WillNotRaise(const AMethod : TTestMethod; const exceptionClass : ExceptClass = nil; const msg : string = ''); overload;
    procedure Contains(const theString : string; const subString : string; const ignoreCase : boolean = true; const message : string = '');overload;
    procedure StartsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    procedure EndsWith(const theString : string; const subString : string;const ignoreCase : boolean = true; const message : string = '');
    procedure InheritsFrom(const descendant : TClass; const parent : TClass; const message : string = '');
    {$IFDEF SUPPORTS_REGEX}
      procedure IsMatch(const regexPattern : string; const theString : string; const message : string = '');
    {$ENDIF}
  public
    constructor Create( const CheckGateway1: ICheckGateway);
    destructor Destroy; override;

  {$ifdef DELPHI_XE_UP}
  private type
    TConcreteAssertGn = class sealed( TAssertGeneric)
      private
        FContrlrObj: TAssertChecker;
        constructor CreateConcreteGn( ContrlrObj: TAssertChecker);
      protected
        procedure Check;  override;
        procedure FailFmt( const Fmt: string; const Args: array of const); override;
      end;
  {$endif}
  end;



function IsBadPointer(P: Pointer):boolean; register;

{$IFDEF DELPHI_XE_DOWN} {$IFDEF ASSEMBLER} {$IFDEF MSWINDOWS} {$IFDEF CPUX86} {$IFNDEF PUREPASCAL}
  {$define ReturnAddressAvaliable}
{$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF}

{$IFDEF ReturnAddressAvaliable}
function ReturnAddress: Pointer; assembler;
{$ENDIF}

implementation








uses Math, Variants, StrUtils, SBD.Utils.XML2
  {$IFDEF SUPPORTS_REGEX}
  , RegularExpressions
  {$ENDIF}
  ;

function IsBadPointer( P: Pointer):Boolean;register;
begin
  try
    Result  := (p = nil) or ((Pointer(P^) <> P) and (Pointer(P^) = P));
  except
    Result := true;
  end
end;

{$IFDEF ReturnAddressAvaliable}
function ReturnAddress: Pointer; assembler;
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ENDIF}





constructor TAssertChecker.Create( const CheckGateway1: ICheckGateway);
begin
FCheckGateway := CheckGateway1;
{$ifdef DELPHI_XE_UP}
FGn := TConcreteAssertGn.CreateConcreteGn( self)
{$endif}
end;


{$ifdef DELPHI_XE_UP}
constructor TAssertChecker.TConcreteAssertGn.CreateConcreteGn( ContrlrObj: TAssertChecker);
begin
FContrlrObj := ContrlrObj;
inherited Create( FContrlrObj as IInterface)
end;

procedure TAssertChecker.TConcreteAssertGn.Check;
begin
FContrlrObj.Check
end;


procedure TAssertChecker.TConcreteAssertGn.FailFmt( const Fmt: string; const Args: array of const);
begin
FContrlrObj.Fail( Format( Fmt, Args), nil)
end;


{$endif}



destructor TAssertChecker.Destroy;
begin
inherited;
{$ifdef DELPHI_XE_UP}
FGn.Free
{$endif}
end;

{$ifdef DELPHI_XE_UP}
function TAssertChecker.Gn: TAssertGeneric;
begin
result := FGn
end;
{$endif}

procedure TAssertChecker.AreEqual( const left, right, tolerance: Extended; const message: string);
begin
Check;
if not Math.SameValue(left,right,tolerance) then
  Fail(Format('left %g but got %g - %s' ,[left,right,message]), ReturnAddress);
end;


procedure TAssertChecker.AreEqual( const left, right: TClass; const message: string);
var
  msg : string;
begin
Check;
  if left <> right then
  begin
    msg := ' is not equal to ';
    if left = nil then
      msg := 'nil' + msg
    else
      msg := left.ClassName + msg;

    if right = nil then
      msg := msg +  'nil'
    else
      msg := msg + right.ClassName;

    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;

procedure TAssertChecker.AreEqual(const left, right: Integer; const message: string);
begin
Check;
  if left <> right then
    Fail(Format('left %d but got %d - %s' ,[left, right, message]), ReturnAddress);
end;

function DateAsMessageString( Datum: TDate): string;
begin
result := SBD.Utils.XML2.Txs_dateTime.EncodeDate( Datum)
end;

function DateTimeAsMessageString( Datum: TDate): string;
begin
result := SBD.Utils.XML2.Txs_dateTime.Encode( Datum, False, 0)
end;

procedure TAssertChecker.AreEqual( const left, right: TDate; const message: string);
begin
Check;
if left <> right then
  Fail( Format('left %s but got %s - %s' ,[DateAsMessageString( left), DateAsMessageString( right), message]), ReturnAddress)
end;

procedure TAssertChecker.AreEqual( const left, right: TDateTime; const message: string);
begin
Check;
if left <> right then
  Fail( Format('left %s but got %s - %s' ,[DateTimeAsMessageString( left), DateTimeAsMessageString( right), message]), ReturnAddress)
end;

procedure TAssertChecker.AreEqual(const left, right: Extended; const message: string);
begin
Check;
AreEqual(left, right, 0, message);
end;

procedure TAssertChecker.AreEqualMemory(const left : Pointer; const right : Pointer; const size : Cardinal; message : string);
begin
Check;
if not CompareMem(left, right, size) then
  Fail('Memory values are not equal. ' + message, ReturnAddress);
end;

procedure TAssertChecker.AreNotEqual(const left, right, tolerance: Extended; const message: string);
begin
Check;
  if Math.SameValue(left, right, tolerance) then
    Fail(Format('%g equals right %g %s' ,[left,right,message]), ReturnAddress);
end;


procedure TAssertChecker.AreNotEqual(const left, right: string;const ignoreCase: boolean; const message: string);

  function AreNotEqualText(const left, right: string; const ignoreCase: boolean): boolean;
  begin
    if ignoreCase then
      Result := SameText(left, right)
    else
      Result := SameStr(left, right);

  end;
begin
Check;
  if AreNotEqualText(left, right, ignoreCase) then
     Fail(Format('[%s] is Equal to [%s] %s', [left, right, message]), ReturnAddress);
end;

procedure TAssertChecker.AreNotEqual(const left, right: TClass; const message: string);
var
  msg : string;
begin
Check;
  if left = right then
  begin
    msg := ' is equal to ';
    if left = nil then
      msg := 'nil' + msg
    else
      msg := left.ClassName + msg;

    if right = nil then
      msg := msg +  'nil'
    else
      msg := msg + right.ClassName;
    if message <> '' then
      msg := msg + '. ' + message;

    Fail(msg, ReturnAddress);
  end;
end;


procedure TAssertChecker.AreNotEqual(const left, right: Integer; const message: string);
begin
Check;
if left = right then
  Fail( Format('%d equals right %d %s' ,[left, right, message]), ReturnAddress);
end;

procedure TAssertChecker.AreNotEqual(
  const left, right: TDate; const message: string);
begin
Check;
if left = right then
  Fail( Format('%s equals right %s %s' ,[DateAsMessageString( left), DateAsMessageString( right), message]), ReturnAddress);
end;

procedure TAssertChecker.AreNotEqual(const left, right: TDateTime;
  const message: string);
begin
Check;
if left = right then
  Fail( Format('%s equals right %s %s' ,[DateTimeAsMessageString( left), DateTimeAsMessageString( right), message]), ReturnAddress);
end;


procedure TAssertChecker.AreNotEqualMemory(const left, right: Pointer; const size: Cardinal; message: string);
begin
Check;
  if CompareMem(left,right, size) then
    Fail('Memory values are equal. ' + message, ReturnAddress);
end;

procedure TAssertChecker.AreNotSame(const left, right: TObject; const message: string);
begin
Check;
  if left.Equals(right) then
    Fail(Format('Object [%s] Equals Object [%s] %s',[left.ToString,right.ToString,message]), ReturnAddress);
end;

procedure TAssertChecker.AreNotSame(const left, right: IInterface; const message: string);
begin
Check;
  if left = right then
    Fail(Format('references are the same. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.AreSame(const left, right: IInterface; const message: string);
begin
Check;
  if left <> right then
    Fail(Format('references are Not the same. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.AreSame(const left, right: TObject; const message: string);
begin
Check;
  if not left.Equals(right) then
    Fail(Format('Object [%s] Not Object [%s] %s',[left.ToString,right.ToString,message]), ReturnAddress);
end;




procedure TAssertChecker.InheritsFrom(const descendant, parent: TClass; const message: string);
var
  msg : string;
begin
Check;
  if (descendant = nil) or (parent = nil) or (not descendant.InheritsFrom(parent)) then
  begin
    msg := ' does not inherit from ';
    if descendant = nil then
      msg := 'nil' + msg
    else
      msg := descendant.ClassName + msg;
    if parent = nil then
      msg := msg + 'nil'
    else
      msg := parent.ClassName + msg;
    msg := msg + '.';
    if True then
    if message <> '' then
      msg := msg + ' ' + message;

    Fail(msg, ReturnAddress);
  end;

end;

procedure TAssertChecker.IsEmpty(const value: IInterfaceList; const message: string);
begin
Check;
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsEmpty(const value: TList; const message: string);
begin
Check;
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsEmpty(const value, message: string);
begin
Check;
  if Length(value) > 0 then
    Fail(Format('String is Not empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsEmpty(const value: Variant; const message: string);
begin
Check;
  if VarIsEmpty(value) or VarIsNull(value) then
    Fail(Format('Variant is Not empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsEmpty(const value: TStrings; const message: string);
begin
Check;
  if value.Count > 0 then
    Fail(Format('List is Not empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsFalse(const condition: boolean; const message: string);
begin
Check;
  if condition then
   Fail(Format('Condition is True when False expected. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotEmpty(const value: TList; const message: string);
begin
Check;
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotEmpty(const value: IInterfaceList; const message: string);
begin
Check;
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotEmpty(const value: TStrings; const message: string);
begin
Check;
  if value.Count = 0 then
   Fail(Format('List is Empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotEmpty(const value, message: string);
begin
Check;
  if value = '' then
   Fail(Format('Variant is Empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotEmpty(const value: Variant; const message: string);
begin
Check;
  if VarIsEmpty(value) then
    Fail(Format('Variant is Empty. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotNull(const condition: IInterface; const message: string);
begin
Check;
  if condition = nil then
    Fail(Format('Interface is Nil when not nil expected. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotNull(const condition: Pointer; const message: string);
begin
Check;
  if condition = nil then
    Fail(Format('Pointer is Nil when not Nil expected. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotNull(const condition: TObject; const message: string);
begin
Check;
  if condition = nil then
    Fail(Format('Object is Nil when Not Nil expected. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNotNull(const condition: Variant; const message: string);
begin
Check;
  if VarIsNull(condition) then
    Fail(Format('Variant is Null when Not Null expcted. %s',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNull(const condition: Variant; const message: string);
begin
Check;
  if not VarIsNull(condition) then
    Fail(Format('Variant is Not Null when Null expected. [%s]',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNull(const condition: IInterface; const message: string);
begin
Check;
  if condition <> nil then
    Fail(Format('Interface is not Nil when nil expected. [%s]',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNull(const condition: TObject; const message: string);
begin
Check;
  if condition <> nil then
    Fail(Format('Object is not nil when nil expected. [%s]',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsNull(const condition: Pointer; const message: string);
begin
Check;
  if condition <> nil then
    Fail(Format('Pointer is not Nil when nil expected. [%s]',[message]), ReturnAddress);
end;

procedure TAssertChecker.IsTrue(const condition: boolean;const message : string);
begin
Check;
  if not condition then
    Fail(Format('Condition is False when True expected. [%s]',[message]), ReturnAddress);
end;

procedure TAssertChecker.WillNotRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
WillNotRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

procedure TAssertChecker.WillRaise( const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
begin
Check;
try
  AMethod;
  Fail( 'Method did not throw any exceptions.', ReturnAddress);
except on E: Exception do
    begin
    if assigned( exceptionClass) and (E.ClassType <> exceptionClass) then
      Fail( Format('Method raised [%s] was expecting [%s]. %s',
        [E.ClassName, exceptionClass.ClassName, E.message]), ReturnAddress)
    end
  end
end;


procedure TAssertChecker.WillNotRaise( const AMethod : TTestLocalMethod; const exceptionClass : ExceptClass; const msg : string);
begin
Check;
try
  AMethod
except on E: Exception do
    begin
    if not assigned( exceptionClass) then
        Fail( Format('Method raised [%s] was expecting not to raise any exception. %s',
          [E.ClassName, E.message]), ReturnAddress)
      else if E.ClassType = exceptionClass then
        Fail( Format('Method raised [%s] was expecting not to raise [%s]. %s',
          [E.ClassName, exceptionClass.ClassName, E.message]), ReturnAddress)
    end
  end
end;


procedure TAssertChecker.WillRaise(const AMethod: TTestMethod; const exceptionClass: ExceptClass; const msg: string);
begin
WillRaise(
    procedure
    begin
      AMethod;
    end,
    exceptionClass, msg);
end;

procedure TAssertChecker.AreEqual(const left : string; const right : string; const message : string);
begin
AreEqual( left, right, true, message);
end;

procedure TAssertChecker.AreEqual(const left, right : string;  const ignoreCase : boolean; const message: string);
begin
Check;
  if ignoreCase then
  begin
    if not SameText(left,right) then
      Fail(Format('[%s] is Not Equal to [%s] %s',[left,right,message]), ReturnAddress);
  end
  else if not SameStr(left,right) then
      Fail(Format('[%s] is Not Equal to [%s] %s',[left,right,message]), ReturnAddress);
end;

procedure TAssertChecker.Contains(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
Check;
  if ignoreCase then
  begin
    if not StrUtils.ContainsText(theString,subString) then
      Fail(Format('[%s] does not contain [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.ContainsStr(theString,subString) then
    Fail(Format('[%s] does not contain [%s] %s',[theString,subString,message]), ReturnAddress);
end;

procedure TAssertChecker.EndsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message : string);
begin
Check;
  if ignoreCase then
  begin
    if not StrUtils.EndsText(theString,subString) then
      Fail(Format('[%s] does not end with [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.EndsStr(theString,subString) then
    Fail(Format('[%s] does not end with [%s] %s',[theString,subString,message]), ReturnAddress);
end;

{$IFDEF SUPPORTS_REGEX}
procedure TAssertChecker.IsMatch(const regexPattern, theString, message: string);
begin
Check;
  if not TRegEx.IsMatch(theString,regexPattern) then
    Fail(Format('[%s] does not match [%s] %s',[theString,regexPattern,message]), ReturnAddress);
end;
{$ENDIF}

procedure TAssertChecker.StartsWith(const theString : string; const subString : string; const ignoreCase : boolean; const message: string);
begin
Check;
  if ignoreCase then
  begin
    if not StrUtils.StartsText(theString,subString) then
      Fail(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]), ReturnAddress);
  end
  else if not StrUtils.StartsStr(theString,subString) then
    Fail(Format('[%s] does Not Start with [%s] %s',[theString,subString,message]), ReturnAddress);
end;


procedure TAssertChecker.Pass;
begin
Check;
FCheckGateway.Pass
//raise ETestPass.Create('Abortive pass')
end;


procedure TAssertChecker.Fail( const message : string; const errorAddrs : pointer);
var
  p: pointer;
begin
// If we have been given a return then use it. (makes the exception appear on level above in the stack)
if errorAddrs <> nil then
  p := errorAddrs
else
  // Otherwise use the return address we can currently get to for where to raise the exception
  p :=  ReturnAddress;
FCheckGateway.Fail( message, p)
end;

procedure TAssertChecker.Warn( const message: string);
begin
FCheckGateway.Warn( message)
end;


procedure TAssertChecker.Check;
begin
FCheckGateway.Check
end;

end.
