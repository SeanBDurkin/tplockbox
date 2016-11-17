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

unit DUnitM.RunInstance;
interface
uses SBD.Messages, DUnitM.UnitTestFramework, SysUtils, DUnitM.MemoryMonitor,
     SBD.Generics, DUnitM.TestNode;

type

TTestNode_RunInstance = class( TInterfacedObject, IRunInstance, ITestResult)
  protected
    FStatus: TTestStatus;
    FTiming: RTimingStats;
    FExecuted: boolean;
    FRunInstanceServices: IRunInstanceServices;
    FStatusInformerProc: TStatusInformerProc;
    FMem: IMemoryMonitor;
    FParent: TReflector; // weak reference to parent, with ARC.
    FWeakClients: TReflector;

    // IRunInstance
    procedure Run( const RunInstanceServices: IRunInstanceServices; OnStatusChangeProc: TStatusInformerProc); virtual;
    procedure Clear;                                virtual;
    function  Parent: IRunInstance;                 virtual;
    function  WorkLoad: integer;                    virtual;
    function  HasMessageList( var Lst: IMessageList): boolean; virtual;
    function  TestNode: ITestNode;                  virtual; abstract;
    function  ChildInstances: IList2<IRunInstance>; virtual;
    function  Description: string;                  virtual;

    // ITestResult
    function GetStatus: TTestStatus;                virtual;
    function ITestResult.Status = GetStatus;
    function IRunInstance.Status = GetStatus;
    function RoundsCompleted: integer;              virtual;
    function RoundsPerTestCase: integer;            virtual;
    function ErrorCount: integer;                   virtual;
    function PassCount: integer;                    virtual;
    function FailCount: integer;                    virtual;
    function WarnCount: integer;                    virtual;
    function Timing: RTimingStats;                  virtual;
    function Messages: IMessageSource;              virtual;
    function TestCase: ITestCase;                   virtual;
    function Fixture: ITestFixture;                 virtual;
    function ChildResults: ISEnumerable<ITestResult>; virtual;
    function Executed: boolean;                     virtual;
    function IgnoredCount: integer;                 virtual;
    function SkippedCount: integer;                 virtual;
    function NodeKind: TResultNodeKind;             virtual;  abstract;
    function ErrorOrFailureMsg( var FailMsg: RMessage): boolean; virtual;

    procedure BeforeRun;         virtual;
    procedure DoInnerRun;        virtual;
    procedure CleanUpAfterRun;   virtual;
    procedure PutSimpleMessage( Code: integer; const sMsg: string; Lv: TMessageLevel); virtual;
    procedure SetStatus( Value: TTestStatus);        virtual;
    function  NodeName: string;

  public
    property Status: TTestStatus read GetStatus write SetStatus;

  public
    constructor Create( ParentAsObject: TReflector; const Mem1: IMemoryMonitor);
  end;


TTestNode_RunInstanceWithChildren = class( TTestNode_RunInstance)
  protected
    FChildren: IList2<IRunInstance>;
    function ChildInstances: IList2<IRunInstance>;  override;
  public
    constructor Create( ParentAsObject: TReflector; const Mem1: IMemoryMonitor);
  end;



implementation








uses SBD.Messages.Solution;

type
TEmptyMessages = class( TSEnumerable<RMessage>, IMessageSource)
  protected
    function GetEnumerator: ISEnumerator<RMessage>; override;
  end;

function TEmptyMessages.GetEnumerator: ISEnumerator<RMessage>;
begin
result := TEmptyCursor<RMessage>.Create( nil)
end;





constructor TTestNode_RunInstance.Create(
  ParentAsObject: TReflector; const Mem1: IMemoryMonitor);
begin
if assigned( ParentAsObject) then
  ParentAsObject.SetWeakReference( FParent);
FWeakClients := TReflector.Create( self);
FRunInstanceServices := nil;
FStatusInformerProc  := nil;
FMem                 := Mem1;
Clear
end;


procedure TTestNode_RunInstance.BeforeRun;
begin
end;

function TTestNode_RunInstance.Description: string;
begin
result := TestNode.Name
end;

procedure TTestNode_RunInstance.DoInnerRun;
begin
end;

function TTestNode_RunInstance.ChildInstances: IList2<IRunInstance>;
begin
result := TList2<IRunInstance>.Create
end;

function TTestNode_RunInstance.ChildResults: ISEnumerable<ITestResult>;
var
  TestResult: ITestResult;
  RunInstance: IRunInstance;
  Lst: IList2<ITestResult>;
begin
Lst    := TList2<ITestResult>.Create;
result := Lst;
for RunInstance in ChildInstances do
  if Supports( RunInstance, ITestResult, TestResult) then
    Lst.Add( TestResult)
end;


procedure TTestNode_RunInstance.CleanUpAfterRun;
begin
end;

procedure TTestNode_RunInstance.Clear;
begin
FStatus := tsNeutral;
FTiming.Clear;
FExecuted := False;
// TODO:
//  ChildResults.Reset
end;


function TTestNode_RunInstance.ErrorCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.ErrorCount)
end;


function TTestNode_RunInstance.ErrorOrFailureMsg(
  var FailMsg: RMessage): boolean;
var
  Lst: IMessageList;
  Filter: TMessageLevelSet;
  M: RMessage;
begin
result := HasMessageList( Lst) and Lst.Has( [lvError, lvFatalError]);
if not result then exit;
if Lst.Has( [lvFatalError]) then
    Filter := [lvFatalError]
  else
    Filter := [lvError];
for M in Lst.AsSource( Filter) do
  begin
  FailMsg := M;
  break
  end
end;


function TTestNode_RunInstance.Executed: boolean;
begin
result := FExecuted
end;

function TTestNode_RunInstance.FailCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.FailCount)
end;

function TTestNode_RunInstance.Fixture: ITestFixture;
var
  TestCase1: ITestCase;
  TestProc1: ITestProcedure;
begin
TestCase1 := TestCase();
if assigned( TestCase1) then
    TestProc1 := TestCase1.Parent
  else
    TestProc1 := nil;
if assigned( TestProc1) then
    result := TestProc1.Parent
  else
    result := nil
end;

function TTestNode_RunInstance.GetStatus: TTestStatus;
begin
result := FStatus
end;

function TTestNode_RunInstance.HasMessageList( var Lst: IMessageList): boolean;
begin
result := False
end;

function TTestNode_RunInstance.IgnoredCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.IgnoredCount)
end;

function TTestNode_RunInstance.Messages: IMessageSource;
var
  Lst: IMessageList;
begin
if HasMessageList( Lst) then
    result := Lst.AsSource( AllMessages)
  else
    result := TEmptyMessages.Create
end;

function TTestNode_RunInstance.NodeName: string;
var
  Node: ITestNode;
begin
Node := TestNode();
if assigned( Node) then
    result := Node.Name
  else
    result := ''
end;

function TTestNode_RunInstance.Parent: IRunInstance;
begin
if (not assigned( FParent)) or
   (not Supports( FParent.FController, IRunInstance, result)) then
  result := nil
end;

function TTestNode_RunInstance.PassCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.PassCount)
end;


procedure TTestNode_RunInstance.PutSimpleMessage(
  Code: integer; const sMsg: string; Lv: TMessageLevel);
var
  Msg: RMessage;
  Lst: IMessageList;
begin
if not HasMessageList( Lst) then exit;
SetFrameworkMessage( Msg, Code, sMsg, Lv);
Lst.Add( Msg);
if assigned( FRunInstanceServices) then
  FRunInstanceServices.Put( self, TestNode(), Msg, RoundsCompleted())
end;

function TTestNode_RunInstance.RoundsCompleted: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.RoundsCompleted)
end;

function TTestNode_RunInstance.RoundsPerTestCase: integer;
begin
result := 1
end;

procedure TTestNode_RunInstance.Run(
  const RunInstanceServices: IRunInstanceServices;
  OnStatusChangeProc: TStatusInformerProc);
begin
FRunInstanceServices := RunInstanceServices;
FStatusInformerProc  := OnStatusChangeProc;
Clear;
BeforeRun;
FTiming.FStart := Now;
DoInnerRun;
FTiming.FEnd := Now;
FTiming.FDuration_ms := Round( (FTiming.FEnd - FTiming.FStart) * MSecsPerDay);
FRunInstanceServices := nil;
FStatusInformerProc  := nil;
CleanUpAfterRun
end;

procedure TTestNode_RunInstance.SetStatus( Value: TTestStatus);
var
  TestNode1: ITestNode;
begin
if FStatus = Value then exit;
FStatus := Value;
TestNode1 := TestNode();
if assigned( FStatusInformerProc) then
  FStatusInformerProc( FStatus);
if assigned( FRunInstanceServices) then
  FRunInstanceServices.StatusChange( self, TestNode(), RoundsCompleted())
end;

function TTestNode_RunInstance.SkippedCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.SkippedCount)
end;

function TTestNode_RunInstance.TestCase: ITestCase;
begin
result := nil
end;

function TTestNode_RunInstance.Timing: RTimingStats;
begin
result := FTiming
end;

function TTestNode_RunInstance.WarnCount: integer;
var
  Child: ITestResult;
begin
result := 0;
for Child in ChildResults do
  Inc( result, Child.WarnCount)
end;

function TTestNode_RunInstance.WorkLoad: integer;
var
  Child: ITestResult;
  Run: IRunInstance;
begin
result := 0;
for Child in ChildResults do
  if Supports( Child, IRunInstance, Run) then
    Inc( result, Run.WorkLoad)
end;



function TTestNode_RunInstanceWithChildren.ChildInstances: IList2<IRunInstance>;
begin
result := FChildren
end;

constructor TTestNode_RunInstanceWithChildren.Create(
  ParentAsObject: TReflector; const Mem1: IMemoryMonitor);
begin
FChildren := TList2<IRunInstance>.Create;
inherited Create( ParentAsObject, Mem1)
end;

end.
