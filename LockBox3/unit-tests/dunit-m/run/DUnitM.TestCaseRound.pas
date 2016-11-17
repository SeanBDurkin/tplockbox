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

unit DUnitM.TestCaseRound;
interface
uses SBD.Messages, DUnitM.UnitTestFramework, SysUtils, DUnitM.MemoryMonitor,
     SBD.Generics, DUnitM.TestNode, DUnitM.RunInstance;

type



TTestCaseRound_RunInstance = class( TTestNode_RunInstance, ICheckGateway)
  private
    FCheckCount: Integer;
    FRoundsCompleted: integer;
    FErrorCount: integer;
    FPassCount: integer;
    FFailCount: integer;
    FWarnCount: integer;
    FExecutionMessages: IMessageList;
    FMessages: IMessageList;
    FTestCase: ITestCaseInternal;
    FTestBed: TObject;
    FRoundNumber: integer;
    FCursor: ITestCaseLineCursoredItem;
    FAbortedAfterSetup: boolean;
    doSetGateway: boolean;
    Fctry: IPlatterFactory;

  private
    procedure Check;
    procedure Pass;
    procedure Fail( const sErrMsg: string; Address: pointer);
    procedure Warn( const sWarning: string);

  protected
    procedure Clear;                                override;
    function  WorkLoad: integer;                    override;
    function  HasMessageList( var Lst: IMessageList): boolean; override;
    function  TestNode: ITestNode;                  override;
    function  RoundsCompleted: integer;             override;
    function  RoundsPerTestCase: integer;           override;
    function  ErrorCount: integer;                  override;
    function  PassCount: integer;                   override;
    function  FailCount: integer;                   override;
    function  WarnCount: integer;                   override;
    function  TestCase: ITestCase;                  override;
    function  IgnoredCount: integer;                override;
    function  SkippedCount: integer;                override;
    function  NodeKind: TResultNodeKind;            override;
    procedure BeforeRun;                            override;
    procedure DoInnerRun;                           override;
    procedure CleanUpAfterRun;                      override;
    function  Description: string;                  override;

  private
    procedure ClearHeapMonitoring;
    function  LeakageDetected( var Leakage: int64): boolean;
    procedure ActivateHeapMonitoring( TurnOn: Boolean);
    procedure Setup;
    procedure Execute;
    procedure TearDown;

  protected
    // function  RowDescription: string;       virtual;
    procedure RawExecute;                   virtual;

  public
    constructor Create(
      ParentAsObject: TReflector; TestBed: TObject;
      const Case1: ITestCaseInternal; RoundNumber1: integer;
      const Mem1: IMemoryMonitor; const Cursor: ITestCaseLineCursoredItem);
  end;

implementation





uses SBD.Messages.Solution;


constructor TTestCaseRound_RunInstance.Create(
  ParentAsObject: TReflector; TestBed: TObject;
  const Case1: ITestCaseInternal; RoundNumber1: integer;
  const Mem1: IMemoryMonitor; const Cursor: ITestCaseLineCursoredItem);
begin
FMessages := TMessageList.Create;
FExecutionMessages := TMessageList.Create;
inherited Create( ParentAsObject, Mem1);
FRoundNumber := RoundNumber1;
FTestCase := Case1;
FTestBed := TestBed;
FCursor := Cursor
end;

procedure TTestCaseRound_RunInstance.Clear;
begin
inherited;
FCheckCount := 0;
FRoundsCompleted := 0;
FErrorCount := 0;
FPassCount := 0;
FFailCount := 0;
FWarnCount := 0;
FMessages.Clear;
FExecutionMessages.Clear;
FAbortedAfterSetup := False
end;


procedure TTestCaseRound_RunInstance.ActivateHeapMonitoring( TurnOn: Boolean);
begin
if not assigned( FMem) then exit;
if TurnOn then
    FMem.HeapMonitoring_ON
  else
    FMem.HeapMonitoring_OFF
end;

procedure TTestCaseRound_RunInstance.BeforeRun;
begin
doSetGateway := Supports( Fixture, IPlatterFactory, Fctry) and assigned( FTestBed);
if doSetGateway then
  Fctry.SetGateway( FTestBed, self)
end;

procedure TTestCaseRound_RunInstance.Check;
begin
Inc( FCheckCount)
end;

procedure TTestCaseRound_RunInstance.CleanUpAfterRun;
begin
FCursor  := nil;
FTestBed := nil
end;

procedure TTestCaseRound_RunInstance.ClearHeapMonitoring;
begin
if assigned( FMem) then
  FMem.Clear
end;

procedure TTestCaseRound_RunInstance.DoInnerRun;
begin
Setup;
if GetStatus = tsExecuting then
  Execute;
if GetStatus = tsTeardown then
  Teardown;
FRoundsCompleted := 1;
if doSetGateway then
  Fctry.SetGateway( FTestBed, nil);
if FFailCount > 0 then
    Status := tsFail
  else if FWarnCount > 0 then
    Status := tsWarn
  else if FAbortedAfterSetup then
    Status := tsSkipped
  else
    begin
    Inc( FPassCount);
    Status := tsPass
    end
end;

function TTestCaseRound_RunInstance.ErrorCount: integer;
begin
result := FErrorCount
end;

procedure TTestCaseRound_RunInstance.Execute;
var
  Ok: boolean;
  sFail: string;
  Msg: RMessage;
  Code: integer;

  procedure Call_Execute;
  begin
  try
    RawExecute
	 except on E: ETestPass do
       Inc( FCheckCount)
    end;
  end;

begin
Code := 0;
if FRunInstanceServices.IsAborted then
    FAbortedAfterSetup := True
  else
    begin
    try
      ActivateHeapMonitoring( True);
      Call_Execute;
      ActivateHeapMonitoring( False);
      Ok := True
    except
      on E: ETestFailure do
        begin
        Ok := False;
        sFail := E.Message;
        Code := TestFailure;
        end;
      on E: Exception do
        begin
        Ok := False;
        Code := TestException;
        sFail := Format( 'Test case failed because of unexpected exception %s: %s', [E.ClassName, E.Message])
        end;
      end;
    UniqueCloneList( FExecutionMessages, FMessages);
    ActivateHeapMonitoring( True);
    FExecutionMessages.Clear;
    ActivateHeapMonitoring( False);
    if Ok and (FCheckCount = 0) then
      begin
      Ok := False;
      sFail := 'Test case failed because no assertions were made.';
      Code := NoChecks;
      end;
    if not Ok then
      begin
      Inc( FFailCount);
      PutSimpleMessage( Code, sFail, lvError)
      end;
    if assigned( FRunInstanceServices) then
      for Msg in FMessages.AsSource( [lvWarning]) do
        begin
        Inc( FWarnCount, 1);
        FRunInstanceServices.Put( self, FTestCase as ITestNode, Msg, FRoundsCompleted);
        end
    end;
FExecuted := True;
Status := tsTeardown
end;

procedure TTestCaseRound_RunInstance.Fail( const sErrMsg: string; Address: pointer);
begin
if GetStatus = tsExecuting then
    raise ETestFailure.Create( sErrMsg) at Address
  else
    raise Exception.Create( 'Assert called outside of the test case. ' + sErrMsg)
end;

function TTestCaseRound_RunInstance.FailCount: integer;
begin
result := FFailCount
end;

function TTestCaseRound_RunInstance.HasMessageList( var Lst: IMessageList): boolean;
begin
result := True;
Lst := FMessages
end;

function TTestCaseRound_RunInstance.IgnoredCount: integer;
begin
result := 0
end;

function TTestCaseRound_RunInstance.LeakageDetected( var Leakage: int64): boolean;
begin
result := assigned( FMem);
if result then
    begin
    Leakage := FMem.Leakage;
    result  := Leakage <> 0
    end
  else
    Leakage := 0
end;

function TTestCaseRound_RunInstance.NodeKind: TResultNodeKind;
begin
result := rnTestCaseRound
end;

procedure TTestCaseRound_RunInstance.Pass;
begin
if FStatus = tsExecuting then
    raise ETestPass.Create( 'Pass')
  else
    raise Exception.Create( 'Assert called outside of the test case.')
end;

function TTestCaseRound_RunInstance.PassCount: integer;
begin
result := FPassCount
end;

procedure TTestCaseRound_RunInstance.RawExecute;
begin
if assigned( FCursor) then
    FCursor.ConductTest
  else
    FTestCase.Execute( FTestBed)
end;

function TTestCaseRound_RunInstance.RoundsCompleted: integer;
begin
result := FRoundsCompleted
end;

function TTestCaseRound_RunInstance.RoundsPerTestCase: integer;
begin
result := 1
end;

function TTestCaseRound_RunInstance.Description: string;
begin
if assigned( FCursor) then
    result := FCursor.LineItemDescription
  else
    result := '(none)'
end;

procedure TTestCaseRound_RunInstance.Setup;
var
  Ok: boolean;
  sErr: string;
  Msg: RMessage;

  procedure Call_Setup;
  begin
  (FTestCase.Parent.Parent as ITestFixtureInternal).Setup( FTestBed)
  end;

begin
Status := tsSetup;
ClearHeapMonitoring;
try
    ActivateHeapMonitoring( True);
    Call_Setup;
    ActivateHeapMonitoring( False);
    Ok := True
  except on E: Exception do
    begin
    Ok := False;
    sErr := E.Message
    end
  end;
if Ok then
    Status := tsExecuting
  else
    begin
    Inc( FErrorCount);
    PutSimpleMessage( SetupError, sErr, lvError);
    Status := tsError
    end
end;

function TTestCaseRound_RunInstance.SkippedCount: integer;
begin
if GetStatus = tsSkipped then
    result := 1
  else
    result := 0
end;

procedure TTestCaseRound_RunInstance.TearDown;
var
  Ok: boolean;
  sErr, sFail: string;
  Leakage: int64;

  procedure Call_Teardown;
  begin
  (FTestCase.Parent.Parent as ITestFixtureInternal).Teardown( FTestBed)
  end;

begin
try
  ActivateHeapMonitoring( True);
  Call_Teardown;
  ActivateHeapMonitoring( False);
  Ok := True
except on E: Exception do
    begin
    Ok := False;
    sErr := E.Message
    end
  end;
if Ok then
    begin
    if LeakageDetected( Leakage) and (not TestCase.Parent.IgnoreLeaks) and
      (FErrorCount = 0) and (FFailCount = 0) then
      begin
      Ok := False;
      sFail := Format( 'Memory leakage detected %d. bytes lost', [Leakage]);
      Inc( FFailCount);
      PutSimpleMessage( LeakageFailure, sFail, lvError)
      end
    end
  else
    begin
    Inc( FErrorCount);
    PutSimpleMessage( TearDownError, sErr, lvError);
    Status := tsError
    end
end;

function TTestCaseRound_RunInstance.TestCase: ITestCase;
begin
result := FTestCase
end;

function TTestCaseRound_RunInstance.TestNode: ITestNode;
begin
result := FTestCase as ITestNode
end;

procedure TTestCaseRound_RunInstance.Warn( const sWarning: string);
var
  Msg: RMessage;
begin
SetFrameworkMessage( Msg, UserIssuedWarning, sWarning, lvWarning);
FExecutionMessages.Add( Msg)
end;

function TTestCaseRound_RunInstance.WarnCount: integer;
begin
result := FWarnCount
end;

function TTestCaseRound_RunInstance.WorkLoad: integer;
begin
result := 1
end;

end.
