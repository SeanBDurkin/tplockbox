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

unit DUnitM.NUnit.Logger.Solution;
interface
uses DUnitM.UnitTestFramework, SBD.ServiceProvider, DUnitM.NUnit, SBD.Messages;

type
TNUnit25_Logger = class( TInterfacedObject, ITestLogger)
  private
    [Injection('NUnit2.5')] FFilenameSource: IFilenameSource;
    [Injection] FNUnit25DomainLogger: INUnit25_ResultsLog;
    FDescription: string;
    FFilename: string;

    function  MenuDescription: string;
    procedure OnEnableChange( const Eng: IUnitTestingEngine; const Proc: ITestProcedure; const Datum: IInterface);
    procedure OnBeginRunInstance( Workload: integer);
    procedure OnEndRunInstance( const RunResult: ITestResult);
    procedure OnChangeStatus(
                        NodeKind: TLoggerNode;
                  const Fixture: ITestFixture;
                  const TestProc: ITestProcedure;
                  const TestCase: ITestCase;
                  const TestResult: ITestResult;
                  const NodeDatum: IInterface);
    procedure OnProgressTick( const GrossResult: ITestResult; WorkDone, WorkLoad: integer);
    procedure Breathe;
    procedure OnAuxilaryMessage( const TestResult: ITestResult; const TestCase: ITestCase; const Msg: RMessage; const NodeDatum: IInterface);
    procedure Shutdown( const Eng: IUnitTestingEngine);
    procedure OnAttachSuite( Suite: ITestSuite;
             SetFixtureDatum: TSetFixtureDatumFunc;
             SetTestProcDatum: TSetTestProcDatumFunc;
             SetTestCaseDatum: TSetTestCaseDatumFunc);
  public
    [Configuration('NUnit2.5')] constructor ServiceModeCreate;
  end;

TLoggerFactory = class( TInterfacedObject, ILoggerFactory, IFilenameSource)
  private
    [Injection] FLoggerProvider: IServiceProvider;
    FParams: IInterface;
    function MenuDescription: string;
    function CreateLogger( const Params: IInterface): ITestLogger;
    function FileName: string;
  public
    [Configuration('NUnit2.5')] constructor ServiceModeCreate;
  end;


procedure RegisterServices( const Provider: IServiceProvider);

implementation






uses IOUtils, SysUtils, Classes, SBD.Generics;

procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( ITestLogger, TNUnit25_Logger);
Provider.RegisterServiceClass( ILoggerFactory, TLoggerFactory);
end;


procedure TNUnit25_Logger.Breathe;
begin
end;

function TNUnit25_Logger.MenuDescription: string;
begin
result := FDescription
end;

procedure TNUnit25_Logger.OnAttachSuite(Suite: ITestSuite;
  SetFixtureDatum: TSetFixtureDatumFunc;
  SetTestProcDatum: TSetTestProcDatumFunc;
  SetTestCaseDatum: TSetTestCaseDatumFunc);
begin
end;

procedure TNUnit25_Logger.OnAuxilaryMessage(
  const TestResult: ITestResult; const TestCase: ITestCase; const Msg: RMessage; const NodeDatum: IInterface);
begin
end;

procedure TNUnit25_Logger.OnBeginRunInstance( Workload: integer);
begin
end;

procedure TNUnit25_Logger.OnChangeStatus(
  NodeKind: TLoggerNode;
  const Fixture: ITestFixture; const TestProc: ITestProcedure;
  const TestCase: ITestCase; const TestResult: ITestResult;
  const NodeDatum: IInterface);
begin
end;

procedure TNUnit25_Logger.OnEnableChange(
  const Eng: IUnitTestingEngine; const Proc: ITestProcedure; const Datum: IInterface);
begin
end;

procedure TNUnit25_Logger.OnEndRunInstance( const RunResult: ITestResult);
var
  Source: TStream;
  NUnit25, RootLevel_NUnit25: INUnit25_TestSuite;
  Total, Errored, NotRun, Ignored, Skipped: integer;
  GrossDuration: double;
  NUnitRunResult: TNUnitResult;
  SourceIsTransferred: boolean;

  procedure ParseResults( var ParentSuite: INUnit25_TestSuite; const Results: ISEnumerable<ITestResult>; doCloseResults: boolean);
  const
    ElementPathTail = '/test-suite/results';
  var
    NUnitRunResult: TNUnitResult;
    TestResult: ITestResult;
    Suite: INUnit25_TestSuite;
    Duration: double;
    Reason: string;
    Msg: RMessage;

  begin
  // Upon entry to this routine, we assume that we are at an open <test-suite>
  //  and below that an open <results> element.
  // The task here is to add the content for the collection of ITestResult's ('Results'),
  //  and then close the <results> element and the <test-suite> element.
  // For fixtures, the content is a test-suite element.
  // For test cases, the content is a test-case element.
  // Test procedures are not directly represented, as this concept is not supported by N-Unit.
  NUnitRunResult := rInconclusive;
  for TestResult in Results do
    begin
    Suite := nil;
    Duration := TestResult.Timing.FDuration_ms / (1000 * SecsPerDay);
    case TestResult.Status of
      tsError, tsSetup, tsTeardown, tsExecuting: NUnitRunResult := rError;
      tsNeutral, tsSkipped:                      NUnitRunResult := rIgnored;
      tsPass, tsWarn:                            NUnitRunResult := rSuccess;
      tsFail:                                    NUnitRunResult := rFailure;
      end;
    if TestResult.ErrorOrFailureMsg( Msg) then
        Reason := Msg.FDisplayText
      else
        Reason := '';
    case TestResult.NodeKind of
      rnNamespaceFixture:
        // Add a <test-suite type="Namespace"> element, and then below that
        //  another <results> element.
        Suite := ParentSuite.AddNamespaceSuite(
          TestResult.Fixture.Name, TestResult.Executed, NUnitRunResult,
          Duration, 0);

      rnLeafFixture:
        // Add a <test-suite type="TestFixture"> element, and then below that
        //  another <results> element.
        Suite := ParentSuite.AddFixtureSuite(
          TestResult.Fixture.Name, TestResult.Fixture.Description,
          TestResult.Executed, NUnitRunResult, Duration, 0, Reason);

      rnTestProc:
        begin
        // Ignore the procedure level. Drill down to the test cases.
        Suite := nil;
        ParseResults( ParentSuite, TestResult.ChildResults, False)
        end;

      rnTestCase:
        begin
        Suite := nil;
        ParentSuite.AddTestCase(
          TestResult.TestCase.DisplayName, TestResult.TestCase.Parent.Description,
          TestResult.Executed, NUnitRunResult, Duration, 0, Reason)
        // Ignore the test case round level. All rounds are summarised and rolled up
        //  into TestCase results.
        end
      end;
    if assigned( Suite) then
      // If suite exists then we have an open <results> element to close off,
      //  as a consequence of our response to the TestResult child
      ParseResults( Suite, TestResult.ChildResults, True)
    end;
  if doCloseResults and assigned( ParentSuite) then
    ParentSuite := ParentSuite.Close // < closes the results,test-suites elements.
  end;

begin
SourceIsTransferred := False;
Source := TFile.Open( FFilename, IOUtils.TFileMode.fmOpenOrCreate);
try
  Source.Size := 0;
  Errored := RunResult.ErrorCount;
  Total   := RunResult.PassCount + RunResult.FailCount + RunResult.WarnCount;
  NotRun  := 0; // To be developed
  Ignored := 0; // To be developed
  Skipped := 0; // To be developed
  if Errored > 0 then
      NUnitRunResult := rError
    else if RunResult.FailCount > 0 then
      NUnitRunResult := rFailure
    else if (RunResult.PassCount + RunResult.WarnCount) > 0 then
      NUnitRunResult := rSuccess
    else
      NUnitRunResult := rIgnored;
  GrossDuration := RunResult.Timing.FDuration_ms / (1000 * SecsPerDay);
  SourceIsTransferred := True;
  NUnit25 := FNUnit25DomainLogger.OpenLog( Source, ParamStr( 0), RunResult.Timing.FStart,
        Total, RunResult.FailCount, Errored, NotRun, Ignored, Skipped, NUnitRunResult, GrossDuration);
  RootLevel_NUnit25 := NUnit25;
  ParseResults( NUnit25, RunResult.ChildResults, True);
  RootLevel_NUnit25.CloseTestResults
finally
  FNUnit25DomainLogger.CloseLog;
  if not SourceIsTransferred then
    Source.Free
  end
end;

procedure TNUnit25_Logger.OnProgressTick(
  const GrossResult: ITestResult; WorkDone, WorkLoad: integer);
begin
end;

constructor TNUnit25_Logger.ServiceModeCreate;
begin
if assigned( FFilenameSource) then
    begin
    FFilename := FFilenameSource.FileName;
    FFilenameSource := nil
    end
  else
    FFilename := '';
if FFilename = '' then
  FFilename :=  TPath.ChangeExtension( ParamStr( 0), '.log');
FDescription := Format( 'NUnit 2.5 log at %s', [TPath.GetFileName( FFilename)])
end;

procedure TNUnit25_Logger.Shutdown( const Eng: IUnitTestingEngine);
begin
end;

{ TLoggerFactory }

function TLoggerFactory.CreateLogger( const Params: IInterface): ITestLogger;
var
  InjectionOverride: IServiceProvider;
begin
FParams := Params;
InjectionOverride := StandardServiceProvider;
InjectionOverride.RegisterLiveService( IFileNameSource, self as IFileNameSource, 'NUnit2.5');
FLoggerProvider.Gn.Acquire<ITestLogger>( nil, result, 'NUnit2.5', InjectionOverride);
InjectionOverride.ShutDown;
FParams := nil
end;

function TLoggerFactory.FileName: string;
var
  FNParam: IFilenameFactoryParameter;
begin
if Supports( FParams, IFilenameFactoryParameter, FNParam) then
    result := FNParam.FileName
  else
    result := ''
end;

function TLoggerFactory.MenuDescription: string;
begin
result := 'NUnit v2.5'
end;

constructor TLoggerFactory.ServiceModeCreate;
begin
end;

end.
