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

unit DUnitM.NUnit;
interface
uses Classes;

type
TNUnitResult = (rFailure, rSuccess, rIgnored, rError, rInconclusive, rNotRunnable);
INUnit25_TestSuite = interface
  ['{B9400675-292A-4646-A890-2246C2CD4060}']
    function AddNamespaceSuite(
      const Name: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer): INUnit25_TestSuite;

    function AddFixtureSuite(
      const Name, Description: string; Executed: boolean; NetResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string): INUnit25_TestSuite;

    procedure AddTestCase(
      const Name, Description: string; Executed: boolean; CaseResult: TNUnitResult;
      Time: double; Asserts: integer;
      const Reason: string);

    function  Close: INUnit25_TestSuite;
    procedure CloseTestResults;
    function  ElementStack: string;
  end;

type
INUnit25_ResultsLog = interface
  ['{3C975A4D-F0A4-4AF2-9DF0-BD6407325F99}']
{$REGION 'HelpInsight'}
/// <remarks>
///  OpenLog() clears the stream and writes like ...
///  <code>
///     &lt;test-results name="etc" &gt;&#x0A;
///       &lt;environment nunit-version="2.5.8.0" /&gt;&#x0A;
///       &lt;culture-info current-culture="en" current-uiculture="en" /&gt;&#x0A;
///        &lt;test-suite name="etc.exe" executed="true" result="Success" success="true" time="0.000" asserts="0" type="Assembly" &gt;&#x0A;
///          &lt;results
///  </code>
/// </remarks>
{$ENDREGION}
    function OpenLog( OutStream1: TStream;
      const ProgName: string;
      RunStamp: TDateTime;
      Total, Failed, Errored, NotRun, Ignored, Skipped: integer;
      RunResult: TNUnitResult;
      GrossDuration: double)  // units of days.
      : INUnit25_TestSuite;

{$REGION 'HelpInsight'}
/// <remarks>
///  CloseLog() closes the stream and assumes that all open elements have been closed.
/// </remarks>
{$ENDREGION}
    procedure CloseLog;
  end;


implementation

end.
