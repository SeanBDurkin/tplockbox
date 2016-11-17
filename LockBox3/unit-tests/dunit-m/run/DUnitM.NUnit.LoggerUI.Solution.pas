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

unit DUnitM.NUnit.LoggerUI.Solution;
interface
uses DUnitM.UnitTestFramework, SBD.ServiceProvider;

type
TLoggerFactoryUI = class( TInterfacedObject, ILoggerFactoryUI)
  private
    [Injection('NUnit2.5')] FFactory: ILoggerFactory;
    function  Factory: ILoggerFactory;
    function  EnquireForCreateLogger( out Params: IInterface): boolean;
    procedure ViewEditProperties( const Logger: ITestLogger; Params: IInterface);
  public
    [Configuration] constructor ServiceModeCreate;
  end;

procedure RegisterServices( const Provider: IServiceProvider);

implementation







uses SysUtils, Dialogs, IOUtils;

type
TFilenameFactoryParameter = class( TInterfacedObject, IFilenameFactoryParameter)
  private
    FFileName: string;
    function FileName: string;
    procedure SetFileName( const Value: string);
    function isValidFilename( const Value: string): boolean;
  end;




procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( ILoggerFactoryUI, TLoggerFactoryUI)
end;

function TLoggerFactoryUI.EnquireForCreateLogger(
  out Params: IInterface): boolean;
var
  FNParam: IFilenameFactoryParameter;
  AFileName: string;
begin
AFileName := 'NUnit25.log';
result := Dialogs.PromptForFileName( AFileName, 'NUnit log filename?', '.log', 'Title', '', True);
if not result then exit;
AFileName := TPath.GetFullPath( AFileName);
FNParam := TFilenameFactoryParameter.Create;
FNParam.SetFileName( AFileName);
Params := FNParam;
result := FNParam.isValidFilename( AFileName)
end;

function TLoggerFactoryUI.Factory: ILoggerFactory;
begin
result := FFactory
end;

constructor TLoggerFactoryUI.ServiceModeCreate;
begin
end;

procedure TLoggerFactoryUI.ViewEditProperties(
  const Logger: ITestLogger; Params: IInterface);
var
  FNParam: IFilenameFactoryParameter;
  FN: string;
begin
if Supports( Params, IFilenameFactoryParameter, FNParam) then
  FN := FNParam.FileName;
Dialogs.ShowMessageFmt( 'DUnit 2.5 log file is %s', [FN])
end;


function TFilenameFactoryParameter.FileName: string;
begin
result := FFileName
end;

function TFilenameFactoryParameter.isValidFilename(
  const Value: string): boolean;
begin
result := True
end;

procedure TFilenameFactoryParameter.SetFileName( const Value: string);
begin
FFileName := Value
end;

end.
