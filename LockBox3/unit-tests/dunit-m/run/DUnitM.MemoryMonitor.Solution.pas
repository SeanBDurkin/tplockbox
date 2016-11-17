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

unit DUnitM.MemoryMonitor.Solution;
interface
uses DUnitM.MemoryMonitor, SBD.ServiceProvider, Classes;

type
TMemoryMonitor = class( TInterfacedObject, IMemoryMonitor)
  private
    FMS1, FMS2: TMemoryManagerState;
    FLeakageOffset: int64;
    FIsMonitoring: boolean;
    procedure Clear;
    procedure HeapMonitoring_ON;
    procedure HeapMonitoring_OFF;
    function  Leakage: int64;
  public
    constructor Create;
  end;


procedure RegisterServices( const Provider: IServiceProvider);
// Provides:
//   GUID                  config        ArrayId                          Mode
//   ----------------------------------------------------------------------------------
//   IMemoryMonitor        ''            'System.TMemoryManagerState'     Competitive
//
// Requires:
//   GUID                  config        ArrayId                          Mode
//   ----------------------------------------------------------------------------------
//   None


implementation








uses SysUtils;




procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterFlyweightService( IMemoryMonitor,
  function( const Config: string; const ServiceProvider: IServiceProvider): IInterface
    begin
    result := TMemoryMonitor.Create as IMemoryMonitor
    end,
  '', 'System.TMemoryManagerState')
end;


constructor TMemoryMonitor.Create;
begin
Clear
end;

procedure TMemoryMonitor.Clear;
begin
FIsMonitoring := False;
FLeakageOffset := 0
end;


procedure TMemoryMonitor.HeapMonitoring_OFF;
var
  i: Integer;
  SMBSize1, SMBSize2: uint64;
begin
if not FIsMonitoring then exit;
SMBSize1 := 0;
SMBSize2 := 0;
GetMemoryManagerState( FMS2);
for i := 0 to NumSmallBlockTypes - 1 do // Iterate through the blocks
  begin
  with FMS1.SmallBlockTypeStates[i] do
    Inc( SMBSize1, (InternalBlockSize * AllocatedBlockCount));
  with FMS2.SmallBlockTypeStates[i] do
    Inc( SMBSize2, (InternalBlockSize * AllocatedBlockCount))
  end;
FLeakageOffset := FLeakageOffset +
                  int64( SMBSize2) - int64( SMBSize1) +
                 (int64( FMS2.TotalAllocatedMediumBlockSize) - int64( FMS1.TotalAllocatedMediumBlockSize)) +
                 (int64( FMS2.TotalAllocatedLargeBlockSize ) - int64( FMS1.TotalAllocatedLargeBlockSize ));
FIsMonitoring := False
end;

procedure TMemoryMonitor.HeapMonitoring_ON;
begin
if FIsMonitoring then exit;
GetMemoryManagerState( FMS1);
FIsMonitoring := True
end;

function TMemoryMonitor.Leakage: int64;
begin
result := FLeakageOffset
end;



end.
