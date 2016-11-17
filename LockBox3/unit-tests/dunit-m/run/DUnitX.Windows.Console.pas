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

unit DUnitX.Windows.Console;

interface

uses
  classes,
  DUnitX.ConsoleWriter.Base;

{$I ../includes/DUnitM.inc}

type
  TDUnitXWindowsConsoleWriter = class(TDUnitXConsoleWriterBase)
  private
    FDefaultForeground : Word;
    FDefaultBackground : Word;
    FLastForeground : TConsoleColour;
    FLastBackground : TConsoleColour;
    FStdOut : THandle;
    function GetForegroundColourCode(const cc: TConsoleColour): Word;
    function GetBackgroundColourCode(const cc: TConsoleColour): Word;
    function GetConsoleWidth : Integer;
  protected
    procedure InternalWriteLn(const s : String); override;
    procedure InternalWrite(const s : String); override;
  public
    procedure SetColour(const foreground: TConsoleColour; const background: TConsoleColour = ccDefault); override;
    constructor Create;override;
    destructor Destroy; override;
  end;


implementation

uses
  {$IFDEF MSWINDOWS}
    {$if CompilerVersion < 23 }
      Windows
    {$else}
      WinAPI.Windows // Delphi XE2 (CompilerVersion 23) added scopes in front of unit names
    {$ifend}
  {$ENDIF}
  , DUnitM.StringUtils
  ;


constructor TDUnitXWindowsConsoleWriter.Create;
var
  dummy : Cardinal;
  consoleInfo : _CONSOLE_SCREEN_BUFFER_INFO;
begin
  inherited;
  FStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if not GetConsoleMode(FStdOut, dummy) then // Not a console handle
    Self.RedirectedStdOut := True;
  Self.ConsoleWidth := GetConsoleWidth;
  FLastForeground := ccDarkYellow; // Just to ensure the first colour change goes through
  FLastBackground := ccDarkYellow;
  // Save the current console colour settings so we can restore them:
  if GetConsoleScreenBufferInfo(FStdOut, consoleInfo) then
  begin
    FDefaultForeground := consoleInfo.wAttributes and (FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY);
    FDefaultBackground := consoleInfo.wAttributes and (BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY);
  end
  else
  begin
    FDefaultForeground := GetForegroundColourCode(ccWhite);
    FDefaultBackground := GetBackgroundColourCode(ccBlack);
  end;
end;

procedure TDUnitXWindowsConsoleWriter.SetColour(const foreground, background: TConsoleColour);
begin
  if (FLastForeground <> foreground) or (FLastBackground <> background) then
  begin
    SetConsoleTextAttribute(FStdOut,
       GetForegroundColourCode(foreground) or
       GetBackgroundColourCode(background));
    FLastForeground := foreground;
    FLastBackground := background;
  end;
end;

function TDUnitXWindowsConsoleWriter.GetForegroundColourCode(const cc : TConsoleColour) : Word;
begin
  Result := 0;
  case cc of
    ccDefault       : Result := FDefaultForeground;
    ccBrightRed     : Result := FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkRed       : Result := FOREGROUND_RED;
    ccBrightBlue    : Result := FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    ccDarkBlue      : Result := FOREGROUND_BLUE;
    ccBrightGreen   : Result := FOREGROUND_GREEN or FOREGROUND_INTENSITY;
    ccDarkGreen     : Result := FOREGROUND_GREEN;
    ccBrightYellow  : Result := FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkYellow    : Result := FOREGROUND_GREEN or FOREGROUND_RED;
    ccBrightAqua    : Result := FOREGROUND_GREEN or FOREGROUND_BLUE or FOREGROUND_INTENSITY;
    ccDarkAqua      : Result := FOREGROUND_GREEN or FOREGROUND_BLUE;
    ccBrightPurple  : Result := FOREGROUND_BLUE or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccDarkPurple    : Result := FOREGROUND_BLUE or FOREGROUND_RED;
    ccGrey          : Result := FOREGROUND_INTENSITY;
    ccBlack         : Result := 0;
    ccBrightWhite   : Result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED or FOREGROUND_INTENSITY;
    ccWhite         : Result := FOREGROUND_BLUE or FOREGROUND_GREEN or FOREGROUND_RED;
  end;
end;


function PadString(const s: string; const totalLength: integer; const padLeft: boolean = True; padChr: Char = ' '): string;
begin
  Result := s;
  while Length(result) < totalLength do
  begin
    if padLeft then
      Result := padChr + Result
    else
      Result := Result + padChr;
  end;
end;

procedure TDUnitXWindowsConsoleWriter.InternalWrite(const s: String);
var
  output : string;
  dummy : Cardinal;
begin
  output := PadString(s, length(s)+ Self.CurrentIndentLevel, True, ' ');
  if Self.RedirectedStdOut then
    System.Write(output)
  else
    WriteConsoleW(FStdOut, PWideChar(output), Length(output), dummy, nil);
end;

procedure TDUnitXWindowsConsoleWriter.InternalWriteLn(const s: String);
var
  output : string;
  dummy : Cardinal;
begin
  output := PadString(s, length(s)+ Self.CurrentIndentLevel, True, ' ') + #13#10;
  if Self.RedirectedStdOut then
    System.Write(output)
  else
    WriteConsoleW(FStdOut, PWideChar(output), Length(output), dummy, nil);
end;

destructor TDUnitXWindowsConsoleWriter.Destroy;
begin
  SetColour(ccDefault); // Restore default console colours
  inherited;
end;

function TDUnitXWindowsConsoleWriter.GetBackgroundColourCode(const cc : TConsoleColour) : Word;
begin
  Result := 0;
  case cc of
    ccDefault       : Result := FDefaultBackground;
    ccBrightRed     : Result := BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkRed       : Result := BACKGROUND_RED;
    ccBrightBlue    : Result := BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    ccDarkBlue      : Result := BACKGROUND_BLUE;
    ccBrightGreen   : Result := BACKGROUND_GREEN or BACKGROUND_INTENSITY;
    ccDarkGreen     : Result := BACKGROUND_GREEN;
    ccBrightYellow  : Result := BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkYellow    : Result := BACKGROUND_GREEN or BACKGROUND_RED;
    ccBrightAqua    : Result := BACKGROUND_GREEN or BACKGROUND_BLUE or BACKGROUND_INTENSITY;
    ccDarkAqua      : Result := BACKGROUND_GREEN or BACKGROUND_BLUE;
    ccBrightPurple  : Result := BACKGROUND_BLUE or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccDarkPurple    : Result := BACKGROUND_BLUE or BACKGROUND_RED;
    ccGrey          : Result := BACKGROUND_INTENSITY;
    ccBlack         : Result := 0;
    ccBrightWhite   : Result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED or BACKGROUND_INTENSITY;
    ccWhite         : Result := BACKGROUND_BLUE or BACKGROUND_GREEN or BACKGROUND_RED;
  end;
end;

function TDUnitXWindowsConsoleWriter.GetConsoleWidth: Integer;
var
  info : CONSOLE_SCREEN_BUFFER_INFO;
begin
  Result := High(Integer); // Default is unlimited width
  if GetConsoleScreenBufferInfo(FStdOut, info) then
    Result := info.dwSize.X;
end;


end.
