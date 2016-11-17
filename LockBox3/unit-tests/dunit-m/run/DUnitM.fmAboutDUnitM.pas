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

unit DUnitM.fmAboutDUnitM;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, HTTPApp, HTTPProd;

type
  TfmAboutDUnitM = class(TForm)
    imgLogo: TImage;
    txtIntro: TStaticText;
    pnlFlow: TFlowPanel;
    pnlTitle: TPanel;
    pnlTitleCaptionBar: TPanel;
    pnlPlatform: TPanel;
    pnlPlatformCaptionBar: TPanel;
    txtPlatform: TStaticText;
    pnlArchitecture: TPanel;
    pnlArchitectureCaptionBar: TPanel;
    txtArchitecture: TStaticText;
    pnlBuild: TPanel;
    pnlBuildCaptionBar: TPanel;
    txtBuild: TStaticText;
    ppStats: TPageProducer;
    scrbxTitle: TScrollBox;
    txtTitle: TStaticText;
    btnDummyButtonToForceScrollBar: TButton;
    procedure ppStatsHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: string; TagParams: TStrings; var ReplaceText: string);
    procedure FormShow(Sender: TObject);
  private
    procedure CheckReplacements;
  public
    { Public declarations }
  end;


implementation







uses IOUtils, DUnitM.RttiUtils, SBD.Environment
     {$if CompilerVersion >= 27.0}
     , System.Types
     {$ifend}
     ;
{$R *.dfm}

procedure TfmAboutDUnitM.CheckReplacements;
var
  Comp: TComponent;
  TextThing: TStaticText;
  DC: HDC;
  MultiLine: TStrings;
  EachLine: string;
  TextSize, GrossTextSize: TSize;
  SaveFont: HFONT;

begin
if not assigned( ppStats) then exit;
for Comp in self do
  if Comp is TStaticText then
    begin
    TextThing := TStaticText( Comp);
    ppStats.HTMLDoc.Text := TextThing.Caption;
    TextThing.Caption := ppStats.Content
    end;
FreeAndNil( ppStats);

MultiLine := TStringList.Create;
MultiLine.Text := txtTitle.Caption;
GrossTextSize.cx := 0;
GrossTextSize.cy := 0;
DC := GetDC( 0);
try
  SaveFont := SelectObject( DC, txtTitle.Font.Handle);
  for EachLine in MultiLine do
    begin
    GetTextExtentPoint32( DC, EachLine, Length( EachLine), TextSize);
    if TextSize.cx > GrossTextSize.cx then
      GrossTextSize.cx := TextSize.cx;
    GrossTextSize.cy := GrossTextSize.cy + TextSize.cy + 10
    end;
  SelectObject( DC, SaveFont)
finally
  ReleaseDC( 0, DC)
  end;
txtTitle.Align := alNone;
txtTitle.SetBounds( txtTitle.Left, txtTitle.Top,
  GrossTextSize.cx + (GetSystemMetrics( SM_CXBORDER) * 4) + 10,
  GrossTextSize.cy + (GetSystemMetrics( SM_CYBORDER) * 4));
btnDummyButtonToForceScrollBar.SetBounds(
  txtTitle.Left + txtTitle.Width - 1,
  txtTitle.Top, 1, 1);
scrbxTitle.HorzScrollBar.Range := txtTitle.Width
end;

procedure TfmAboutDUnitM.FormShow(Sender: TObject);
begin
CheckReplacements
end;

type
{$if CompilerVersion < 20.00}
    TrueNativeInt  = integer;
{$else}
    TrueNativeInt  = NativeInt;
{$ifend}

procedure TfmAboutDUnitM.ppStatsHTMLTag(
  Sender: TObject; Tag: TTag; const TagString: string; TagParams: TStrings; var ReplaceText: string);
const
  ArchStrings: array[ TProcessorArchitecture] of string = (
      'x86 (32 bit)',
      'x64 (64 bit)',
      'Intel Itanium Processor Family (IPF) (64 bit)',
      'iOS',
      'OS X',
      'Android',
      'Linux',
      'Unrecognised architecture');
begin
if TagString = 'AppTitle' then
    ReplaceText := Application.Title

  else if TagString = 'AppFileName' then
    ReplaceText := TPath.GetFileName( Application.ExeName)

  else if TagString = 'AppPath' then
    ReplaceText := TPath.GetDirectoryName( Application.ExeName)

  else if TagString = 'Platform' then
    begin
    if SizeOf( TrueNativeInt) = 4 then
        ReplaceText := 'Win32'
      else
        ReplaceText := 'Win64'
    end
  else if TagString = 'OSArchitecture' then
    ReplaceText := ArchStrings[ TProcessorInformation.Architecture]

  else if TagString = 'OSVersion' then
    ReplaceText := TProcessorInformation.OSVersion

  else if TagString = 'Now' then
    {$if CompilerVersion >= 22.0}
      ReplaceText := FormatDateTime( FormatSettings.LongDateFormat, Now)
    {$else}
      ReplaceText := FormatDateTime(                LongDateFormat, Now)
    {$ifend}
  else if TagString = 'TZ' then
    ReplaceText := TProcessorInformation.UTC_Offset

  else if TagString = 'CompilerVersion' then
    ReplaceText := Format( '%2.2f', [CompilerVersion])

  else if TagString = 'CompilerName' then
    ReplaceText := TCompileInformation.CompilerName

  else if TagString = 'BuildWithPackages' then
    ReplaceText := BoolToStr( TCompileInformation.BuiltWithRuntimePackages, True)

  else if TagString = 'Config' then
    begin
    {$IFDEF DEBUG}
      ReplaceText := 'DEBUG'
    {$ELSE}
      {$IFDEF RELEASE}
        ReplaceText := 'RELEASE'
      {$ELSE}
        ReplaceText := '(unrecognised)'
      {$ENDIF}
    {$ENDIF}
    end;
end;

end.
