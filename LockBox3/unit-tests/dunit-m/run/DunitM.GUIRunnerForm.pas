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

unit DUnitM.GUIRunnerForm;

interface

{$if RTLVersion < 24.00}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitM.BaseExecutive, AppEvnts,
  DUnitM.ViewModel_VCLForms, PlatformDefaultStyleActnCtrls, Menus, ActnPopup,
  ComCtrls, ImgList, ActnMan, ActnColorMaps, ActnList, ExtCtrls, StdCtrls,
  ToolWin, ActnCtrls, XPStyleActnCtrls, Generics.Collections, DUnitM.UnitTestFramework;
{$else}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitM.BaseExecutive, AppEvnts,
  DUnitM.ViewModel_VCLForms, PlatformDefaultStyleActnCtrls, Menus, ActnPopup,
  ComCtrls, ImgList, ActnMan, ActnColorMaps, ActnList, ExtCtrls, StdCtrls,
  ToolWin, ActnCtrls, XPStyleActnCtrls, Generics.Collections, DUnitM.UnitTestFramework,
  System.Actions, System.ImageList;
{$ifend}

const
  doAutoScroll = True;

type
  TmfmGUIRunner = class(TForm)
    appevMain: TApplicationEvents;
    actmngrMain: TActionManager;
    tlbrMain: TActionToolBar;
    sbarMain: TStatusBar;
    pnlTestCases: TPanel;
    splTestCases: TSplitter;
    actRun: TAction;
    actSelectAll: TAction;
    actSelectFailed: TAction;
    actClear: TAction;
    actToggleSelection: TAction;
    coloursXP: TXPColorMap;
    imglstGlyphs16x16: TImageList;
    pbarTests: TProgressBar;
    actAttachLogger: TAction;
    actDetachLogger: TAction;
    actEditLoggerProps: TAction;
    actHaltOnFirstFailure: TAction;
    actNotYetDeveloped: TAction;
    popupLogMemo: TPopupActionBar;
    actAbout: TAction;
    actClearLog: TAction;
    miClearLog: TMenuItem;
    memoLog: TRichEdit;
    actmngrIntraRun: TActionManager;
    actAbort: TAction;
    actClearAll: TAction;
    actSelectDefault: TAction;
    procedure FormDestroy(Sender: TObject);
    procedure appevMainIdle(Sender: TObject; var Done: Boolean);
    procedure actRunExecute(Sender: TObject);
    procedure actAttachLoggerExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actAbortExecute(Sender: TObject);
    procedure actAbortUpdate(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actSelectFailedExecute(Sender: TObject);
    procedure actSelectFailedUpdate(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure actClearUpdate(Sender: TObject);
    procedure actToggleSelectionExecute(Sender: TObject);
    procedure actEditLoggerPropsExecute(Sender: TObject);
    procedure actDetachLoggerExecute(Sender: TObject);
    procedure actRunUpdate(Sender: TObject);
    procedure actClearAllExecute(Sender: TObject);
    procedure actSelectDefaultExecute(Sender: TObject);
    procedure actHaltOnFirstFailureExecute(Sender: TObject);
    procedure actHaltOnFirstFailureUpdate(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actAboutUpdate(Sender: TObject);

  private
    FViewModel: IViewModel_VCLForms;

    function  CreateAttachLoggerAction( const Factory: ILoggerFactoryUI): TAction;
    function  CreateLoggerAction(
      Template: TAction; const Factory1: ILoggerFactoryUI; const Logger: ITestLogger;
      const Properties: IInterface; const DisplayName: string; AnchorPoint: TActionClientItem): TAction;

  protected
    procedure Loaded; override;
  end;

var
  mfmGUIRunner: TmfmGUIRunner;

implementation






uses RichEdit, DUnitM.fmAboutDUnitM;
{$R *.dfm}

type
TFactoryAction = class( TAction)
  public
    FFactory: ILoggerFactoryUI;
    constructor CreateFactoryAction( AOwner: TComponent; const Factory: ILoggerFactoryUI);
  end;

TLoggerAction = class( TAction)
  public
    FFactory: ILoggerFactoryUI;
    FLogger: ITestLogger;
    FProps: IInterface;
    FAnchorPoint: TActionClientItem;
    constructor CreateLoggerAction(
      AOwner: TComponent; const Factory1: ILoggerFactoryUI; const Logger: ITestLogger;
      const Props: IInterface; AnchorPoint: TActionClientItem);
  end;

TConcreteView = class( TViewModel_VCLForms)
  protected
    function  TreeOwner: TComponent;         override;
    function  TreeParent: TWinControl;       override;
    function  TreeName: string;              override;
    procedure IntegrateSecondariesIntoMenus;              override;
    procedure ClearLog;                                   override;
    procedure Put( Level: TPutLevel; const Line: string; const Args: array of const); override;
    procedure SetDisplayState( Value: TSuiteRunnerState); override;
    procedure Breathe;                                    override;
    procedure InitiateView( TestCaseCount: integer);      override;
    procedure EnterOperation( isEntering: boolean);       override;
    function  GetShowProgressBar: boolean;                override;
    procedure SetShowProgressBar( Value: boolean);        override;
    function  GetProgressPosition: integer;               override;
    procedure SetProgressPosition( Value: integer);       override;
    function  GetProgressMax: integer;                    override;
    procedure SetProgressMax( Value: integer);            override;

  private
    FSensitivity: TPutLevel;
    FBreathing: boolean;
    function  Form: TmfmGUIRunner;

  public
    constructor Create;                                     override;
  end;


procedure AssignAction( Destination, Source: TAction);
begin
Destination.OnExecute          := Source.OnExecute;
Destination.OnUpdate           := Source.OnUpdate;
Destination.AutoCheck          := Source.AutoCheck;
Destination.Caption            := Source.Caption;
Destination.Checked            := Source.Checked;
Destination.DisableIfNoHandler := Source.DisableIfNoHandler;
Destination.Enabled            := Source.Enabled;
Destination.GroupIndex         := Source.GroupIndex;
Destination.HelpContext        := Source.HelpContext;
Destination.HelpKeyword        := Source.HelpKeyword;
Destination.HelpType           := Source.HelpType;
Destination.Hint               := Source.Hint;
Destination.ImageIndex         := Source.ImageIndex;
Destination.ShortCut           := Source.ShortCut;
Destination.SecondaryShortCuts := Source.SecondaryShortCuts;
Destination.Visible            := Source.Visible;
Destination.OnHint             := Source.OnHint
end;



procedure TmfmGUIRunner.Loaded;
begin
inherited;
FViewModel := TConcreteView.Create;
FViewModel.FormLoaded( self)
end;


function TmfmGUIRunner.CreateLoggerAction(
  Template: TAction; const Factory1: ILoggerFactoryUI;
  const Logger: ITestLogger; const Properties: IInterface; const DisplayName: string;
  AnchorPoint: TActionClientItem): TAction;
begin
result := TLoggerAction.CreateLoggerAction( Template.Owner, Factory1, Logger, Properties, AnchorPoint);
result.ActionList := Template.ActionList;
AssignAction( result, Template);
if Pos( '%s', result.Caption) > 0 then
  result.Caption  := Format( result.Caption, [DisplayName]);
result.Category := '(dynamic)'
end;

procedure TmfmGUIRunner.actAbortExecute( Sender: TObject);
begin
FViewModel.RequestAbort
end;

procedure TmfmGUIRunner.actAbortUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := (not FViewModel.IsAbortRequested) and
                               FViewModel.Model.IsRunning
end;

procedure TmfmGUIRunner.actAboutExecute( Sender: TObject);
var
  Dlg: TfmAboutDUnitM;
begin
Dlg := TfmAboutDUnitM.Create( Self);
try
  Dlg.ShowModal
finally
  Dlg.Free
  end
end;

procedure TmfmGUIRunner.actAboutUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.CanDialog
end;


function FindMenuByCaption( ParentMenu: TActionClients; const Caption: string; var Finding: TActionClients): boolean;
var
  Item: TCollectionItem;
begin
result  := False;
Finding := nil;
if assigned( ParentMenu) then
  for Item in ParentMenu do
    begin
    result := SameText( StringReplace( (Item as TActionClientItem).Caption, '&', '', [rfReplaceAll]), Caption);
    if not result then continue;
    Finding := TActionClientItem( Item).Items;
    break
    end
end;



procedure TmfmGUIRunner.actAttachLoggerExecute( Sender: TObject);
var
  Logger: ITestLogger;
  Properties: IInterface;
  DisplayName: string;
  Anchor: TActionClientItem;
  OptionsMenu, SecondaryLoggersMenu: TActionClients;

  procedure AddActionMenuItem( Menu: TActionClients; Action: TAction);
  begin
  Menu.Add.Action := CreateLoggerAction(
    Action, (Sender as TFactoryAction).FFactory, Logger, Properties, DisplayName, Anchor)
  end;

begin
  // For each attached secondary logger, we will also have ...
  //   - Logger properties
  //   - Detach
with Sender as TFactoryAction do
  begin
  if not FFactory.EnquireForCreateLogger( Properties) then exit;
  Logger := FFactory.Factory.CreateLogger( Properties);
  DisplayName := Logger.MenuDescription;
  FViewModel.AttachSecondaryLogger( Logger);
  if FindMenuByCaption( tlbrMain.ActionClient.Items, 'Options', OptionsMenu) and
     FindMenuByCaption( OptionsMenu, 'Secondary loggers', SecondaryLoggersMenu) then
    begin
    Anchor := SecondaryLoggersMenu.Add;
    Anchor.Caption := DisplayName;
    AddActionMenuItem( Anchor.Items, actEditLoggerProps);
    AddActionMenuItem( Anchor.Items, actDetachLogger   )
    end;
  end;
end;

procedure TmfmGUIRunner.actClearAllExecute( Sender: TObject);
begin
FViewModel.ClearAll
end;

procedure TmfmGUIRunner.actClearExecute( Sender: TObject);
begin
FViewModel.ClearSelections
end;

procedure TmfmGUIRunner.actClearLogExecute( Sender: TObject);
begin
FViewModel.ClearLog
end;

procedure TmfmGUIRunner.actClearUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.CanClearSelections
end;

procedure TmfmGUIRunner.actDetachLoggerExecute( Sender: TObject);
begin
FViewModel.DetachSecondaryLogger( (Sender as TLoggerAction).FLogger);
(Sender as TLoggerAction).FAnchorPoint.Destroy
end;

procedure TmfmGUIRunner.actEditLoggerPropsExecute( Sender: TObject);
begin
with Sender as TLoggerAction do
  FFactory.ViewEditProperties( FLogger, FProps)
end;

procedure TmfmGUIRunner.actHaltOnFirstFailureExecute( Sender: TObject);
begin
FViewModel.DoHaltOnFirstFailure := (Sender as TAction).Checked
end;

procedure TmfmGUIRunner.actHaltOnFirstFailureUpdate( Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.CanToggleHaltOnFirstFailure;
(Sender as TAction).Checked := FViewModel.DoHaltOnFirstFailure
end;

procedure TmfmGUIRunner.actRunExecute( Sender: TObject);
begin
FViewModel.Run
end;

procedure TmfmGUIRunner.actRunUpdate(Sender: TObject);
begin
(Sender as TAction).Enabled := FViewModel.Model.Can_Run
end;

procedure TmfmGUIRunner.actSelectAllExecute( Sender: TObject);
begin
FViewModel.SelectAll
end;

procedure TmfmGUIRunner.actSelectDefaultExecute(Sender: TObject);
begin
FViewModel.SelectDefault
end;

procedure TmfmGUIRunner.actSelectFailedExecute( Sender: TObject);
begin
FViewModel.SelectFailed
end;

procedure TmfmGUIRunner.actSelectFailedUpdate( Sender: TObject);
var
  Can: boolean;
begin
Can := FViewModel.CanSelectFailed;
actSelectFailed.Enabled := Can
end;

procedure TmfmGUIRunner.actToggleSelectionExecute( Sender: TObject);
begin
FViewModel.ToggleSelections
end;

procedure TmfmGUIRunner.appevMainIdle( Sender: TObject; var Done: Boolean);
begin
appevMain.OnIdle := nil;
FViewModel.FirstIdleEvent
end;

//procedure TmfmGUIRunner.CreateDynamicMenuItems( SecondaryLoggers: IAdditionalLoggers);
//var
//  Options, Addend: TActionClientItem;
//  Factory: ILoggerFactoryUI;
//begin
//Options := (tlbrMain.ActionClient as TActionBarItem).Items.Add;
//Options.Caption := 'Options';
//Addend := Options.Items.Add;
//Addend.Action := actHaltOnFirstFailure;
//Addend := Options.Items.Add;
//Addend.Action := actFailOnLeak;
//Addend := Options.Items.Add;
//Addend.Caption := 'Secondary loggers';
//FSecondaryLoggersItem := Addend;
//
//  // For each attached secondary logger, we will also have ...
//  //   - Logger properties
//  //   - Detach
//  //   - Secondary logger log level
//  //    -- Information   (group 2+)
//  //    -- Warning
//  //    -- Error
//
//for Factory in SecondaryLoggers do
//  begin
//  Addend := FSecondaryLoggersItem.Items.Add;
//  Addend.Action := CreateAttachLoggerAction( Factory);
//  end;
//tlbrMain.RecreateControls;
//end;

function TmfmGUIRunner.CreateAttachLoggerAction( const Factory: ILoggerFactoryUI): TAction;
begin
result := TFactoryAction.CreateFactoryAction( actAttachLogger.Owner, Factory);
result.ActionList := actAttachLogger.ActionList;
AssignAction( result, actAttachLogger);
result.Caption  := Format( result.Caption, [Factory.Factory.MenuDescription]);
result.Category := '(dynamic)'
end;

procedure TmfmGUIRunner.FormCreate(Sender: TObject);
begin
memoLog.Clear
end;

procedure TmfmGUIRunner.FormDestroy( Sender: TObject);
begin
FViewModel.FormDestroyed
end;




procedure TConcreteView.Breathe;
begin
if FBreathing then exit;
FBreathing := True;
try
  PostMessage( FForm.Handle, CM_UPDATEACTIONS, 0, 0);
  Application.ProcessMessages;
finally
  FBreathing := False
  end
end;


procedure TConcreteView.ClearLog;
begin
Form.memoLog.Clear
end;

constructor TConcreteView.Create;
begin
FSensitivity := Low( TPutLevel);
FBreathing   := False;
inherited Create
end;

procedure TConcreteView.EnterOperation( isEntering: boolean);
var
  SubList: TCollectionItem;

  procedure AlignActionClientsWithActions( List: TCustomActionList);
  var
    Action: TContainedAction;
    EnableableAction: TCustomAction;
    Enabled: boolean;
  begin
  for Action in List do
    begin
    if not (Action is TCustomAction) then continue;
    EnableableAction := TCustomAction( Action);
    Enabled := EnableableAction.Enabled;
    EnableableAction.Enabled := not Enabled;
    EnableableAction.Enabled :=     Enabled
    end;
  end;

begin
inherited;
if isEntering then
    Form.actmngrMain.State := asSuspended
  else
    begin
    Form.actmngrMain.State := asNormal;
    // This next fragment of code is a work-around for a defect in the
    //  way actions work in D2010.
    AlignActionClientsWithActions( Form.actmngrMain);
    for SubList in Form.actmngrMain.LinkedActionLists do
      AlignActionClientsWithActions( (SubList as TActionListItem).ActionList)
    end
end;

function TConcreteView.Form: TmfmGUIRunner;
begin
result := FForm as TmfmGUIRunner
end;

function TConcreteView.GetProgressMax: integer;
begin
result := Form.pbarTests.Max
end;

function TConcreteView.GetProgressPosition: integer;
begin
result := Form.pbarTests.Position
end;

function TConcreteView.GetShowProgressBar: boolean;
begin
result := Form.pbarTests.Visible
end;

procedure TConcreteView.InitiateView( TestCaseCount: integer);
begin
inherited;
ShowProgressBar := False;
ProgressPosition := 0;
ProgressMax := TestCaseCount
end;

procedure TConcreteView.IntegrateSecondariesIntoMenus;
var
  Gang: IAdditionalLoggers;
  SecondaryLogger: ILoggerFactoryUI;
  OptionsMenu, SecondaryLoggersMenu: TActionClients;
begin
if FExecutive.Services.Acquire( ILoggerFactoryUI, nil, Gang, '', nil) and
   FindMenuByCaption( Form.tlbrMain.ActionClient.Items, 'Options', OptionsMenu) and
   FindMenuByCaption( OptionsMenu, 'Attach new logger', SecondaryLoggersMenu) then
     for SecondaryLogger in Gang do
       with SecondaryLoggersMenu.Add do
         // The Addend type is TActionClientItem
         Action := Form.CreateAttachLoggerAction( SecondaryLogger);

end;








procedure TConcreteView.Put( Level: TPutLevel; const Line: string; const Args: array of const);
var
  Size1: integer;
  Style1: TFontStyles;
  Colour1: TColor;
  OldCharRange, NewCharRange: TCharRange;
begin
Size1 := 10;
Colour1 := clBlack;
if FSensitivity > Level then exit;
case Level of
  lvDebug:      begin
                Size1   := 8;
                Style1  := [];
                Colour1 := clDkGray
                end;

  lvNormal:     begin
                Size1   := 10;
                Style1  := [];
                Colour1 := clBlack
                end;

  lvHighLight:  begin
                Size1   := 12;
                Style1  := [fsBold];
                Colour1 := clRed
                end;
  end;
with Form.memoLog do
  begin
  SendMessage( Handle, EM_EXGETSEL, 0, LParam( @OldCharRange));
  try
    NewCharRange.cpMin := Form.memoLog.GetTextLen;
    NewCharRange.cpMax := NewCharRange.cpMin;
    SendMessage( Handle, EM_EXSETSEL, 0, LParam( @NewCharRange));
    SelAttributes.Size  := Size1;
    SelAttributes.Style := Style1;
    SelAttributes.Color := Colour1;
    SelText := Format( Line, Args) + #13#10
  finally
    if not doAutoScroll then
      SendMessage( Handle, EM_EXSETSEL, 0, LParam( @OldCharRange))
    end
  end
end;

procedure TConcreteView.SetDisplayState( Value: TSuiteRunnerState);
const
  StateStrings: array[ TSuiteRunnerState] of string = (
  // rsIdle, rsSettingUp, rsExecuting, rsTearingDown, rsBetweenTests
     'Idle', 'Setting up', 'Executing', 'Tearing down', 'In between');
begin
inherited SetDisplayState( Value);
Form.sbarMain.Panels[0].Text := StateStrings[ Value]
end;

procedure TConcreteView.SetProgressMax( Value: integer);
var
  s: string;
begin
Form.pbarTests.Max := Value;
if ShowProgressBar then
    s := Format( 'of %d', [Value])
  else
    s := '';
Form.sbarMain.Panels[2].Text := s
end;

procedure TConcreteView.SetProgressPosition( Value: integer);
var
  s: string;
begin
Form.pbarTests.Position := Value;
if ShowProgressBar then
    s := Format( 'Test cases = %d', [Value])
  else
    s := '';
Form.sbarMain.Panels[1].Text := s
end;

procedure TConcreteView.SetShowProgressBar( Value: boolean);
begin
Form.pbarTests.Visible := Value
end;

function TConcreteView.TreeName: string;
begin
result := 'treeTestSuite'
end;

function TConcreteView.TreeOwner: TComponent;
begin
result := FForm
end;

function TConcreteView.TreeParent: TWinControl;
begin
result :=(FForm as TmfmGUIRunner).pnlTestCases
end;

{ TFactoryAction }

constructor TFactoryAction.CreateFactoryAction(
  AOwner: TComponent; const Factory: ILoggerFactoryUI);
begin
inherited Create( AOwner);
FFactory := Factory
end;

{ TLoggerAction }

constructor TLoggerAction.CreateLoggerAction(
  AOwner: TComponent; const Factory1: ILoggerFactoryUI; const Logger: ITestLogger;
  const Props: IInterface; AnchorPoint: TActionClientItem);
begin
inherited Create( AOwner);
FFactory     := Factory1;
FLogger      := Logger;
FProps       := Props;
FAnchorPoint := AnchorPoint
end;

end.
