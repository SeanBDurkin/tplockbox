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

unit DUnitM.TestSuiteDxVirtualTree;
interface
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DUnitM.VirtualTrees, DUnitM.ViewModel_Tree, DUnitM.UnitTestFramework,
  ImgList, DUnitM.dmVirtualTreeNonVisualSupport, SBD.ServiceProvider;

type
  TTestSuiteVirtualTreeObj = class;
  TTestSuiteVirtualTree = class(TFrame)
    procedure TreeFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure TreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure TreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure TreeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure TreeGetHint(
      Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

  public
    Tree:TVirtualStringTree;
    procedure ConstructVirtualTree;
  private
    FObj:  TTestSuiteVirtualTreeObj;
    FDataModule: TdmVirtualTreeNonVisualSupport;
  private const
    KindImages : array[ TVisualTestSuiteNodeKind ] of integer = (
      // nkNeutral, nkTestCase, nkTestProc, nkTestFixture, nkGroup
                -1,          0,         10,             1,       1);
    StateImages: array[ TTestStatus] of integer = (
      // tsNeutral, tsSetup, tsTeardown, tsExecuting, tsPass, tsWarn, tsFail, tsError, tsSkipped
                -1,       5,          6,           7,      2,      3,      4,       9,        -1);
   end;

  TTestSuiteVirtualTreeObj = class( TInterfacedObject, IVisualTestSuiteTree)
  private
    FFrame: TTestSuiteVirtualTree;
    FTree: TVirtualStringTree;
    FPopulating: boolean;
    function  FactoryDisplayName: string;
    function  Nodes( const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
    function  AllNodes: IEnumerable<IVisualTestSuiteNode>;
    function  Change( const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
    procedure InvalidateRendering( const DeltaNode: IVisualTestSuiteNode);
    procedure BeforePopulate;
    procedure AfterPopulate;
    procedure SetChecked( const Node: IVisualTestSuiteNode; Value: boolean; Source: TSetCheckedSource);

  private type
    TChangeContext = class( TInterfacedObject, IVisualTestSuiteTreeChangeContext)
      private
        FTreeObj: TTestSuiteVirtualTreeObj;
        FParent: IVisualTestSuiteNode;
        FParentV: PVirtualNode;
        function  ChildNodes: IEnumerable<IVisualTestSuiteNode>;
        function  Insert( const Sibling: IVisualTestSuiteNode; Position: TInserPosition; AddCount: integer): IVisualTestSuiteNode;
        function  Append( AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
        procedure AttachRenderer( const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
        procedure Delete( const Victim: IVisualTestSuiteNode);
      public
        constructor Create( TreeObj1: TTestSuiteVirtualTreeObj; const Parent1: IVisualTestSuiteNode);
        destructor Destroy; override;
      end;

  public
    constructor CreateFromFactory( AOwner: TComponent; AParent: TWinControl; const AName: string);
    destructor Destroy; override;
  end;


procedure RegisterServices( const Provider: IServiceProvider);

implementation






{$R *.dfm}

type
RNodeDatum = record
    FToken: IVisualTestSuiteNode;
    FRenderer: INodeRenderer;
  end;
PNodeDatum = ^RNodeDatum;

IVisualTestSuiteNodeEx = interface( IVisualTestSuiteNode)
  ['{AACA02D2-26B9-41F4-8C15-6AA2ECEA90F3}']
    function GetNode: PVirtualNode;
  end;

TVisualTestSuiteNode = class( TInterfacedObject, IVisualTestSuiteNode, IVisualTestSuiteNodeEx)
  private
    FDatum: pointer;
    FNode: PVirtualNode;
    function  GetDatum: pointer;
    procedure SetDatum( Value: pointer);
    function  GetNode: PVirtualNode;
  public
    constructor Create( Node1: PVirtualNode);
  end;

TTestSuiteNodeList = class( TInterfacedObject, IEnumerable, IEnumerable<IVisualTestSuiteNode>)
  private
    FTree:TVirtualStringTree;
    FParentV, FStartV: PVirtualNode;
    FAddCount: integer;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
    function IEnumerable<IVisualTestSuiteNode>.GetEnumerator = GetEnumeratorIntf;
  public
    constructor Create( Tree1:TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
  end;

TTestSuiteNodeCursor = class( TInterfacedObject, IEnumerator, IEnumerator<IVisualTestSuiteNode>)
  private
    FTree:TVirtualStringTree;
    FParentV, FStartV: PVirtualNode;
    FAddCount: integer;
    FCurrentV: PVirtualNode;
    FIndex   : integer;

    function  GetCurrent: TObject;
    function  MoveNext: Boolean;
    procedure Reset;
    function  GetCurrentIntf: IVisualTestSuiteNode;
    function  IEnumerator<IVisualTestSuiteNode>.GetCurrent = GetCurrentIntf;
  public
    constructor Create( Tree1:TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
  end;


constructor TTestSuiteVirtualTreeObj.CreateFromFactory( AOwner: TComponent; AParent: TWinControl; const AName: string);
var
  Owner: TComponent;
  Parent: TWinControl;
begin
FPopulating := False;
FFrame := TTestSuiteVirtualTree.Create( nil);
FFrame.ConstructVirtualTree;
FFrame.Loaded;
FFrame.FObj := self;
FTree := FFrame.Tree;
FFrame.RemoveComponent( FTree);
FTree.NodeDataSize := SizeOf( RNodeDatum);
Owner := AOwner;
Parent := AParent;
FTree.Name := AName;
if assigned( Owner) and (Owner is TWinControl) and (not assigned( Parent)) then
  Parent := Owner as TWinControl;
if assigned( Parent) and (not assigned( Owner)) then
  Owner := Parent;
if assigned( Owner) then
  Owner.InsertComponent( FTree);
if assigned( Parent) then
  FTree.Parent := Parent
end;

destructor TTestSuiteVirtualTreeObj.Destroy;
begin
FFrame.Free;
inherited
end;

function TTestSuiteVirtualTreeObj.Change(
  const Parent: IVisualTestSuiteNode): IVisualTestSuiteTreeChangeContext;
begin
result := TChangeContext.Create( self, Parent)
end;

function TTestSuiteVirtualTreeObj.FactoryDisplayName: string;
begin
result := FTree.ClassName
end;

procedure TTestSuiteVirtualTreeObj.InvalidateRendering(
  const DeltaNode: IVisualTestSuiteNode);
begin
FTree.InvalidateNode( (DeltaNode as IVisualTestSuiteNodeEx).GetNode)
end;

type
TVisualTestSuiteTreeFactory = class( TInterfacedObject, IVisualTestSuiteTreeFactory)
  private
    function MakeVisualTestSuiteTree( AOwner: TComponent; AParent: TWinControl; const AName: string): IVisualTestSuiteTree;
  public
    [Configuration] constructor ServiceModeCreate;
  end;

procedure RegisterServices( const Provider: IServiceProvider);
begin
Provider.RegisterServiceClass( IVisualTestSuiteTreeFactory, TVisualTestSuiteTreeFactory)
end;

function TVisualTestSuiteTreeFactory.MakeVisualTestSuiteTree( AOwner: TComponent; AParent: TWinControl; const AName: string): IVisualTestSuiteTree;
begin
result := TTestSuiteVirtualTreeObj.CreateFromFactory( AOwner, AParent, AName)
end;

function TTestSuiteVirtualTreeObj.Nodes(
  const Parent: IVisualTestSuiteNode): IEnumerable<IVisualTestSuiteNode>;
var
  ParentV: PVirtualNode;
begin
if assigned( Parent) then
    ParentV := (Parent as IVisualTestSuiteNodeEx).GetNode
  else
    ParentV := nil;
result := TTestSuiteNodeList.Create( FTree, ParentV, nil, MaxInt)
end;

type
TTestSuiteAllNodes = class( TInterfacedObject, IEnumerable<IVisualTestSuiteNode>)
  private
    FNodesRec: TVTVirtualNodeEnumeration;
    FTree: TBaseVirtualTree;
    function GetEnumerator: IEnumerator;
    function GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
    function IEnumerable<IVisualTestSuiteNode>.GetEnumerator = GetEnumeratorIntf;

  private type
    TTestSuiteAllNodesCursor = class( TInterfacedObject, IEnumerator, IEnumerator<IVisualTestSuiteNode>)
      private
        FEnumaratorRec: TVTVirtualNodeEnumerator;
        FTree: TBaseVirtualTree;
        function  GetCurrent: TObject;
        function  MoveNext: Boolean;
        procedure Reset;
        function  GetCurrentIntf: IVisualTestSuiteNode;
        function  IEnumerator<IVisualTestSuiteNode>.GetCurrent = GetCurrentIntf;
      public
        constructor Create( Tree1: TBaseVirtualTree; Enum: TVTVirtualNodeEnumerator);
      end;

  public
    constructor Create( Tree1: TBaseVirtualTree; Nodes: TVTVirtualNodeEnumeration);
  end;

function TTestSuiteVirtualTreeObj.AllNodes: IEnumerable<IVisualTestSuiteNode>;
begin
result := TTestSuiteAllNodes.Create( FTree, FTree.Nodes(False))
end;



procedure TTestSuiteVirtualTreeObj.SetChecked(
  const Node: IVisualTestSuiteNode;
  Value: boolean; Source: TSetCheckedSource);
var
  InitialCheckState: TCheckState;
  NextCheckState: TCheckState;
  DemandedCheckState: TCheckState;
begin
InitialCheckState := FTree.CheckState[ (Node as IVisualTestSuiteNodeEx).GetNode];
case Value of
  False: DemandedCheckState := csUncheckedNormal;
  True : DemandedCheckState := csCheckedNormal;
  end;
case Source of
  csPopulation, csUser  :
    NextCheckState := DemandedCheckState;
  csPostPopulate:
    begin
    case InitialCheckState of
      csMixedNormal, csMixedPressed: NextCheckState := InitialCheckState;
      else                           NextCheckState := DemandedCheckState;
      end
    end;
end;
if NextCheckState <> InitialCheckState then
  FTree.CheckState[ (Node as IVisualTestSuiteNodeEx).GetNode] := NextCheckState
end;

constructor TTestSuiteVirtualTreeObj.TChangeContext.Create(
  TreeObj1: TTestSuiteVirtualTreeObj; const Parent1: IVisualTestSuiteNode);
begin
FTreeObj := TreeObj1;
FParent  := Parent1;
FTreeObj.FTree.BeginUpdate;
if assigned( FParent) then
    FParentV := (FParent as IVisualTestSuiteNodeEx).GetNode
  else
    FParentV := nil
end;

destructor TTestSuiteVirtualTreeObj.TChangeContext.Destroy;
begin
FTreeObj.FTree.EndUpdate;
inherited
end;

function TTestSuiteVirtualTreeObj.TChangeContext.Append(
  AddCount: integer): IEnumerable<IVisualTestSuiteNode>;
var
  StartV: PVirtualNode;
begin
with FTreeObj.FTree do
  begin
  StartV := GetLastChildNoInit( FParentV);
  ChildCount[ FParentV] := ChildCount[ FParentV] + AddCount
  end;
if assigned( FParentV) then
  Include( FParentV.States, vsHasChildren);
result := TTestSuiteNodeList.Create( FTreeObj.FTree, FParentV, StartV, AddCount)
end;

procedure TTestSuiteVirtualTreeObj.TChangeContext.AttachRenderer(
  const Newbie: IVisualTestSuiteNode; const Renderer: INodeRenderer);
var
  DatumRec: PNodeDatum;
begin
DatumRec := FTreeObj.FTree.GetNodeData( (Newbie as IVisualTestSuiteNodeEx).GetNode);
DatumRec^.FRenderer := Renderer;
if assigned( Renderer) then
  Renderer.Attached( Newbie)
end;

function TTestSuiteVirtualTreeObj.TChangeContext.ChildNodes: IEnumerable<IVisualTestSuiteNode>;
begin
result := TTestSuiteNodeList.Create( FTreeObj.FTree, FParentV, nil, MaxInt)
end;

procedure TTestSuiteVirtualTreeObj.TChangeContext.Delete(
  const Victim: IVisualTestSuiteNode);
var
  Node: PVirtualNode;
begin
Node := (Victim as IVisualTestSuiteNodeEx).GetNode;
FTreeObj.FTree.DeleteNode( Node)
end;


function TTestSuiteVirtualTreeObj.TChangeContext.Insert(
  const Sibling: IVisualTestSuiteNode; Position: TInserPosition;
  AddCount: integer): IVisualTestSuiteNode;
var
  DatumRec: PNodeDatum;
  Node, Base: PVirtualNode;
  Mode: TVTNodeAttachMode;
  Idx: Integer;
begin
result := nil;
Base := (Sibling as IVisualTestSuiteNodeEx).GetNode;
FTreeObj.FTree.DeleteNode( Node);
case Position of
  iBefore: Mode := amInsertBefore;
  iAfter : Mode := amInsertAfter;
  end;
for Idx := 1 to AddCount do
  begin
  FTreeObj.FTree.InsertNode( Base, Mode);
  DatumRec := FTreeObj.FTree.GetNodeData( Node);
  Include( Node.States, vsInitialUserData);
  Node.CheckType := ctTriStateCheckBox;
  DatumRec^.FToken := TVisualTestSuiteNode.Create( Node);
  if not assigned( result) then
    // Only return the first one.
    result := DatumRec^.FToken;
  Base := Node
  end
end;

procedure TTestSuiteVirtualTreeObj.BeforePopulate;
begin
FPopulating := True;
FTree.BeginUpdate;
FTree.Clear
end;

procedure TTestSuiteVirtualTreeObj.AfterPopulate;
var
  Node: PVirtualNode;
begin
for Node in FTree.Nodes do
  FTree.Expanded[ Node] := True;
FTree.EndUpdate;
FPopulating := False
end;

procedure TTestSuiteVirtualTree.ConstructVirtualTree;
// We are not linking with the dfm.
// All properties are set at run-time via this method.
begin
if assigned( Tree) then exit;
FDataModule := TdmVirtualTreeNonVisualSupport.Create( Self);
Tree := TVirtualStringTree.Create( self);
with Tree do
  begin
  Left := 0;
  Top := 0;
  Width := 455;
  Height := 240;
  Align := alClient;
  Header.AutoSizeIndex := 0;
  Header.Font.Charset := DEFAULT_CHARSET;
  Header.Font.Color := clWindowText;
  Header.Font.Height := -11;
  Header.Font.Name := 'Tahoma';
  Header.Font.Style := [];
  Images      := FDataModule.imglstStates16x16;
  StateImages := FDataModule.imglstStates16x16;
  TabOrder := 0;
  ShowHint := True;
  HintMode := hmHint;
  TreeOptions.MiscOptions := [toCheckSupport, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick];
  TreeOptions.PaintOptions := [toHideFocusRect, toHideSelection, toShowButtons, toShowDropmark, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages];
  TreeOptions.SelectionOptions := [toFullRowSelect];
  OnChecked := TreeChecked;
  OnFreeNode := TreeFreeNode;
  OnGetText := TreeGetText;
  OnGetImageIndex := TreeGetImageIndex;
  OnInitNode := TreeInitNode;
  OnGetHint  := TreeGetHint;
  with Header.Columns.Add do
    begin
    MinWidth := 200;
    Position := 0;
    Width := 300;
    Text := 'Test case'
    end;
  with Header.Columns.Add do
    begin
    Position := 1;
    Text := 'Run count'
    end
  end
end;

procedure TTestSuiteVirtualTree.TreeChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  DatumRec: PNodeDatum;
begin
if FObj.FPopulating then exit;
DatumRec := Sender.GetNodeData( Node);
if assigned( DatumRec^.FRenderer) then
  DatumRec^.FRenderer.SetChecked( Node^.CheckState in
   [csCheckedNormal, csCheckedPressed, csMixedNormal, csMixedPressed], csUser)
end;

procedure TTestSuiteVirtualTree.TreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
begin
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if assigned( Rndr) then
  Rndr.Detach;
Finalize( DatumRec^)
end;

procedure TTestSuiteVirtualTree.TreeGetHint(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
begin
DatumRec := Sender.GetNodeData( Node);
if assigned( DatumRec) then
  Rndr := DatumRec^.FRenderer;
if assigned( Rndr) then
  HintText := Rndr.Hint
end;

procedure TTestSuiteVirtualTree.TreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
begin
if Column <> 0 then exit;
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if not assigned( Rndr) then exit;
case Kind of
  ikNormal,
  ikSelected: ImageIndex := KindImages [ Rndr.GetKind ];
  ikState:    ImageIndex := StateImages[ Rndr.GetState];
  ikOverlay:  ;
  end;
end;

procedure TTestSuiteVirtualTree.TreeGetText(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  DatumRec: PNodeDatum;
  Rndr: INodeRenderer;
  Count: integer;
begin
if TextType <> ttNormal then exit;
DatumRec := Sender.GetNodeData( Node);
Rndr := DatumRec^.FRenderer;
if not assigned( Rndr) then exit;
case Column of
  0: CellText := Rndr.GetDisplayName;
  1: begin
     Count := Rndr.GetFullCycleCount;
     if Count = -1 then
         CellText := ''
       else
         CellText := Format( '%d / %d', [Rndr.GetDoneCycleCount, Count])
     end;
  end;
end;

procedure TTestSuiteVirtualTree.TreeInitNode( Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
end;

{ TVisualTestSuiteNode }

constructor TVisualTestSuiteNode.Create( Node1: PVirtualNode);
begin
FNode := Node1;
FDatum := nil
end;

function TVisualTestSuiteNode.GetDatum: pointer;
begin
result := FDatum
end;

function TVisualTestSuiteNode.GetNode: PVirtualNode;
begin
result := FNode
end;

procedure TVisualTestSuiteNode.SetDatum( Value: pointer);
begin
FDatum := Value
end;


constructor TTestSuiteNodeList.Create(
  Tree1: TVirtualStringTree; ParentV1, StartV1: PVirtualNode; AddCount1: integer);
begin
FTree     := Tree1;
FParentV  := ParentV1;
FStartV   := StartV1;
FAddCount := AddCount1
end;

function TTestSuiteNodeList.GetEnumerator: IEnumerator;
begin
result := nil
end;

function TTestSuiteNodeList.GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
begin
result := TTestSuiteNodeCursor.Create( FTree, FParentV, FStartV, FAddCount)
end;

{ TTestSuiteNodeCursor }

constructor TTestSuiteNodeCursor.Create(Tree1: TVirtualStringTree; ParentV1,
  StartV1: PVirtualNode; AddCount1: integer);
begin
FTree     := Tree1;
FParentV  := ParentV1;
FStartV   := StartV1;
FAddCount := AddCount1;
Reset
end;

procedure TTestSuiteNodeCursor.Reset;
begin
FCurrentV := FStartV;
FIndex    := 0
end;

function TTestSuiteNodeCursor.GetCurrent: TObject;
begin
result := nil
end;

function TTestSuiteNodeCursor.GetCurrentIntf: IVisualTestSuiteNode;
var
  DatumRec: PNodeDatum;
begin
DatumRec := FTree.GetNodeData( FCurrentV);
result := DatumRec.FToken;
if assigned( result) then exit;
Include( FCurrentV.States, vsInitialUserData);
FCurrentV.CheckType := ctTriStateCheckBox;
result := TVisualTestSuiteNode.Create( FCurrentV);
DatumRec.FToken := result
end;

function TTestSuiteNodeCursor.MoveNext: boolean;
var
  NextV: PVirtualNode;
begin
if assigned( FCurrentV) then
    NextV := FCurrentV^.NextSibling
  else if (not assigned( FStartV)) and assigned( FParentV) then
    NextV := FParentV.FirstChild
  else if not assigned( FStartV) then
    NextV := FTree.GetFirstNoInit
  else
    NextV := nil;
result := assigned( NextV) and (FIndex < FAddCount);
if not result then exit;
FCurrentV := NextV;
Inc( FIndex)
end;


{ TTestSuiteAllNodes }

constructor TTestSuiteAllNodes.Create( Tree1: TBaseVirtualTree; Nodes: TVTVirtualNodeEnumeration);
begin
FTree     := Tree1;
FNodesRec := Nodes
end;

function TTestSuiteAllNodes.GetEnumerator: IEnumerator;
begin
result := nil
end;

function TTestSuiteAllNodes.GetEnumeratorIntf: IEnumerator<IVisualTestSuiteNode>;
begin
result := TTestSuiteAllNodesCursor.Create( FTree, FNodesRec.GetEnumerator)
end;

{ TTestSuiteAllNodes.TTestSuiteAllNodesCursor }

constructor TTestSuiteAllNodes.TTestSuiteAllNodesCursor.Create( Tree1: TBaseVirtualTree; Enum: TVTVirtualNodeEnumerator);
begin
FTree          := Tree1;
FEnumaratorRec := Enum
end;

function TTestSuiteAllNodes.TTestSuiteAllNodesCursor.GetCurrent: TObject;
begin
result := nil
end;

function TTestSuiteAllNodes.TTestSuiteAllNodesCursor.GetCurrentIntf: IVisualTestSuiteNode;
var
  DatumRec: PNodeDatum;
begin
DatumRec := FTree.GetNodeData( FEnumaratorRec.Current);
result   := DatumRec^.FToken
end;

function TTestSuiteAllNodes.TTestSuiteAllNodesCursor.MoveNext: Boolean;
begin
result := FEnumaratorRec.MoveNext
end;

procedure TTestSuiteAllNodes.TTestSuiteAllNodesCursor.Reset;
begin
end;

constructor TVisualTestSuiteTreeFactory.ServiceModeCreate;
begin
end;

end.
