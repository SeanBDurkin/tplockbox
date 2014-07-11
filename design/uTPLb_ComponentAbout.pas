{* ***** BEGIN LICENSE BLOCK *****
Copyright 2009 Sean B. Durkin

This file is part of TurboPower LockBox.
TurboPower LockBox is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

TurboPower LockBox is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the Lesser GNU General Public License
along with TurboPower LockBox.  If not, see <http://www.gnu.org/licenses/>.

The Initial Developer of the Original Code for TurboPower LockBox version 2
and earlier was TurboPower Software.

 * ***** END LICENSE BLOCK ***** *}
// Copyright 2009 Sean B. Durkin
unit uTPLb_ComponentAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, OleCtrls, SHDocVw, ComCtrls, jpeg;

type
  TTPLb_fmComponentAbout = class(TForm)
    pnlAbout: TPanel;
    btnClose: TButton;
    pgAboutContent: TPageControl;
    tsMain: TTabSheet;
    tsWelcome: TTabSheet;
    tsProject: TTabSheet;
    tsCopyLeft: TTabSheet;
    tsAuthors: TTabSheet;
    tsSupport: TTabSheet;
    tsAlgorithms: TTabSheet;
    Image1: TImage;
    lblTitle: TLabel;
    Label1: TLabel;
    lblRunTimeVersion: TLabel;
    lblDesignTimeVersion: TLabel;
    Memo1: TMemo;
    WebBrowser1: TWebBrowser;
    Label4: TLabel;
    Memo2: TMemo;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo3: TMemo;
    Label9: TLabel;
    Memo4: TMemo;
    ListBox1: TListBox;
    Image2: TImage;
    Bevel1: TBevel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    memoSupport: TMemo;
    memoHelp: TMemo;
    Panel1: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    ListBox2: TListBox;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Panel2: TPanel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    ListBox3: TListBox;
    Panel3: TPanel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label31: TLabel;
    ListBox4: TListBox;
    lblLogoAttribution: TLabel;
    lblTpsfaIntro: TStaticText;
    lblTpsfaContact: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    procedure lblTpsfaContactClick(Sender: TObject);
  private
    { Private declarations }

  public
    procedure UpdateAbout( SelectedComponent: TComponent);
  end;

var
  TPLb_fmComponentAbout: TTPLb_fmComponentAbout;

implementation













uses uTPLb_InfoUtils;
{$R *.dfm}
{
Content Plan
============
Page 1: Main
0. Big Title: TurboPower LockBox 3
2. Product definition statement.
3. Logo or nice graphic for LockBox 3
7. run-time PE version number
8. design-time PE version
4. Brief statement about FOSS: LGPL

Page 2: Welcome message
1. Embedded video by me introducing the component suite.

Page 3: Brought to you by ...
Statement about project organisation and control (tpfsa)
5. tpfsa email
6. sourceforge url

Page 4: CopyLeft
12. Copyright  (mark and statement of application)
10. Statement of copying permission
9.  LGPL 3 logo as clickable link
11. LGPL and GPL full wording

Page 5: Authors
13. About authors. List -
  13.1 Name
  13.2 Photo
  13.3 Email
14 Statement welcoming contributions and adding to the list of authors.

Page 6: Support
15 Statement about official support.
16 Statement about wiki documentation.

Page 7: Algorithms
17 About Selected Hash -
  17.1 DisplayName
  17.2 Feature list
  17.3 Definition URL
  17.4 Wikipedia URL
  17.5 Block size
  17.6 Digest size
18 About Selected Chaining Mode -
  DisplayName
  Feature list
  Definition URL
  Wikipedia URL
19 About Selected Block Cipher -
  DisplayName
  Feature list
  Definition URL
  Wikipedia URL
  Block size

}
{ TTPLb_fmComponentAbout }

procedure TTPLb_fmComponentAbout.lblTpsfaContactClick(Sender: TObject);
begin
//
end;

procedure TTPLb_fmComponentAbout.UpdateAbout( SelectedComponent: TComponent);
var
  LibName: string;
  FileVersion: string;
begin
Get_TP_LockBox3_Info( LibName, FileVersion);
lblRunTimeVersion.Caption := Format( 'Run-time package %s is version %s .',
  [LibName, FileVersion]);
Get_dclTP_LockBox3_Info( LibName, FileVersion);
lblDesignTimeVersion.Caption := Format( 'Design-time package %s is version %s .',
  [LibName, FileVersion]);
end;

end.
