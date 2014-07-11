// ***** BEGIN LICENSE BLOCK *****
// * Version: MPL 1.1
// *
// * The contents of this file are subject to the Mozilla Public License Version
// * 1.1 (the "License"); you may not use this file except in compliance with
// * the License. You may obtain a copy of the License at
// * http://www.mozilla.org/MPL/
// *
// * Software distributed under the License is distributed on an "AS IS" basis,
// * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
// * for the specific language governing rights and limitations under the
// * License.
// *
// * The Original Code is TurboPower LockBox
// *
// * The Initial Developer of the Original Code is
// * TurboPower Software
// *
// * Portions created by the Initial Developer are Copyright (C) 1997-2002
// * the Initial Developer. All Rights Reserved.
// *
// * Contributor(s):
// *
// * ***** END LICENSE BLOCK *****
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("L207_r41.res");
USEPACKAGE("vcl40.bpi");
USEUNIT("..\source\LbUtils.pas");
USEUNIT("..\source\LbBigInt.pas");
USEUNIT("..\source\LBCIPHER.PAS");
USEUNIT("..\source\LbClass.pas");
USEUNIT("..\source\LbConst.pas");
USEUNIT("..\source\LbDSA.pas");
USEUNIT("..\source\LBPROC.PAS");
USEUNIT("..\source\LbRandom.pas");
USEUNIT("..\source\LbRSA.pas");
USEUNIT("..\source\LbString.pas");
USEUNIT("..\source\LbAsym.pas");
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return 1;
}
//---------------------------------------------------------------------------
