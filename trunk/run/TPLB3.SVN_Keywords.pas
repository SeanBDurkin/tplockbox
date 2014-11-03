{$I TPLB3.Common.inc}

unit TPLB3.SVN_Keywords;
interface
const
  TPLB3Runtime_SVN_Keyword_Date    : string = '$Date: ? $';
  TPLB3Runtime_SVN_Keyword_Revision: string = '$Revision: ? $';
  TPLB3Runtime_SVN_Keyword_Author  : string = '$Author: ? $';
  TPLB3Runtime_SVN_Keyword_HeadURL : string = '$HeadURL: ? $';
  TPLB3Runtime_SVN_Keyword_Id      : string = '$Id: TPLB3.SVN_Keywords.pas ? ? ? $';


function TPLB3Runtime_SVN_Revision: integer;


implementation

{$WARNINGS OFF}
function TPLB3Runtime_SVN_Revision: integer;
var
  s, Pattern: string;
  P, Code: integer;
begin
s := TPLB3Runtime_SVN_Keyword_Revision;
Pattern := '$Revision: ';
P := Pos( Pattern, s);
if P > 0 then
  Delete( s, P, Length( Pattern));
Pattern := ' ';
P := Pos( Pattern, s);
if P > 0 then
  SetLength( s, P-1);
Val( s, result, Code);
if (s = '') or (Code <> 0) then
  result := -1 // Signifying unknown version
end;
{$WARNINGS ON}

{ Here is a scratch pad area for forcing changes to file in SVN.

}
end.
