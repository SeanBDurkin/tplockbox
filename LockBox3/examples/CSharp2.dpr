program CSharp2;

uses
  Vcl.Forms,
  umfmCSharp2Demo in 'umfmCSharp2Demo.pas' {mfmCSharp2Demo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmCSharp2Demo, mfmCSharp2Demo);
  Application.Run;
end.
