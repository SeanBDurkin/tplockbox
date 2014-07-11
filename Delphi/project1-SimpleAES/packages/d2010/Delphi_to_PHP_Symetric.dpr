program Delphi_to_PHP_Symetric;

uses
  Forms,
  umfmDelphi_to_PHP_Symetric in '..\..\run\umfmDelphi_to_PHP_Symetric.pas' {mfmDelphi_to_PHP_Symetric};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TmfmDelphi_to_PHP_Symetric, mfmDelphi_to_PHP_Symetric);
  Application.Run;
end.
