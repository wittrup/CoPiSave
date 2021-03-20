program CoPiSave;

{ Reduce EXE size by disabling as much of RTTI as possible (delphi 2009/2010) }
{$IF CompilerVersion >= 21.0}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

// This will strip the relocation information which is not needed in a .exe.
// Don't add this to a DLL or package!
{$SetPEFlags 1}

uses
  Vcl.Forms,
  main in 'main.pas' {Form1};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.ShowMainForm := False;
  Application.Run;
end.
