unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Vcl.Graphics, Vcl.Forms,
  Vcl.Dialogs, IniFiles, Clipbrd, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.StdCtrls, System.Classes, Vcl.Controls, IOutils, StrUtils;

type
  TForm1 = class(TForm)
    fod: TFileOpenDialog;
    TrayIcon1: TTrayIcon;
    Panel1: TPanel;
    PathOut: TLabeledEdit;
    EditSuffix: TLabeledEdit;
    procedure EditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PathOutDblClick(Sender: TObject);
    procedure SaveClipBrdJPG(const Folder: String);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
  private
    { Private declarations }
    Ini: TIniFile;
    LastBmp: TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.WMClipboardUpdate(var Msg: TMessage);
begin
  SaveClipBrdJPG(PathOut.Text);
end;


procedure TForm1.WMSysCommand(var Msg: TWMSysCommand);
begin
  case Msg.CmdType of
    SC_MINIMIZE: Hide;
  end;
  inherited;
end;


function IsSameBitmap(Bitmap1, Bitmap2: TBitmap): Boolean;
var
 Stream1, Stream2: TMemoryStream;
begin
  Assert((Bitmap1 <> nil) and (Bitmap2 <> nil), 'Params can''t be nil');
  Result:= False;
  if (Bitmap1.Height <> Bitmap2.Height) or (Bitmap1.Width <> Bitmap2.Width) then
     Exit;
  Stream1:= TMemoryStream.Create;
  try
    Bitmap1.SaveToStream(Stream1);
    Stream2:= TMemoryStream.Create;
    try
      Bitmap2.SaveToStream(Stream2);
      if Stream1.Size = Stream2.Size Then
        Result:= CompareMem(Stream1.Memory, Stream2.Memory, Stream1.Size);
    finally
      Stream2.Free;
    end;
  finally
    Stream1.Free;
  end;
end;


function ForceDirectories(FullPath: string): Boolean;   // Works with UNC paths
begin
  TDirectory.CreateDirectory(FullPath);
  Result:= DirectoryExists(FullPath);
end;


procedure TForm1.SaveClipBrdJPG(const Folder: string);
var
  Bmp: TBitmap;
  PNGImage: TPngImage;
  FileName: String;
  ClipBoardHasPicture: Boolean;
  {$IFDEF DEBUG}
    i: integer;
    s: string;
  {$ENDIF}
begin
  if Not DirectoryExists(Folder) then begin
    ForceDirectories(Folder);
  end;

  ClipBoardHasPicture := False;
  Try
    ClipBoardHasPicture := Clipboard.HasFormat(CF_PICTURE)  // 49564
                   And Not Clipboard.HasFormat(CF_UNICODETEXT);  // 13
    except
//on E : EClipboardException do begin
//      'Cannot open clipboard: Access is denied'
//    end;
  end;

  {$IFDEF DEBUG}
  OutputDebugString(PChar('ClipBoardHasPicture := ' +
                    IfThen(ClipBoardHasPicture,'True','False')));
  for i := 0 to clipboard.FormatCount do begin
    s := s + inttostr(clipboard.Formats[i]) + ', ';
  end;
  s := Copy(s, 0, length(s) - 2);
  OutputDebugString(PChar(s));
  {$ENDIF}

  if ClipBoardHasPicture then begin
    FileName := Folder + '\' + FormatDateTime('yyyy-mm-dd hhnnss.zzz', Now) +
                IfThen(EditSuffix.GetTextLen > 0, ' ', '') + EditSuffix.Text;
    Bmp := TBitmap.Create;
    PNGImage := TPngImage.Create;
    try
      // TODO: Handle EInvalidGraphic 'Unsupported clipboard format'
      Bmp.Assign(Clipboard);
      if Not IsSameBitmap(Bmp, LastBmp) then begin
        LastBmp.Assign(Bmp);
        PNGImage.Assign(Bmp);
        PNGImage.SaveToFile(FileName + '.png');
      end;
    finally
      PNGImage.Free;
      Bmp.Free;
    end;
  end;
end;

procedure TForm1.TrayIcon1DblClick(Sender: TObject);
begin
  if Form1.Visible then Hide
  else begin
    Show;
    Perform(WM_SYSCOMMAND, SC_RESTORE, 0);
  end;
end;

procedure TForm1.PathOutDblClick(Sender: TObject);
begin
  if fod.Execute then PathOut.Text := fod.FileName;
end;

procedure TForm1.EditChange(Sender: TObject);
var
  target: TLabeledEdit;
begin
  target := (Sender As TLabeledEdit);
  Ini.WriteString('Settings', Target.Name, Target.Text);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  target: TLabeledEdit;
begin
  AddClipboardFormatListener(Handle);
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  for i := 0 to Form1.ComponentCount - 1 do
    if form1.Components[i] is TLabeledEdit then begin
      target := (form1.Components[i] As TLabeledEdit);
      target.Text := Ini.ReadString('Settings', target.Name, target.Text);
    end;
  LastBmp := TBitmap.Create;
end;

procedure TForm1.FormDblClick(Sender: TObject);
begin
  Form1.Hide;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  RemoveClipboardFormatListener(Handle);
  Ini.Free;
  LastBmp.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  PathOut.Width := Panel1.ClientWidth - 16;
end;

end.
