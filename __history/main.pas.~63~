unit main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Vcl.Graphics, Vcl.Forms,
  Vcl.Dialogs, IniFiles, Clipbrd, Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, Vcl.StdCtrls, System.Classes, Vcl.Controls;

type
  TForm1 = class(TForm)
    fod: TFileOpenDialog;
    TrayIcon1: TTrayIcon;
    Panel1: TPanel;
    Edit1: TLabeledEdit;
    Panel2: TPanel;
    procedure SaveClipBrdJPG(const Folder: String);
    procedure Edit1DblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure WMClipboardUpdate(var Msg: TMessage); message WM_CLIPBOARDUPDATE;
    procedure WMSysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;

    procedure FormDblClick(Sender: TObject);
    procedure TrayIcon1DblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
  SaveClipBrdJPG(Edit1.Text);
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

procedure TForm1.SaveClipBrdJPG(const Folder: string);
var
  Bmp: TBitmap;
  PNGImage: TPngImage;
  FileName: String;
  ClipBoardHasPicture: Boolean;
begin
  FileName := Folder + '\' + FormatDateTime('yyyy-mm-dd hhnnss', Now);
  if DirectoryExists(Folder) then begin
    ClipBoardHasPicture := False;
    Try ClipBoardHasPicture := Clipboard.HasFormat(CF_PICTURE); Except End;
    if ClipBoardHasPicture then begin
      Bmp := TBitmap.Create;
      PNGImage := TPngImage.Create;
      try
        Bmp.Assign(Clipboard);
        PNGImage.Assign(Bmp);
        if Not IsSameBitmap(Bmp, LastBmp) then begin
          LastBmp.Assign(Bmp);
          PNGImage.SaveToFile(FileName + '.png');
        end;
      finally
        PNGImage.Free;
        Bmp.Free;
      end;
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

procedure TForm1.Edit1Change(Sender: TObject);
begin
  Ini.WriteString('Settings', 'PathOut', Edit1.Text);
end;

procedure TForm1.Edit1DblClick(Sender: TObject);
begin
  if fod.Execute then Edit1.Text := fod.FileName;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddClipboardFormatListener(Handle);
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  Edit1.Text := Ini.ReadString('Settings', 'PathOut', Edit1.Text);
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
  Edit1.Width := Panel1.ClientWidth - 16;
end;

end.
