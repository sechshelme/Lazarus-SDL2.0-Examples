unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, FileUtil;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  slFile, slHeader: TStringList;
  s: string;
  i, j: integer;
  p: SizeInt;
begin
  Memo1.Clear;
  slHeader := TStringList.Create;
  slHeader.LoadFromFile('SDL_pixels.pp');
  for j := 0 to slHeader.Count - 1 do begin
    s := slHeader[j];
    p := pos('SDL_DEFINE_PIXELFORMAT', s);
    if p > 20 then  begin
      WriteLn(s);
    end;
    //      slHeader[j] := StringReplace(slHeader[j], 'SDLCALL', '', [rfReplaceAll]);
    //      slHeader[j] := StringReplace(slHeader[j], 'DECLSPEC', '', [rfReplaceAll]);
  end;
  slHeader.SaveToFile('SDL3_pixels.pas');
  slHeader.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.
