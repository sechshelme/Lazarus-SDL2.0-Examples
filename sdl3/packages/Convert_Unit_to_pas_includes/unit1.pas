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
  slFile, unit_source, inc_dest: TStringList;
  i, j: integer;
  path: string;
begin
  Memo1.Clear;
  slFile := FindAllFiles('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_units', '*.pas');

  for i := 0 to slFile.Count - 1 do begin
    unit_source := TStringList.Create;
    inc_dest := TStringList.Create;
    unit_source.LoadFromFile(slFile[i]);

    path := ExtractFileName(slFile[i]);
    path := ChangeFileExt(path, '.inc');

    path:='/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_includes/'+path;

    Memo1.Lines.Add(path);


    j := 0;
    repeat
      inc_dest.Add(unit_source[j]);


      Inc(j)
    until j >= unit_source.Count;
        inc_dest.SaveToFile(path);

    for j := 0 to unit_source.Count - 1 do begin
      //      unit_source[j] := StringReplace(unit_source[j], 'SDLCALL', '', [rfReplaceAll]);
      //    unit_source[j] := StringReplace(unit_source[j], 'DECLSPEC', '', [rfReplaceAll]);
    end;
    unit_source.SaveToFile(slFile[i]);
    inc_dest.Free;
    unit_source.Free;
  end;

  slFile.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.
