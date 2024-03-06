unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  gtk2,
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
  slFile, unit_source, inc_dest, SDL3pas_include: TStringList;
  i, j: integer;
  s, path: string;
  p: SizeInt;
begin
  Memo1.Clear;
  slFile := FindAllFiles('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_units', '*.pas');

  for i := 0 to slFile.Count - 1 do begin
    unit_source := TStringList.Create;
    inc_dest := TStringList.Create;
    unit_source.LoadFromFile(slFile[i]);


    j := 0;

    // $PACKRECORDS C suchen
    repeat
      s := unit_source[j];
      p := pos('{$PACKRECORDS C}',s);
      if p <> 0 then begin
        Inc(j);
      end;
      Inc(j);
      if j >= unit_source.Count then begin
        WriteLn('Überlauf  {$PACKRECORDS C}');
        halt;
      end;
    until p<>0;

    inc_dest.Add('{$DEFINE read_interface}');

        // implementation C suchen
repeat
      s := unit_source[j];
      p := pos('implementation',s);
      if p = 1 then begin
        Inc(j);
      end else   inc_dest.Add(unit_source[j]);
      Inc(j);
      if j >= unit_source.Count then begin
        WriteLn('Überlauf  implementation');
        halt;
      end;
    until p=1;

    inc_dest.Add('{$UNDEF read_interface}');
    inc_dest.Add('');
    inc_dest.Add('');
    inc_dest.Add('{$DEFINE read_implementation}');
    inc_dest.Add('');

    // end. C suchen
    repeat
      s := unit_source[j];
      p := pos('end.',s);
      if p = 1 then begin
      end else   inc_dest.Add(unit_source[j]);

      Inc(j);
      if j >= unit_source.Count then begin
        WriteLn(slFile[i],'    Überlauf  end.');
//        halt;
      end;
    until p=1;

    inc_dest.Add('{$UNDEF read_implementation}');


    path := ExtractFileName(slFile[i]);
    path := ChangeFileExt(path, '.inc');
    path := '/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_includes/' + path;
    Memo1.Lines.Add(path);

    inc_dest.SaveToFile(path);

    inc_dest.Free;
    unit_source.Free;
  end;

  // === SDL3_includes.inc

  SDL3pas_include := TStringList.Create;
  SDL3pas_include.Add('{%MainUnit sdl3.pas}');

  for i := 0 to slFile.Count - 1 do begin
    path := ExtractFileName(slFile[i]);
    SDL3pas_include.Add('{$include ' + path + '.inc}');
  end;

  SDL3pas_include.SaveToFile('/n4800/DATEN/Programmierung/mit_GIT/Lazarus/Tutorial/SDL-2/sdl3/packages/pas_includes/SDL3_includes.inc');
  SDL3pas_include.Free;

  slFile.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Height := 1000;
  Width := 1000;
end;

end.
