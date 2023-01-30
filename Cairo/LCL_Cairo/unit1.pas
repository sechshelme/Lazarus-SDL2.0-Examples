unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CairoGraphics;

type

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    procedure FormCreate(Sender: TObject);
  private
    CPB: TCairoPaintBox;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}
procedure TForm1.FormCreate(Sender: TObject);
begin
  CPB := TCairoPaintBox.Create(Self);
  CPB.Parent := Self;
end;

end.
