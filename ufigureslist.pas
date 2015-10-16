unit UFiguresList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UFigures, math;

type

  { TFiguresList }

  TFiguresList = class
    private
      FFigures: array of TFigure;
      FNumberOfFiguresShown: integer;
    public
      constructor Create;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AFigure: TFigure);
      procedure Undo(all: boolean = false);
      procedure Redo(all: boolean = false);
      function Last: TFigure;
  end;

var
  Figures: TFiguresList;

implementation

{ TFiguresList }

constructor TFiguresList.Create;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TFiguresList.Draw(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
  begin
    ACanvas.Pen := FFigures[i].Pen;
    ACanvas.Brush := FFigures[i].Brush;
    FFigures[i].Draw(ACanvas);
  end;
end;

procedure TFiguresList.Add(AFigure: TFigure);
begin
  inc(FNumberOfFiguresShown);
  SetLength(FFigures, FNumberOfFiguresShown);
  FFigures[High(FFigures)] := AFigure;
end;

procedure TFiguresList.Undo(all: boolean);
begin
  if all then
  FNumberOfFiguresShown := 0 else FNumberOfFiguresShown := max(FNumberOfFiguresShown - 1, 0);
end;

procedure TFiguresList.Redo(all: boolean);
begin
  if all then FNumberOfFiguresShown := Length(FFigures)
  else FNumberOfFiguresShown := min(FNumberOfFiguresShown + 1, Length(FFigures));
end;

function TFiguresList.Last: TFigure;
begin
  Last := FFigures[FNumberOfFiguresShown - 1];
end;

initialization
  Figures := TFiguresList.Create;
end.

