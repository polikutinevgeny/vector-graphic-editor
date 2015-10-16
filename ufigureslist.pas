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
      FLength: integer;
    public
      constructor Create;
      procedure Display(ACanvas: TCanvas);
      procedure Add(AFigure: TFigure);
      procedure Remove(all: boolean = false);
      procedure Restore(all: boolean = false);
      function Last: TFigure;
  end;

var
  Figures: TFiguresList;

implementation

{ TFiguresList }

constructor TFiguresList.Create;
begin
  FLength := 0;
end;

procedure TFiguresList.Display(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to FLength - 1 do
  begin
    ACanvas.Pen := FFigures[i].Pen;
    ACanvas.Brush := FFigures[i].Brush;
    FFigures[i].Draw(ACanvas);
  end;
end;

procedure TFiguresList.Add(AFigure: TFigure);
begin
  inc(FLength);
  SetLength(FFigures, FLength);
  FFigures[High(FFigures)] := AFigure;
end;

procedure TFiguresList.Remove(all: boolean);
begin
  if all then FLength := 0 else FLength := max(FLength - 1, 0);
end;

procedure TFiguresList.Restore(all: boolean);
begin
  if all then FLength := Length(FFigures)
  else FLength := min(FLength + 1, Length(FFigures));
end;

function TFiguresList.Last: TFigure;
begin
  Last := FFigures[FLength - 1];
end;

initialization
  Figures := TFiguresList.Create;
end.

