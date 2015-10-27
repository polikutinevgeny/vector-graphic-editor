unit UFiguresList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UFigures, math, UAdditionalTypes, sysutils;

type

  { TFiguresList }

  TFiguresList = class
    private
      FFigures: array of TFigure;
      FNumberOfFiguresShown: integer;
      FZoomRect: TRectangle;
    public
      property ZoomRectangle: TRectangle read FZoomRect write FZoomRect;
      constructor Create;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AFigure: TFigure);
      procedure Undo;
      procedure UndoAll;
      procedure Redo;
      procedure RedoAll;
      function Last: TFigure;
      function TopLeft: TFloatPoint;
      function BottomRight: TFloatPoint;
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
    FFigures[i].Draw(ACanvas);
  if FZoomRect <> nil then
    FZoomRect.Draw(ACanvas);
end;

procedure TFiguresList.Add(AFigure: TFigure);
begin
  inc(FNumberOfFiguresShown);
  SetLength(FFigures, FNumberOfFiguresShown);
  FFigures[High(FFigures)] := AFigure;
end;

procedure TFiguresList.Undo;
begin
  FNumberOfFiguresShown := max(FNumberOfFiguresShown - 1, 0);
end;

procedure TFiguresList.UndoAll;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TFiguresList.Redo;
begin
  FNumberOfFiguresShown := min(FNumberOfFiguresShown + 1, Length(FFigures));
end;

procedure TFiguresList.RedoAll;
begin
  FNumberOfFiguresShown := Length(FFigures);
end;

function TFiguresList.Last: TFigure;
begin
  Last := FFigures[FNumberOfFiguresShown - 1];
end;

function TFiguresList.TopLeft: TFloatPoint;
var
  t, l: Double;
  i: integer;
begin
  if FNumberOfFiguresShown < 1 then
    raise Exception.Create('No figures to look at');
  t := FFigures[0].Top;
  l := FFigures[0].Left;
  for i := 1 to FNumberOfFiguresShown - 1 do
  begin
    t := Min(t, FFigures[i].Top);
    l := Min(l, FFigures[i].Left);
  end;
  Result := FloatPoint(l, t);
end;

function TFiguresList.BottomRight: TFloatPoint;
var
  b, r: Double;
  i: integer;
begin
  if FNumberOfFiguresShown < 1 then
    raise Exception.Create('No figures to look at');
  b := FFigures[0].Bottom;
  r := FFigures[0].Right;
  for i := 1 to FNumberOfFiguresShown - 1 do
  begin
    b := Max(b, FFigures[i].Bottom);
    r := Max(r, FFigures[i].Right);
  end;
  Result := FloatPoint(r, b);
end;

initialization
  Figures := TFiguresList.Create;
end.

