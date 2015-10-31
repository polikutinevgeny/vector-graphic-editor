unit UFiguresList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UFigures, math, UGeometry, UViewPort;

type

  { TFiguresList }

  TFiguresList = class
    private
      FFigures: array of TFigure;
      FNumberOfFiguresShown: integer;
      FZoomRect: TRectangle;
      function FImageSize: TFloatRect;
    public
      property ZoomRectangle: TRectangle read FZoomRect write FZoomRect;
      property ImageSize: TFloatRect read FImageSize;
      constructor Create;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AFigure: TFigure);
      procedure Undo;
      procedure UndoAll;
      procedure Redo;
      procedure RedoAll;
      function Last: TFigure;
      function IsEmpty: Boolean;
  end;

var
  Figures: TFiguresList;

implementation

{ TFiguresList }

function TFiguresList.FImageSize: TFloatRect;
var i: integer;
begin
  if FNumberOfFiguresShown > 0 then
    begin
      Result := FFigures[0].Rect;
      for i := 1 to FNumberOfFiguresShown - 1 do
      begin
        Result.Left := Min(Result.Left, FFigures[i].Rect.Left);
        Result.Right := Max(Result.Right, FFigures[i].Rect.Right);
        Result.Top := Min(Result.Top, FFigures[i].Rect.Top);
        Result.Bottom := Max(Result.Bottom, FFigures[i].Rect.Bottom);
      end;
    end
  else
    Result := FloatRect(FloatPoint(0, 0), FloatPoint(0, 0));
end;

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
  if IsEmpty then
    begin
      VP.Scale := 1;
      VP.ViewPosition := VP.PortSize / 2;
    end;
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

function TFiguresList.IsEmpty: Boolean;
begin
  Result := FNumberOfFiguresShown < 1;
end;

initialization
  Figures := TFiguresList.Create;
end.

