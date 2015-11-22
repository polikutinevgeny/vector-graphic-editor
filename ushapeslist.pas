unit UShapesList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UShapes, math, UGeometry, UViewPort, UInspector, Classes;

type

  { TShapesList }

  TShapesList = class
    private
      FFigures: array of TShape;
      FNumberOfFiguresShown: integer;
      FSelectionRectangle: TRectangle;
      function FImageSize: TFloatRect;
    public
      property SelectionRectangle: TRectangle read FSelectionRectangle
        write FSelectionRectangle;
      property ImageSize: TFloatRect read FImageSize;
      constructor Create;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AFigure: TShape);
      procedure Undo;
      procedure UndoAll;
      procedure Redo;
      procedure RedoAll;
      procedure Select;
      procedure Select(APoint: TPoint);
      procedure LoadSelected;
      procedure UnSelect;
      function PointOnFigure(APoint: TPoint): Boolean;
      procedure ShiftSelected(AShift: TPoint);
      function IsEmpty: Boolean;
  end;

var
  Figures: TShapesList;

implementation

{ TShapesList }

function TShapesList.FImageSize: TFloatRect;
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

constructor TShapesList.Create;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TShapesList.Draw(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
    FFigures[i].Draw(ACanvas);
  for i := 0 to FNumberOfFiguresShown - 1 do
    if FFigures[i].Selected then
      FFigures[i].DrawSelection(ACanvas);
  if FSelectionRectangle <> nil then
    FSelectionRectangle.Draw(ACanvas);
end;

procedure TShapesList.Add(AFigure: TShape);
begin
  inc(FNumberOfFiguresShown);
  SetLength(FFigures, FNumberOfFiguresShown);
  FFigures[High(FFigures)] := AFigure;
end;

procedure TShapesList.Undo;
begin
  FNumberOfFiguresShown := max(FNumberOfFiguresShown - 1, 0);
  if IsEmpty then
    begin
      VP.Scale := 1;
      VP.ViewPosition := VP.PortSize / 2;
    end;
end;

procedure TShapesList.UndoAll;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TShapesList.Redo;
begin
  FNumberOfFiguresShown := min(FNumberOfFiguresShown + 1, Length(FFigures));
end;

procedure TShapesList.RedoAll;
begin
  FNumberOfFiguresShown := Length(FFigures);
end;

procedure TShapesList.Select;
var i: Integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
    FFigures[i].Select(VP.WorldToScreen(FSelectionRectangle.Rect));
end;

procedure TShapesList.Select(APoint: TPoint);
var i: Integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
    FFigures[i].Select(APoint);
end;

procedure TShapesList.LoadSelected;
var
  a: array of TShape;
  i: Integer;
begin
  SetLength(a, 0);
  for i := 0 to FNumberOfFiguresShown - 1 do
    if FFigures[i].Selected then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := FFigures[i];
    end;
  Inspector.Load(a);
end;

procedure TShapesList.UnSelect;
var
  i: Integer;
begin
  for i := 0 to High(FFigures) do
    FFigures[i].Selected := False;
end;

function TShapesList.PointOnFigure(APoint: TPoint): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to FNumberOfFiguresShown - 1 do
  begin
    Result := FFigures[i].Selected and FFigures[i].PointOnFigure(APoint);
    if Result then
      exit;
  end;
end;

procedure TShapesList.ShiftSelected(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
  begin
    if FFigures[i].Selected then
      FFigures[i].Shift(AShift);
  end;
end;

function TShapesList.IsEmpty: Boolean;
begin
  Result := FNumberOfFiguresShown < 1;
end;

initialization
  Figures := TShapesList.Create;
end.

