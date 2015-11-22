unit UShapesList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UShapes, math, UGeometry, UViewPort, UInspector, Classes;

type

  { TShapesList }

  TShapesList = class
    private
      FShapes: array of TShape;
      FSelectionRectangle: TRectangle;
      function GetImageSize: TFloatRect;
    public
      property SelectionRectangle: TRectangle read FSelectionRectangle
        write FSelectionRectangle;
      property ImageSize: TFloatRect read GetImageSize;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AShape: TShape);
      procedure Select;
      procedure Select(APoint: TPoint);
      procedure Delete;
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

function TShapesList.GetImageSize: TFloatRect;
var i: integer;
begin
  if Length(FShapes) > 0 then
    begin
      Result := FShapes[0].Rect;
      for i := 1 to High(FShapes) do
      begin
        Result.Left := Min(Result.Left, FShapes[i].Rect.Left);
        Result.Right := Max(Result.Right, FShapes[i].Rect.Right);
        Result.Top := Min(Result.Top, FShapes[i].Rect.Top);
        Result.Bottom := Max(Result.Bottom, FShapes[i].Rect.Bottom);
      end;
    end
  else
    Result := FloatRect(FloatPoint(0, 0), FloatPoint(0, 0));
end;

procedure TShapesList.Draw(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Draw(ACanvas);
  for i := 0 to High(FShapes) do
    if FShapes[i].Selected then
      FShapes[i].DrawSelection(ACanvas);
  if FSelectionRectangle <> nil then
    FSelectionRectangle.Draw(ACanvas);
end;

procedure TShapesList.Add(AShape: TShape);
begin
  SetLength(FShapes, Length(FShapes) + 1);
  FShapes[High(FShapes)] := AShape;
end;

procedure TShapesList.Select;
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Select(VP.WorldToScreen(FSelectionRectangle.Rect));
end;

procedure TShapesList.Select(APoint: TPoint);
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Select(APoint);
end;

procedure TShapesList.Delete;
var i, c: Integer;
begin
  for i := 0 to High(FShapes) do
  begin
    if FShapes[i].Selected then
    begin
      FShapes[i].Free;
      FShapes[i] := nil;
    end;
  end;
  c := 0;
  i := 0;
  while i <= High(FShapes) do
  begin
    if FShapes[i] = nil then
      c += 1
    else
      FShapes[i - c] := FShapes[i];
    i += 1;
  end;
  SetLength(FShapes, Length(FShapes) - c);
  if Length(FShapes) = 0 then
  begin
    VP.Scale := 1;
    VP.ViewPosition := VP.PortSize / 2;
  end;
end;

procedure TShapesList.LoadSelected;
var
  a: array of TShape;
  i: Integer;
begin
  SetLength(a, 0);
  for i := 0 to High(FShapes) do
    if FShapes[i].Selected then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := FShapes[i];
    end;
  Inspector.Load(a);
end;

procedure TShapesList.UnSelect;
var
  i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Selected := False;
end;

function TShapesList.PointOnFigure(APoint: TPoint): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to High(FShapes) do
  begin
    Result := FShapes[i].Selected and FShapes[i].PointOnFigure(APoint);
    if Result then
      exit;
  end;
end;

procedure TShapesList.ShiftSelected(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to High(FShapes) do
  begin
    if FShapes[i].Selected then
      FShapes[i].Shift(AShift);
  end;
end;

function TShapesList.IsEmpty: Boolean;
begin
  Result := Length(FShapes) < 1;
end;

initialization
  Figures := TShapesList.Create;
end.

