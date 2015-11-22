unit UShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UViewPort, UGeometry, math, LCLIntf, LCLType;

type

  { TShape }

  TPenWidth = type Integer;
  TShape = class abstract
  private
    FPoints: TFloatPoints;
    FRect: TFloatRect;
    FPenColor: TColor;
    FPenWidth: TPenWidth;
    FPenStyle: TPenStyle;
    FSelected: Boolean;
    function GetRect: TFloatRect;
  public
    constructor Create; virtual;
    procedure SetPoint(APoint: TPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    procedure Select(ARect: TRect); virtual; abstract;
    procedure Select(APoint: TPoint); virtual; abstract;
    procedure DrawSelection(ACanvas: TCanvas);
    function PointOnFigure(APoint: TPoint): Boolean;
    procedure Shift(AShift: TPoint);
    property Rect: TFloatRect read GetRect;
    property Selected: Boolean read FSelected write FSelected;
  published
    property PenColor: TColor read FPenColor write FPenColor;
    property PenWidth: TPenWidth read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
  end;

  { TFill }

  TFill = class abstract(TShape)
    private
      FBrushColor: TColor;
      FBrushStyle: TBrushStyle;
    public
      constructor Create; override;
      procedure Draw(ACanvas: TCanvas); override;
    published
      property BrushColor: TColor read FBrushColor write FBrushColor;
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
  end;

  { TPolyline }

  TPolyline = class(TShape)
  public
    procedure Select(ARect: TRect); override;
    procedure Select(APoint: TPoint); override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TShape)
  public
    procedure Select(ARect: TRect); override;
    procedure Select(APoint: TPoint); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    procedure Select(ARect: TRect); override;
    procedure Select(APoint: TPoint); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    procedure Select(ARect: TRect); override;
    procedure Select(APoint: TPoint); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRoundRect }

  TRadius = type Integer;
  TRoundRect = class(TFill)
  private
    FRadiusX: Integer;
    FRadiusY: Integer;
  public
    procedure Select(ARect: TRect); override;
    procedure Select(APoint: TPoint); override;
    procedure Draw(ACanvas: TCanvas); override;
  published
    property RadiusX: TRadius read FRadiusX write FRadiusX;
    property RadiusY: TRadius read FRadiusY write FRadiusY;
  end;

implementation

{ TFill }

constructor TFill.Create;
begin
  inherited Create;
  FBrushColor := clWhite;
  FBrushStyle := bsSolid;
end;

procedure TFill.Draw(ACanvas: TCanvas);
begin
  inherited Draw(ACanvas);
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
end;

{ TShape }

function TShape.GetRect: TFloatRect;
var
  p1, p2, dp: TPoint;
  r: TRect;
begin
  dp := Point(
    Round(PenWidth * VP.Scale / 2) + 5,
    Round(PenWidth * VP.Scale / 2) + 5);
  r := VP.WorldToScreen(FRect);
  p1 := r.TopLeft - dp;
  p2 := r.BottomRight + dp;
  Result := VP.ScreenToWorld(UGeometry.Rect(p1, p2));
end;

constructor TShape.Create;
begin
  FPenWidth := 1;
  FPenColor := clBlack;
  FPenStyle := psSolid;
  FSelected := False;
end;

procedure TShape.SetPoint(APoint: TPoint);
begin
  SetLength(FPoints, 2);
  FPoints[0] := VP.ScreenToWorld(APoint);
  FPoints[1] := FPoints[0];
  FRect := FloatRect(FPoints[0], FPoints[0]);
end;

procedure TShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := round(FPenWidth * VP.Scale);
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Style := FPenStyle;
end;

procedure TShape.MovePoint(APoint: TPoint);
var
  i: integer;
begin
  FPoints[High(FPoints)] := VP.ScreenToWorld(APoint);
  FRect := FloatRect(FPoints[0], FPoints[0]);
  for i := 1 to High(FPoints) do
  begin
    FRect.Left := Min(FRect.Left, FPoints[i].X);
    FRect.Right := Max(FRect.Right, FPoints[i].X);
    FRect.Top := Min(FRect.Top, FPoints[i].Y);
    FRect.Bottom := Max(FRect.Bottom, FPoints[i].Y);
  end;
end;

procedure TShape.DrawSelection(ACanvas: TCanvas);
var
  p1, p2, dp: TPoint;
begin
  ACanvas.Pen.Width := 3;
  ACanvas.Pen.Color := clGreen;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(VP.WorldToScreen(GetRect));
end;

function TShape.PointOnFigure(APoint: TPoint): Boolean;
var
  r: TRect;
begin
  r := VP.WorldToScreen(FRect);
  Result := (APoint.X >= r.Left) and (APoint.X <= r.Right) and
    (APoint.Y >= r.Top) and (APoint.Y <= r.Bottom)
end;

procedure TShape.Shift(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(AShift) / VP.Scale;
  FRect.Left += AShift.X / VP.Scale;
  FRect.Right += AShift.X / VP.Scale;
  FRect.Top += AShift.Y / VP.Scale;
  FRect.Bottom += AShift.Y / VP.Scale;;
end;

{ TPolyline }

procedure TPolyline.Select(ARect: TRect);
var
  i: integer;
begin
  //I don't know how to do this with regions, so I wrote it myself
  FSelected := False;
  for i := 0 to High(FPoints) - 1 do
  begin
    FSelected := Intersection(ARect,
      VP.WorldToScreen(FPoints[i]),
      VP.WorldToScreen(FPoints[i+1]));
    if FSelected then
      Exit;
  end;
end;

procedure TPolyline.Select(APoint: TPoint);
var
  i: integer;
begin
  FSelected := False;
  for i := 0 to High(FPoints) - 1 do
  begin
    FSelected := CircleSegmentIntersection(
      VP.WorldToScreen(FPoints[i]), VP.WorldToScreen(FPoints[i+1]), APoint,
      Round(FPenWidth * VP.Scale + 3));
    if FSelected then
      Exit;
  end;
end;

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(VP.WorldToScreen(FPoints));
end;

procedure TPolyline.AddPoint(APoint: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := VP.ScreenToWorld(APoint);
  FRect.Left := Min(FRect.Left, FPoints[High(FPoints)].X);
  FRect.Right := Max(FRect.Right, FPoints[High(FPoints)].X);
  FRect.Top := Min(FRect.Top, FPoints[High(FPoints)].Y);
  FRect.Bottom := Max(FRect.Bottom, FPoints[High(FPoints)].Y);
end;

{ TLine }

procedure TLine.Select(ARect: TRect);
begin
  //I don't know how to do this with regions, so I wrote it myself
  FSelected := Intersection(ARect, VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1]));
end;

procedure TLine.Select(APoint: TPoint);
begin
  FSelected := CircleSegmentIntersection(
    VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]), APoint,
    Round(FPenWidth * VP.Scale + 3));
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]));
end;

{ TRectangle }

procedure TRectangle.Select(ARect: TRect);
var r: HRGN;
begin
  r := CreateRectRgnIndirect(VP.WorldToScreen(FRect));
  FSelected := RectInRegion(r, ARect);
  DeleteObject(r);
end;

procedure TRectangle.Select(APoint: TPoint);
var r: HRGN;
begin
  r := CreateRectRgnIndirect(VP.WorldToScreen(FRect));
  FSelected := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(VP.WorldToScreen(UGeometry.FloatRect(FPoints[0], FPoints[1])));
end;

{ TEllipse }

procedure TEllipse.Select(ARect: TRect);
var r: HRGN;
begin
  r := CreateEllipticRgnIndirect(VP.WorldToScreen(FRect));
  FSelected := RectInRegion(r, ARect);
  DeleteObject(r);
end;

procedure TEllipse.Select(APoint: TPoint);
var r: HRGN;
begin
  r := CreateEllipticRgnIndirect(VP.WorldToScreen(FRect));
  FSelected := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(VP.WorldToScreen(UGeometry.FloatRect(FPoints[0], FPoints[1])));
end;

{ TRoundRect }

procedure TRoundRect.Select(ARect: TRect);
var r: HRGN;
begin
  r := CreateRoundRectRgn(
    VP.WorldToScreen(FPoints[0]).X, VP.WorldToScreen(FPoints[0]).Y,
    VP.WorldToScreen(FPoints[1]).X, VP.WorldToScreen(FPoints[1]).Y,
    Round(FRadiusX * VP.Scale), Round(FRadiusY * VP.Scale)
    );
  FSelected := RectInRegion(r, ARect);
  DeleteObject(r);
end;

procedure TRoundRect.Select(APoint: TPoint);
var r: HRGN;
begin
  r := CreateRoundRectRgn(
    VP.WorldToScreen(FPoints[0]).X, VP.WorldToScreen(FPoints[0]).Y,
    VP.WorldToScreen(FPoints[1]).X, VP.WorldToScreen(FPoints[1]).Y,
    Round(FRadiusX * VP.Scale), Round(FRadiusY * VP.Scale));
  FSelected := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(VP.WorldToScreen(UGeometry.FloatRect(FPoints[0], FPoints[1])),
    Round(FRadiusX * VP.Scale),
    Round(FRadiusY * VP.Scale));
end;

end.
