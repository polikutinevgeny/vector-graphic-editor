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
    FPrevSelected: Boolean;
    function GetRect: TFloatRect;
  public
    constructor Create; virtual;
    procedure SetPoint(APoint: TPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    procedure DrawSelection(ACanvas: TCanvas);
    function PointOnSelectionRect(APoint: TPoint): Boolean;
    function PointInShape(APoint: TPoint): Boolean; virtual; abstract;
    function RectInShape(ARect: TRect): Boolean; virtual; abstract;
    procedure Shift(AShift: TPoint);
    property Rect: TFloatRect read GetRect;
    property Selected: Boolean read FSelected write FSelected;
    property PrevSelected: Boolean read FPrevSelected write FPrevSelected;
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
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TShape)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRoundRect }

  TRadius = type Integer;
  TRoundRect = class(TFill)
  private
    FRadiusX: Integer;
    FRadiusY: Integer;
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
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
    Round(PenWidth * VP.Scale + 5 / VP.Scale),
    Round(PenWidth * VP.Scale + 5 / VP.Scale));
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
  FPrevSelected := False;
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
begin
  ACanvas.Pen.Width := 3;
  ACanvas.Pen.Color := clGreen;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(VP.WorldToScreen(GetRect));
end;

function TShape.PointOnSelectionRect(APoint: TPoint): Boolean;
var
  r: TRect;
begin
  r := VP.WorldToScreen(Rect);
  Result := PtInRect(r, APoint);
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

function TPolyline.PointInShape(APoint: TPoint): Boolean;
var
  i: integer;
begin
  //I don't know how to do this with regions, so I wrote it myself
  for i := 0 to High(FPoints) - 1 do
  begin
    Result := CircleSegmentIntersection(
      VP.WorldToScreen(FPoints[i]), VP.WorldToScreen(FPoints[i+1]), APoint,
      Round(FPenWidth * VP.Scale + 3));
    if Result then
      Exit;
  end;
end;

function TPolyline.RectInShape(ARect: TRect): Boolean;
var
  i: integer;
begin
  //I don't know how to do this with regions, so I wrote it myself
  for i := 0 to High(FPoints) - 1 do
  begin
    Result := Intersection(ARect,
      VP.WorldToScreen(FPoints[i]),
      VP.WorldToScreen(FPoints[i+1]));
    if Result then
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

function TLine.PointInShape(APoint: TPoint): Boolean;
begin
  //I don't know how to do this with regions, so I wrote it myself
  Result := CircleSegmentIntersection(
    VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]), APoint,
    Round(FPenWidth * VP.Scale + 3));
end;

function TLine.RectInShape(ARect: TRect): Boolean;
begin
  //I don't know how to do this with regions, so I wrote it myself
  Result := Intersection(ARect, VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1]));
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]));
end;

{ TRectangle }

function TRectangle.PointInShape(APoint: TPoint): Boolean;
var r: HRGN;
begin
  if FSelected then
    Exit;
  r := CreateRectRgnIndirect(VP.WorldToScreen(FRect));
  Result := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

function TRectangle.RectInShape(ARect: TRect): Boolean;
var r: HRGN;
begin
  r := CreateRectRgnIndirect(VP.WorldToScreen(FRect));
  Result := RectInRegion(r, ARect);
  DeleteObject(r);
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(VP.WorldToScreen(UGeometry.FloatRect(FPoints[0], FPoints[1])));
end;

{ TEllipse }

function TEllipse.PointInShape(APoint: TPoint): Boolean;
var r: HRGN;
begin
  r := CreateEllipticRgnIndirect(VP.WorldToScreen(FRect));
  Result := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

function TEllipse.RectInShape(ARect: TRect): Boolean;
var r: HRGN;
begin
  r := CreateEllipticRgnIndirect(VP.WorldToScreen(FRect));
  Result := RectInRegion(r, ARect);
  DeleteObject(r);
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(VP.WorldToScreen(UGeometry.FloatRect(FPoints[0], FPoints[1])));
end;

{ TRoundRect }

function TRoundRect.PointInShape(APoint: TPoint): Boolean;
var r: HRGN;
begin
  r := CreateRoundRectRgn(
    VP.WorldToScreen(FPoints[0]).X, VP.WorldToScreen(FPoints[0]).Y,
    VP.WorldToScreen(FPoints[1]).X, VP.WorldToScreen(FPoints[1]).Y,
    Round(FRadiusX * VP.Scale), Round(FRadiusY * VP.Scale)
    );
  Result := PtInRegion(r, APoint.X, APoint.Y);
  DeleteObject(r);
end;

function TRoundRect.RectInShape(ARect: TRect): Boolean;
var r: HRGN;
begin
  r := CreateRoundRectRgn(
    VP.WorldToScreen(FPoints[0]).X, VP.WorldToScreen(FPoints[0]).Y,
    VP.WorldToScreen(FPoints[1]).X, VP.WorldToScreen(FPoints[1]).Y,
    Round(FRadiusX * VP.Scale), Round(FRadiusY * VP.Scale));
  Result := RectInRegion(r, ARect);
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
