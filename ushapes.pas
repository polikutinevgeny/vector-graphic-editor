unit UShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UViewPort, UGeometry, math, LCLIntf, LCLType, sysutils,
  UBaseShape;

type

  { TFill }

  TBrushColor = type TColor;
  TFill = class abstract(TShape)
    private
      FBrushColor: TBrushColor;
      FBrushStyle: TBrushStyle;
      procedure SetPoints(AValue: TStrings); override;
    public
      constructor Create; override;
      procedure Draw(ACanvas: TCanvas); override;
      procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
      procedure SetPoint(APoint: TPoint); override;
    published
      property BrushColor: TBrushColor read FBrushColor write FBrushColor;
      property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
  end;

  { TPolyline }

  TPolyline = class(TShape)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TShape)
  private
    procedure SetPoints(AValue: TStrings); override;
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
    procedure SetPoint(APoint: TPoint); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
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
    procedure DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double); override;
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

procedure TFill.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
end;

procedure TFill.SetPoint(APoint: TPoint);
begin
  inherited SetPoint(APoint);
  SetLength(FPoints, 2);
  FPoints[1] := VP.ScreenToWorld(APoint);
end;

procedure TFill.SetPoints(AValue: TStrings);
begin
  inherited SetPoints(AValue);
  if not InRange(Length(FPoints), 2, 2) then
    raise Exception.Create('Invalid number of points');
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
      Round(FPenWidth * VP.Scale / 2 + 5));
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

procedure TPolyline.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point((FPoints[i] - TopLeft) * Scale);
  ACanvas.Polyline(p);
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
    Round(FPenWidth * VP.Scale / 2 + 5));
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

procedure TLine.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point((FPoints[i] - TopLeft) * Scale);
  ACanvas.Line(p[0], p[1]);
end;

procedure TLine.SetPoint(APoint: TPoint);
begin
  inherited SetPoint(APoint);
  SetLength(FPoints, 2);
  FPoints[1] := VP.ScreenToWorld(APoint);
end;

procedure TLine.SetPoints(AValue: TStrings);
begin
  inherited SetPoints(AValue);
  if not InRange(Length(FPoints), 2, 2) then
    raise Exception.Create('Invalid number of points');
end;

{ TRectangle }

function TRectangle.PointInShape(APoint: TPoint): Boolean;
var r: HRGN;
begin
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

procedure TRectangle.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point((FPoints[i] - TopLeft) * Scale);
  ACanvas.Rectangle(UGeometry.Rect(p[0], p[1]));
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

procedure TEllipse.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point((FPoints[i] - TopLeft) * Scale);
  ACanvas.Ellipse(UGeometry.Rect(p[0], p[1]));
end;

{ TRoundRect }

function TRoundRect.PointInShape(APoint: TPoint): Boolean;
var r: HRGN;
begin
  r := CreateRoundRectRgn(
    VP.WorldToScreen(FPoints[0]).X, VP.WorldToScreen(FPoints[0]).Y,
    VP.WorldToScreen(FPoints[1]).X, VP.WorldToScreen(FPoints[1]).Y,
    Round(FRadiusX * VP.Scale), Round(FRadiusY * VP.Scale));
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

procedure TRoundRect.DrawExport(ACanvas: TCanvas; TopLeft: TPoint; Scale: Double);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawExport(ACanvas, TopLeft, Scale);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point((FPoints[i] - TopLeft) * Scale);
  ACanvas.RoundRect(UGeometry.Rect(p[0], p[1]), FRadiusX, FRadiusY);
end;

initialization
  RegisterClass(TPolyline);
  RegisterClass(TLine);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRect);
  RegisterClass(TEllipse);
end.
