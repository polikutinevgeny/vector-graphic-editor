unit UShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UViewPort, UGeometry, math, LCLIntf, LCLType, sysutils;

type

  { TShape }

  TPenWidth = type Integer;
  TPenColor = type TColor;
  TLeft = type Double;
  TRight = type Double;
  TTop = type Double;
  TBottom = type Double;
  TShape = class abstract(TPersistent)
  private
    FPoints: TFloatPoints;
    FRect: TFloatRect;
    FPenColor: TPenColor;
    FPenWidth: TPenWidth;
    FPenStyle: TPenStyle;
    FSelected: Boolean;
    FPrevSelected: Boolean;
    function GetPoints: TStrings;
    function GetRect: TFloatRect; virtual;
    procedure SetLeft(d: TLeft);
    procedure SetRight(d: TRight);
    procedure SetTop(d: TTop);
    procedure SetBottom(d: TBottom);
    procedure UpdateRect;
  public
    constructor Create; virtual;
    procedure SetPoint(APoint: TPoint); virtual;
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); virtual;
    procedure MovePoint(APoint: TPoint);
    procedure DrawSelection(ACanvas: TCanvas);
    function PointInShape(APoint: TPoint): Boolean; virtual; abstract;
    function RectInShape(ARect: TRect): Boolean; virtual; abstract;
    procedure Shift(AShift: TPoint);
    function PointInEditPoint(APoint: TPoint): Integer;
    procedure MoveEditPoint(AShift: TPoint; AIndex: Integer); virtual;
    procedure SetPoints(AValue: String); virtual;
    property Rect: TFloatRect read GetRect;
    property TrueRect: TFloatRect read FRect;
    property IsSelected: Boolean read FSelected write FSelected;
    property PrevSelected: Boolean read FPrevSelected write FPrevSelected;
  published
    property PenColor: TPenColor read FPenColor write FPenColor;
    property PenWidth: TPenWidth read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property AlignLeft: TLeft write SetLeft;
    property AlignRight: TRight write SetRight;
    property AlignTop: TTop write SetTop;
    property AlignBottom: TBottom write SetBottom;
    property Points: TStrings read GetPoints;
  end;

  { TFill }

  TBrushColor = type TColor;
  TFill = class abstract(TShape)
    private
      FBrushColor: TBrushColor;
      FBrushStyle: TBrushStyle;
    public
      constructor Create; override;
      procedure Draw(ACanvas: TCanvas); override;
      procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
      procedure SetPoint(APoint: TPoint); override;
      procedure SetPoints(AValue: String); override;
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
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TShape)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
    procedure SetPoint(APoint: TPoint); override;
    procedure SetPoints(AValue: String); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    function PointInShape(APoint: TPoint): Boolean; override;
    function RectInShape(ARect: TRect): Boolean; override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
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
    procedure DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint); override;
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

procedure TFill.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  ACanvas.Brush.Color := FBrushColor;
  ACanvas.Brush.Style := FBrushStyle;
end;

procedure TFill.SetPoint(APoint: TPoint);
begin
  inherited SetPoint(APoint);
  SetLength(FPoints, 2);
  FPoints[1] := VP.ScreenToWorld(APoint);
end;

procedure TFill.SetPoints(AValue: String);
begin
  inherited SetPoints(AValue);
  if not InRange(Length(FPoints), 2, 2) then
    raise Exception.Create('File is damaged');
end;

{ TShape }

function TShape.GetRect: TFloatRect;
var
  p1, p2, dp: TPoint;
  r: TRect;
begin
  dp := Point(
    Round(PenWidth * VP.Scale / 2 + 5 * VP.Scale),
    Round(PenWidth * VP.Scale / 2 + 5 * VP.Scale));
  r := VP.WorldToScreen(FRect);
  p1 := r.TopLeft - dp;
  p2 := r.BottomRight + dp;
  Result := VP.ScreenToWorld(UGeometry.Rect(p1, p2));
end;

function TShape.GetPoints: TStrings;
var i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to High(FPoints) do
  begin
    Result.Add(FloatToStr(FPoints[i].X));
    Result.Add(FloatToStr(FPoints[i].Y));
  end;
end;

procedure TShape.SetLeft(d: TLeft);
var
  i: Integer;
  dx: Double;
begin
  dx := d - FRect.Left;
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(dx, 0);
  FRect.Left += dx;
  FRect.Right += dx;
end;

procedure TShape.SetPoints(AValue: String);
var
  i: Integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  Delete(AValue, 1, 1);
  Delete(AValue, Length(AValue), 1);
  sl.DelimitedText := AValue;
  if sl.Count mod 2 = 1 then
    raise Exception.Create('File is damaged');
  SetLength(FPoints, sl.Count div 2);
  for i := 0 to sl.Count - 1 do
    if i mod 2 = 0 then
      FPoints[i div 2].X := StrToFloat(sl[i])
    else
      FPoints[i div 2].Y := StrToFloat(sl[i]);
  sl.Free;
  UpdateRect;
end;

procedure TShape.SetRight(d: TRight);
var
  i: Integer;
  dx: Double;
begin
  dx := d - FRect.Right;
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(dx, 0);
  FRect.Left += dx;
  FRect.Right += dx;
end;

procedure TShape.SetTop(d: TTop);
var
  i: Integer;
  dy: Double;
begin
  dy := d - FRect.Top;
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(0, dy);
  FRect.Top += dy;
  FRect.Bottom += dy;
end;

procedure TShape.SetBottom(d: TBottom);
var
  i: Integer;
  dy: Double;
begin
  dy := d - FRect.Bottom;
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(0, dy);
  FRect.Top += dy;
  FRect.Bottom += dy;
end;

procedure TShape.UpdateRect;
var i: Integer;
begin
  FRect := FloatRect(FPoints[0], FPoints[0]);
  for i := 1 to High(FPoints) do
  begin
    FRect.Left := Min(FRect.Left, FPoints[i].X);
    FRect.Right := Max(FRect.Right, FPoints[i].X);
    FRect.Top := Min(FRect.Top, FPoints[i].Y);
    FRect.Bottom := Max(FRect.Bottom, FPoints[i].Y);
  end;
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
  SetLength(FPoints, 1);
  FPoints[0] := VP.ScreenToWorld(APoint);
  FRect := FloatRect(FPoints[0], FPoints[0]);
end;

procedure TShape.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := round(FPenWidth * VP.Scale);
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Style := FPenStyle;
end;

procedure TShape.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
begin
  ACanvas.Pen.Width := round(FPenWidth * VP.Scale);
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Style := FPenStyle;
end;

procedure TShape.MovePoint(APoint: TPoint);
var
  i: integer;
begin
  if Length(FPoints) = 1 then
  begin
    SetLength(FPoints, 2);
  end;
  FPoints[High(FPoints)] := VP.ScreenToWorld(APoint);
  UpdateRect;
end;

procedure TShape.DrawSelection(ACanvas: TCanvas);
var
  i: Integer;
  p: TPoint;
begin
  ACanvas.Pen.Width := 1;
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(VP.WorldToScreen(Rect));
  ACanvas.Pen.Style := psSolid;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Brush.Color := clWhite;
  for i := 0 to High(FPoints) do
  begin
    p := VP.WorldToScreen(FPoints[i]);
    ACanvas.EllipseC(p.X, p.Y, 5, 5);
  end;
end;

procedure TShape.Shift(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(AShift) / VP.Scale;
  FRect.Left += AShift.X / VP.Scale;
  FRect.Right += AShift.X / VP.Scale;
  FRect.Top += AShift.Y / VP.Scale;
  FRect.Bottom += AShift.Y / VP.Scale;
end;

function TShape.PointInEditPoint(APoint: TPoint): Integer;
var
  r: HRGN;
  i: Integer;
  p: TPoint;
  b: Boolean;
begin
  Result := -1;
  for i := High(FPoints) downto 0 do
  begin
    p := VP.WorldToScreen(FPoints[i]);
    r := CreateEllipticRgn(p.X - 5, p.Y - 5, p.X + 5, p.Y + 5);
    b := PtInRegion(r, APoint.X, APoint.Y);
    DeleteObject(r);
    if b then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TShape.MoveEditPoint(AShift: TPoint; AIndex: Integer);
var i: Integer;
begin
  FPoints[AIndex] += FloatPoint(AShift) / VP.Scale;
  UpdateRect;
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

procedure TPolyline.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point(FPoints[i]) - TopLeft;
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

procedure TLine.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point(FPoints[i]) - TopLeft;
  ACanvas.Line(p[0], p[1]);
end;

procedure TLine.SetPoint(APoint: TPoint);
begin
  inherited SetPoint(APoint);
  SetLength(FPoints, 2);
  FPoints[1] := VP.ScreenToWorld(APoint);
end;

procedure TLine.SetPoints(AValue: String);
begin
  inherited SetPoints(AValue);
  if not InRange(Length(FPoints), 2, 2) then
    raise Exception.Create('File is damaged');
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

procedure TRectangle.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point(FPoints[i]) - TopLeft;
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

procedure TEllipse.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point(FPoints[i]) - TopLeft;
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

procedure TRoundRect.DrawBitmap(ACanvas: TCanvas; TopLeft: TPoint);
var
  p: TPoints;
  i: Integer;
begin
  inherited DrawBitmap(ACanvas, TopLeft);
  SetLength(p, Length(FPoints));
  for i := 0 to High(FPoints) do
    p[i] := Point(FPoints[i]) - TopLeft;
  ACanvas.RoundRect(UGeometry.Rect(p[0], p[1]), FRadiusX, FRadiusY);
end;

initialization
  RegisterClass(TPolyline);
  RegisterClass(TLine);
  RegisterClass(TRectangle);
  RegisterClass(TRoundRect);
  RegisterClass(TEllipse);
end.
