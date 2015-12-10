unit UBaseShape;

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
  protected
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
    procedure SetPoints(AValue: TStrings); virtual;
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
    property Points: TStrings read GetPoints write SetPoints;
  end;

implementation

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

procedure TShape.SetPoints(AValue: TStrings);
var i: Integer;
begin
  if AValue.Count mod 2 = 1 then
    raise Exception.Create('File is damaged');
  SetLength(FPoints, AValue.Count div 2);
  for i := 0 to AValue.Count div 2 - 1 do
  begin
    FPoints[i].X := StrToFloat(AValue[2 * i]);
    FPoints[i].Y := StrToFloat(AValue[2 * i + 1]);
  end;
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
begin
  FPoints[AIndex] += FloatPoint(AShift) / VP.Scale;
  UpdateRect;
end;

end.

