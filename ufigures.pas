unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UViewPort, UGeometry, math;

type

  { TFigure }

  TFigure = class abstract
  private
    FPoints: TFloatPoints;
    FRect: TFloatRect;
    FPenColor: TColor;
    FPenWidth: Integer;
    FPenStyle: TPenStyle;
    FSelected: Boolean;
  public
    constructor Create; virtual;
    procedure SetPoint(APoint: TPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    procedure Select(ARect: TRect); virtual; abstract;
    procedure DrawSelection(ACanvas: TCanvas);
    function PointOnFigure(APoint: TPoint): Boolean;
    procedure Shift(AShift: TPoint);
    property Rect: TFloatRect read FRect;
    property Selected: Boolean read FSelected write FSelected;
  published
    property PenColor: TColor read FPenColor write FPenColor;
    property PenWidth: Integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
  end;

  { TFill }

  TFill = class abstract(TFigure)
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

  TPolyline = class(TFigure)
  public
    procedure Select(ARect: TRect); override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TFigure)
  public
    procedure Select(ARect: TRect); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    procedure Select(ARect: TRect); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    procedure Select(ARect: TRect); override;
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRoundRect }

  TRoundRect = class(TFill)
  private
    FRadius: Integer;
  public
    procedure Select(ARect: TRect); override;
    procedure Draw(ACanvas: TCanvas); override;
  published
    property Radius: Integer read FRadius write FRadius;
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

{ TFigure }

constructor TFigure.Create;
begin
  FPenWidth := 1;
  FPenColor := clBlack;
  FPenStyle := psSolid;
  FSelected := False;
end;

procedure TFigure.SetPoint(APoint: TPoint);
begin
  SetLength(FPoints, 2);
  FPoints[0] := VP.ScreenToWorld(APoint);
  FPoints[1] := FPoints[0];
  FRect := FloatRect(FPoints[0], FPoints[0]);
end;

procedure TFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := round(FPenWidth * VP.Scale);
  ACanvas.Pen.Color := FPenColor;
  ACanvas.Pen.Style := FPenStyle;
end;

procedure TFigure.MovePoint(APoint: TPoint);
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

procedure TFigure.DrawSelection(ACanvas: TCanvas);
var
  p1, p2: TPoint;
begin
  ACanvas.Pen.Width := 3;
  ACanvas.Pen.Color := clGreen;
  ACanvas.Pen.Style := psDash;
  ACanvas.Brush.Style := bsClear;
  p1 := VP.WorldToScreen(FloatPoint(FRect.Left, FRect.Top)) -
    Point(PenWidth div 2 + 5, PenWidth div 2 + 5);
  p2 := VP.WorldToScreen(FloatPoint(FRect.Right, FRect.Bottom)) +
    Point(PenWidth div 2 + 5, PenWidth div 2 + 5);
  ACanvas.Rectangle(UGeometry.Rect(p1, p2));
end;

function TFigure.PointOnFigure(APoint: TPoint): Boolean;
var
  r: TRect;
begin
  r := VP.WorldToScreen(FRect);
  Result := (APoint.x >= r.Left) and (APoint.x <= r.Right) and
    (APoint.y >= r.Top) and (APoint.y <= r.Bottom)
end;

procedure TFigure.Shift(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to High(FPoints) do
    FPoints[i] += FloatPoint(AShift) / VP.Scale;
  FRect := FloatRect(FPoints[0], FPoints[0]);
  for i := 1 to High(FPoints) do
  begin
    FRect.Left := Min(FRect.Left, FPoints[i].X);
    FRect.Right := Max(FRect.Right, FPoints[i].X);
    FRect.Top := Min(FRect.Top, FPoints[i].Y);
    FRect.Bottom := Max(FRect.Bottom, FPoints[i].Y);
  end;
end;

{ TPolyline }

procedure TPolyline.Select(ARect: TRect);
var
  i: integer;
begin
  FSelected := False;
  for i := 0 to High(FPoints) - 1 do
  begin
    FSelected := Intersection(aRect,
      VP.WorldToScreen(FPoints[i]),
      VP.WorldToScreen(FPoints[i+1]));
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
  FSelected := Intersection(ARect, VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1]));
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]));
end;

{ TRectangle }

procedure TRectangle.Select(ARect: TRect);
begin
  FSelected := Intersection(ARect, VP.WorldToScreen(FRect));
end;

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(VP.WorldToScreen(FRect));
end;

{ TEllipse }

procedure TEllipse.Select(ARect: TRect);
begin
  FSelected := Intersection(ARect, VP.WorldToScreen(FRect));
end;

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(VP.WorldToScreen(FRect));
end;

{ TRoundRect }

procedure TRoundRect.Select(ARect: TRect);
begin
  FSelected := Intersection(ARect, VP.WorldToScreen(FRect));
end;

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(VP.WorldToScreen(FRect), Radius, Radius);
end;

end.
