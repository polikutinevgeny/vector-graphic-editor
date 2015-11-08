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
  public
    constructor Create; virtual;
    procedure SetPoint(APoint: TPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    property Rect: TFloatRect read FRect;
  published
    property PenColor: TColor read FPenColor write FPenColor;
    property PenWidth: Integer read FPenWidth write FPenWidth;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
  end;

  { TFill }

  TFill = class(TFigure)
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
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  end;

  { TLine }

  TLine = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRectangle }

  TRectangle = class(TFill)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TEllipse }

  TEllipse = class(TFill)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

  { TRoundRect }

  TRoundRect = class(TFill)
  private
    FRadius: Integer;
  public
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

{ TPolyline }

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

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(VP.WorldToScreen(FPoints[0]), VP.WorldToScreen(FPoints[1]));
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(UGeometry.Rect(VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1])));
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(UGeometry.Rect(VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1])));
end;

{ TRoundRect }

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(UGeometry.Rect(VP.WorldToScreen(FPoints[0]),
    VP.WorldToScreen(FPoints[1])), Radius, Radius);
end;

end.
