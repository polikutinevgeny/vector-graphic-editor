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
  public
    constructor Create(APoint: TPoint);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    property Rect: TFloatRect read FRect;
  end;

type

  { TPolyline }

  TPolyline = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  end;

type

  { TLine }

  TLine = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

type

  { TRectangle }

  TRectangle = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

type

  { TEllipse }

  TEllipse = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

type

  { TRoundRect }

  TRoundRect = class(TFigure)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

implementation

{ TFigure }

constructor TFigure.Create(APoint: TPoint);
begin
  SetLength(FPoints, 2);
  FPoints[0] := VP.ScreenToWorld(APoint);
  FPoints[1] := VP.ScreenToWorld(APoint);
  FRect := FloatRect(FPoints[0], FPoints[0]);
end;

procedure TFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Width := round(VP.Scale);
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
    VP.WorldToScreen(FPoints[1])), 10, 10);
end;

end.
