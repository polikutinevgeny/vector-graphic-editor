unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UViewingPort, UAdditionalTypes, math;

type

  { TFigure }

  TFigure = class abstract
  private
    FPen: TPen;
    FBrush: TBrush;
    FPoints: TFloatPoints;
    FTop: Double;
    FLeft: Double;
    FBottom: Double;
    FRight: Double;
  public
    constructor Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    property Pen: TPen read FPen;
    property Brush: TBrush read FBrush;
    property Top: Double read FTop;
    property Bottom: Double read FBottom;
    property Left: Double read FLeft;
    property Right: Double read FRight;
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

constructor TFigure.Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  SetLength(FPoints, 2);
  FPen := TPen.Create;
  FPen.Assign(APen);
  FBrush := TBrush.Create;
  FBrush.Assign(ABrush);
  FPoints[0] := ViewingPort.ScreenToWorld(APoint);
  FPoints[1] := ViewingPort.ScreenToWorld(APoint);
  FLeft := FPoints[0].X;
  FRight := FPoints[0].X;
  FTop := FPoints[0].Y;
  FBottom := FPoints[0].Y;
end;

procedure TFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(FPen);
  ACanvas.Brush.Assign(FBrush);
end;

procedure TFigure.MovePoint(APoint: TPoint);
begin
  FPoints[High(FPoints)] := ViewingPort.ScreenToWorld(APoint);
  FLeft := Min(FLeft, FPoints[High(FPoints)].X);
  FRight := Max(FRight, FPoints[High(FPoints)].X);
  FTop := Min(FTop, FPoints[High(FPoints)].Y);
  FBottom := Max(FBottom, FPoints[High(FPoints)].Y);
end;

{ TPolyline }

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(ViewingPort.WorldToScreen(FPoints));
end;

procedure TPolyline.AddPoint(APoint: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := ViewingPort.ScreenToWorld(APoint);
  FLeft := Min(FLeft, FPoints[High(FPoints)].X);
  FRight := Max(FRight, FPoints[High(FPoints)].X);
  FTop := Min(FTop, FPoints[High(FPoints)].Y);
  FBottom := Max(FBottom, FPoints[High(FPoints)].Y);
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(ViewingPort.WorldToScreen(FPoints[0]),
    ViewingPort.WorldToScreen(FPoints[1]));
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var p1, p2: TPoint;
begin
  inherited;
  p1 := ViewingPort.WorldToScreen(FPoints[0]);
  p2 := ViewingPort.WorldToScreen(FPoints[1]);
  ACanvas.Rectangle(p1.X, p1.Y, p2.X, p2.Y);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
var p1, p2: TPoint;
begin
  inherited;
  p1 := ViewingPort.WorldToScreen(FPoints[0]);
  p2 := ViewingPort.WorldToScreen(FPoints[1]);
  ACanvas.Ellipse(p1.X, p1.Y, p2.X, p2.Y);
end;

{ TRoundRect }

procedure TRoundRect.Draw(ACanvas: TCanvas);
var p1, p2: TPoint;
begin
  inherited;
  p1 := ViewingPort.WorldToScreen(FPoints[0]);
  p2 := ViewingPort.WorldToScreen(FPoints[1]);
  ACanvas.RoundRect(p1.X, p1.Y, p2.X, p2.Y, 10, 10);
end;

end.
