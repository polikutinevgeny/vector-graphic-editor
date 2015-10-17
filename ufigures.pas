unit UFigures;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics;

type

  { TFigure }

  TFigure = class abstract
  private
    FPen: TPen;
    FBrush: TBrush;
    FPoints: array of TPoint;
  public
    constructor Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
    procedure Draw(ACanvas: TCanvas); virtual;
    procedure MovePoint(APoint: TPoint);
    property Pen: TPen read FPen;
    property Brush: TBrush read FBrush;
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
  FPoints[0] := APoint;
  FPoints[1] := APoint;
end;

procedure TFigure.Draw(ACanvas: TCanvas);
begin
  ACanvas.Pen.Assign(FPen);
  ACanvas.Brush.Assign(FBrush);
end;

procedure TFigure.MovePoint(APoint: TPoint);
begin
  FPoints[High(FPoints)] := APoint;
end;

{ TPolyline }

procedure TPolyline.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Polyline(FPoints);
end;

procedure TPolyline.AddPoint(APoint: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

{ TLine }

procedure TLine.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Line(FPoints[0], FPoints[1]);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Rectangle(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.Ellipse(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y);
end;

{ TRoundRect }

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  inherited;
  ACanvas.RoundRect(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y,
    10, 10);
end;

end.
