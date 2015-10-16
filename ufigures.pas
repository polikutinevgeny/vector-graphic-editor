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
  public
    constructor Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
      virtual; abstract;
    procedure Draw(ACanvas: TCanvas); virtual; abstract;
    property Pen: TPen read FPen write FPen;
    property Brush: TBrush read FBrush write FBrush;
  end;

type

  { TPencil }

  TPencil = class(TFigure)
  public
    constructor Create(APoint: TPoint; APen: TPen; ABrush: TBrush); override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure AddPoint(APoint: TPoint);
  private
    FPoints: array of TPoint;
  end;

type

  { TLine }

  TLine = class(TFigure)
  public
    constructor Create(APoint: TPoint; APen: TPen; ABrush: TBrush); override;
    procedure Draw(ACanvas: TCanvas); override;
    procedure MoveSecondPoint(APoint: TPoint);
  private
    FPoints: array[0..1] of TPoint;
  end;

type

  { TPolyline }

  TPolyline = class(TPencil)
  public
    procedure MoveNextPoint(APoint: TPoint);
  end;

type

  { TRectangle }

  TRectangle = class(TLine)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

type

  { TEllipse }

  TEllipse = class(TLine)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

type

  { TRoundRect }

  TRoundRect = class(TLine)
  public
    procedure Draw(ACanvas: TCanvas); override;
  end;

implementation

{ TPencil }

constructor TPencil.Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  SetLength(FPoints, 2);
  FPen := TPen.Create;
  FPen.Assign(APen);
  FBrush := TBrush.Create;
  FBrush.Assign(ABrush);
  FPoints[0] := APoint;
  FPoints[1] := APoint;
end;

procedure TPencil.Draw(ACanvas: TCanvas);
begin
  ACanvas.Polyline(FPoints);
end;

procedure TPencil.AddPoint(APoint: TPoint);
begin
  SetLength(FPoints, Length(FPoints) + 1);
  FPoints[High(FPoints)] := APoint;
end;

{ TPolyline }

procedure TPolyline.MoveNextPoint(APoint: TPoint);
begin
  FPoints[High(FPoints)] := APoint;
end;

{ TLine }

constructor TLine.Create(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  FPen := TPen.Create;
  FPen.Assign(APen);
  FBrush := TBrush.Create;
  FBrush.Assign(ABrush);
  FPoints[0] := APoint;
  FPoints[1] := APoint;
end;

procedure TLine.Draw(ACanvas: TCanvas);
begin
  ACanvas.Line(FPoints[0], FPoints[1]);
end;

procedure TLine.MoveSecondPoint(APoint: TPoint);
begin
  FPoints[1] := APoint;
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
  ACanvas.Rectangle(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
  ACanvas.Ellipse(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y);
end;

{ TRoundRect }

procedure TRoundRect.Draw(ACanvas: TCanvas);
begin
  ACanvas.RoundRect(FPoints[0].X, FPoints[0].Y, FPoints[1].X, FPoints[1].Y,
    10, 10);
end;

end.
