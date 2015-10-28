unit UViewingPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ExtCtrls, Math, UAdditionalTypes;

type

  { TViewingPort }

  TViewingPort = class
    const
      MaxScale = 20;
      MinScale = 0.25;
    private
      FScale: Double;
      FViewPosition: TFloatPoint;
      FPortSize: TPoint;
      FHorizontalSBPosition: Integer;
      FVerticalSBPosition: Integer;
      procedure SetScale(AScale: Double);
    public
      property ViewPosition: TFloatPoint read FViewPosition
        write FViewPosition;
      property Scale: Double read FScale write SetScale;
      constructor Create;
      function WorldToScreen(APoint: TFloatPoint): TPoint;
      function WorldToScreen(APoints: TFloatPoints): TPoints; overload;
      function ScreenToWorld(APoint: TPoint): TFloatPoint;
      procedure ScaleTo(APoint1, APoint2: TFloatPoint);
      procedure RecalculateScroll(APaintBox: TPaintBox; AHorizontalSB,
        AVerticalSB: TScrollBar; ATopLeft, ABottomRight: TFloatPoint);
      procedure ScaleMouseWheel(APoint: TPoint; Delta: Integer);
  end;

var
  ViewingPort: TViewingPort;

implementation

{ TViewingPort }

procedure TViewingPort.SetScale(AScale: Double);
begin
  if not ((AScale = FScale) or (AScale > MaxScale) or (AScale < MinScale)) then
    FScale := AScale;
end;

constructor TViewingPort.Create;
begin
  FScale := 1;
  FViewPosition := FloatPoint(0, 0);
end;

function TViewingPort.WorldToScreen(APoint: TFloatPoint): TPoint;
begin
  Result := Point((APoint - FViewPosition) * FScale + FPortSize / 2);
end;

function TViewingPort.WorldToScreen(APoints: TFloatPoints): TPoints;
var i: integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := WorldToScreen(APoints[i]);
  end;
end;

function TViewingPort.ScreenToWorld(APoint: TPoint): TFloatPoint;
begin
  Result := (APoint + FViewPosition * FScale - FPortSize / 2) / FScale;
end;

procedure TViewingPort.ScaleTo(APoint1, APoint2: TFloatPoint);
var scl: double;
begin
  if (APoint1.X - APoint2.X = 0) or (APoint1.Y - APoint2.Y = 0) then
    exit;
  scl := Min(FPortSize.X / abs(APoint1.X - APoint2.X),
    FPortSize.Y / abs(APoint1.Y - APoint2.Y));
  scl := Min(Max(MinScale, scl), MaxScale);
  FScale := scl;
end;

procedure TViewingPort.RecalculateScroll(APaintBox: TPaintBox; AHorizontalSB,
  AVerticalSB: TScrollBar; ATopLeft, ABottomRight: TFloatPoint);
var
  left, right, top, bottom: double;
  fp1, fp2, WorldSize: TFloatPoint;
  Amplitude: integer;
begin
  {Updating size of viewing port}
  FPortSize := Point(APaintBox.Width, APaintBox.Height);
  {Selecting maximum and minimum points}
  fp1 := ScreenToWorld(Point(0, 0));
  fp2 := ScreenToWorld(FPortSize);
  left := Min(fp1.X, ATopLeft.X);
  top := Min(fp1.Y, ATopLeft.Y);
  right := Max(fp2.X, ABottomRight.X);
  bottom := Max(fp2.Y, ABottomRight.Y);
  {Determining full size of our canvas}
  WorldSize.X := right - left;
  WorldSize.Y := bottom - top;
  if WorldSize.X * WorldSize.Y = 0 then
    exit;
  {Setting an horizontal scrollbox}
  Amplitude := AHorizontalSB.Max - AHorizontalSB.Min;
  AHorizontalSB.PageSize := round(FPortSize.X * Amplitude / (WorldSize.X * FScale));
  if AHorizontalSB.PageSize >= Amplitude then
    AHorizontalSB.Visible := False
  else
  begin
    AHorizontalSB.Visible := True;
    if AHorizontalSB.Position = FHorizontalSBPosition then
      AHorizontalSB.Position := round((FViewPosition.X
        - (left + FPortSize.X / FScale / 2))
        / WorldSize.X * Amplitude)
    else
      FViewPosition.X := AHorizontalSB.Position * WorldSize.X / Amplitude
        + (left + FPortSize.X / FScale / 2);
    FHorizontalSBPosition := AHorizontalSB.Position;
  end;
  {Setting an vertical scrollbox}
  Amplitude := AVerticalSB.Max - AVerticalSB.Min;
  AVerticalSB.PageSize := round(FPortSize.Y * Amplitude / (WorldSize.Y * FScale));
  if AVerticalSB.PageSize >= Amplitude then
    AVerticalSB.Visible := False
  else
  begin
    AVerticalSB.Visible := True;
    if AVerticalSB.Position = FVerticalSBPosition then
      AVerticalSB.Position := round((FViewPosition.Y
        - (top + FPortSize.Y / FScale / 2))
        / WorldSize.Y * Amplitude)
    else
      FViewPosition.Y := AVerticalSB.Position * WorldSize.Y / Amplitude
        + (top + FPortSize.Y / FScale / 2);
    FVerticalSBPosition := AVerticalSB.Position;
  end;
end;

procedure TViewingPort.ScaleMouseWheel(APoint: TPoint; Delta: Integer);
begin
  if not ((FScale <= MinScale) or (Delta < 0)) then
  begin
    FViewPosition := ScreenToWorld(APoint);
    FScale := Max(FScale / 1.25, MinScale);
  end
  else if not ((FScale >= MaxScale) or (Delta > 0)) then
  begin
    FViewPosition := ScreenToWorld(APoint);
    FScale := Min(FScale * 1.25, MaxScale);
  end;
end;

end.

