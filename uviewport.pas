unit UViewPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, StdCtrls, ExtCtrls, Math, UGeometry;

type

  { TViewPort }

  TViewPort = class
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
      property PortSize: TPoint read FPortSize;
      constructor Create;
      function WorldToScreen(APoint: TFloatPoint): TPoint;
      function WorldToScreen(APoints: TFloatPoints): TPoints; overload;
      function ScreenToWorld(APoint: TPoint): TFloatPoint;
      procedure ScaleTo(ARect: TFloatRect);
      procedure RecalculateScroll(APaintBox: TPaintBox; AHorizontalSB,
        AVerticalSB: TScrollBar; ARect: TFloatRect);
      procedure ScaleMouseWheel(APoint: TPoint; Delta: Integer);
  end;

var
  VP: TViewPort;

implementation

{ TViewPort }

procedure TViewPort.SetScale(AScale: Double);
begin
  if (AScale <> FScale) and InRange(AScale, MinScale, MaxScale) then
    FScale := AScale;
end;

constructor TViewPort.Create;
begin
  FScale := 1;
  FViewPosition := FloatPoint(0, 0);
end;

function TViewPort.WorldToScreen(APoint: TFloatPoint): TPoint;
begin
  Result := Point((APoint - FViewPosition) * FScale + FPortSize / 2);
end;

function TViewPort.WorldToScreen(APoints: TFloatPoints): TPoints;
var i: integer;
begin
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i] := WorldToScreen(APoints[i]);
  end;
end;

function TViewPort.ScreenToWorld(APoint: TPoint): TFloatPoint;
begin
  Result := (APoint + FViewPosition * FScale - FPortSize / 2) / FScale;
end;

procedure TViewPort.ScaleTo(ARect: TFloatRect);
var scl: double;
begin
  if (ARect.Left = ARect.Right) or (ARect.Top = ARect.Bottom) then
    exit;
  scl := Min(
    FPortSize.X / abs(ARect.Left - ARect.Right),
    FPortSize.Y / abs(ARect.Top - ARect.Bottom));
  FScale := EnsureRange(scl, MinScale, MaxScale);
end;

procedure TViewPort.RecalculateScroll(APaintBox: TPaintBox; AHorizontalSB,
  AVerticalSB: TScrollBar; ARect: TFloatRect);
var
  left, right, top, bottom: double;
  fp1, fp2, worldsize: TFloatPoint;
  amplitude: integer;
begin
  {Updating size of viewing port}
  FPortSize := Point(APaintBox.Width, APaintBox.Height);
  {Selecting maximum and minimum points}
  fp1 := ScreenToWorld(Point(0, 0));
  fp2 := ScreenToWorld(FPortSize);
  left := Min(fp1.X, ARect.Left);
  top := Min(fp1.Y, ARect.Top);
  right := Max(fp2.X, ARect.Right);
  bottom := Max(fp2.Y, ARect.Bottom);
  {Determining full size of our canvas}
  worldsize.X := right - left;
  worldsize.Y := bottom - top;
  if (worldsize.X = 0) or (worldsize.Y = 0) then
    exit;
  {Setting an horizontal scrollbox}
  amplitude := AHorizontalSB.Max - AHorizontalSB.Min;
  AHorizontalSB.PageSize := round(FPortSize.X * amplitude / (worldsize.X * FScale));
  if AHorizontalSB.PageSize >= amplitude then
    AHorizontalSB.Visible := False
  else
  begin
    AHorizontalSB.Visible := True;
    if AHorizontalSB.Position = FHorizontalSBPosition then
      AHorizontalSB.Position := round(
        (FViewPosition.X - (left + FPortSize.X / FScale / 2))
        * amplitude / worldsize.X)
    else
      FViewPosition.X := AHorizontalSB.Position * worldsize.X / amplitude
        + (left + FPortSize.X / FScale / 2);
    FHorizontalSBPosition := AHorizontalSB.Position;
  end;
  {Setting an vertical scrollbox}
  amplitude := AVerticalSB.Max - AVerticalSB.Min;
  AVerticalSB.PageSize := round(FPortSize.Y * amplitude / (worldsize.Y * FScale));
  if AVerticalSB.PageSize >= amplitude then
    AVerticalSB.Visible := False
  else
  begin
    AVerticalSB.Visible := True;
    if AVerticalSB.Position = FVerticalSBPosition then
      AVerticalSB.Position := round((FViewPosition.Y
        - (top + FPortSize.Y / FScale / 2))
        / worldsize.Y * amplitude)
    else
      FViewPosition.Y := AVerticalSB.Position * worldsize.Y / amplitude
        + (top + FPortSize.Y / FScale / 2);
    FVerticalSBPosition := AVerticalSB.Position;
  end;
end;

procedure TViewPort.ScaleMouseWheel(APoint: TPoint; Delta: Integer);
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

