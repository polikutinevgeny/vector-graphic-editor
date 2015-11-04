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
      procedure SetScroll(var APageSize: Integer; var AVisible: Boolean;
        var APosition: Integer; AWSize, APSize, AMin: Double;
        var SBPos: Integer; var AVPos: Double);
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
  sbp, sbps: Integer;
  sbv: Boolean;
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
  sbp := AHorizontalSB.Position;
  sbv := AHorizontalSB.Visible;
  sbps := AHorizontalSB.PageSize;
  SetScroll(sbps, sbv, sbp,
    worldsize.X, FPortSize.X, left, FHorizontalSBPosition, FViewPosition.X);
  AHorizontalSB.Visible := sbv;
  AHorizontalSB.PageSize := sbps;
  AHorizontalSB.Position := sbp;
  sbp := AVerticalSB.Position;
  sbv := AVerticalSB.Visible;
  sbps := AVerticalSB.PageSize;
  SetScroll(sbps, sbv, sbp,
    worldsize.Y, FPortSize.Y, top, FVerticalSBPosition, FViewPosition.Y);
  AVerticalSB.Visible := sbv;
  AVerticalSB.PageSize := sbps;
  AVerticalSB.Position := sbp;
end;

procedure TViewPort.SetScroll(var APageSize: Integer; var AVisible: Boolean;
  var APosition: Integer; AWSize, APSize, AMin: Double; var SBPos: Integer;
  var AVPos: Double);
var
  psshift: double;
begin
  psshift := APSize / FScale / 2;
  APageSize := round(APSize * 1000 / (AWSize * FScale));
  if APageSize >= 1000 then
    AVisible := False
  else
  begin
    AVisible := True;
    if APosition = SBPos then
      APosition := round((AVPos - AMin - psshift) * 1000 / AWSize)
    else
      AVPos := APosition * AWSize / 1000 + AMin + psshift;
    SBPos := APosition;
  end;
end;

procedure TViewPort.ScaleMouseWheel(APoint: TPoint; Delta: Integer);
var mem: TFloatPoint;
begin
  if not ((FScale <= MinScale) or (Delta < 0)) then
  begin
    mem := ScreenToWorld(APoint);
    FViewPosition := ScreenToWorld(APoint);
    FScale := Max(FScale / 1.25, MinScale);
    FViewPosition += mem - ScreenToWorld(APoint);
  end
  else if not ((FScale >= MaxScale) or (Delta > 0)) then
  begin
    mem := ScreenToWorld(APoint);
    FViewPosition := ScreenToWorld(APoint);
    FScale := Min(FScale * 1.25, MaxScale);
    FViewPosition += mem - ScreenToWorld(APoint);
  end;
end;

end.

