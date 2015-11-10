unit UViewPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Math, UGeometry;

type
  TScrollBarType = (sbHorizontal, sbVertical);

  TScrollUpdateEvent = procedure(AVisible: Boolean; APageSize,
    APosition: Integer; AKind: TScrollBarType) of object;

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
      FScrollUpdateEvent: TScrollUpdateEvent;
      procedure SetScale(AScale: Double);
    public
      property ViewPosition: TFloatPoint read FViewPosition
        write FViewPosition;
      property Scale: Double read FScale write SetScale;
      property PortSize: TPoint read FPortSize write FPortSize;
      property ScrollUpdateEvent: TScrollUpdateEvent read FScrollUpdateEvent
        write FScrollUpdateEvent;
      constructor Create;
      function WorldToScreen(APoint: TFloatPoint): TPoint;
      function WorldToScreen(APoints: TFloatPoints): TPoints; overload;
      function WorldToScreen(AFloatRect: TFloatRect): TRect; overload;
      function ScreenToWorld(APoint: TPoint): TFloatPoint;
      function ScreenToWorld(ARect: TRect): TFloatRect; overload;
      procedure ScaleTo(ARect: TFloatRect);
      procedure SetScroll(APosition: Integer; AWSize, AMin: Double;
        AKind: TScrollBarType);
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

function TViewPort.WorldToScreen(AFloatRect: TFloatRect): TRect;
begin
  Result := Rect(
    WorldToScreen(FloatPoint(AFloatRect.Left, AFloatRect.Top)),
    WorldToScreen(FloatPoint(AFloatRect.Right, AFloatRect.Bottom)));
end;

function TViewPort.ScreenToWorld(APoint: TPoint): TFloatPoint;
begin
  Result := (APoint + FViewPosition * FScale - FPortSize / 2) / FScale;
end;

function TViewPort.ScreenToWorld(ARect: TRect): TFloatRect;
begin
  Result := FloatRect(
    ScreenToWorld(Point(ARect.Left, ARect.Top)),
    ScreenToWorld(Point(ARect.Right, ARect.Bottom)));
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

procedure TViewPort.SetScroll(APosition: Integer; AWSize, AMin: Double;
  AKind: TScrollBarType);
var
  psshift: Double;
  psize, pagesize: Integer;
  visible: Boolean;
  sbpos: ^Integer;
  vpos: ^Double;
begin
  if AKind = sbHorizontal then
  begin
    psize := FPortSize.X;
    sbpos := @FHorizontalSBPosition;
    vpos := @FViewPosition.X;
  end
  else
  begin
    psize := FPortSize.Y;
    sbpos := @FVerticalSBPosition;
    vpos := @FViewPosition.Y;
  end;
  psshift := psize / FScale / 2;
  pagesize := round(psize * 10000 / (AWSize * FScale));
  if pagesize >= 10000 then
    visible := False
  else
  begin
    visible := True;
    {Updating scrollbar position when it is not moved, else updating view position}
    if APosition = sbpos^ then
      APosition := round((vpos^ - AMin - psshift) * 10000 / AWSize)
    else
      vpos^ := APosition * AWSize / 10000 + AMin + psshift;
    sbpos^ := APosition;
  end;
  FScrollUpdateEvent(visible, pagesize, APosition, AKind);
end;

procedure TViewPort.ScaleMouseWheel(APoint: TPoint; Delta: Integer);
var mem: TFloatPoint;
begin
  if not ((FScale <= MinScale) or (Delta > 0)) then
  begin
    mem := ScreenToWorld(APoint);
    FViewPosition := ScreenToWorld(APoint);
    FScale := Max(FScale / 1.25, MinScale);
    FViewPosition += mem - ScreenToWorld(APoint);
  end
  else if not ((FScale >= MaxScale) or (Delta < 0)) then
  begin
    mem := ScreenToWorld(APoint);
    FViewPosition := ScreenToWorld(APoint);
    FScale := Min(FScale * 1.25, MaxScale);
    FViewPosition += mem - ScreenToWorld(APoint);
  end;
end;

end.

