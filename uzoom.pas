unit UZoom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, StdCtrls, ExtCtrls, Math;

type
  TFloatPoint = record
    X, Y: double;
  end;

type
  TFloatPoints = array of TFloatPoint;

type
  TPoints = array of TPoint;

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
      procedure SetScale(AScale: Double);
    public
      property ViewPosition: TFloatPoint read FViewPosition
        write FViewPosition;
      property Scale: Double read FScale write SetScale;
      property PortSize: TPoint read FPortSize write FPortSize;
      constructor Create;
      function WorldToScreen(APoint: TFloatPoint): TPoint;
      function WorldToScreen(APoints: TFloatPoints): TPoints; overload;
      function ScreenToWorld(APoint: TPoint): TFloatPoint;
      procedure MovePosition(APoint: TPoint);
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
  FViewPosition.X := 0;
  FViewPosition.Y := 0;
end;

function TViewingPort.WorldToScreen(APoint: TFloatPoint): TPoint;
begin
  Result.X := round((APoint.X - FViewPosition.X) * FScale + PortSize.X / 2);
  Result.Y := round((APoint.Y - FViewPosition.Y) * FScale + PortSize.Y / 2);
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
  Result.X := (APoint.X + FViewPosition.X * FScale - PortSize.X / 2) / FScale;
  Result.Y := (APoint.Y + FViewPosition.Y * FScale - PortSize.Y / 2) / FScale;
end;

procedure TViewingPort.MovePosition(APoint: TPoint);
begin
  FViewPosition.X += APoint.X / FScale;
  FViewPosition.Y += APoint.Y / FScale;
end;

end.

