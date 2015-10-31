unit UGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, math;

type
  TFloatPoint = record
    X, Y: double;
  end;

type
  TFloatRect = record
    Left, Right, Top, Bottom: double;
  end;

function FloatPoint(X, Y: Double): TFloatPoint;
function FloatPoint(APoint: TPoint): TFloatPoint;
function Point(AFloatPoint: TFloatPoint): TPoint; overload;
function Rect(a, b: TPoint): TRect; overload;
function FloatRect(a, b: TFloatPoint): TFloatRect;
operator + (a, b: TFloatPoint): TFloatPoint;
operator - (a, b: TFloatPoint): TFloatPoint;
operator + (a, b: TPoint): TPoint;
operator - (a, b: TPoint): TPoint;
operator + (a: TPoint; b: TFloatPoint): TFloatPoint;
operator + (a: TFloatPoint; b: TPoint): TFloatPoint;
operator - (a: TFloatPoint; b: TPoint): TFloatPoint;
operator - (a: TPoint; b: TFloatPoint): TFloatPoint;
operator / (a: TFloatPoint; b: Double): TFloatPoint;
operator * (a: TFloatPoint; b: Double): TFloatPoint;
operator / (a: TPoint; b: Double): TFloatPoint;

type
  TFloatPoints = array of TFloatPoint;

type
  TPoints = array of TPoint;

implementation

function FloatPoint(X, Y: Double): TFloatPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function FloatPoint(APoint: TPoint): TFloatPoint;
begin
  Result.X := APoint.X;
  Result.Y := APoint.Y;
end;

function Point(AFloatPoint: TFloatPoint): TPoint;
begin
  Result.X := round(AFloatPoint.X);
  Result.Y := round(AFloatPoint.Y);
end;

function Rect(a, b: TPoint): TRect;
begin
  Result.Left := Min(a.X, b.X);
  Result.Top := Min(a.Y, b.Y);
  Result.Right := Max(a.X, b.X);
  Result.Bottom := Max(a.Y, b.Y);
end;

function FloatRect(a, b: TFloatPoint): TFloatRect;
begin
  Result.Left := Min(a.X, b.X);
  Result.Top := Min(a.Y, b.Y);
  Result.Right := Max(a.X, b.X);
  Result.Bottom := Max(a.Y, b.Y);
end;

operator + (a, b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator - (a, b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator + (a, b: TPoint): TPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator - (a, b: TPoint): TPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator + (a: TPoint; b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator + (a: TFloatPoint; b: TPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator - (a: TFloatPoint; b: TPoint): TFloatPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator - (a: TPoint; b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator / (a: TFloatPoint; b: Double): TFloatPoint;
begin
  Result.X := a.X / b;
  Result.Y := a.Y / b;
end;

operator * (a: TFloatPoint; b: Double): TFloatPoint;
begin
  Result.X := a.X * b;
  Result.Y := a.Y * b;
end;

operator / (a: TPoint; b: Double): TFloatPoint;
begin
  Result.X := a.X / b;
  Result.Y := a.Y / b;
end;

end.

