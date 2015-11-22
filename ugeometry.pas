unit UGeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, math;

type
  TFloatPoint = record
    X, Y: Double;
  end;

type
  TFloatRect = record
    Left, Right, Top, Bottom: Double;
  end;

function FloatPoint(X, Y: Double): TFloatPoint;
function FloatPoint(APoint: TPoint): TFloatPoint;
function Point(AFloatPoint: TFloatPoint): TPoint; overload;
function Rect(a, b: TPoint): TRect; overload;
function FloatRect(a, b: TFloatPoint): TFloatRect;
function Intersection(a, b, c, d: TPoint): Boolean;
function Intersection(ARect: TRect; p1, p2: TPoint): Boolean; overload;
function Intersection(ARect1, ARect2: TRect): Boolean; overload;
function IsPointIn(p, p1, p2: TPoint): Boolean;
function IsPointIn(p: TPoint; rect: TRect): Boolean; overload;
function IsRectIn(rect1, rect2: TRect): Boolean;
function CircleSegmentIntersection(p1, p2, center: TPoint; r: Integer): Boolean;
operator +(a, b: TFloatPoint): TFloatPoint;
operator -(a, b: TFloatPoint): TFloatPoint;
operator +(a, b: TPoint): TPoint;
operator -(a, b: TPoint): TPoint;
operator +(a: TPoint; b: TFloatPoint): TFloatPoint;
operator +(a: TFloatPoint; b: TPoint): TFloatPoint;
operator -(a: TFloatPoint; b: TPoint): TFloatPoint;
operator -(a: TPoint; b: TFloatPoint): TFloatPoint;
operator /(a: TFloatPoint; b: Double): TFloatPoint;
operator *(a: TFloatPoint; b: Double): TFloatPoint;
operator /(a: TPoint; b: Double): TFloatPoint;

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

function Intersection(a, b, c, d: TPoint): Boolean;

  procedure swap(var a, b: Integer);
  var t: Integer;
  begin
    t := a;
    a := b;
    b := t;
  end;

  function area(a, b, c: TPoint): Integer;
  begin
    Result := (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
  end;

  function intersect(a, b, c, d: Integer): Boolean;
  begin
    if (a > b) then
      swap(a, b);
    if (c > d) then
      swap(c, d);
    Result :=  Max(a, c) <= Min(b, d);
  end;

begin
  Result := intersect(a.x, b.x, c.x, d.x) and
            intersect(a.y, b.y, c.y, d.y) and
	    (area(a,b,c) * area(a,b,d) <= 0) and
	    (area(c,d,a) * area(c,d,b) <= 0);
end;

function Intersection(ARect: TRect; p1, p2: TPoint): Boolean;
begin
  Result := IsPointIn(p1, ARect) or IsPointIn(p2, ARect);
  if Result then
    exit;
  Result :=
    Intersection(Point(ARect.Left, ARect.Top), Point(ARect.Right, ARect.Top), p1, p2) or
    Intersection(Point(ARect.Left, ARect.Bottom), Point(ARect.Right, ARect.Bottom), p1, p2) or
    Intersection(Point(ARect.Left, ARect.Top), Point(ARect.Left, ARect.Bottom), p1, p2) or
    Intersection(Point(ARect.Right, ARect.Top), Point(ARect.Right, ARect.Bottom), p1, p2);
end;

function Intersection(ARect1, ARect2: TRect): Boolean;
begin
  Result := IsRectIn(ARect1, ARect2) or IsRectIn(ARect2, ARect1);
  if Result then
    exit;
  Result :=
    Intersection(ARect2, Point(ARect1.Left, ARect1.Top), Point(ARect1.Right, ARect1.Top)) or
    Intersection(ARect2, Point(ARect1.Left, ARect1.Bottom), Point(ARect1.Right, ARect1.Bottom)) or
    Intersection(ARect2, Point(ARect1.Left, ARect1.Top), Point(ARect1.Left, ARect1.Bottom)) or
    Intersection(ARect2, Point(ARect1.Right, ARect1.Top), Point(ARect1.Right, ARect1.Bottom));
end;

function IsPointIn(p, p1, p2: TPoint): Boolean;
begin
  Result :=
    (p.X <= Max(p1.x, p2.x)) and (p.X >= Min(p1.x, p2.x)) and
    (p.Y <= Max(p1.y, p2.y)) and (p.Y >= Min(p1.y, p2.y));
end;

function IsPointIn(p: TPoint; rect: TRect): Boolean;
begin
  Result := IsPointIn(p, Point(rect.Left,rect.Top),
    Point(rect.Right, rect.Bottom));
end;

function IsRectIn(rect1, rect2: TRect): Boolean;
begin
  Result :=
    IsPointIn(Point(rect1.left, rect1.top), rect2) or
    IsPointIn(Point(rect1.right, rect1.top), rect2) or
    IsPointIn(Point(rect1.left, rect1.bottom), rect2) or
    IsPointIn(Point(rect1.right, rect1.bottom), rect2);
end;

function CircleSegmentIntersection(p1, p2, center: TPoint; r: Integer): Boolean;
var
  x1, y1, x2, y2, dx, dy, a, b, c: Double;
begin
  x1 := p1.X - center.X;
  x2 := p2.X - center.X;
  y1 := p1.Y - center.Y;
  y2 := p2.Y - center.Y;
  dx := x2 - x1;
  dy := y2 - y1;
  a := dx * dx + dy * dy;
  b := 2 * (x1 * dx + y1 * dy);
  c := x1 * x1 + y1 * y1 - R * R;
  if -b < 0 then
  begin
    Result := c < 0;
    exit;
  end;
  if -b < (2 * a) then
  begin
    Result := (4 * a * c - b * b) < 0;
    exit;
  end;
  Result := a + b + c < 0;
end;

operator +(a, b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator -(a, b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator +(a, b: TPoint): TPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator -(a, b: TPoint): TPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator +(a: TPoint; b: TFloatPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator +(a: TFloatPoint; b: TPoint): TFloatPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

operator -(a: TFloatPoint; b: TPoint): TFloatPoint;
begin
  Result.X := a.X - b.X;
  Result.Y := a.Y - b.Y;
end;

operator -(a: TPoint; b: TFloatPoint): TFloatPoint;
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
