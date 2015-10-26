unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UFigures, UFiguresList, UZoom, math;

type

{ TTool }

  TTool = Class abstract
    private
      FIcon: TBitmap;
      FFillable: boolean;
      FCaption: string;
    public
      constructor Create; virtual;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        virtual; abstract;
      procedure MouseMove(APoint: TPoint); virtual;
      procedure MouseUp; virtual;
      procedure DoubleClick; virtual;
      procedure ChangePen(APen: TPen); virtual;
      property Icon: TBitmap read FIcon;
      property Fillable: boolean read FFillable;
      property Caption: string read FCaption;
    end;

type

{ TPenTool }

  TPenTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
    end;

type

  { TLineTool }

  TLineTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TPolylineTool }

  TPolylineTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
      procedure DoubleClick; override;
      procedure ChangePen(APen: TPen); override;
    private
      FDrawingNow: boolean;
  end;

type

  { TRectangleTool }

  TRectangleTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TEllipseTool }

  TEllipseTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TRoundRectTool }

  TRoundRectTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TZoomInTool }

  TZoomInTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
  end;

type

  { TZoomOutTool }

  TZoomOutTool = Class(TTool)
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
  end;

type

  { THandTool }

  THandTool = Class(TTool)
    private
      FStartPoint: TPoint;
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
  end;

type

  { TRectangleZoomTool }

  TRectangleZoomTool = Class(TTool)
    private
      FPointOne: TFloatPoint;
      FPointTwo: TFloatPoint;
    public
      constructor Create; override;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure MouseMove(APoint: TPoint); override;
      procedure MouseUp; override;
  end;

var
  Tools: array of TTool;

implementation

procedure RegisterTool(ATool: TTool);
begin
  SetLength(Tools, Length(Tools) + 1);
  Tools[High(Tools)] := ATool;
end;

{ TRectangleZoomTool }

constructor TRectangleZoomTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom to area';
  FFillable := False;
end;

procedure TRectangleZoomTool.MouseClick(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
var
  p: TPen;
  b: TBrush;
begin
  p := TPen.Create;
  p.Color := clBlue;
  p.Style := psDot;
  b := TBrush.Create;
  b.Color := clYellow;
  b.Style := bsDiagCross;
  Figures.ZoomRectangle := TRectangle.Create(APoint, p, b);
  FPointOne := ViewingPort.ScreenToWorld(APoint);
  FPointTwo := ViewingPort.ScreenToWorld(APoint);
end;

procedure TRectangleZoomTool.MouseMove(APoint: TPoint);
begin
  Figures.ZoomRectangle.MovePoint(APoint);
  FPointTwo := ViewingPort.ScreenToWorld(APoint);
end;

procedure TRectangleZoomTool.MouseUp;
begin
  ViewingPort.ViewPosition := FloatPoint((FPointOne.X + FPointTwo.X) / 2,
    (FPointOne.Y + FPointTwo.Y) / 2);
  ViewingPort.ScaleTo(FPointOne, FPointTwo);
  Figures.ZoomRectangle.Free;
  Figures.ZoomRectangle := nil;
end;

{ THandTool }

constructor THandTool.Create;
begin
  inherited Create;
  FCaption := 'Hand';
  FFillable := False;
end;

procedure THandTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  FStartPoint := APoint;
end;

procedure THandTool.MouseMove(APoint: TPoint);
var d: TPoint;
begin
  d.X := FStartPoint.X - APoint.X;
  d.Y := FStartPoint.Y - APoint.Y;
  ViewingPort.MovePosition(d);
  FStartPoint := APoint;
end;

{ TZoomOutTool }

constructor TZoomOutTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom out';
  FFillable := False;
end;

procedure TZoomOutTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  ViewingPort.ViewPosition := ViewingPort.ScreenToWorld(APoint);
  ViewingPort.Scale := ViewingPort.Scale - 0.25;
end;

procedure TZoomOutTool.MouseMove(APoint: TPoint);
begin
  //Nothing to see here, move along
end;

{ TZoomInTool }

constructor TZoomInTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom in';
  FFillable := False;
end;

procedure TZoomInTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  ViewingPort.ViewPosition := ViewingPort.ScreenToWorld(APoint);
  ViewingPort.Scale := ViewingPort.Scale + 0.25;
end;

procedure TZoomInTool.MouseMove(APoint: TPoint);
begin
  //Nothing to see here, move along
end;

{ TTool }

constructor TTool.Create;
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('icons/' + ClassName + '.bmp');
end;

procedure TTool.MouseMove(APoint: TPoint);
begin
  Figures.Last.MovePoint(APoint);
end;

procedure TTool.MouseUp;
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.DoubleClick;
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.ChangePen(APen: TPen);
begin
  {The same thing as above, but I think I will need it in the near future}
end;

{ TRoundRectTool }

constructor TRoundRectTool.Create;
begin
  inherited Create;
  FCaption := 'Rounded rectangle';
  FFillable := true;
end;

procedure TRoundRectTool.MouseClick(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  Figures.Add(TRoundRect.Create(APoint, APen, ABrush));
end;

{ TEllipseTool }

constructor TEllipseTool.Create;
begin
  inherited Create;
  FCaption := 'Ellipse';
  FFillable := true;
end;

procedure TEllipseTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TEllipse.Create(APoint, APen, ABrush));
end;

{ TRectangleTool }

constructor TRectangleTool.Create;
begin
  inherited Create;
  FCaption := 'Rectangle';
  FFillable := true;
end;

procedure TRectangleTool.MouseClick(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  Figures.Add(TRectangle.Create(APoint, APen, ABrush));
end;

{ TPolylineTool }

constructor TPolylineTool.Create;
begin
  inherited Create;
  FCaption := 'Polyline';
  FFillable := false;
end;

procedure TPolylineTool.MouseClick(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  if not FDrawingNow then
  begin
    Figures.Add(TPolyline.Create(APoint, APen, ABrush));
    FDrawingNow := true;
  end
  else
    TPolyline(Figures.Last).AddPoint(APoint);
end;

procedure TPolylineTool.MouseMove(APoint: TPoint);
begin
  if FDrawingNow then TPolyline(Figures.Last).MovePoint(APoint);
end;

procedure TPolylineTool.DoubleClick;
begin
  FDrawingNow := false;
end;

procedure TPolylineTool.ChangePen(APen: TPen);
begin
  if FDrawingNow then TPolyline(Figures.Last).Pen.Assign(APen);
end;

{ TLineTool }

constructor TLineTool.Create;
begin
  inherited Create;
  FCaption := 'Line';
  FFillable := false;
end;

procedure TLineTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TLine.Create(APoint, APen, ABrush));
end;

{ TPenTool }

constructor TPenTool.Create;
begin
  inherited Create;
  FCaption := 'Pencil';
  FFillable := false;
end;

procedure TPenTool.MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TPolyline.Create(APoint, APen, ABrush));
end;

procedure TPenTool.MouseMove(APoint: TPoint);
begin
  TPolyline(Figures.Last).AddPoint(APoint);
end;

initialization
  RegisterTool(TPenTool.Create);
  RegisterTool(TLineTool.Create);
  RegisterTool(TPolylineTool.Create);
  RegisterTool(TRectangleTool.Create);
  RegisterTool(TEllipseTool.Create);
  RegisterTool(TRoundRectTool.Create);
  RegisterTool(TZoomInTool.Create);
  RegisterTool(TZoomOutTool.Create);
  RegisterTool(THandTool.Create);
  RegisterTool(TRectangleZoomTool.Create);
end.

