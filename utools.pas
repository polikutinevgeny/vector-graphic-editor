unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UFigures, UFiguresList;

type

{ TTool }

  TTool = Class
    private
      FIcon: TBitmap;
      FFillable: boolean;
      FPrettyName: string;
    public
      constructor Create; virtual;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        virtual; abstract;
      procedure ContinueDrawing(APoint: TPoint); virtual;
      procedure StopDrawing; virtual;
      procedure ChangePen(APen: TPen); virtual;
      property Icon: TBitmap read FIcon;
      property Fillable: boolean read FFillable;
      property PrettyName: string read FPrettyName;
    end;

type

{ TPenTool }

  TPenTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure ContinueDrawing(APoint: TPoint); override;
    end;

type

  { TLineTool }

  TLineTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TPolylineTool }

  TPolylineTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
      procedure ContinueDrawing(APoint: TPoint); override;
      procedure StopDrawing; override;
      procedure ChangePen(APen: TPen); override;
    private
      FDrawingNow: boolean;
      procedure SetPoint(APoint: TPoint);
  end;

type

  { TRectangleTool }

  TRectangleTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TEllipseTool }

  TEllipseTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

type

  { TRoundRectTool }

  TRoundRectTool = Class(TTool)
    public
      constructor Create; override;
      procedure StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
        override;
  end;

var
  Tools: array[0..5] of TTool;

implementation

{ TTool }

constructor TTool.Create;
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('icons/' + ClassName + '.bmp');
  FFillable := true;
end;

procedure TTool.ContinueDrawing(APoint: TPoint);
begin
  TLine(Figures.Last).MoveSecondPoint(APoint);
end;

procedure TTool.StopDrawing;
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
  FPrettyName := 'Rounded rectangle';
end;

procedure TRoundRectTool.StartDrawing(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  Figures.Add(TRoundRect.Create(APoint, APen, ABrush));
end;

{ TEllipseTool }

constructor TEllipseTool.Create;
begin
  inherited Create;
  FPrettyName := 'Ellipse';
end;

procedure TEllipseTool.StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TEllipse.Create(APoint, APen, ABrush));
end;

{ TRectangleTool }

constructor TRectangleTool.Create;
begin
  inherited Create;
  FPrettyName := 'Rectangle';
end;

procedure TRectangleTool.StartDrawing(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  Figures.Add(TRectangle.Create(APoint, APen, ABrush));
end;

{ TPolylineTool }

constructor TPolylineTool.Create;
begin
  inherited Create;
  FPrettyName := 'Polyline';
  FFillable := false;
end;

procedure TPolylineTool.StartDrawing(APoint: TPoint; APen: TPen;
  ABrush: TBrush);
begin
  if not FDrawingNow then
  begin
    Figures.Add(TPolyline.Create(APoint, APen, ABrush));
    FDrawingNow := true;
  end
  else
    SetPoint(APoint);
end;

procedure TPolylineTool.ContinueDrawing(APoint: TPoint);
begin
  if FDrawingNow then TPolyline(Figures.Last).MoveNextPoint(APoint);
end;

procedure TPolylineTool.StopDrawing;
begin
  FDrawingNow := false;
end;

procedure TPolylineTool.ChangePen(APen: TPen);
begin
  if FDrawingNow then Figures.Last.Pen.Assign(APen);
end;

procedure TPolylineTool.SetPoint(APoint: TPoint);
begin
  TPolyline(Figures.Last).AddPoint(APoint);
end;

{ TLineTool }

constructor TLineTool.Create;
begin
  inherited Create;
  FPrettyName := 'Line';
  FFillable := false;
end;

procedure TLineTool.StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TLine.Create(APoint, APen, ABrush));
end;

{ TPenTool }

constructor TPenTool.Create;
begin
  inherited Create;
  FPrettyName := 'Pencil';
  FFillable := false;
end;

procedure TPenTool.StartDrawing(APoint: TPoint; APen: TPen; ABrush: TBrush);
begin
  Figures.Add(TPencil.Create(APoint, APen, ABrush));
end;

procedure TPenTool.ContinueDrawing(APoint: TPoint);
begin
  TPencil(Figures.Last).AddPoint(APoint);
end;

initialization
  Tools[0] := TPenTool.Create;
  Tools[1] := TLineTool.Create;
  Tools[2] := TPolylineTool.Create;
  Tools[3] := TRectangleTool.Create;
  Tools[4] := TEllipseTool.Create;
  Tools[5] := TRoundRectTool.Create;
end.

