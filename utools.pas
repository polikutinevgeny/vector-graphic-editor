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
      FCaption: string;
    public
      constructor Create; virtual;
      procedure MouseClick(APoint: TPoint; APen: TPen; ABrush: TBrush);
        virtual; abstract;
      procedure MouseMove(APoint: TPoint); virtual;
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
      procedure SetPoint(APoint: TPoint);
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

procedure TTool.MouseMove(APoint: TPoint);
begin
  TLine(Figures.Last).MovePoint(APoint);
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
    SetPoint(APoint);
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
  Tools[0] := TPenTool.Create;
  Tools[1] := TLineTool.Create;
  Tools[2] := TPolylineTool.Create;
  Tools[3] := TRectangleTool.Create;
  Tools[4] := TEllipseTool.Create;
  Tools[5] := TRoundRectTool.Create;
end.

