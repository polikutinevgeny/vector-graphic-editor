unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UShapes, UShapesList, UViewPort, UGeometry, UInspector;

type

{ TTool }

  TTool = Class abstract
    private
      FIcon: TBitmap;
      FCaption: string;
    public
      constructor Create; virtual;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); virtual; abstract;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); virtual;
      procedure MouseUp; virtual;
      procedure Leave; virtual;
      function GetShape: TShape; virtual; abstract;
      function CreateShape: TShape; virtual; abstract;
      property Icon: TBitmap read FIcon;
      property Caption: string read FCaption;
    end;

  { TShapeTool }

  TShapeTool = Class(TTool)
    private
      FShape: TShape;
      FIsTemp: Boolean;
    public
      constructor Create; override;
      function GetShape: TShape; override;
      function CreateShape: TShape; override;
      procedure MouseUp; override;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
      procedure Leave; override;
  end;

{ TPenTool }

  TPenTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
    end;

  { TLineTool }

  TLineTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
  end;

  { TPolylineTool }

  TPolylineTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseUp; override;
      procedure Leave; override;
    private
      FDrawingNow: boolean;
  end;

  { TRectangleTool }

  TRectangleTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
  end;

  { TEllipseTool }

  TEllipseTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
  end;

  { TRoundRectTool }

  TRoundRectTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateShape: TShape; override;
  end;

  { TZoomInTool }

  TZoomInTool = Class(TTool)
    public
      constructor Create; override;
      function GetShape: TShape; override;
      function CreateShape: TShape; override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
  end;

  { TZoomOutTool }

  TZoomOutTool = Class(TTool)
    public
      constructor Create; override;
      function GetShape: TShape; override;
      function CreateShape: TShape; override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
  end;

  { THandTool }

  THandTool = Class(TTool)
    private
      FStartPoint: TPoint;
    public
      constructor Create; override;
      function GetShape: TShape; override;
      function CreateShape: TShape; override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
  end;

  { TRectangleZoomTool }

  TRectangleZoomTool = Class(TTool)
    private
      FPointOne: TFloatPoint;
      FPointTwo: TFloatPoint;
    public
      constructor Create; override;
      function GetShape: TShape; override;
      function CreateShape: TShape; override;
      procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
      procedure MouseUp; override;
  end;

  { TSelectionTool }

  TSelectionTool = Class(TTool)
  private
    FMoveMode: Boolean;
    FStartPoint: TPoint;
  public
    constructor Create; override;
    function GetShape: TShape; override;
    function CreateShape: TShape; override;
    procedure MouseClick(APoint: TPoint; Shift: TShiftState); override;
    procedure MouseMove(APoint: TPoint; Shift: TShiftState); override;
    procedure MouseUp; override;
    procedure Leave; override;
  end;

  ClassOfTool = class of TTool;
  ArrayOfTool = array of TTool;

  { TToolsContainer }

  TToolsContainer = class
    private
      FTools: ArrayOfTool;
    public
      property Tools: ArrayOfTool read FTools;
      procedure RegisterTool(AClass: ClassOfTool);
  end;

var
  ToolContainer: TToolsContainer;

implementation

{ TSelectionTool }

procedure TSelectionTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  if FMoveMode then
  begin
    Figures.ShiftSelected(APoint - FStartPoint);
    FStartPoint := APoint;
    exit;
  end;
  Figures.SelectionRectangle.MovePoint(APoint);
  if ssCtrl in Shift then
    Figures.SwitchSelect
  else
    Figures.Select;
end;

procedure TSelectionTool.MouseClick(APoint: TPoint; Shift: TShiftState);
begin
  if not (ssCtrl in Shift) then
  begin
    if (ssShift in Shift) and Figures.PointOnFigure(APoint) then
    begin
      FMoveMode := True;
      FStartPoint := APoint;
      Exit;
    end;
    Figures.UnSelect;
    Figures.Select(APoint);
  end
  else
    Figures.SwitchSelect(APoint);
  Figures.SelectionRectangle := TRectangle.Create;
  Figures.SelectionRectangle.SetPoint(APoint);
  Figures.SelectionRectangle.PenStyle := psDot;
  Figures.SelectionRectangle.BrushStyle := bsClear;
end;

procedure TSelectionTool.MouseUp;
begin
  Figures.LoadSelected;
  Figures.SelectionRectangle.Free;
  Figures.SelectionRectangle := nil;
  FMoveMode := False;
end;

procedure TSelectionTool.Leave;
begin
  Figures.UnSelect;
  Inspector.OnParamsUpdate;
  FMoveMode := False;
end;

constructor TSelectionTool.Create;
begin
  inherited Create;
  FCaption := 'Selection';
  FMoveMode := False;
end;

function TSelectionTool.GetShape: TShape;
begin
  Result := nil;
end;

function TSelectionTool.CreateShape: TShape;
begin
  Result := nil;
end;

{ TToolsContainer }

procedure TToolsContainer.RegisterTool(AClass: ClassOfTool);
begin
  SetLength(FTools, Length(FTools) + 1);
  FTools[High(FTools)] := AClass.Create;
end;

{ TShapeTool }

constructor TShapeTool.Create;
begin
  inherited Create;
  FIsTemp := True;
end;

function TShapeTool.GetShape: TShape;
begin
  Result := FShape;
end;

function TShapeTool.CreateShape: TShape;
begin
  FIsTemp := True;
  Result := FShape;
end;

procedure TShapeTool.MouseUp;
begin
  Inspector.LoadNew(CreateShape);
end;

procedure TShapeTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  FShape.MovePoint(APoint);
end;

procedure TShapeTool.MouseClick(APoint: TPoint; Shift: TShiftState);
begin
  Figures.Add(FShape);
  FIsTemp := False;
  FShape.SetPoint(APoint);
end;

procedure TShapeTool.Leave;
begin
  if FIsTemp then
    FShape.Free;
end;

{ TRectangleZoomTool }

constructor TRectangleZoomTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom to area';
end;

function TRectangleZoomTool.GetShape: TShape;
begin
  Result := nil;
end;

function TRectangleZoomTool.CreateShape: TShape;
begin
  Result := nil;
end;

procedure TRectangleZoomTool.MouseClick(APoint: TPoint; Shift: TShiftState);
begin
  Figures.SelectionRectangle := TRectangle.Create;
  Figures.SelectionRectangle.SetPoint(APoint);
  Figures.SelectionRectangle.PenStyle := psDot;
  Figures.SelectionRectangle.BrushStyle := bsClear;
  FPointOne := VP.ScreenToWorld(APoint);
  FPointTwo := VP.ScreenToWorld(APoint);
end;

procedure TRectangleZoomTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  Figures.SelectionRectangle.MovePoint(APoint);
  FPointTwo := VP.ScreenToWorld(APoint);
end;

procedure TRectangleZoomTool.MouseUp;
begin
  if not Figures.IsEmpty then
  begin
    VP.ViewPosition := (FPointOne + FPointTwo) / 2;
    VP.ScaleTo(FloatRect(FPointOne, FPointTwo));
  end;
  Figures.SelectionRectangle.Free;
  Figures.SelectionRectangle := nil;
end;

{ THandTool }

constructor THandTool.Create;
begin
  inherited Create;
  FCaption := 'Hand';
end;

function THandTool.GetShape: TShape;
begin
  Result := Nil;
end;

function THandTool.CreateShape: TShape;
begin
  Result := Nil;
end;

procedure THandTool.MouseClick(APoint: TPoint; Shift: TShiftState);
begin
  FStartPoint := APoint;
end;

procedure THandTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  if not Figures.IsEmpty then
    VP.ViewPosition := VP.ViewPosition + (FStartPoint - APoint) / VP.Scale;
  FStartPoint := APoint;
end;

{ TZoomOutTool }

constructor TZoomOutTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom out';
end;

function TZoomOutTool.GetShape: TShape;
begin
  Result := Nil;
end;

function TZoomOutTool.CreateShape: TShape;
begin
  Result := Nil;
end;

procedure TZoomOutTool.MouseClick(APoint: TPoint; Shift: TShiftState);
var mem: TFloatPoint;
begin
  if not Figures.IsEmpty then
  begin
    mem := VP.ScreenToWorld(APoint);
    VP.ViewPosition := VP.ScreenToWorld(APoint);
    VP.Scale := VP.Scale - 0.25;
    VP.ViewPosition := VP.ViewPosition + mem - VP.ScreenToWorld(APoint);
  end;
end;

{ TZoomInTool }

constructor TZoomInTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom in';
end;

function TZoomInTool.GetShape: TShape;
begin
  Result := Nil;
end;

function TZoomInTool.CreateShape: TShape;
begin
  Result := Nil;
end;

procedure TZoomInTool.MouseClick(APoint: TPoint; Shift: TShiftState);
var mem: TFloatPoint;
begin
  if not Figures.IsEmpty then
  begin
    mem := VP.ScreenToWorld(APoint);
    VP.ViewPosition := VP.ScreenToWorld(APoint);
    VP.Scale := VP.Scale + 0.25;
    VP.ViewPosition := VP.ViewPosition + mem - VP.ScreenToWorld(APoint);
  end;
end;

{ TTool }

constructor TTool.Create;
begin
  FIcon := TBitmap.Create;
  FIcon.LoadFromFile('icons/' + ClassName + '.bmp');
end;

procedure TTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.MouseUp;
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.Leave;
begin
  {This procedure is called on leaving the tool and does nothing by default}
end;

{ TRoundRectTool }

constructor TRoundRectTool.Create;
begin
  inherited Create;
  FCaption := 'Rounded rectangle';
end;

function TRoundRectTool.CreateShape: TShape;
begin
  FShape := TRoundRect.Create;
  Result := inherited CreateShape;
end;

{ TEllipseTool }

constructor TEllipseTool.Create;
begin
  inherited Create;
  FCaption := 'Ellipse';
end;

function TEllipseTool.CreateShape: TShape;
begin
  FShape := TEllipse.Create;
  Result := inherited CreateShape;
end;

{ TRectangleTool }

constructor TRectangleTool.Create;
begin
  inherited Create;
  FCaption := 'Rectangle';
end;

function TRectangleTool.CreateShape: TShape;
begin
  FShape := TRectangle.Create;
  Result := inherited CreateShape;
end;

{ TPolylineTool }

constructor TPolylineTool.Create;
begin
  inherited Create;
  FCaption := 'Polyline';
end;

function TPolylineTool.CreateShape: TShape;
begin
  FShape := TPolyline.Create;
  Result := inherited CreateShape;
end;

procedure TPolylineTool.MouseClick(APoint: TPoint; Shift: TShiftState);
begin
  if ssRight in Shift then
  begin
    FDrawingNow := false;
    Inspector.LoadNew(CreateShape);
    Exit;
  end;
  if not FDrawingNow then
  begin
    inherited;
    FDrawingNow := true;
  end
  else
    TPolyline(FShape).AddPoint(APoint);
end;

procedure TPolylineTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  if FDrawingNow then FShape.MovePoint(APoint);
end;

procedure TPolylineTool.MouseUp;
begin
  {Do not create new shape}
end;

procedure TPolylineTool.Leave;
begin
  FDrawingNow := false;
  inherited;
end;

{ TLineTool }

constructor TLineTool.Create;
begin
  inherited Create;
  FCaption := 'Line';
end;

function TLineTool.CreateShape: TShape;
begin
  FShape := TLine.Create;
  Result := inherited CreateShape;
end;

{ TPenTool }

constructor TPenTool.Create;
begin
  inherited Create;
  FCaption := 'Pencil';
end;

function TPenTool.CreateShape: TShape;
begin
  FShape := TPolyline.Create;
  Result := inherited CreateShape;
end;

procedure TPenTool.MouseMove(APoint: TPoint; Shift: TShiftState);
begin
  TPolyline(FShape).AddPoint(APoint);
end;

initialization
  ToolContainer := TToolsContainer.Create;
  ToolContainer.RegisterTool(TPenTool);
  ToolContainer.RegisterTool(TLineTool);
  ToolContainer.RegisterTool(TPolylineTool);
  ToolContainer.RegisterTool(TRectangleTool);
  ToolContainer.RegisterTool(TEllipseTool);
  ToolContainer.RegisterTool(TRoundRectTool);
  ToolContainer.RegisterTool(TZoomInTool);
  ToolContainer.RegisterTool(TZoomOutTool);
  ToolContainer.RegisterTool(THandTool);
  ToolContainer.RegisterTool(TRectangleZoomTool);
  ToolContainer.RegisterTool(TSelectionTool);
end.

