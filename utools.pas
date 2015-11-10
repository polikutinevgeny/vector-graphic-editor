unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UFigures, UFiguresList, UViewPort, UGeometry, UInspector;

type

{ TTool }

  TTool = Class abstract
    private
      FIcon: TBitmap;
      FCaption: string;
    public
      constructor Create; virtual;
      procedure MouseClick(APoint: TPoint); virtual; abstract;
      procedure MouseMove(APoint: TPoint); virtual;
      procedure MouseUp; virtual;
      procedure DoubleClick; virtual;
      function GetParamObject: TObject; virtual; abstract;
      function CreateParamObject: TObject; virtual; abstract;
      property Icon: TBitmap read FIcon;
      property Caption: string read FCaption;
    end;

  { TShapeTool }

  TShapeTool = Class(TTool)
    private
      FShape: TFigure;
    public
      function GetParamObject: TObject; override;
      function CreateParamObject: TObject; override;
      procedure MouseUp; override;
      procedure MouseMove(APoint: TPoint); override;
  end;

{ TPenTool }

  TPenTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
    end;

  { TLineTool }

  TLineTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
  end;

  { TPolylineTool }

  TPolylineTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
      procedure MouseUp; override;
      procedure DoubleClick; override;
    private
      FDrawingNow: boolean;
  end;

  { TRectangleTool }

  TRectangleTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
  end;

  { TEllipseTool }

  TEllipseTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
  end;

  { TRoundRectTool }

  TRoundRectTool = Class(TShapeTool)
    public
      constructor Create; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
  end;

  { TZoomInTool }

  TZoomInTool = Class(TTool)
    public
      constructor Create; override;
      function GetParamObject: TObject; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
  end;

  { TZoomOutTool }

  TZoomOutTool = Class(TTool)
    public
      constructor Create; override;
      function GetParamObject: TObject; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
  end;

  { THandTool }

  THandTool = Class(TTool)
    private
      FStartPoint: TPoint;
    public
      constructor Create; override;
      function GetParamObject: TObject; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
  end;

  { TRectangleZoomTool }

  TRectangleZoomTool = Class(TTool)
    private
      FPointOne: TFloatPoint;
      FPointTwo: TFloatPoint;
    public
      constructor Create; override;
      function GetParamObject: TObject; override;
      function CreateParamObject: TObject; override;
      procedure MouseClick(APoint: TPoint); override;
      procedure MouseMove(APoint: TPoint); override;
      procedure MouseUp; override;
  end;

  { TSelectionTool }

  TSelectionTool = Class(TTool)
  public
    constructor Create; override;
    function GetParamObject: TObject; override;
    function CreateParamObject: TObject; override;
    procedure MouseClick(APoint: TPoint); override;
    procedure MouseMove(APoint: TPoint); override;
    procedure MouseUp; override;
    procedure DoubleClick; override;
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

procedure TSelectionTool.MouseMove(APoint: TPoint);
begin
  Figures.SelectionRectangle.MovePoint(APoint);
  Figures.Select;
end;

procedure TSelectionTool.MouseClick(APoint: TPoint);
begin
  Figures.UnSelect;
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
end;

procedure TSelectionTool.DoubleClick;
begin
  Figures.UnSelect;
  Inspector.ParamsUpdateEvent;
end;

constructor TSelectionTool.Create;
begin
  inherited Create;
  FCaption := 'Selection';
end;

function TSelectionTool.GetParamObject: TObject;
begin
  Result := nil;
end;

function TSelectionTool.CreateParamObject: TObject;
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

function TShapeTool.GetParamObject: TObject;
begin
  Result := FShape;
end;

function TShapeTool.CreateParamObject: TObject;
begin
  Result := FShape;
end;

procedure TShapeTool.MouseUp;
begin
  Inspector.LoadNew(CreateParamObject);
end;

procedure TShapeTool.MouseMove(APoint: TPoint);
begin
  FShape.MovePoint(APoint);
end;

{ TRectangleZoomTool }

constructor TRectangleZoomTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom to area';
end;

function TRectangleZoomTool.GetParamObject: TObject;
begin
  Result := nil;
end;

function TRectangleZoomTool.CreateParamObject: TObject;
begin
  Result := nil;
end;

procedure TRectangleZoomTool.MouseClick(APoint: TPoint);
begin
  Figures.SelectionRectangle := TRectangle.Create;
  Figures.SelectionRectangle.SetPoint(APoint);
  Figures.SelectionRectangle.PenStyle := psDot;
  Figures.SelectionRectangle.BrushStyle := bsClear;
  FPointOne := VP.ScreenToWorld(APoint);
  FPointTwo := VP.ScreenToWorld(APoint);
end;

procedure TRectangleZoomTool.MouseMove(APoint: TPoint);
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

function THandTool.GetParamObject: TObject;
begin
  Result := Nil;
end;

function THandTool.CreateParamObject: TObject;
begin
  Result := Nil;
end;

procedure THandTool.MouseClick(APoint: TPoint);
begin
  FStartPoint := APoint;
end;

procedure THandTool.MouseMove(APoint: TPoint);
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

function TZoomOutTool.GetParamObject: TObject;
begin
  Result := Nil;
end;

function TZoomOutTool.CreateParamObject: TObject;
begin
  Result := Nil;
end;

procedure TZoomOutTool.MouseClick(APoint: TPoint);
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

procedure TZoomOutTool.MouseMove(APoint: TPoint);
begin
  //Nothing to see here, move along
end;

{ TZoomInTool }

constructor TZoomInTool.Create;
begin
  inherited Create;
  FCaption := 'Zoom in';
end;

function TZoomInTool.GetParamObject: TObject;
begin
  Result := Nil;
end;

function TZoomInTool.CreateParamObject: TObject;
begin
  Result := Nil;
end;

procedure TZoomInTool.MouseClick(APoint: TPoint);
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
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.MouseUp;
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

procedure TTool.DoubleClick;
begin
  {Do nothing, because I need it to be called and not to throw exceptions}
end;

{ TRoundRectTool }

constructor TRoundRectTool.Create;
begin
  inherited Create;
  FCaption := 'Rounded rectangle';
end;

function TRoundRectTool.CreateParamObject: TObject;
begin
  FShape := TRoundRect.Create;
  Result := inherited CreateParamObject;
end;

procedure TRoundRectTool.MouseClick(APoint: TPoint);
begin
  Figures.Add(FShape);
  FShape.SetPoint(APoint);
end;

{ TEllipseTool }

constructor TEllipseTool.Create;
begin
  inherited Create;
  FCaption := 'Ellipse';
end;

function TEllipseTool.CreateParamObject: TObject;
begin
  FShape := TEllipse.Create;
  Result := inherited CreateParamObject;
end;

procedure TEllipseTool.MouseClick(APoint: TPoint);
begin
  Figures.Add(FShape);
  FShape.SetPoint(APoint);
end;

{ TRectangleTool }

constructor TRectangleTool.Create;
begin
  inherited Create;
  FCaption := 'Rectangle';
end;

function TRectangleTool.CreateParamObject: TObject;
begin
  FShape := TRectangle.Create;
  Result := inherited CreateParamObject;
end;

procedure TRectangleTool.MouseClick(APoint: TPoint);
begin
  Figures.Add(FShape);
  FShape.SetPoint(APoint);
end;

{ TPolylineTool }

constructor TPolylineTool.Create;
begin
  inherited Create;
  FCaption := 'Polyline';
end;

function TPolylineTool.CreateParamObject: TObject;
begin
  FShape := TPolyline.Create;
  Result := inherited CreateParamObject;
end;

procedure TPolylineTool.MouseClick(APoint: TPoint);
begin
  if not FDrawingNow then
  begin
    Figures.Add(FShape);
    FShape.SetPoint(APoint);
    FDrawingNow := true;
  end
  else
    TPolyline(FShape).AddPoint(APoint);
end;

procedure TPolylineTool.MouseMove(APoint: TPoint);
begin
  if FDrawingNow then FShape.MovePoint(APoint);
end;

procedure TPolylineTool.MouseUp;
begin
  {Do not create new shape}
end;

procedure TPolylineTool.DoubleClick;
begin
  FDrawingNow := false;
  Inspector.LoadNew(CreateParamObject);
end;

{ TLineTool }

constructor TLineTool.Create;
begin
  inherited Create;
  FCaption := 'Line';
end;

function TLineTool.CreateParamObject: TObject;
begin
  FShape := TLine.Create;
  Result := inherited CreateParamObject;
end;

procedure TLineTool.MouseClick(APoint: TPoint);
begin
  Figures.Add(FShape);
  FShape.SetPoint(APoint);
end;

{ TPenTool }

constructor TPenTool.Create;
begin
  inherited Create;
  FCaption := 'Pencil';
end;

function TPenTool.CreateParamObject: TObject;
begin
  FShape := TPolyline.Create;
  Result := inherited CreateParamObject;
end;

procedure TPenTool.MouseClick(APoint: TPoint);
begin
  Figures.Add(FShape);
  FShape.SetPoint(APoint);
end;

procedure TPenTool.MouseMove(APoint: TPoint);
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

