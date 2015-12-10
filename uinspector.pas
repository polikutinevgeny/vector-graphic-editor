unit UInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, typinfo, Graphics, ExtCtrls, StdCtrls, UBaseShape, UGeometry;

type

  TParamsUpdateEvent = procedure of object;

  { TParamEditor }

  TParamEditor = class
    protected
      FShapes: array of TShape;
      FLabel: TLabel;
      FPropInfo: PPropInfo;
      procedure Change(Sender: TObject); virtual;
    public
      constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); virtual;
      destructor Destroy; override;
  end;

  { TInspector }

  TInspector = class
    private
      FShapes: array of TShape;
      FEditors: array of TParamEditor;
      FPenColor, FBrushColor: TColor;
      FDefaultParams: Boolean;
      FPanel: TPanel;
      FParamsUpdateEvent: TParamsUpdateEvent;
      function SearchPropertyInObject(AShape: TShape; AProperty: PPropInfo): Boolean;
      function CheckPropertyInAllObjects(AShapes: array of TShape; AProperty: PPropInfo): Boolean;
    public
      property OnParamsUpdate: TParamsUpdateEvent read FParamsUpdateEvent
        write FParamsUpdateEvent;
      constructor Create(APanel: TPanel);
      procedure LoadNew(AShape: TShape);
      procedure Load(AShapes: array of TShape);
      procedure Clean;
  end;

  TParamEditorClass = class of TParamEditor;

  { ArrayOfEditor }

  TArrayOfEditor = array of record
    Item: TParamEditorClass;
    Kind: String;
  end;

  { TEditorContainer }

  TEditorContainer = class
  private
    FEditors: TArrayOfEditor;
  public
    procedure RegisterEditor(AClass: TParamEditorClass; AKind: ShortString);
    property Editors: TArrayOfEditor read FEditors;
  end;

var
  Inspector: TInspector;
  EditorContainer: TEditorContainer;
  ShiftEditorContainer: TEditorContainer;

implementation

var
  PropNames : TStringList;

{ TEditorContainer }

procedure TEditorContainer.RegisterEditor(AClass: TParamEditorClass;
  AKind: ShortString);
begin
  SetLength(FEditors, Length(FEditors) + 1);
  FEditors[High(FEditors)].Item := AClass;
  FEditors[High(FEditors)].Kind := AKind;
end;

{ TInspector }

constructor TInspector.Create(APanel: TPanel);
begin
  FPanel:= APanel;
  FPenColor:= clBlack;
  FBrushColor:= clWhite;
end;

procedure TInspector.LoadNew(AShape: TShape);
var
  shapes: array of TShape;
begin
  if AShape = nil then begin
    Clean;
    exit;
  end;
  FDefaultParams := true;
  SetLength(shapes, 1);
  shapes[0] := AShape;
  Load(shapes);
end;

function TInspector.SearchPropertyInObject(AShape: TShape;
  AProperty: PPropInfo): Boolean;
var
  i, j: integer;
  list: PPropList;
begin
  j := GetPropList(AShape, list);
  Result := false;
  for i := 0 to j - 1 do
    if list^[i] = AProperty then
    begin
      Result := true;
      exit;
    end;
end;

function TInspector.CheckPropertyInAllObjects(AShapes: array of TShape;
  AProperty: PPropInfo): Boolean;
var
  j: Integer;
begin
  Result := True;
  for j := 1 to High(AShapes) do
    begin
      Result := SearchPropertyInObject(AShapes[j], AProperty);
      if not Result then
        exit;
    end;
end;

procedure TInspector.Load(AShapes: array of TShape);
var
  list: PPropList;
  i, j, k: integer;
begin
  Clean;
  if Length(AShapes) = 0 then
    Exit;
  SetLength(FShapes, Length(AShapes));
  for i := 0 to High(AShapes) do
    FShapes[i] := AShapes[i];
  FPanel.Tag := 10;
  j := GetPropList(FShapes[0], list);
  for i := 0 to j - 1 do
  begin
    if CheckPropertyInAllObjects(FShapes, list^[i]) then
    begin
      for k := 0 to High(EditorContainer.Editors) do
        if list^[i]^.PropType^.Name = EditorContainer.Editors[k].Kind then
        begin
          SetLength(FEditors, Length(FEditors) + 1);
          FEditors[High(FEditors)] := EditorContainer.Editors[k].Item.Create(
            FShapes, list^[i], FPanel, FDefaultParams);
          Break;
        end;
    end;
  end;
  if Length(FShapes) > 1 then
    for i := 0 to j - 1 do
    begin
      for k := 0 to High(ShiftEditorContainer.Editors) do
        if list^[i]^.PropType^.Name = ShiftEditorContainer.Editors[k].Kind then
        begin
          SetLength(FEditors, Length(FEditors) + 1);
          FEditors[High(FEditors)] := ShiftEditorContainer.Editors[k].Item.Create(
            FShapes, list^[i], FPanel, FDefaultParams);
          Break;
        end;
    end;
  FDefaultParams := False;
end;

procedure TInspector.Clean;
var i: integer;
begin
  for i := 0 to High(FEditors) do
    FEditors[i].Destroy;
  SetLength(FEditors, 0);
  SetLength(FShapes, 0);
end;

{ TParamEditor }

procedure TParamEditor.Change(Sender: TObject);
begin
  Inspector.OnParamsUpdate;
  OnUpdateFileStatus(True);
end;

constructor TParamEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var i: integer;
begin
  SetLength(FShapes, Length(AShapes));
  for i := 0 to High(AShapes) do
    FShapes[i] := AShapes[i];
  FPropInfo := APropInfo;
  FLabel := TLabel.Create(nil);
  FLabel.Parent := APanel;
  FLabel.Caption := PropNames.Values[APropInfo^.Name];
  FLabel.Left := 10;
  FLabel.Width := trunc(APanel.Width / 2) - 20;
  FLabel.Top := APanel.Tag;
  if FLabel.Caption = '' then FLabel.Caption := APropInfo^.Name;
  APanel.Tag := APanel.Tag + 35;
end;

destructor TParamEditor.Destroy;
begin
  FLabel.Free;
end;

initialization
  EditorContainer := TEditorContainer.Create;
  ShiftEditorContainer := TEditorContainer.Create;
  PropNames := TStringList.Create;
  PropNames.Values['BrushStyle'] :='Fill style:';
  PropNames.Values['PenWidth'] :='Pen width:';
  PropNames.Values['PenStyle'] :='Pen style:';
  PropNames.Values['RadiusX'] :='Radius X:';
  PropNames.Values['RadiusY'] := 'Radius Y:';
end.

