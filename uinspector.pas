unit UInspector;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, Graphics, ExtCtrls, StdCtrls;

type

  TParamsUpdateEvent = procedure of object;

  { TParamEditor }

  TParamEditor = class
    protected
      FShapes: array of TObject;
      FLabel: TLabel;
      FPropInfo: PPropInfo;
      procedure Change(Sender: TObject); virtual;
    public
      constructor Create(AShapes: array of TObject; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); virtual;
      destructor Destroy; override;
      procedure Refresh; virtual; abstract;
  end;

  { TInspector }

  TInspector = class
    private
      FShapes: array of TObject;
      FEditors: array of TParamEditor;
      FPenColor, FBrushColor: TColor;
      FDefaultParams: Boolean;
      FPanel: TPanel;
      FParamsUpdateEvent: TParamsUpdateEvent;
      function SearchPropertyInObject(AShape: TObject; AProperty: PPropInfo): Boolean;
      function CheckPropertyInAllObjects(AShapes: array of TObject; AProperty: PPropInfo): Boolean;
    public
      property ParamsUpdateEvent: TParamsUpdateEvent read FParamsUpdateEvent
        write FParamsUpdateEvent;
      constructor Create(APanel: TPanel);
      procedure LoadNew(AShape: TObject);
      procedure Load(AShapes: array of TObject);
      procedure Refresh;
      procedure SetPenColor(AColor: TColor);
      procedure SetBrushColor(AColor: TColor);
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

procedure TInspector.LoadNew(AShape: TObject);
var
  shapes: array of TObject;
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

function TInspector.SearchPropertyInObject(AShape: TObject;
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

function TInspector.CheckPropertyInAllObjects(AShapes: array of TObject;
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

procedure TInspector.Load(AShapes: array of TObject);
var
  list: PPropList;
  i, j: integer;
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
      for j := 0 to High(EditorContainer.Editors) do
        if list^[i]^.PropType^.Name = EditorContainer.Editors[j].Kind then
        begin
          SetLength(FEditors, Length(FEditors) + 1);
          FEditors[High(FEditors)] := EditorContainer.Editors[j].Item.Create(
            FShapes, list^[i], FPanel, FDefaultParams);
          Break;
        end;
    end;
  end;
  if FDefaultParams then
  begin
    SetPenColor(FPenColor);
    SetBrushColor(FBrushColor);
  end;
  FDefaultParams := False;
end;

procedure TInspector.Refresh;
var i: integer;
begin
  for i := 0 to High(FEditors) do
    FEditors[i].Refresh;
end;

procedure TInspector.SetPenColor(AColor: TColor);
var
  list: PPropList;
  i, j, k: integer;
begin
  FPenColor := AColor;
  if FShapes = nil then exit;
  for k := 0 to High(FShapes) do
  begin
    j := GetPropList(FShapes[k], list);
    for i := 0 to j - 1 do
      if list^[i]^.Name = 'PenColor' then
        SetInt64Prop(FShapes[k], list^[i], AColor);
  end;
end;

procedure TInspector.SetBrushColor(AColor: TColor);
var
  list: PPropList;
  i, j, k: integer;
begin
  FBrushColor := AColor;
  if FShapes = nil then exit;
  for k := 0 to High(FShapes) do
  begin
    j := GetPropList(FShapes[k], list);
    for i := 0 to j - 1 do
      if list^[i]^.Name = 'BrushColor' then
        SetInt64Prop(FShapes[k], list^[i], AColor);
  end;
end;

procedure TInspector.Clean;
var i: integer;
begin
  for i := 0 to High(FEditors) do
    FEditors[i].Destroy;
  SetLength(FEditors, 0);
end;

{ TParamEditor }

procedure TParamEditor.Change(Sender: TObject);
begin
  Inspector.ParamsUpdateEvent;
end;

constructor TParamEditor.Create(AShapes: array of TObject;
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
  if FLabel.Caption = '' Then FLabel.Caption := APropInfo^.Name;
  APanel.Tag := APanel.Tag + 35;
end;

destructor TParamEditor.Destroy;
begin
  FLabel.Free;
end;

initialization

PropNames := TStringList.Create;
PropNames.Values['BrushStyle'] :='Fill style:';
PropNames.Values['PenWidth'] :='Pen width:';
PropNames.Values['PenStyle'] :='Pen style:';
PropNames.Values['RadiusX'] :='Radius X:';
PropNames.Values['RadiusY'] := 'Radius Y:';

end.

