unit UPaletteEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, UBaseShape, UInspector, typinfo, Graphics;

type

  TMainColorUpdateEvent = procedure(Sender: TObject) of object;
  TSecondaryColorUpdateEvent = procedure(Sender: TObject) of object;

  { TPenColorEditor }

  TPenColorEditor = class(TParamEditor)
  private
    procedure Change(Sender: TObject); override;
    procedure Refresh;
  public
    constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
      APanel: TPanel; ADefaultParams: Boolean); override;
    destructor Destroy; override;
  end;

  { TBrushColorEditor }

  TBrushColorEditor = class(TParamEditor)
  private
    procedure Change(Sender: TObject); override;
    procedure Refresh;
  public
    constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
      APanel: TPanel; ADefaultParams: Boolean); override;
    destructor Destroy; override;
  end;

var
  PenColor, BrushColor: ExtCtrls.TShape;
  OnMainColorUpdate: TMainColorUpdateEvent;
  OnBrushColorUpdate: TSecondaryColorUpdateEvent;

implementation

{ TBrushColorEditor }

procedure TBrushColorEditor.Change(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to High(FShapes) do
    SetInt64Prop(FShapes[i], FPropInfo, BrushColor.Brush.Color);
  inherited Change(Sender);
end;

procedure TBrushColorEditor.Refresh;
var
  j: Int64;
  i, c, r, g, b: Integer;
begin
  c := 0;
  r := 0;
  g := 0;
  b := 0;
  for i := 0 to High(FShapes) do
    begin
      c += 1;
      j := GetInt64Prop(FShapes[i], FPropInfo);
      r += Red(j);
      g += Green(j);
      b += Blue(j);
    end;
  BrushColor.Brush.Color := RGBToColor(r div c, g div c, b div c);
end;

constructor TBrushColorEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var i: Integer;
begin
  SetLength(FShapes, Length(AShapes));
  for i := 0 to High(AShapes) do
    FShapes[i] := AShapes[i];
  FPropInfo := APropInfo;
  if ADefaultParams then
    for i:= 0 to High(FShapes) do
      SetInt64Prop(FShapes[i], FPropInfo, BrushColor.Brush.Color);
  Refresh;
  OnBrushColorUpdate := @Change;
end;

destructor TBrushColorEditor.Destroy;
begin
  OnBrushColorUpdate := nil;
end;

{ TPenColorEditor }

procedure TPenColorEditor.Change(Sender: TObject);
var i: Integer;
begin
  for i:= 0 to High(FShapes) do
    SetInt64Prop(FShapes[i], FPropInfo, PenColor.Brush.Color);
  inherited Change(Sender);
end;

procedure TPenColorEditor.Refresh;
var
  j: Int64;
  i, c, r, g, b: Integer;
begin
  c := 0;
  r := 0;
  g := 0;
  b := 0;
  for i := 0 to High(FShapes) do
    begin
      c += 1;
      j := GetInt64Prop(FShapes[i], FPropInfo);
      r += Red(j);
      g += Green(j);
      b += Blue(j);
    end;
  PenColor.Brush.Color := RGBToColor(r div c, g div c, b div c);
end;

constructor TPenColorEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var i: Integer;
begin
  SetLength(FShapes, Length(AShapes));
  for i := 0 to High(AShapes) do
    FShapes[i] := AShapes[i];
  FPropInfo := APropInfo;
  if ADefaultParams then
    for i:= 0 to High(FShapes) do
      SetInt64Prop(FShapes[i], FPropInfo, PenColor.Brush.Color);
  Refresh;
  OnMainColorUpdate := @Change;
end;

destructor TPenColorEditor.Destroy;
begin
  OnMainColorUpdate := nil;
end;

initialization
  EditorContainer.RegisterEditor(TPenColorEditor, 'TPenColor');
  EditorContainer.RegisterEditor(TBrushColorEditor, 'TBrushColor');
end.

