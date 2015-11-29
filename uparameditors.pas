unit UParamEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Spin, ExtCtrls, StdCtrls, Controls, FPCanvas, typinfo,
  UInspector, LCLType, Graphics, math, UShapes, Buttons;

type

  { TIntegerEditor }

  TIntegerEditor = class(TParamEditor)
  private
    FSpinEdit: TSpinEdit;
    procedure Change(Sender: TObject); override;
    procedure Refresh;
  public
    constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); override;
    destructor Destroy; override;
  end;

  { TPenStyleEditor }

  TPenStyleEditor = class(TParamEditor)
    private
      FComboBox: TComboBox;
      procedure ComboBoxDrawItem(AControl: TWinControl; AIndex: Integer;
        ARect: TRect; AState: TOwnerDrawState);
      procedure Change(Sender: TObject); override;
      procedure Refresh;
    public
      constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); override;
      destructor Destroy; override;
  end;

  { TBrushStyleEditor }

  TBrushStyleEditor = class(TParamEditor)
    private
      FComboBox: TComboBox;
      procedure ComboBoxDrawItem(AControl: TWinControl; AIndex: Integer;
        ARect: TRect; AState: TOwnerDrawState);
      procedure Change(Sender: TObject); override;
      procedure Refresh;
    public
      constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); override;
      destructor Destroy; override;
    end;

  { TPositionEditor }

  TPositionEditor = class abstract(TParamEditor)
    private
      FButton: TSpeedButton;
    public
      constructor Create(AShapes: array of TShape; APropInfo: PPropInfo;
        APanel: TPanel; ADefaultParams: Boolean); override;
      destructor Destroy; override;
  end;

  { TLeftEditor }

  TLeftEditor = class(TPositionEditor)
    private
      procedure Change(Sender: TObject); override;
  end;

  { TRightEditor }

  TRightEditor = class(TPositionEditor)
    private
      procedure Change(Sender: TObject); override;
  end;

  { TTopEditor }

  TTopEditor = class(TPositionEditor)
    private
      procedure Change(Sender: TObject); override;
  end;

  { TBottomEditor }

  TBottomEditor = class(TPositionEditor)
    private
      procedure Change(Sender: TObject); override;
  end;

var
  Inspector: TInspector;

implementation

var
  PropValues : TStringList;

{ TBottomEditor }

procedure TBottomEditor.Change(Sender: TObject);
var
  i: Integer;
  bottom: Double;
begin
  bottom := FShapes[0].TrueRect.Bottom;
  for i := 1 to High(FShapes) do
    bottom := Max(bottom, FShapes[i].TrueRect.Bottom);
  for i := 0 to High(FShapes) do
    SetFloatProp(FShapes[i], FPropInfo, bottom);
  inherited Change(Sender);
end;

{ TTopEditor }

procedure TTopEditor.Change(Sender: TObject);
var
  i: Integer;
  top: Double;
begin
  top := FShapes[0].TrueRect.Top;
  for i := 1 to High(FShapes) do
    top := Min(top, FShapes[i].TrueRect.Top);
  for i := 0 to High(FShapes) do
    SetFloatProp(FShapes[i], FPropInfo, top);
  inherited Change(Sender);
end;

{ TRightEditor }

procedure TRightEditor.Change(Sender: TObject);
var
  i: Integer;
  right: Double;
begin
  right := FShapes[0].TrueRect.Right;
  for i := 1 to High(FShapes) do
    right := Max(right, FShapes[i].TrueRect.Right);
  for i := 0 to High(FShapes) do
    SetFloatProp(FShapes[i], FPropInfo, right);
  inherited Change(Sender);
end;

{ TLeftEditor }

procedure TLeftEditor.Change(Sender: TObject);
var
  i: Integer;
  left: Double;
begin
  left := FShapes[0].TrueRect.Left;
  for i := 1 to High(FShapes) do
    left := Min(left, FShapes[i].TrueRect.Left);
  for i := 0 to High(FShapes) do
    SetFloatProp(FShapes[i], FPropInfo, left);
  inherited Change(Sender);
end;

{ TPositionEditor }

constructor TPositionEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var i: Integer;
begin
  SetLength(FShapes, Length(AShapes));
  for i := 0 to High(AShapes) do
    FShapes[i] := AShapes[i];
  FPropInfo := APropInfo;
  FButton := TSpeedButton.Create(nil);
  FButton.Caption := 'Align ' + FPropInfo^.Name;
  FButton.Top := APanel.Tag;
  FButton.Left := 10;
  FButton.Width := APanel.Width - 20;
  FButton.Height := 30;
  FButton.OnClick := @Change;
  FButton.Parent := APanel;
  APanel.Tag := APanel.Tag + 35;
end;

destructor TPositionEditor.Destroy;
begin
  FButton.Free;
end;

{ TBrushStyleEditor }

procedure TBrushStyleEditor.ComboBoxDrawItem(AControl: TWinControl;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  combobox: TCombobox;
begin
  combobox := TCombobox(AControl);
  combobox.Canvas.Brush.Color := clWhite;
  if odFocused in AState then
    combobox.Canvas.Brush.Color := cl3DLight;
  combobox.Canvas.FillRect(ARect);
  combobox.Canvas.Brush.Color := clBlack;
  combobox.Canvas.Brush.Style := TFPBrushStyle(combobox.Items.Objects[AIndex]);
  combobox.Canvas.FillRect(ARect);
end;

procedure TBrushStyleEditor.Change(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(FShapes) do
    SetInt64Prop(FShapes[i], FPropInfo, FComboBox.ItemIndex);
  PropValues.Values[FPropInfo^.Name]:= IntToStr(FComboBox.ItemIndex);
  inherited Change(Sender);
end;

constructor TBrushStyleEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var
  i: TFPBrushStyle;
  j: integer;
begin
  FComboBox := TComboBox.Create(nil);
  for i in TFPBrushStyle do
    if not (i in [bsImage, bsPattern]) then
      FComboBox.AddItem('', TShape(i));
  FComboBox.OnDrawItem := @ComboBoxDrawItem;
  FComboBox.OnChange := @Change;
  FComboBox.Style := csOwnerDrawFixed;
  FComboBox.ReadOnly := True;
  FComboBox.Top := APanel.Tag;
  FComboBox.Left := trunc(APanel.Width / 2) + 10;
  FComboBox.Width := trunc(APanel.Width / 2) - 20;
  FComboBox.Parent := APanel;
  inherited Create(AShapes, APropInfo, APanel, ADefaultParams);
  if ADefaultParams then
    for j:= 0 to High(FShapes) do
      SetInt64Prop(FShapes[j], FPropInfo, StrToInt64(PropValues.Values[FPropInfo^.Name]));
  Refresh;
end;

destructor TBrushStyleEditor.Destroy;
begin
  FComboBox.Free;
  inherited Destroy;
end;

procedure TBrushStyleEditor.Refresh;
var
  j: int64;
  i: integer;
begin
  j := GetInt64Prop(FShapes[0], FPropInfo);
  for i := 1 to High(FShapes) do
    if GetInt64Prop(FShapes[i], FPropInfo) <> j then
    begin
      j := ord(bsClear);
      break;
    end;
  FComboBox.ItemIndex := j;
end;

{ TPenStyleEditor }

procedure TPenStyleEditor.ComboBoxDrawItem(AControl: TWinControl;
  AIndex: Integer; ARect: TRect; AState: TOwnerDrawState);
var
  combobox: TComboBox;
begin
  combobox := TComboBox(AControl);
  combobox.Canvas.Brush.Color := clWhite;
  if odFocused in AState then
    combobox.Canvas.Brush.Color := cl3DLight;
  combobox.Canvas.FillRect(ARect);
  combobox.Canvas.Pen.Color := clBlack;
  combobox.Canvas.Pen.Style := TFPPenStyle(combobox.Items.Objects[AIndex]);
  combobox.Canvas.Line(
    ARect.Left, (ARect.Bottom + ARect.Top) div 2,
    ARect.Right, (ARect.Bottom + ARect.Top) div 2);
end;

procedure TPenStyleEditor.Change(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(FShapes) do
    SetInt64Prop(FShapes[i], FPropInfo, FComboBox.ItemIndex);
  PropValues.Values[FPropInfo^.Name]:= IntToStr(FComboBox.ItemIndex);
  inherited Change(Sender);
end;

constructor TPenStyleEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var
  i: TFPPenStyle;
  j: integer;
begin
  FComboBox := TComboBox.Create(nil);
  for i in TFPPenStyle do
    if not (i in [psinsideFrame, psPattern, psClear]) then
      FComboBox.AddItem('', TShape(i));
  FComboBox.OnDrawItem := @ComboBoxDrawItem;
  FComboBox.OnChange := @Change;
  FComboBox.Style := csOwnerDrawFixed;
  FComboBox.ReadOnly := True;
  FComboBox.Top := APanel.Tag;
  FComboBox.Left := trunc(APanel.Width / 2) + 10;
  FComboBox.Width := trunc(APanel.Width / 2) - 20;
  FComboBox.Parent := APanel;
  inherited Create(AShapes, APropInfo, APanel, ADefaultParams);
  if ADefaultParams then
    for j := 0 to high(FShapes) do
      SetInt64Prop(FShapes[j], FPropInfo,
        StrToInt64(PropValues.Values[FPropInfo^.Name]));
  Refresh;
end;

destructor TPenStyleEditor.Destroy;
begin
  FComboBox.Free;
  inherited Destroy;
end;

procedure TPenStyleEditor.Refresh;
var
  j: int64;
  i: integer;
begin
  j := GetInt64Prop(FShapes[0], FPropInfo);
  for i := 1 to high(FShapes) do
    if GetInt64Prop(FShapes[i], FPropInfo) <> j then
    begin
      j := ord(psSolid);
      break;
    end;
  FComboBox.ItemIndex := j;
  exit;
end;

{ TIntegerEditor }

procedure TIntegerEditor.Change(Sender: TObject);
var i: integer;
begin
  for i:= 0 to High(FShapes) do
    SetInt64Prop(FShapes[i], FPropInfo, TSpinEdit(Sender).Value);
  PropValues.Values[FPropInfo^.Name]:= IntToStr(TSpinEdit(Sender).Value);
  inherited Change(Sender);
end;

constructor TIntegerEditor.Create(AShapes: array of TShape;
  APropInfo: PPropInfo; APanel: TPanel; ADefaultParams: Boolean);
var
  i: integer;
begin
  FSpinEdit := TSpinEdit.Create(nil);
  FSpinEdit.MinValue := 1;
  FSpinEdit.MaxValue := 100;
  FSpinEdit.Width:= trunc(APanel.Width / 2) - 20;
  FSpinEdit.Left:= trunc(APanel.Width / 2) + 10;
  FSpinEdit.Top:= APanel.Tag;
  FSpinEdit.Parent:= APanel;
  inherited Create(AShapes, APropInfo, APanel, ADefaultParams);
  if ADefaultParams then
    for i:= 0 to High(FShapes) do
      SetInt64Prop(FShapes[i], FPropInfo,
        StrToInt64(PropValues.Values[FPropInfo^.Name]));
  Refresh;
  FSpinEdit.OnChange := @Change;
end;

destructor TIntegerEditor.Destroy;
begin
  FSpinEdit.Free;
  inherited Destroy;
end;

procedure TIntegerEditor.Refresh;
var
  j: Int64;
  i: Integer;
begin
  j := GetInt64Prop(FShapes[0], FPropInfo);
  for i:= 0 to High(FShapes) do
    if GetInt64Prop(FShapes[i], FPropInfo) <> j then
    begin
      j := Max(j, GetInt64Prop(FShapes[i], FPropInfo));
    end;
  FSpinEdit.Value := j;
end;

initialization
  EditorContainer := TEditorContainer.Create;
  EditorContainer.RegisterEditor(TIntegerEditor, 'TPenWidth');
  EditorContainer.RegisterEditor(TIntegerEditor, 'TRadius');
  EditorContainer.RegisterEditor(TPenStyleEditor, 'TFPPenStyle');
  EditorContainer.RegisterEditor(TBrushStyleEditor, 'TFPBrushStyle');
  ShiftEditorContainer := TEditorContainer.Create;
  ShiftEditorContainer.RegisterEditor(TLeftEditor, 'TLeft');
  ShiftEditorContainer.RegisterEditor(TRightEditor, 'TRight');
  ShiftEditorContainer.RegisterEditor(TTopEditor, 'TTop');
  ShiftEditorContainer.RegisterEditor(TBottomEditor, 'TBottom');
  PropValues := TStringList.Create;
  PropValues.Values['PenWidth'] := '3';
  PropValues.Values['PenStyle'] := '0';
  PropValues.Values['BrushStyle'] := '1';
  PropValues.Values['RadiusX'] := '10';
  PropValues.Values['RadiusY'] := '10';
end.

