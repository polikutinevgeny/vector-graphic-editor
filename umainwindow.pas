unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  UTools, UFiguresList, Buttons, Spin, StdCtrls, ComCtrls, Grids,
  UViewPort, UGeometry, types, math;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    ColorDialog: TColorDialog;
    PaletteDG: TDrawGrid;
    HorizontalSB: TScrollBar;
    PalettePanel: TPanel;
    MainColor: TShape;
    SecondaryColor: TShape;
    ShowAllMI: TMenuItem;
    ViewMI: TMenuItem;
    VerticalSB: TScrollBar;
    ZoomCB: TComboBox;
    ZoomLabel: TLabel;
    MainMenu: TMainMenu;
    EditMI, ClearMI, FileMI, AboutMI, ExitMI, UndoMI, RedoMI: TMenuItem;
    PenSizeLabel, LineStyleLabel,
      FillStyleLabel: TLabel;
    ControlsPanel, ModifierPanel: TPanel;
    FillStyleCB, PenStyleCB: TComboBox;
    PaintBox: TPaintBox;
    SizeSE: TSpinEdit;
    StatusBar: TStatusBar;
    procedure AboutMIClick(Sender: TObject);
    procedure ClearMIClick(Sender: TObject);
    procedure HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaletteDGDblClick(Sender: TObject);
    procedure PaletteDGDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure PaletteDGMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure RedoMIClick(Sender: TObject);
    procedure ShowAllMIClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure PenChanged(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure UpdatePen;
    procedure UpdateBrush;
    procedure SwitchBrushModifiers(AState: boolean);
    procedure VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ZoomCBChange(Sender: TObject);
    procedure UpdateScroll(AVisible: Boolean; APageSize, APosition: Integer;
      AKind: TScrollBarKind);
    procedure RecalculateScrollbars;
  private
    FCurrentToolIndex: Integer;
    FCleared: boolean;
    FMousePressed: boolean;
    FPaletteColors: array of TColor;
    FPen: TPen;
    FBrush: TBrush;
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.AboutMIClick(Sender: TObject);
begin
  ShowMessage('Vector Graphic Editor' + chr(10) +
    'Поликутин Евгений, Б8103а, 2015');
end;

procedure TMainWindow.ExitMIClick(Sender: TObject);
begin
  Close();
end;

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i, r, g, b: integer;
  bt: TSpeedButton;
begin
  FCurrentToolIndex := 0;
  FCleared := False;
  FMousePressed := False;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  for i := 0 to High(Tools) do
    begin
      bt := TSpeedButton.Create(Self);
      bt.Parent := Self.ControlsPanel;
      bt.Width := 60;
      bt.Height := 60;
      bt.Top := 10 + 70 * (i div 2);
      bt.Left := 10 + 70 * (i mod 2);
      bt.Glyph := Tools[i].Icon;
      bt.Tag := i;
      bt.OnClick := @ToolClick;
      bt.Flat := true;
      bt.ShowHint := true;
      bt.Hint := Tools[i].Caption;
    end;
  VP := TViewPort.Create;
  VP.ViewPosition := FloatPoint(PaintBox.Width / 2, PaintBox.Height / 2);
  VP.ScrollUpdateEvent := @UpdateScroll;
  {Generating palette}
  SetLength(FPaletteColors, PaletteDG.ColCount * PaletteDG.RowCount);
  for i := 64 to 79 do
    FPaletteColors[i] := RGBToColor(16 * i, 16 * i, 16 * i);
  for r := 0 to 3 do
    for g := 0 to 3 do
      for b := 0 to 3 do
        FPaletteColors[r * 4 + g * 16 + b] :=
          RGBToColor(r * 85, g * 85, b * 85);
  PaletteDG.FocusRectVisible := False;
end;

procedure TMainWindow.PaintBoxDblClick(Sender: TObject);
begin
  Tools[FCurrentToolIndex].DoubleClick;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      UpdatePen;
      UpdateBrush;
      FMousePressed := True;
      FCleared := False;
      Tools[FCurrentToolIndex].MouseClick(Point(X, Y), FPen, FBrush);
      PaintBox.Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FMousePressed then
    begin
      Tools[FCurrentToolIndex].MouseMove(Point(X, Y));
      PaintBox.Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxPaint(Sender: TObject);
begin
  VP.PortSize := Point(PaintBox.Width, PaintBox.Height);
  RecalculateScrollbars;
  ZoomCB.Text := FloatToStr(VP.Scale * 100);
  {Making canvas white}
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  {Drawing all figures}
  Figures.Draw(PaintBox.Canvas);
end;

procedure TMainWindow.ToolClick(Sender: TObject);
begin
  Tools[FCurrentToolIndex].DoubleClick;
  FCurrentToolIndex := TSpeedButton(Sender).Tag;
  StatusBar.Panels[0].Text := 'Current tool: '
    + Tools[FCurrentToolIndex].Caption;
  SwitchBrushModifiers(Tools[FCurrentToolIndex].Fillable);
end;

procedure TMainWindow.PenChanged(Sender: TObject);
begin
  UpdatePen;
  PaintBox.Invalidate;
end;

procedure TMainWindow.ClearMIClick(Sender: TObject);
begin
  Figures.UndoAll;
  Tools[FCurrentToolIndex].DoubleClick;
  FCleared := True;
  VP.Scale := 1;
  VP.ViewPosition := FloatPoint(PaintBox.Width, PaintBox.Height) / 2;
  PaintBox.Invalidate;
end;

procedure TMainWindow.ShowAllMIClick(Sender: TObject);
begin
  if not Figures.IsEmpty then
  begin
    VP.ViewPosition := FloatPoint(
      (Figures.ImageSize.Left + Figures.ImageSize.Right) / 2,
      (Figures.ImageSize.Top + Figures.ImageSize.Bottom) / 2);
    VP.ScaleTo(Figures.ImageSize);
  end;
  PaintBox.Invalidate;
end;

procedure TMainWindow.HorizontalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMainWindow.ColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := TShape(Sender).Brush.Color;
  if not ColorDialog.Execute then
    exit;
  TShape(Sender).Brush.Color := ColorDialog.Color;
  UpdatePen;
  UpdateBrush;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FMousePressed := False;
  Tools[FCurrentToolIndex].MouseUp;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if not Figures.IsEmpty then
    VP.ScaleMouseWheel(MousePos, WheelDelta);
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaletteDGDblClick(Sender: TObject);
var t: integer;
begin
  t := PaletteDG.ColCount * PaletteDG.Row + PaletteDG.Col;
  ColorDialog.Color := FPaletteColors[t];
  if not ColorDialog.Execute then
    exit;
  FPaletteColors[t] := ColorDialog.Color;
  MainColor.Brush.Color := FPaletteColors[t];
  UpdatePen;
  PaletteDG.InvalidateCell(PaletteDG.Col, PaletteDG.Row);
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaletteDGDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  PaletteDG.Canvas.Brush.Color:= FPaletteColors[aRow*PaletteDG.ColCount+aCol];
  PaletteDG.Canvas.FillRect(aRect);
end;

procedure TMainWindow.PaletteDGMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var t, c, r: Integer;
begin
  PaletteDG.MouseToCell(X, Y, c, r);
  t := PaletteDG.ColCount * r + c;
  if Button = mbLeft then
  begin
    MainColor.Brush.Color:= FPaletteColors[t];
    UpdatePen;
  end;
  if Button = mbRight then
  begin
    SecondaryColor.Brush.Color:= FPaletteColors[t];
    UpdateBrush;
  end;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UndoMIClick(Sender: TObject);
begin
  Tools[FCurrentToolIndex].DoubleClick;
  FMousePressed := False;
  {If the previous action Cleared everything we will undo it, otherwise we
  will delete the last figure drawn}
  if FCleared then
  begin
    Figures.RedoAll;
    FCleared := false;
  end
  else
    Figures.Undo;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UpdatePen;
begin
  FPen.Color := MainColor.Brush.Color;
  FPen.Style := TPenStyle(PenStyleCB.ItemIndex);
  FPen.Width := SizeSE.Value;
  Tools[FCurrentToolIndex].ChangePen(FPen);
end;

procedure TMainWindow.UpdateBrush;
begin
  FBrush.Color := SecondaryColor.Brush.Color;
  FBrush.Style := TBrushStyle(FillStyleCB.ItemIndex);
end;

procedure TMainWindow.SwitchBrushModifiers(AState: boolean);
begin
  FillStyleLabel.Visible := AState;
  FillStyleCB.Visible := AState;
end;

procedure TMainWindow.VerticalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMainWindow.ZoomCBChange(Sender: TObject);
begin
  if not Figures.IsEmpty then
    VP.Scale := StrToFloatDef(ZoomCB.Text, 100) / 100;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UpdateScroll(AVisible: Boolean; APageSize,
  APosition: Integer; AKind: TScrollBarKind);
begin
  if AKind = sbHorizontal then
  begin
    HorizontalSB.Visible := AVisible;
    HorizontalSB.PageSize := APageSize;
    HorizontalSB.Position := APosition;
  end
  else
  begin
    VerticalSB.Visible := AVisible;
    VerticalSB.PageSize := APageSize;
    VerticalSB.Position := APosition;
  end;
end;

procedure TMainWindow.RecalculateScrollbars;
var
  l, r, t, b: double;
  fp1, fp2, worldsize: TFloatPoint;
  imagesize: TFloatRect;
begin
  imagesize := Figures.ImageSize;
  fp1 := VP.ScreenToWorld(Point(0, 0));
  fp2 := VP.ScreenToWorld(VP.PortSize);
  l := Min(fp1.X, imagesize.Left);
  t := Min(fp1.Y, imagesize.Top);
  r := Max(fp2.X, imagesize.Right);
  b := Max(fp2.Y, imagesize.Bottom);
  worldsize := FloatPoint(r, b) - FloatPoint(l, t);
  if not ((worldsize.X = 0) or (worldsize.Y = 0)) then
  begin
    VP.SetScroll(HorizontalSB.Position, worldsize.X, l, sbHorizontal);
    VP.SetScroll(VerticalSB.Position, worldsize.Y, t, sbVertical);
  end;
end;

procedure TMainWindow.RedoMIClick(Sender: TObject);
begin
  Figures.Redo;
  PaintBox.Invalidate;
end;

end.

