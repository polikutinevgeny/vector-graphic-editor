unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  UTools, UFiguresList, Buttons, Spin, StdCtrls, ComCtrls, ColorBox, Grids,
  UViewPort, UGeometry, types;

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
  private
    FCurrentToolIndex: Integer;
    FCleared: boolean;
    FMousePressed: boolean;
    FPaletteColors: array of TColor;
    FPen: TPen;
    FBrush: TBrush;
    FPaletteCell: TPoint;
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
  i: integer;
  b: TSpeedButton;
begin
  FCurrentToolIndex := 0;
  FCleared := False;
  FMousePressed := False;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  for i := 0 to High(Tools) do
    begin
      b := TSpeedButton.Create(Self);
      b.Parent := Self.ControlsPanel;
      b.Width := 60;
      b.Height := 60;
      b.Top := 10 + 70 * (i div 2);
      b.Left := 10 + 70 * (i mod 2);
      b.Glyph := Tools[i].Icon;
      b.Tag := i;
      b.OnClick := @ToolClick;
      b.Flat := true;
      b.ShowHint := true;
      b.Hint := Tools[i].Caption;
    end;
  VP := TViewPort.Create;
  VP.ViewPosition := FloatPoint(PaintBox.Width / 2, PaintBox.Height / 2);
  {Generating palette}
  SetLength(FPaletteColors, PaletteDG.ColCount * PaletteDG.RowCount);
  for i := 0 to High(FPaletteColors) do
    FPaletteColors[i] := clWhite;
  FPaletteColors[0] := TColor($E373C0);
  FPaletteColors[1] := TColor($38B25A);
  FPaletteColors[2] := TColor($2041E5);
  FPaletteColors[3] := TColor($6E613C);
  FPaletteColors[4] := TColor($463290);
  FPaletteColors[5] := TColor($226A93);
  FPaletteColors[6] := TColor($DF9679);
  FPaletteColors[7] := TColor($9D45DB);
  FPaletteColors[8] := TColor($2F6041);
  FPaletteColors[9] := TColor($A3AC4F);
  FPaletteColors[10] := TColor($9A79D4);
  FPaletteColors[11] := TColor($944575);
  FPaletteColors[12] := TColor($74AD58);
  FPaletteColors[13] := TColor($409B88);
  FPaletteColors[14] := TColor($318BE5);
  FPaletteColors[15] := TColor($6C39D9);
  FPaletteColors[16] := TColor($C7A66A);
  FPaletteColors[17] := TColor($AF88A4);
  FPaletteColors[18] := TColor($37A5C4);
  FPaletteColors[19] := TColor($203192);
  FPaletteColors[20] := TColor($5C4570);
  FPaletteColors[21] := TColor($DA6E6A);
  FPaletteColors[22] := TColor($DE4AD8);
  FPaletteColors[23] := TColor($5A84CC);
  FPaletteColors[24] := TColor($4846DF);
  FPaletteColors[25] := TColor($CA86D1);
  FPaletteColors[26] := TColor($8D5348);
  FPaletteColors[27] := TColor($70328A);
  FPaletteColors[28] := TColor($7474DB);
  FPaletteColors[29] := TColor($25476E);
  FPaletteColors[30] := TColor($A83EAE);
  FPaletteColors[31] := TColor($2C5CC9);
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
  VP.RecalculateScroll(PaintBox, HorizontalSB, VerticalSB, Figures.ImageSize);
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
  t:= PaletteDG.ColCount * FPaletteCell.y + FPaletteCell.x;
  ColorDialog.Color := FPaletteColors[t];
  if not ColorDialog.Execute then
    exit;
  FPaletteColors[t] := ColorDialog.Color;
  MainColor.Brush.Color := FPaletteColors[t];
  UpdatePen;
  PaletteDG.InvalidateCell(FPaletteCell.X, FPaletteCell.Y);
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
var
  t: Integer;
begin
  PaletteDG.MouseToCell(x, y, FPaletteCell.X, FPaletteCell.Y);
  t := PaletteDG.ColCount * FPaletteCell.Y + FPaletteCell.X;
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

procedure TMainWindow.RedoMIClick(Sender: TObject);
begin
  Figures.Redo;
  PaintBox.Invalidate;
end;

end.

