unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  UTools, UFiguresList, Buttons, Spin, StdCtrls, ComCtrls, ColorBox, UViewingPort,
  UAdditionalTypes, types;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    HorizontalSB: TScrollBar;
    VerticalSB: TScrollBar;
    ZoomCB: TComboBox;
    ZoomLabel: TLabel;
    MainMenu: TMainMenu;
    EditMI, ClearMI, FileMI, AboutMI, ExitMI, UndoMI, RedoMI: TMenuItem;
    PenSizeLabel, PenColorLabel, LineStyleLabel, FillColorLabel,
      FillStyleLabel: TLabel;
    ControlsPanel, ModifierPanel: TPanel;
    FillStyleCB, PenStyleCB: TComboBox;
    FillColorCB, PenColorCB: TColorBox;
    PaintBox: TPaintBox;
    SizeSE: TSpinEdit;
    StatusBar: TStatusBar;
    procedure AboutMIClick(Sender: TObject);
    procedure ClearMIClick(Sender: TObject);
    procedure FillColorCBChange(Sender: TObject);
    procedure FillStyleCBChange(Sender: TObject);
    procedure HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PenColorCBChange(Sender: TObject);
    procedure ExitMIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxDblClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure RedoMIClick(Sender: TObject);
    procedure PenStyleCBChange(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure SizeChanged(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure UpdatePen;
    procedure UpdateBrush;
    procedure SwitchBrushModifiers(AState: boolean);
    procedure VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ZoomCBChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurrentToolIndex: Integer = 0;
  Cleared: boolean = False;
  MousePressed: boolean = False;
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
  ViewingPort := TViewingPort.Create;
  ViewingPort.ViewPosition := FloatPoint(PaintBox.Width div 2,
    PaintBox.Height div 2);
  HorizontalSB.Parent.DoubleBuffered := True;
end;

procedure TMainWindow.PaintBoxDblClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].DoubleClick;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      MousePressed := True;
      Cleared := False;
      Tools[CurrentToolIndex].MouseClick(Point(X, Y), PaintBox.Canvas.Pen,
        PaintBox.Canvas.Brush);
      PaintBox.Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if MousePressed then
    begin
      Tools[CurrentToolIndex].MouseMove(Point(X, Y));
      PaintBox.Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxPaint(Sender: TObject);
begin
  if not Figures.IsEmpty then
    ViewingPort.RecalculateScroll(PaintBox, HorizontalSB, VerticalSB,
      Figures.TopLeft, Figures.BottomRight)
  else
    ViewingPort.RecalculateScroll(PaintBox, HorizontalSB, VerticalSB,
      FloatPoint(0, 0), FloatPoint(0, 0));
  ZoomCB.Text := FloatToStr(ViewingPort.Scale * 100);
  {Making canvas white}
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  {Drawing all figures}
  Figures.Draw(PaintBox.Canvas);
  UpdatePen;
  UpdateBrush;
end;

procedure TMainWindow.ToolClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].DoubleClick;
  CurrentToolIndex := TSpeedButton(Sender).Tag;
  StatusBar.Panels[0].Text := 'Current tool: '
    + Tools[CurrentToolIndex].Caption;
  SwitchBrushModifiers(Tools[CurrentToolIndex].Fillable);
end;

procedure TMainWindow.SizeChanged(Sender: TObject);
begin
  UpdatePen;
  PaintBox.Invalidate;
end;

procedure TMainWindow.ClearMIClick(Sender: TObject);
begin
  Figures.UndoAll;
  Tools[CurrentToolIndex].DoubleClick;
  Cleared := True;
  ViewingPort.Scale := 1;
  ViewingPort.ViewPosition := FloatPoint(PaintBox.Width / 2,
    PaintBox.Height / 2);
  PaintBox.Invalidate;
end;

procedure TMainWindow.PenStyleCBChange(Sender: TObject);
begin
  UpdatePen;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PenColorCBChange(Sender: TObject);
begin
  UpdatePen;
  PaintBox.Invalidate;
end;

procedure TMainWindow.FillColorCBChange(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TMainWindow.FillStyleCBChange(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TMainWindow.HorizontalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MousePressed := False;
  Tools[CurrentToolIndex].MouseUp;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if not Figures.IsEmpty then
    ViewingPort.ScaleMouseWheel(MousePos, WheelDelta);
  PaintBox.Invalidate;
end;

procedure TMainWindow.UndoMIClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].DoubleClick;
  {If the previous action cleared everything we will undo it, otherwise we
  will delete the last figure drawn}
  if Cleared then
  begin
    Figures.RedoAll;
    Cleared := false;
  end
  else
  begin
    Figures.Undo;
    if Figures.IsEmpty then
    begin
      ViewingPort.Scale := 1;
      ViewingPort.ViewPosition := FloatPoint(PaintBox.Width / 2,
        PaintBox.Height / 2);
    end;
  end;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UpdatePen;
begin
  PaintBox.Canvas.Pen.Style := TPenStyle(PenStyleCB.ItemIndex);
  PaintBox.Canvas.Pen.Color := PenColorCB.Selected;
  PaintBox.Canvas.Pen.Width := SizeSE.Value;
  Tools[CurrentToolIndex].ChangePen(PaintBox.Canvas.Pen);
end;

procedure TMainWindow.UpdateBrush;
begin
  PaintBox.Canvas.Brush.Style := TBrushStyle(FillStyleCB.ItemIndex);
  {If we set color while using bsClear brush style the system automatically sets
  brush style to bsSolid, so we need to avoid it}
  if PaintBox.Canvas.Brush.Style <> bsClear then
    PaintBox.Canvas.Brush.Color := FillColorCB.Selected;
end;

procedure TMainWindow.SwitchBrushModifiers(AState: boolean);
begin
  FillStyleLabel.Visible := AState;
  FillColorLabel.Visible := AState;
  FillStyleCB.Visible := AState;
  FillColorCB.Visible := AState;
  ModifierPanel.Height := 45 + 45 * ord(AState);
end;

procedure TMainWindow.VerticalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  PaintBox.Invalidate;
end;

procedure TMainWindow.ZoomCBChange(Sender: TObject);
begin
  if not Figures.IsEmpty then
    ViewingPort.Scale := StrToFloatDef(ZoomCB.Text, 100) / 100;
  PaintBox.Invalidate;
end;

procedure TMainWindow.RedoMIClick(Sender: TObject);
begin
  Figures.Redo;
  PaintBox.Invalidate;
end;

end.

