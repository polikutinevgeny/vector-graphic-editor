unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  UTools, UFiguresList, Buttons, Spin, StdCtrls, ComCtrls, ColorBox;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    MainMenu: TMainMenu;
    EditMI, ClearMI, FileMI, AboutMI, ExitMI, UndoMI, RedoMI: TMenuItem;
    Label1, Label2, Label3, Label4, Label5: TLabel;
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
    procedure SwitchBrushModifiers(p: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainWindow: TMainWindow;
  CurrentToolIndex: Integer = 0;
  Cleared: boolean = False;
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
      b.Hint := Tools[i].PrettyName;
    end;
end;

procedure TMainWindow.PaintBoxDblClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].StopDrawing;
  Invalidate;
end;

procedure TMainWindow.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    begin
      Cleared := False;
      Tools[CurrentToolIndex].StartDrawing(Point(X, Y), PaintBox.Canvas.Pen,
        PaintBox.Canvas.Brush);
      Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if (ssLeft in Shift) then
    begin
      Tools[CurrentToolIndex].ContinueDrawing(Point(X, Y));
      Invalidate;
    end;
  {Showing coords on statusbar}
  StatusBar.Panels[0].Text := 'X: ' + IntToStr(X);
  StatusBar.Panels[1].Text := 'Y: ' + IntToStr(Y);
end;

procedure TMainWindow.PaintBoxPaint(Sender: TObject);
begin
  {Making canvas white}
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  {Drawing all figures}
  Figures.Display(PaintBox.Canvas);
  UpdatePen;
  UpdateBrush;
end;

procedure TMainWindow.ToolClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].StopDrawing;
  CurrentToolIndex := TSpeedButton(Sender).Tag;
  StatusBar.Panels[2].Text := 'Current tool: '
    + Tools[CurrentToolIndex].PrettyName;
  SwitchBrushModifiers(Tools[CurrentToolIndex].Fillable);
end;

procedure TMainWindow.SizeChanged(Sender: TObject);
begin
  UpdatePen;
  Invalidate;
end;

procedure TMainWindow.ClearMIClick(Sender: TObject);
begin
  Figures.Remove(true);
  Tools[CurrentToolIndex].StopDrawing;
  Cleared := True;
  Invalidate;
end;

procedure TMainWindow.PenStyleCBChange(Sender: TObject);
begin
  UpdatePen;
  Invalidate;
end;

procedure TMainWindow.PenColorCBChange(Sender: TObject);
begin
  UpdatePen;
  Invalidate;
end;

procedure TMainWindow.FillColorCBChange(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TMainWindow.FillStyleCBChange(Sender: TObject);
begin
  UpdateBrush;
end;

procedure TMainWindow.UndoMIClick(Sender: TObject);
begin
  Tools[CurrentToolIndex].StopDrawing;
  {If the previous action cleared everything we will undo it, otherwise we
  will delete the last figure drawn}
  if Cleared then
    begin
      Figures.Restore(true);
      Cleared := false;
    end
  else Figures.Remove;
  Invalidate;
end;

procedure TMainWindow.UpdatePen;
begin
  case PenStyleCB.ItemIndex of
  0: PaintBox.Canvas.Pen.Style := psSolid;
  1: PaintBox.Canvas.Pen.Style := psDot;
  2: PaintBox.Canvas.Pen.Style := psDash;
  3: PaintBox.Canvas.Pen.Style := psDashDot;
  4: PaintBox.Canvas.Pen.Style := psDashDotDot;
  end;
  PaintBox.Canvas.Pen.Color := PenColorCB.Selected;
  PaintBox.Canvas.Pen.Width := SizeSE.Value;
  Tools[CurrentToolIndex].ChangePen(PaintBox.Canvas.Pen);
end;

procedure TMainWindow.UpdateBrush;
begin
  case FillStyleCB.ItemIndex of
  0: PaintBox.Canvas.Brush.Style := bsSolid;
  1: PaintBox.Canvas.Brush.Style := bsClear;
  2: PaintBox.Canvas.Brush.Style := bsHorizontal;
  3: PaintBox.Canvas.Brush.Style := bsVertical;
  4: PaintBox.Canvas.Brush.Style := bsFDiagonal;
  5: PaintBox.Canvas.Brush.Style := bsBDiagonal;
  6: PaintBox.Canvas.Brush.Style := bsCross;
  7: PaintBox.Canvas.Brush.Style := bsDiagCross;
  end;
  {If we set color while using bsClear brush style the system automatically sets
  brush style to bsSolid, so we need to avoid it}
  if PaintBox.Canvas.Brush.Style <> bsClear then
    PaintBox.Canvas.Brush.Color := FillColorCB.Selected;
end;

procedure TMainWindow.SwitchBrushModifiers(p: boolean);
begin
  Label5.Visible := p;
  Label4.Visible := p;
  FillStyleCB.Visible := p;
  FillColorCB.Visible := p;
  ModifierPanel.Height := 45 + 45 * Integer(p);
end;

procedure TMainWindow.RedoMIClick(Sender: TObject);
begin
  Figures.Restore;
  Invalidate;
end;

end.

