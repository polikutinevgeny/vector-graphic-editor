unit UMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  UTools, UShapesList, Buttons, StdCtrls, ComCtrls, Grids,
  UViewPort, UGeometry, types, math, UInspector, UPaletteEditor, UHistory;

type

  { TMainWindow }

  TMainWindow = class(TForm)
    ColorDialog: TColorDialog;
    ExportMI: TMenuItem;
    ClearMI: TMenuItem;
    PasteMI: TMenuItem;
    CopyMI: TMenuItem;
    RedoAllMI: TMenuItem;
    UndoAllMI: TMenuItem;
    RedoMI: TMenuItem;
    UndoMI: TMenuItem;
    SaveAsMI: TMenuItem;
    ExportDialog: TSaveDialog;
    SaveMI: TMenuItem;
    OpenMI: TMenuItem;
    NewMI: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ZOrderMI: TMenuItem;
    MoveUpMI: TMenuItem;
    MoveDownMI: TMenuItem;
    BottomMI: TMenuItem;
    TopMI: TMenuItem;
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
    EditMI, DeleteMI, FileMI, AboutMI, ExitMI: TMenuItem;
    EditorsPanel, ToolsPanel: TPanel;
    PaintBox: TPaintBox;
    StatusBar: TStatusBar;
    procedure AboutMIClick(Sender: TObject);
    procedure BottomMIClick(Sender: TObject);
    procedure ClearMIClick(Sender: TObject);
    procedure CopyMIClick(Sender: TObject);
    procedure DeleteMIClick(Sender: TObject);
    procedure ExportMIClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure HorizontalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ColorMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MoveDownMIClick(Sender: TObject);
    procedure MoveUpMIClick(Sender: TObject);
    procedure NewMIClick(Sender: TObject);
    procedure OpenMIClick(Sender: TObject);
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
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure PasteMIClick(Sender: TObject);
    procedure RedoAllMIClick(Sender: TObject);
    procedure RedoMIClick(Sender: TObject);
    procedure SaveAsMIClick(Sender: TObject);
    procedure SaveMIClick(Sender: TObject);
    procedure ShowAllMIClick(Sender: TObject);
    procedure ToolClick(Sender: TObject);
    procedure TopMIClick(Sender: TObject);
    procedure UndoAllMIClick(Sender: TObject);
    procedure UndoMIClick(Sender: TObject);
    procedure VerticalSBScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure ZoomCBChange(Sender: TObject);
    procedure UpdateScroll(AVisible: Boolean; APageSize, APosition: Integer;
      AKind: TScrollBarType);
    procedure RecalculateScrollbars;
    procedure ZOrderSwitch(AEnabled: Boolean);
    procedure EditStatusUpdate;
    procedure UndoSwitch(AEnabled: Boolean);
    procedure RedoSwitch(AEnabled: Boolean);
  private
    FCurrentToolIndex: Integer;
    FCleared: boolean;
    FMousePressed: boolean;
    FPaletteColors: array of TColor;
    FNameSet: Boolean;
    FName: String;
    function ReadyToCloseFile: Boolean;
    procedure UndoRedo(AString: String);
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

procedure TMainWindow.BottomMIClick(Sender: TObject);
begin
  Figures.ZBottom;
  PaintBox.Invalidate;
end;

procedure TMainWindow.ClearMIClick(Sender: TObject);
begin
  ToolContainer.Tools[FCurrentToolIndex].Leave;
  Inspector.LoadNew(ToolContainer.Tools[FCurrentToolIndex].CreateShape);
  Figures.Clear;
  PaintBox.Invalidate;
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
  PenColor := MainColor;
  BrushColor := SecondaryColor;
  Inspector := TInspector.Create(EditorsPanel);
  Inspector.OnParamsUpdate := @PaintBox.Invalidate;
  Figures := TShapesList.Create;
  Figures.OnZOrderSwitch := @ZOrderSwitch;
  FNameSet := False;
  FName := 'unnamed';
  FormatSettings.DecimalSeparator := '.';
  OnUpdateFileStatus := @EditStatusUpdate;
  THistory.OnUndoSwitch := @UndoSwitch;
  THistory.OnRedoSwitch := @RedoSwitch;
  OnUpdateFileStatus;
  for i := 0 to High(ToolContainer.Tools) do
    begin
      bt := TSpeedButton.Create(Self);
      bt.Parent := Self.ToolsPanel;
      bt.Width := 60;
      bt.Height := 60;
      bt.Top := 10;
      bt.Left := 10 + 70 * i;
      bt.Glyph := ToolContainer.Tools[i].Icon;
      bt.Tag := i;
      bt.OnClick := @ToolClick;
      bt.Flat := true;
      bt.ShowHint := true;
      bt.Hint := ToolContainer.Tools[i].Caption;
      if i = 0 then
        bt.Click;
    end;
  VP := TViewPort.Create;
  VP.ViewPosition := FloatPoint(PaintBox.Width / 2, PaintBox.Height / 2);
  VP.OnScrollUpdate := @UpdateScroll;
  {Generate palette}
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

procedure TMainWindow.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) or (Button = mbRight) then
    begin
      FMousePressed := True;
      FCleared := False;
      ToolContainer.Tools[FCurrentToolIndex].MouseClick(Point(X, Y), Shift);
      PaintBox.Invalidate;
    end;
  if Button = mbRight then
    FMousePressed := False;
end;

procedure TMainWindow.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if FMousePressed then
    begin
      ToolContainer.Tools[FCurrentToolIndex].MouseMove(Point(X, Y), Shift);
      PaintBox.Invalidate;
    end;
end;

procedure TMainWindow.PaintBoxPaint(Sender: TObject);
begin
  VP.PortSize := Point(PaintBox.Width, PaintBox.Height);
  RecalculateScrollbars;
  ZoomCB.Text := FloatToStr(VP.Scale * 100);
  {Make canvas white}
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.FillRect(0, 0, PaintBox.Width, PaintBox.Height);
  {Draw all figures}
  Figures.Draw(PaintBox.Canvas);
end;

procedure TMainWindow.CopyMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  Figures.Copy;
  PaintBox.Invalidate;
end;

procedure TMainWindow.PasteMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  Figures.Paste;
  PaintBox.Invalidate;
end;

procedure TMainWindow.RedoAllMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  UndoRedo(History.RedoAll);
end;

procedure TMainWindow.RedoMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  UndoRedo(History.Redo);
end;

procedure TMainWindow.SaveAsMIClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Figures.Save(SaveDialog.FileName);
    FName := SaveDialog.FileName;
    FNameSet := True;
    OnUpdateFileStatus;
  end;
end;

procedure TMainWindow.SaveMIClick(Sender: TObject);
begin
  if FNameSet then
    Figures.Save(FName)
  else if SaveDialog.Execute then
  begin
    Figures.Save(SaveDialog.FileName);
    FNameSet := True;
    FName := SaveDialog.FileName;
  end
  else
    Exit;
  OnUpdateFileStatus;
end;

procedure TMainWindow.ToolClick(Sender: TObject);
begin
  ToolContainer.Tools[FCurrentToolIndex].Leave;
  FCurrentToolIndex := TSpeedButton(Sender).Tag;
  StatusBar.Panels[0].Text := 'Current tool: '
    + ToolContainer.Tools[FCurrentToolIndex].Caption;
  Inspector.LoadNew(ToolContainer.Tools[FCurrentToolIndex].CreateShape);
end;

procedure TMainWindow.TopMIClick(Sender: TObject);
begin
  Figures.ZTop;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UndoAllMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  UndoRedo(History.RedoAll);
end;

procedure TMainWindow.UndoMIClick(Sender: TObject);
begin
  if FMousePressed then
    Exit;
  UndoRedo(History.Undo);
end;

procedure TMainWindow.DeleteMIClick(Sender: TObject);
begin
  Figures.Delete;
  PaintBox.Invalidate;
end;

procedure TMainWindow.ExportMIClick(Sender: TObject);
begin
  if (not Figures.IsEmpty) and (ExportDialog.Execute) then
    Figures.ExportToBMP(ExportDialog.FileName);
end;

procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := ReadyToCloseFile;
end;

procedure TMainWindow.ShowAllMIClick(Sender: TObject);
begin
  if not Figures.IsEmpty then
    Figures.ShowAll;
  PaintBox.Invalidate;
end;

procedure TMainWindow.HorizontalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ScrollPos := Min(ScrollPos, HorizontalSB.Max - HorizontalSB.PageSize);
  PaintBox.Invalidate;
end;

procedure TMainWindow.ColorMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ColorDialog.Color := TShape(Sender).Brush.Color;
  if not ColorDialog.Execute then
    exit;
  TShape(Sender).Brush.Color := ColorDialog.Color;
  PaintBox.Invalidate;
end;

procedure TMainWindow.MoveDownMIClick(Sender: TObject);
begin
  Figures.ZDown;
  PaintBox.Invalidate;
end;

procedure TMainWindow.MoveUpMIClick(Sender: TObject);
begin
  Figures.ZUp;
  PaintBox.Invalidate;
end;

procedure TMainWindow.NewMIClick(Sender: TObject);
begin
  if FMousePressed or (not ReadyToCloseFile) then
    Exit;
  Figures.New;
  FNameSet := False;
  FName := 'unnamed';
  OnUpdateFileStatus;
  PaintBox.Invalidate;
end;

procedure TMainWindow.OpenMIClick(Sender: TObject);
begin
  if FMousePressed or (not ReadyToCloseFile) then
    Exit;
  if OpenDialog.Execute then
    if FileExists(OpenDialog.FileName) then
    begin
      FNameSet := Figures.Load(OpenDialog.FileName);
      if FNameSet then
        FName := OpenDialog.FileName
      else
        FName := 'unnamed';
      OnUpdateFileStatus;
    end
    else
     MessageDlg('File not found', mtWarning, [mbOk], 0);
  PaintBox.Invalidate;
end;

procedure TMainWindow.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FMousePressed := False;
    ToolContainer.Tools[FCurrentToolIndex].MouseUp;
    PaintBox.Invalidate;
  end;
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
  PaletteDG.InvalidateCell(PaletteDG.Col, PaletteDG.Row);
  PaintBox.Invalidate;
  if OnMainColorUpdate <> nil then
    OnMainColorUpdate(nil);
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
    if OnMainColorUpdate <> nil then
      OnMainColorUpdate(nil);
  end;
  if Button = mbRight then
  begin
    SecondaryColor.Brush.Color:= FPaletteColors[t];
    if OnBrushColorUpdate <> nil then
      OnBrushColorUpdate(nil);
  end;
  PaintBox.Invalidate;
end;

procedure TMainWindow.VerticalSBScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ScrollPos := Min(ScrollPos, VerticalSB.Max - VerticalSB.PageSize);
  PaintBox.Invalidate;
end;

procedure TMainWindow.ZoomCBChange(Sender: TObject);
begin
  if not Figures.IsEmpty then
    VP.Scale := StrToFloatDef(ZoomCB.Text, 100) / 100;
  PaintBox.Invalidate;
end;

procedure TMainWindow.UpdateScroll(AVisible: Boolean; APageSize,
  APosition: Integer; AKind: TScrollBarType);
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

procedure TMainWindow.ZOrderSwitch(AEnabled: Boolean);
begin
  ZOrderMI.Enabled := AEnabled;
  DeleteMI.Enabled := AEnabled;
end;

procedure TMainWindow.EditStatusUpdate;
begin
  if History.IsChanged then
    Caption := 'Vector Graphic Editor - ' + FName + '*'
  else
    Caption := 'Vector Graphic Editor - ' + FName;
end;

procedure TMainWindow.UndoSwitch(AEnabled: Boolean);
begin
  UndoMI.Enabled := AEnabled;
  UndoAllMI.Enabled := AEnabled;
end;

procedure TMainWindow.RedoSwitch(AEnabled: Boolean);
begin
  RedoMI.Enabled := AEnabled;
  RedoAllMI.Enabled := AEnabled;
end;

function TMainWindow.ReadyToCloseFile: Boolean;
begin
  Result := True;
  if History.IsChanged then
    case MessageDlg('File is not saved! Save the file?', mtWarning,
      mbYesNoCancel, 0) of
      mrYes:
        if FNameSet then
          Figures.Save(FName)
        else if SaveDialog.Execute then
          Figures.Save(SaveDialog.FileName)
        else
          Result := False;
      mrCancel:
        Result := False;
    end;
end;

procedure TMainWindow.UndoRedo(AString: String);
begin
  ToolContainer.Tools[FCurrentToolIndex].Reset;
  Figures.LoadState(AString);
  OnUpdateFileStatus;
  PaintBox.Invalidate;
end;

end.

