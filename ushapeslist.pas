unit UShapesList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UBaseShape, UShapes, math, UGeometry, UViewPort, UInspector, Classes,
  sysutils, Dialogs, UShapeJSONConverter, UHistory, Clipbrd;

type

  { TShapesList }
  TZOrderEvent = procedure(Enabled: Boolean) of object;
  TShapesList = class
    private
      FOnZOrderSwitch: TZOrderEvent;
      FShapes: array of TShape;
      FSelectionRectangle: TRectangle;
      function GetImageSize: TFloatRect;
    public
      property SelectionRectangle: TRectangle read FSelectionRectangle
        write FSelectionRectangle;
      property ImageSize: TFloatRect read GetImageSize;
      property OnZOrderSwitch: TZOrderEvent read FOnZOrderSwitch
        write FOnZOrderSwitch;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AShape: TShape);
      procedure Select;
      procedure Select(APoint: TPoint);
      procedure SwitchSelect;
      procedure SwitchSelect(APoint: TPoint);
      constructor Create;
      function PointOnEditPoint(APoint: TPoint; var AShape: TShape;
        var AIndex: Integer): Boolean;
      procedure Delete;
      procedure LoadSelected;
      procedure UnSelect;
      procedure ZUp;
      procedure ZDown;
      procedure ZTop;
      procedure ZBottom;
      function PointOnFigure(APoint: TPoint): Boolean;
      procedure ShiftSelected(AShift: TPoint);
      function IsEmpty: Boolean;
      procedure Save(AFile: String);
      function Load(AFile: String): Boolean;
      procedure New;
      procedure ExportToBMP(AFile: String);
      procedure ShowAll;
      procedure Undo;
      procedure Redo;
      procedure UndoAll;
      procedure RedoAll;
      procedure UpdateHistory;
      procedure Copy;
      procedure Paste;
  end;

var
  Figures: TShapesList;

implementation

{ TShapesList }

function TShapesList.GetImageSize: TFloatRect;
var i: integer;
begin
  if Length(FShapes) > 0 then
    begin
      Result := FShapes[0].Rect;
      for i := 1 to High(FShapes) do
      begin
        Result.Left := Min(Result.Left, FShapes[i].Rect.Left);
        Result.Right := Max(Result.Right, FShapes[i].Rect.Right);
        Result.Top := Min(Result.Top, FShapes[i].Rect.Top);
        Result.Bottom := Max(Result.Bottom, FShapes[i].Rect.Bottom);
      end;
    end
  else
    Result := FloatRect(FloatPoint(0, 0), FloatPoint(0, 0));
end;

procedure TShapesList.Draw(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Draw(ACanvas);
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
      FShapes[i].DrawSelection(ACanvas);
  if FSelectionRectangle <> nil then
    FSelectionRectangle.Draw(ACanvas);
end;

procedure TShapesList.Add(AShape: TShape);
begin
  SetLength(FShapes, Length(FShapes) + 1);
  FShapes[High(FShapes)] := AShape;
end;

procedure TShapesList.Select;
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].IsSelected :=
      FShapes[i].RectInShape(VP.WorldToScreen(FSelectionRectangle.TrueRect));
end;

procedure TShapesList.Select(APoint: TPoint);
var i: Integer;
begin
  for i := High(FShapes) downto 0 do
  begin
    FShapes[i].IsSelected := FShapes[i].PointInShape(APoint);
    if FShapes[i].IsSelected then
      Exit;
  end;
end;

procedure TShapesList.SwitchSelect;
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].IsSelected :=
      FShapes[i].RectInShape(VP.WorldToScreen(FSelectionRectangle.TrueRect)) xor
      FShapes[i].PrevSelected;
end;

procedure TShapesList.SwitchSelect(APoint: TPoint);
var i: Integer;
begin
  for i := High(FShapes) downto 0 do
  begin
    FShapes[i].IsSelected := FShapes[i].PointInShape(APoint) xor
      FShapes[i].PrevSelected;
    if FShapes[i].PointInShape(APoint) then
      break;
  end;
end;

constructor TShapesList.Create;
begin
  History := THistory.Create(SaveJSON(FShapes));
  OnUpdateEditor := @UpdateHistory;
end;

function TShapesList.PointOnEditPoint(APoint: TPoint; var AShape: TShape;
  var AIndex: Integer): Boolean;
var i, j: Integer;
begin
  AShape := nil;
  AIndex := -1;
  for i := High(FShapes) downto 0 do
  begin
    if not FShapes[i].IsSelected then
      continue;
    j := FShapes[i].PointInEditPoint(APoint);
    Result := j <> -1;
    if Result then
    begin
      AShape := FShapes[i];
      AIndex := j;
      Exit;
    end;
  end;
end;

procedure TShapesList.Delete;
var i, c: Integer;
begin
  c := 0;
  for i := 0 to High(FShapes) do
  begin
    if FShapes[i].IsSelected then
    begin
      c += 1;
      FShapes[i].Free;
    end
    else
      FShapes[i - c] := FShapes[i];
  end;
  SetLength(FShapes, Length(FShapes) - c);
  if Length(FShapes) = 0 then
  begin
    VP.Scale := 1;
    VP.ViewPosition := VP.PortSize / 2;
  end;
  Inspector.LoadNew(nil);
  FOnZOrderSwitch(False);
  History.AddNew(SaveJSON(FShapes));
  OnUpdateFileStatus;
end;

procedure TShapesList.LoadSelected;
var
  a: array of TShape;
  i: Integer;
begin
  SetLength(a, 0);
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := FShapes[i];
      FShapes[i].PrevSelected := True;
    end
    else
      FShapes[i].PrevSelected := False;
  Inspector.Load(a);
  FOnZOrderSwitch(Length(a) >= 2);
end;

procedure TShapesList.UnSelect;
var
  i: Integer;
begin
  for i := 0 to High(FShapes) do
  begin
    FShapes[i].IsSelected := False;
    FShapes[i].PrevSelected := False;
  end;
  FOnZOrderSwitch(False);
end;

procedure TShapesList.ZUp;
var
  i: Integer;
  s: TShape;
begin
  for i := High(FShapes) downto 0 do
  begin
    if FShapes[i].IsSelected and (i < High(FShapes)) and
      not FShapes[i + 1].IsSelected then
    begin
      s := FShapes[i];
      FShapes[i] := FShapes[i + 1];
      FShapes[i + 1] := s;
    end;
  end;
  UpdateHistory;
end;

procedure TShapesList.ZDown;
var
  i: Integer;
  s: TShape;
begin
  for i := 0 to High(FShapes) do
  begin
    if FShapes[i].IsSelected and (i > 0) and not FShapes[i - 1].IsSelected then
    begin
      s := FShapes[i];
      FShapes[i] := FShapes[i - 1];
      FShapes[i - 1] := s;
    end;
  end;
  UpdateHistory;
end;

procedure TShapesList.ZTop;
var
  i, j: Integer;
  temp: array of TShape;
begin
  SetLength(temp, Length(FShapes));
  j := 0;
  for i := 0 to High(FShapes) do
    if not FShapes[i].IsSelected then
    begin
      temp[j] := FShapes[i];
      j += 1;
    end;
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
    begin
      temp[j] := FShapes[i];
      j += 1;
    end;
  FShapes := temp;
  UpdateHistory;
end;

procedure TShapesList.ZBottom;
var
  i, j: Integer;
  temp: array of TShape;
begin
  SetLength(temp, Length(FShapes));
  j := 0;
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
    begin
      temp[j] := FShapes[i];
      j += 1;
    end;
  for i := 0 to High(FShapes) do
    if not FShapes[i].IsSelected then
    begin
      temp[j] := FShapes[i];
      j += 1;
    end;
  FShapes := temp;
  UpdateHistory;
end;

function TShapesList.PointOnFigure(APoint: TPoint): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to High(FShapes) do
  begin
    Result := FShapes[i].IsSelected and FShapes[i].PointInShape(APoint);
    if Result then
      exit;
  end;
end;

procedure TShapesList.ShiftSelected(AShift: TPoint);
var i: Integer;
begin
  for i := 0 to High(FShapes) do
  begin
    if FShapes[i].IsSelected then
      FShapes[i].Shift(AShift);
  end;
end;

function TShapesList.IsEmpty: Boolean;
begin
  Result := Length(FShapes) < 1;
end;

procedure TShapesList.Save(AFile: String);
var
  f: Text;
begin
  Assign(f, AFile);
  Rewrite(f);
  WriteLn(f, SaveJSON(FShapes));
  Close(f);
  History.InformOfSave;
  OnUpdateFileStatus;
end;

function TShapesList.Load(AFile: String): Boolean;
var
  f: TStringList;
begin
  Result := True;
  f := TStringList.Create;
  f.LoadFromFile(AFile);
  New;
  try
    FShapes := LoadJSON(f.Text);
    if not IsEmpty then
      ShowAll;
    History.Clear(SaveJSON(FShapes));
  except
    on E: Exception do
    begin
      New;
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
  OnUpdateFileStatus;
  f.Free;
  FOnZOrderSwitch(False);
end;

procedure TShapesList.New;
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Free;
  SetLength(FShapes, 0);
  VP.Scale := 1;
  VP.ViewPosition := VP.PortSize / 2;
  History.Clear(SaveJSON(FShapes));
  FOnZOrderSwitch(False);
end;

procedure TShapesList.ExportToBMP(AFile: String);
var
  bmp: TBitmap;
  i: Integer;
  t: TFloatRect;
begin
  bmp := TBitmap.Create;
  t := GetImageSize;
  bmp.Height := round(t.Bottom - t.Top);
  bmp.Width := round(t.Right - t.Left);
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
  for i := 0 to High(FShapes) do
    FShapes[i].DrawBitmap(bmp.Canvas, UGeometry.Point(
      UGeometry.FloatPoint(t.Left, t.Top)));
  bmp.SaveToFile(AFile);
  bmp.Free;
end;

procedure TShapesList.ShowAll;
var t: TFloatRect;
begin
  t := ImageSize;
  VP.ViewPosition := FloatPoint((t.Left + t.Right) / 2, (t.Top + t.Bottom) / 2);
  VP.ScaleTo(t);
end;

procedure TShapesList.Undo;
begin
  FShapes := LoadJSON(History.Undo);
  Inspector.LoadNew(nil);
  FOnZOrderSwitch(False);
end;

procedure TShapesList.Redo;
begin
  FShapes := LoadJSON(History.Redo);
  Inspector.LoadNew(nil);
  FOnZOrderSwitch(False);
end;

procedure TShapesList.UndoAll;
begin
  FShapes := LoadJSON(History.UndoAll);
  Inspector.LoadNew(nil);
  FOnZOrderSwitch(False);
end;

procedure TShapesList.RedoAll;
begin
  FShapes := LoadJSON(History.RedoAll);
  Inspector.LoadNew(nil);
  FOnZOrderSwitch(False);
end;

procedure TShapesList.UpdateHistory;
begin
  History.AddNew(SaveJSON(FShapes));
  OnUpdateFileStatus;
end;

procedure TShapesList.Copy;
var
  i: Integer;
  t: TShapes;
begin
  SetLength(t, 0);
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
    begin
      SetLength(t, Length(t) + 1);
      t[High(t)] := FShapes[i];
    end;
  if Length(t) > 0 then
    Clipboard.AsText := SaveJSON(t);
end;

procedure TShapesList.Paste;
var
  a: TFloatPoint;
  r, p: TFloatRect;
  i: Integer;
  t: TShapes;
begin
  try
    t := LoadJSON(Clipboard.AsText);
    SetLength(FShapes, Length(FShapes) + Length(t));
    for i := Length(FShapes) - Length(t) to High(FShapes) do
      FShapes[i] := t[i - Length(FShapes) + Length(t)];
    if Length(t) > 0 then
    begin
      r := t[0].Rect;
      for i := 1 to High(t) do
      begin
        p := t[i].Rect;
        r.Left := Min(r.Left, p.Left);
        r.Right := Max(r.Right, p.Right);
        r.Top := Min(r.Top, p.Top);
        r.Bottom := Max(r.Bottom, p.Bottom);
      end;
      a := FloatPoint(r.Right + r.Left, r.Bottom + r.Top) / 2;
      for i:= 0 to High(t) do
        t[i].Shift(UGeometry.Point((VP.ScreenToWorld(VP.PortSize div 2) - a) * VP.Scale));
      UpdateHistory;
    end
  except
    //well, it is ok to paste some unrelated text
  end;
end;

end.

