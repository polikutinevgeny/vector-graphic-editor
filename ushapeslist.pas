unit UShapesList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UShapes, math, UGeometry, UViewPort, UInspector, Classes, fpjson,
  fpjsonrtti, sysutils, Dialogs;

type

  { TShapesList }
  TZOrderEvent = procedure(Enabled: Boolean) of object;
  TShapesList = class
    private
      FOnZOrderSwitch: TZOrderEvent;
      FShapes: array of TShape;
      FSelectionRectangle: TRectangle;
      FChanged: Boolean;
      function GetImageSize: TFloatRect;
    public
      property SelectionRectangle: TRectangle read FSelectionRectangle
        write FSelectionRectangle;
      property ImageSize: TFloatRect read GetImageSize;
      property OnZOrderSwitch: TZOrderEvent read FOnZOrderSwitch
        write FOnZOrderSwitch;
      property Changed: Boolean read FChanged write FChanged;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AShape: TShape);
      procedure Select;
      procedure Select(APoint: TPoint);
      procedure SwitchSelect;
      procedure SwitchSelect(APoint: TPoint);
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
end;

procedure TShapesList.LoadSelected;
var
  a: array of TShape;
  i: Integer;
  f: Boolean;
begin
  f := True;
  SetLength(a, 0);
  for i := 0 to High(FShapes) do
    if FShapes[i].IsSelected then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := FShapes[i];
      if f then
        FOnZOrderSwitch(True);
      FShapes[i].PrevSelected := True;
    end
    else
      FShapes[i].PrevSelected := False;
  Inspector.Load(a);
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
  Streamer: TJSONStreamer;
  Data, t, root: TJSONObject;
  i: integer;
  f: Text;
begin
  Assign(f, AFile);
  Rewrite(f);
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
  root := TJSONObject.Create;
  Data := TJSONObject.Create;
  for i := 0 to High(FShapes) do
  begin
    t := Streamer.ObjectToJSON(FShapes[i]);
    t.Delete('AlignLeft');
    t.Delete('AlignRight');
    t.Delete('AlignTop');
    t.Delete('AlignBottom');
    Data.Add(FShapes[i].ClassName, t);
  end;
  root.Add('Vector graphics format by Polikutin Evgeny', Data);
  WriteLn(f, root.AsJSON);
  root.Free;
  Streamer.Free;
  Close(f);
  OnUpdateFileStatus(False);
end;

function TShapesList.Load(AFile: String): Boolean;
var
  DeStreamer: TJSONDeStreamer;
  t: TJSONData;
  i: integer;
  f: TFileStream;
  shape: TShape;
  str: TStrings;
begin
  Result := True;
  f := TFileStream.Create(AFile, fmOpenRead);
  DeStreamer := TJSONDeStreamer.Create(nil);
  New;
  try
    t := GetJSON(f).FindPath('Vector graphics format by Polikutin Evgeny');
    if t = nil then
      raise Exception.Create('Invalid file signature');
    for i := 0 to t.Count - 1 do
    begin
      shape := (GetClass((t as TJSONObject).Names[i]).Create as TShape);
      if shape = nil then
        raise Exception.Create('File is corrupted');
      DeStreamer.JSONToObject((t.Items[i] as TJSONObject), shape);
      str := TStringList.Create;
      DeStreamer.JSONToStrings(t.Items[i].FindPath('Points'), str);
      shape.Points := str;
      str.Free;
      Add(shape);
    end
  except
    on E: Exception do
    begin
      New;
      MessageDlg(E.Message, mtError, [mbOK], 0);
      Result := False;
    end;
  end;
  if Result and (not IsEmpty) then
  begin
    VP.ViewPosition := FloatPoint(
      (ImageSize.Left + ImageSize.Right) / 2,
      (ImageSize.Top + ImageSize.Bottom) / 2);
    VP.ScaleTo(Figures.ImageSize);
  end;
  OnUpdateFileStatus(False);
  DeStreamer.Free;
  f.Free;
end;

procedure TShapesList.New;
var i: Integer;
begin
  for i := 0 to High(FShapes) do
    FShapes[i].Free;
  SetLength(FShapes, 0);
  VP.Scale := 1;
  VP.ViewPosition := VP.PortSize / 2;
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

end.

