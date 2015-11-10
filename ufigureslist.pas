unit UFiguresList;

{$mode objfpc}{$H+}

interface

uses
  Graphics, UFigures, math, UGeometry, UViewPort, UInspector;

type

  { TFiguresList }

  TFiguresList = class
    private
      FFigures: array of TFigure;
      FNumberOfFiguresShown: integer;
      FSelectionRectangle: TRectangle;
      function FImageSize: TFloatRect;
    public
      property SelectionRectangle: TRectangle read FSelectionRectangle
        write FSelectionRectangle;
      property ImageSize: TFloatRect read FImageSize;
      constructor Create;
      procedure Draw(ACanvas: TCanvas);
      procedure Add(AFigure: TFigure);
      procedure Undo;
      procedure UndoAll;
      procedure Redo;
      procedure RedoAll;
      procedure Select;
      procedure LoadSelected;
      procedure UnSelect;
      function IsEmpty: Boolean;
  end;

var
  Figures: TFiguresList;

implementation

{ TFiguresList }

function TFiguresList.FImageSize: TFloatRect;
var i: integer;
begin
  if FNumberOfFiguresShown > 0 then
    begin
      Result := FFigures[0].Rect;
      for i := 1 to FNumberOfFiguresShown - 1 do
      begin
        Result.Left := Min(Result.Left, FFigures[i].Rect.Left);
        Result.Right := Max(Result.Right, FFigures[i].Rect.Right);
        Result.Top := Min(Result.Top, FFigures[i].Rect.Top);
        Result.Bottom := Max(Result.Bottom, FFigures[i].Rect.Bottom);
      end;
    end
  else
    Result := FloatRect(FloatPoint(0, 0), FloatPoint(0, 0));
end;

constructor TFiguresList.Create;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TFiguresList.Draw(ACanvas: TCanvas);
var i: integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
    FFigures[i].Draw(ACanvas);
  for i := 0 to FNumberOfFiguresShown - 1 do
    if FFigures[i].Selected then
      FFigures[i].DrawSelection(ACanvas);
  if FSelectionRectangle <> nil then
    FSelectionRectangle.Draw(ACanvas);
end;

procedure TFiguresList.Add(AFigure: TFigure);
begin
  inc(FNumberOfFiguresShown);
  SetLength(FFigures, FNumberOfFiguresShown);
  FFigures[High(FFigures)] := AFigure;
end;

procedure TFiguresList.Undo;
begin
  FNumberOfFiguresShown := max(FNumberOfFiguresShown - 1, 0);
  if IsEmpty then
    begin
      VP.Scale := 1;
      VP.ViewPosition := VP.PortSize / 2;
    end;
end;

procedure TFiguresList.UndoAll;
begin
  FNumberOfFiguresShown := 0;
end;

procedure TFiguresList.Redo;
begin
  FNumberOfFiguresShown := min(FNumberOfFiguresShown + 1, Length(FFigures));
end;

procedure TFiguresList.RedoAll;
begin
  FNumberOfFiguresShown := Length(FFigures);
end;

procedure TFiguresList.Select;
var i: integer;
begin
  for i := 0 to FNumberOfFiguresShown - 1 do
    FFigures[i].Select(VP.WorldToScreen(FSelectionRectangle.Rect));
end;

procedure TFiguresList.LoadSelected;
var
  a: array of TObject;
  i: Integer;
begin
  SetLength(a, 0);
  for i := 0 to FNumberOfFiguresShown - 1 do
    if FFigures[i].Selected then
    begin
      SetLength(a, Length(a) + 1);
      a[High(a)] := FFigures[i];
    end;
  Inspector.Load(a);
end;

procedure TFiguresList.UnSelect;
var
  i: Integer;
begin
  for i := 0 to High(FFigures) do
    FFigures[i].Selected := False;
end;

function TFiguresList.IsEmpty: Boolean;
begin
  Result := FNumberOfFiguresShown < 1;
end;

initialization
  Figures := TFiguresList.Create;
end.

