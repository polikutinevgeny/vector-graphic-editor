unit UHistory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  TControlsSwitchEvent = procedure(AStatus: Boolean) of object;

  { THistory }

  THistory = class
    private
      FLog: array of String;
      FCurrentPosition: Integer;
      FSave: Integer;
      function GetChangeStatus: Boolean;
      procedure CheckBounds;
    public
      constructor Create(AJSON: String);
      function Undo: String;
      function Redo: String;
      function UndoAll: String;
      function RedoAll: String;
      procedure AddNew(AJSON: String);
      procedure Clear(AJSON: String);
      procedure InformOfSave;
      property IsChanged: Boolean read GetChangeStatus;
    class var
      OnUndoSwitch: TControlsSwitchEvent;
      OnRedoSwitch: TControlsSwitchEvent
  end;

var
  History: THistory;

implementation

{ THistory }

function THistory.GetChangeStatus: Boolean;
begin
  Result := FSave <> FCurrentPosition;
end;

procedure THistory.CheckBounds;
begin
  OnUndoSwitch(FCurrentPosition <> 0);
  OnRedoSwitch(FCurrentPosition <> High(FLog));
end;

constructor THistory.Create(AJSON: String);
begin
  SetLength(FLog, 1);
  FCurrentPosition := 0;
  FSave := 0;
  FLog[FCurrentPosition] := AJSON;
end;

function THistory.Undo: String;
begin
  FCurrentPosition := EnsureRange(FCurrentPosition - 1, 0, High(FLog));
  Result := FLog[FCurrentPosition];
  CheckBounds;
end;

function THistory.Redo: String;
begin
  FCurrentPosition := EnsureRange(FCurrentPosition + 1, 0, High(FLog));
  Result := FLog[FCurrentPosition];
  CheckBounds;
end;

function THistory.UndoAll: String;
begin
  FCurrentPosition := 0;
  Result := FLog[FCurrentPosition];
  CheckBounds;
end;

function THistory.RedoAll: String;
begin
  FCurrentPosition := High(FLog);
  Result := FLog[FCurrentPosition];
  CheckBounds;
end;

procedure THistory.AddNew(AJSON: String);
begin
  if AJSON = FLog[FCurrentPosition] then
    Exit;
  SetLength(FLog, FCurrentPosition + 2);
  FCurrentPosition += 1;
  FLog[FCurrentPosition] := AJSON;
  CheckBounds;
end;

procedure THistory.Clear(AJSON: String);
begin
  SetLength(FLog, 1);
  FCurrentPosition := 0;
  FSave := 0;
  FLog[FCurrentPosition] := AJSON;
  OnUndoSwitch(False);
  OnRedoSwitch(False);
end;

procedure THistory.InformOfSave;
begin
  FSave := FCurrentPosition;
end;

end.

