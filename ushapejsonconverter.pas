unit UShapeJSONConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UBaseShape, fpjson, fpjsonrtti;

function LoadJSON(AJSON: String): TShapes;
function SaveJSON(AShapes: TShapes): String;

implementation

function ShapePropsToJSON(AShape: TShape): TJSONObject;
var Streamer: TJSONStreamer;
begin
  Streamer := TJSONStreamer.Create(nil);
  Streamer.Options := Streamer.Options + [jsoTStringsAsArray];
  Result := Streamer.ObjectToJSON(AShape);
  Result.Delete('AlignLeft');
  Result.Delete('AlignRight');
  Result.Delete('AlignTop');
  Result.Delete('AlignBottom');
  Streamer.Free;
end;

function JSONToShape(Name: String; AJSON: TJSONObject): TShape;
var
  DeStreamer: TJSONDeStreamer;
  str: TStrings;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  Result := (GetClass(Name).Create as TShape);
  if Result = nil then
    raise Exception.Create('Invalid class name');
  DeStreamer.JSONToObject(AJSON, Result);
  str := TStringList.Create;
  DeStreamer.JSONToStrings(AJSON.FindPath('Points'), str);
  Result.Points := str;
  str.Free;
  DeStreamer.Free;
end;

function LoadJSON(AJSON: String): TShapes;
var
  DeStreamer: TJSONDeStreamer;
  t, d: TJSONData;
  i: integer;
begin
  DeStreamer := TJSONDeStreamer.Create(nil);
  d := GetJSON(AJSON);
  if d = nil then
    raise Exception.Create('No valid JSON found');
  t := d.FindPath('Vector graphics format by Polikutin Evgeny');
  if t = nil then
    raise Exception.Create('Invalid signature');
  SetLength(Result, t.Count);
  for i := 0 to t.Count - 1 do
    Result[i] := JSONToShape(
      (t as TJSONObject).Names[i], (t.Items[i] as TJSONObject));
  DeStreamer.Free;
  d.Free;
end;

function SaveJSON(AShapes: TShapes): String;
var
  streamer: TJSONStreamer;
  data, root: TJSONObject;
  i: integer;
begin
  streamer := TJSONStreamer.Create(nil);
  streamer.Options := streamer.Options + [jsoTStringsAsArray];
  root := TJSONObject.Create;
  data := TJSONObject.Create;
  for i := 0 to High(AShapes) do
    data.Add(AShapes[i].ClassName, ShapePropsToJSON(AShapes[i]));
  root.Add('Vector graphics format by Polikutin Evgeny', data);
  Result := root.FormatJSON([foSingleLineArray]);
  root.Free;
  streamer.Free;
end;

end.

