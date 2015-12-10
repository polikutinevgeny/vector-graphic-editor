unit UShapeJSONConverter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UShapes, jsonparser, fpjson, fpjsonrtti, fpjsonrpc;

function ShapePropsToJSON(AShape: TShape): TJSONObject;
function JSONToShape(Name: String; AJSON: TJSONObject): TShape;

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

end.

