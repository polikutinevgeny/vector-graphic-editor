program VectorGraphicEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainWindow, UTools, UFigures, UFiguresList, UViewingPort,
  UAdditionalTypes
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainWindow, MainWindow);
  Application.Run;
end.

