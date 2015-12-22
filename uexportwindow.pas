unit UExportWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, Buttons, math, UShapesList;

type

  { TExportDialog }

  TExportDialog = class(TForm)
    SaveDialog: TSaveDialog;
    ExportSB: TSpeedButton;
    CancelSB: TSpeedButton;
    WidthLbl: TLabel;
    HeightLbl: TLabel;
    WidthSE: TSpinEdit;
    HeightSE: TSpinEdit;
    procedure CancelSBClick(Sender: TObject);
    procedure HeightSEChange(Sender: TObject);
    procedure ExportSBClick(Sender: TObject);
    procedure WidthSEChange(Sender: TObject);
  private
    { private declarations }
  public
    ImgWidth, ImgHeight: Integer;
    AspectRatio: Double;
    { public declarations }
  end;

var
  ExportDialog: TExportDialog;

implementation

{$R *.lfm}

{ TExportDialog }

procedure TExportDialog.ExportSBClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    case SaveDialog.FilterIndex of
      1: Figures.ExportToBMP(SaveDialog.FileName, ImgWidth, ImgHeight);
      2: Figures.ExportToPNG(SaveDialog.FileName, ImgWidth, ImgHeight);
      3: Figures.ExportToJPG(SaveDialog.FileName, ImgWidth, ImgHeight);
    end;
    Close;
  end;
end;

procedure TExportDialog.WidthSEChange(Sender: TObject);
begin
  if WidthSE.Value = ImgWidth then
    Exit;
  ImgWidth := WidthSE.Value;
  ImgHeight := ceil(WidthSE.Value / AspectRatio);
  HeightSE.Value := ImgHeight;
end;

procedure TExportDialog.CancelSBClick(Sender: TObject);
begin
  Close;
end;

procedure TExportDialog.HeightSEChange(Sender: TObject);
begin
  if HeightSE.Value = ImgHeight then
    Exit;
  ImgHeight := HeightSE.Value;
  ImgWidth := ceil(HeightSE.Value * AspectRatio);
  WidthSE.Value := ImgWidth;
end;

end.

