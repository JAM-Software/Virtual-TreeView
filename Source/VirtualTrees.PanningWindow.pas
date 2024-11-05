unit VirtualTrees.PanningWindow;

interface

uses
  System.Types,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Graphics,
  VirtualTrees.Types;

type
  TPanningWindow = class(TCustomForm)
  private
    FPanningImage: TIcon;
  public
    constructor CreateAndShow(ParentForm: TCustomForm; const ImageName: TPanningCursor; const Pos: TPoint);
    destructor Destroy; override;
  end;


implementation

uses
  WinApi.Windows;

{ TPanningForm }

constructor TPanningWindow.CreateAndShow(ParentForm: TCustomForm; const ImageName: TPanningCursor; const Pos: TPoint);
var
  Image: TImage;
  Size: Integer;
begin
  inherited CreateNew(ParentForm);
  PopupMode := pmExplicit;
  PopupParent := ParentForm;
  TransparentColor := True;
  TransparentColorValue := clBtnFace;
  Autosize := True;
  BorderStyle := bsNone;
  StyleElements := [];
  Image := TImage.Create(Self);
  Image.Left := 0;
  Image.Top := 0;
  Image.Parent := Self;
  Image.Align := alClient;

  Size := 32;
  {$if CompilerVersion > 31}Size := MulDiv(Size, ParentForm.CurrentPPI, 96);{$ifend}
  FPanningImage := TIcon.Create;
  FPanningImage.Handle := LoadImage(0, MAKEINTRESOURCE(ImageName), IMAGE_CURSOR, Size, Size, LR_DEFAULTCOLOR or LR_LOADTRANSPARENT);
  Image.Picture.Assign(FPanningImage);
  Image.AutoSize := True;
  Left := Pos.X - (FPanningImage.Width div 2); //Left + Width div 2;
  Top := Pos.Y - (FPanningImage.Height div 2);//Top + Height div 2;
  Position := poDesigned;
  // This prevents a focus chnage compare to using TShow()
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  Visible := True;
end;

destructor TPanningWindow.Destroy();
begin
  FPanningImage.Free();
  inherited;
end;

end.
