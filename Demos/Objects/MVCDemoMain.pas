unit MVCDemoMain;

{ (c) 2000 Marian Aldenhövel
           Hainstraße 8
           53121 Bonn
           +49 228 6203366
           Fax: +49 228 624031
           marian@mba-software.de

  Free: You may use this code in every way you find it useful or fun.

  Main form for the MVCDemo-Project. See MVCTypes.pas for Details. }

interface

uses Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,Dialogs,
     MVCTypes,MVCPanel,StdCtrls,ExtCtrls,ImgList,VirtualTrees,ComCtrls,
  Buttons;

type TfmMVCDemo=class(TForm)
       pnlControls:TPanel;
       ImageList1:TImageList;
       edCaption:TEdit;
       Label1:TLabel;
       Label2:TLabel;
       edSubcaption:TEdit;
       Label3:TLabel;
       edIncidence: TEdit;
       Label4: TLabel;
       UpDown1: TUpDown;
       btnAdd: TButton;
       btnDelete: TButton;
       cbLive: TCheckBox;
       timLive: TTimer;
       procedure FormCreate(Sender:TObject);
       procedure edCaptionChange(Sender:TObject);
       procedure TreeViewChange(Sender:TBaseVirtualTree;Node:PVirtualNode);
       procedure edIncidenceKeyPress(Sender:TObject;var Key:Char);
       procedure edSubcaptionChange(Sender:TObject);
       procedure edIncidenceChange(Sender:TObject);
       procedure btnAddClick(Sender:TObject);
       procedure btnDeleteClick(Sender: TObject);
       procedure cbLiveClick(Sender: TObject);
       procedure timLiveTimer(Sender: TObject);
     private
       P:TMVCPanel;
       FTree:TMVCTree;
       procedure UpdateFromNode;
       function FocusedNode:TMVCNode;
       function CreateDefaultTree:TMVCTree;
     end;

var fmMVCDemo:TfmMVCDemo;

implementation

{$R *.DFM}

function TfmMVCDemo.CreateDefaultTree:TMVCTree;
{ recurse and curse :-) }
var i,j,k:integer;
begin
  Result:=TMVCTree.Create;
  for i:=0 to 2 do
    with Result.Root.CreateChild do
      begin
        Caption:='Root';
        SubCaption:='Number '+IntToStr(i);
        Incidence:=5+random(30);
        for j:=0 to 2 do
          with CreateChild do
            begin
              Caption:='Child';
              SubCaption:='Number '+IntToStr(j);
              Incidence:=random(64);
              for k:=0 to 1 do
                with CreateChild do
                  begin
                    Caption:='Grandchild';
                    SubCaption:='Number '+IntToStr(k);
                    Incidence:=random(64);
                  end;
            end;
      end;
end;

procedure TfmMVCDemo.FormCreate(Sender: TObject);
begin
  P:=TMVCPanel.Create(Self);
  with P do
    begin
      Parent:=Self;
      Align:=alClient;
      TreeView.Images:=ImageList1;
      { Now this is what it's all about:
        You have a structure - represented here by a call that creates
        a tree. All you do is assign it to a property of the Viewer,
        bingo. }
      FTree:=CreateDefaultTree;
      Tree:=FTree;
      P.TreeView.OnChange:=TreeViewChange;
      P.TreeView.FullExpand(NIL);
      UpdateFromNode;
    end;
end;

procedure TfmMVCDemo.TreeViewChange(Sender:TBaseVirtualTree;Node:PVirtualNode);
begin
  UpdateFromNode;
end;

procedure TfmMVCDemo.UpdateFromNode;
begin
  if FocusedNode=NIL
    then
      begin
        edCaption.Text:=      '';
        edCaption.Enabled:=   False;
        edSubCaption.Text:=   '';
        edSubCaption.Enabled:=False;
        edIncidence.Text:=    '';
        edIncidence.Enabled:= False;
        btnDelete.Enabled:=False;
      end
    else
      begin
        edCaption.Text:=      FocusedNode.Caption;
        edCaption.Enabled:=   True;
        edSubCaption.Text:=   FocusedNode.SubCaption;
        edSubCaption.Enabled:=True;
        edIncidence.Text:=    IntToStr(FocusedNode.Incidence);
        edIncidence.Enabled:= True;
        btnDelete.Enabled:=   True;
      end;
end;

function TfmMVCDemo.FocusedNode:TMVCNode;
begin
  with P.TreeView do
    if FocusedNode<>NIL
      then Result:=MVCNode[FocusedNode]
      else Result:=NIL;
end;

procedure TfmMVCDemo.edIncidenceKeyPress(Sender:TObject;var Key:Char);
begin
  if not CharInSet(Key, ['0'..'9',#8]) then Key:=#0;
end;

procedure TfmMVCDemo.edSubcaptionChange(Sender:TObject);
begin
  if FocusedNode<>NIL
    then FocusedNode.SubCaption:=edSubCaption.Text;
end;

procedure TfmMVCDemo.edCaptionChange(Sender:TObject);
begin
  if FocusedNode<>NIL then FocusedNode.Caption:=edCaption.Text;
end;

procedure TfmMVCDemo.edIncidenceChange(Sender:TObject);
begin
  try
    if FocusedNode<>NIL then
      if edIncidence.Text=''
        then FocusedNode.Incidence:=0
        else FocusedNode.Incidence:=StrToInt(edIncidence.Text);
  except
    ShowMessage('Enter a number between 0 and 63');
  end;
end;

procedure TfmMVCDemo.btnAddClick(Sender:TObject);
var R:TMVCNode;
begin
  if FocusedNode<>NIL
    then R:=FocusedNode
    else R:=FTree.Root;
  with R do
    begin
      R:=CreateChild;
      R.Caption:='New';
    end;
end;

procedure TfmMVCDemo.btnDeleteClick(Sender: TObject);
begin
  FocusedNode.Free;  
end;

procedure TfmMVCDemo.cbLiveClick(Sender: TObject);
begin
  timLive.Enabled:=cbLive.Checked;
end;

procedure TfmMVCDemo.timLiveTimer(Sender: TObject);
var N:TMVCNode;
begin
  { Change the Incidence-Field of one node on every
    level in one branch of the tree. }
  N:=FTree.Root;
  while Assigned(N) do
    begin
      N.Incidence:=5+random(63);
      if N.ChildCount>0
        then N:=N.Child[random(N.ChildCount)]
        else N:=NIL;
    end;    
end;

initialization
  Randomize;
end.
