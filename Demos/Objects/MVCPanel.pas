unit MVCPanel;

{ (c) 2000 Marian Aldenhövel
           Hainstraße 8
           53121 Bonn
           +49 228 6203366
           Fax: +49 228 624031
           marian@mba-software.de

  Free: You may use this code in every way you find it useful or fun.

  This declares a Panel that hold another Panel and a TMVCTreeView. In
  this Application is not at all useful, you could just as well create the
  components at designtime.

  The reason why the component is here is because it hints at the
  possibility to use a TMVCTreeView in a hypothetical compound component
  TMVMTreeEditor that adds more controls that modify the same structure.

  It also shows how to initialize the Columns of a runtime-created
  TVirtualTree.
}

interface

uses Windows,Controls,Graphics,SysUtils,Classes,ExtCtrls,StdCtrls,
     MVCTypes,VirtualTrees, VirtualTrees.BaseTree;

type TMVCPanel=class(TCustomPanel)
                 private
                   FpnlTop:TPanel;
                   FTrvItems:TMVCTreeView;
                   procedure SetItems(aTree:TMVCTree);
                   function GetItems:TMVCTree;
                 protected
                   procedure CreateWnd; override;
                 public
                   constructor Create(aOwner:TComponent); override;

                   property TreeView:TMVCTreeView read FtrvItems;
                   property Tree:TMVCTree read GetItems write SetItems;
                 end;

implementation

procedure TMVCPanel.SetItems(aTree:TMVCTree);
begin
  { Just link the Items to the TreeView, no processing of our own. }
  FtrvItems.Tree:=aTree;
end;

function TMVCPanel.GetItems:TMVCTree;
begin
  Result:=FtrvItems.Tree;
end;

constructor TMVCPanel.Create(aOwner:TComponent);
begin
  inherited Create(aOwner);
  BevelOuter:=bvNone;
  Caption:=' ';

  FpnlTop:=TPanel.Create(Self);
  with FpnlTop do
    begin
      Parent:=Self;
      Align:=alTop;
      Height:=30;
      Caption:='SomePanel';
      BevelOuter:=bvNone;
      BevelInner:=bvLowered;
    end;

  FtrvItems:=TMVCTreeView.Create(Self);
  with FtrvItems do
    begin
      Parent:=Self;
      Align:=alClient;
    end;
end;

procedure TMVCPanel.CreateWnd;
begin
  inherited CreateWnd;

  with FtrvItems, TreeOptions do
    begin
      PaintOptions:=PaintOptions+[TVTPaintOption.toShowButtons,        // display collapse/expand
                        TVTPaintOption.toShowHorzGridLines,  // display horizontal lines
                        TVTPaintOption.toShowRoot,           // show lines also at root level
                        TVTPaintOption.toShowTreeLines,      // display tree lines to show
                                                             // hierarchy of nodes
                                                             // buttons left to a node
                        TVTPaintOption.toShowVertGridLines]; // display vertical lines
                                                             // (depending on columns) to
                                                             // simulate a grid
      MiscOptions := MiscOptions + [TVTMiscOption.toEditable];
      SelectionOptions := SelectionOptions + [TVTSelectionOption.toExtendedFocus];
                                              // to simulate a grid
      with Header do
        begin
          Height := 18;
          Options := Options + [TVTHeaderOption.hoVisible];
          Background := clBtnFace;
          AutoSize := True;
          with Columns.Add do
            begin
              Text:='Caption';
              Width:=300;
            end;
          with Columns.Add do
            begin
              Text:='SubCaption';
              Width:=100;
            end;
          with Columns.Add do
            begin
              Text:='Incidence';
              Width:=100;
            end;
        end;
    end;
end;

end.
