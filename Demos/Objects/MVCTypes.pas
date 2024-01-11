unit MVCTypes;

{ (c) 2000 Marian Aldenhövel
           Hainstraße 8
           53121 Bonn
           +49 228 6203366
           Fax: +49 228 624031
           marian@mba-software.de

  Free: You may use this code in every way you find it useful or fun.

  Say you have a hierarchical (a flat list is a special case of such
  a hierarchy :-)) structure in memory or a file. Now you need to
  visualize that structure or give the user the ability to edit it.

  Traditionally you have two options:

  1) Store the hierarchy in a TTreeView and should you need more
     data than a simple hierarchy store pointers to that data in the
     Data-property of the TTreeNode.

     This is an indecent mix of structure and visualization. The minute
     you need to access and process that structure whithout displaying
     it you either need to duplicate it or find yourself using
     invisible TTreeView - so called TTreeViewNotViews :-).

  2) Fill a TreeView with your Data and take very good care to
     propagate each and every change and edit from the TTreeView to
     your Data.

     This is a real pain in many parts of the body.

  Using Mike Lischkes fantastic virtual tree you can use a more modern
  approach:

  Make a descendant of the Treeview that "knows" about your structure.
  The Tree can be linked to your data and will now automatically
  follow changes and vice-versa. I call this the "assign-and-forget"-
  Method.

  This demo ist a simplified version of a component I use in one of my
  current projects. Read the code to find out about it.

  Note that in this demo the definition and implementation of both
  Structure and Tree are kept in a single unit. This is just for
  ease of reading and distribution - in a real-world project it is
  a good idea to keep them separate even if it means exposing fields
  of the Nodes you would not need to expose otherwise. That way it
  is easier to replace the visualization or change the implementation
  of the structure without compromising the system.

  }


interface

uses Windows,Messages,SysUtils,Graphics,VirtualTrees,Classes,StdCtrls,
     Controls,Forms,ImgList, VirtualTrees.Types, VirtualTrees.BaseTree,
     System.Types, System.UITypes;

type { TMVCNode is the encapsulation of a single Node in the structure.
       This implementation is a bit bloated because in my project
       everything is in memory at all times.
       In such an implementation there is not much "virtual" about the
       tree anymore - still it's of incredible usefulness as you will
       see. }
     TMVCNode=class(TObject)
       private
         { Here's the data associated with a single Node in the
           tree. This structure defines a caption and a subcaption, add
           whatever defines your data completely. }
         FParent:TMVCNode;
         FChildren:TList;
         FCheckState:TCheckState;
         FCaption,FSubCaption:string;
         { The FIncidence-Field is special in the way that it's
           value is never displayed directly but used to
           graphically alter the node's display. In my project it
           is a "weight" showing the number of hits from a
           database. Here it is displayed next to the caption
           as a colored speck. }
         FIncidence:integer;

        { Here's where we start to think of visualization. This
          field points to the VirtualNode in a virtual tree that
          is associated with the Node - if there is one,
          otherwise it is NIL, there may be no virtual node
          allocated for the TMVCNode or no tree linked. }
         FVirtualNode:PVirtualNode;

         { Here are reader/writer-methods for the properties that
           define our data. We need set-properties because we want
           to update linked nodes directly. }
         procedure SetCaption(aCaption:string);
         procedure SetSubCaption(aSubCaption:string);
         procedure SetCheckState(aCheckState:TCheckState);
         procedure SetIncidence(aValue:integer);
         function GetChildCount:integer;
         function GetChild(n:integer):TMVCNode;
       public
         constructor Create;
         destructor Destroy; override;

         { Take a look at our data and pick an icon from the
           Imagelist to be displayed in the tree. }
         function GetImageIndex:integer; virtual;
         { Tell the tree to invalidate the node it displayes the
           information for this Node in. It will be repainted
           next }
         procedure InvalidateVirtualNode;

         { properties exposing our internal data to the world.
           set-methods are given so the Node can always invalidate
           its node. }
         property CheckState:TCheckState read FCheckState write SetCheckState;
         property Caption:string read FCaption write SetCaption;
         property SubCaption:string read FSubCaption write SetSubCaption;
         property Incidence:integer read FIncidence write SetIncidence;

         property Parent:TMVCNode read FParent;
         property ChildCount:integer read GetChildCount;
         property Child[n:integer]:TMVCNode read GetChild;
         function CreateChild:TMVCNode;
         procedure RemoveChild(n:integer);
         procedure DestroyChild(n:integer);

         { This field is only exposed because I advise you to put
           the Tree-Code in a separate unit and then you won't get
           far privatizing it. Allowing public write-access to it
           is a bit hairy though, you should never write to the
           field outside of this or the Tree-unit.
           This is where a friend-declaration is missing from OP}
         property VirtualNode:PVirtualNode read FVirtualNode write FVirtualNode;
       end;

     { TMVCTree keeps the TMVCNodes. It also maintains the link to a
       virtual treeview. }
     TMVCTree=class
       private
         { This is the Root of the Tree. In this Demo that Root is there
           purely to hold the structure together, it is never displayed -
           just like TVirtualTrees own Root. }
         FRoot:TMVCNode;

         { The Viewer-Field points to any component or object
           used to visualize or edit this structure. It is
           not really used in this demo, in a real application
           you will find situations where you have to find out
           whether you are linked and if so where to.

           Why is the Viewer declared as TObject and not as
           the specific class? Two reasons:

           1) The viewer should be implemented in a different
              unit, either there or here you will have to
              cast or you will build a circular reference. I
              choose to cast here because

           2) You may want to have _different_ viewers for
              the same structure. Keeping that in mind you
              may even want to change the declaration to
              a list of linked viewers... }
         FSettingViewer:integer;
         FViewer:TObject;

         { Access-methods to expose the list in a type safe
           way. }

         { A set-method that updates the link to a viewer. }
         procedure SetViewer(aViewer:TObject);

       public
         constructor Create;
         destructor Destroy; override;

         property Root:TMVCNode read FRoot;

         { Assign to this to create or break the link with
           a viewer. If you are about to add, remove or edit
           a zillion Nodes you can call BeginUpdate and
           EndUpdate. In this demo they just do the same for
           any assigned viewer - Caution: The demo does not
           make provisions for the case where you call
           BeginUpdate and then switch to another viewer! }
         property Viewer:TObject read FViewer write SetViewer;
         procedure BeginUpdate;
         procedure EndUpdate;
       end;

     { Here's the Viewer. I have descended from the base class to maximize
       the functionality that is moved to our code, should you be happy
       with any of the predeclared descendants use of them. }
     TMVCEditLink=class;
     TMVCTreeView=class(TVTAncestor)
       private
         { This is a pointer to the structure associated with
           this viewer. }
         FTree:TMVCTree;
         FInternalDataOffset: Cardinal;               // offset to the internal data

         { Make and break the link with a list }
         procedure SetTree(aTree:TMVCTree);

         { These are for direct access to our structure
           through the viewer. You can use them to find
           the TMVCNode that corresponds to a selected
           VirtualNode for instance. }
         function GetMVCNode(VirtualNode:PVirtualNode):TMVCNode;
         procedure SetMVCNode(VirtualNode:PVirtualNode; aNode:TMVCNode);

          function GetOptions: TVirtualTreeOptions;
          procedure SetOptions(const Value: TVirtualTreeOptions);
       protected
         { Overridden methods of the tree, see their implementation for
           details on what they do and why they are overridden. }
         function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): Integer; override;
         procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
         function DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean; override;
         procedure DoInitNode(aParent,aNode:PVirtualNode;
                              var aInitStates:TVirtualNodeInitStates); override;
         procedure DoFreeNode(aNode:PVirtualNode); override;
         function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
           var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList; override;
         procedure DoChecked(aNode:PVirtualNode); override;
         function DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink; override;
         function InternalData(Node: PVirtualNode): Pointer;
         function InternalDataSize: Cardinal;

         function GetOptionsClass: TTreeOptionsClass; override;
       public
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         { Properties for the link to a list and the individual Node.
           these form the interface to the application. See the main form
           to check it out. }
         property Tree:TMVCTree read FTree write SetTree;
         property MVCNode[VirtualNode:PVirtualNode]:TMVCNode read GetMVCNode;

         function GetNodeText(aNode:TMVCNode;
                              aColumn:integer):string;
         procedure SetNodeText(aNode:TMVCNode;
                               aColumn:integer;
                               aText:string);
       published
         { We descend from the base class, publish whatever you want to.
           The demo only needs the Header, it is initialized in the fixed
           panel-code. }
         property TreeOptions: TVirtualTreeOptions read GetOptions write SetOptions;
         property Header;
         property Images;
         property OnChange;
       end;

   TMVCEdit=class(TCustomEdit)
              private
                FLink:TMVCEditLink;
                procedure WMChar(var Message: TWMChar); message WM_CHAR;
                procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
                procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
              protected
                procedure AutoAdjustSize;
                procedure CreateParams(var Params:TCreateParams); override;
              public
                constructor Create(Link:TMVCEditLink); reintroduce;
              end;

   TMVCEditLink=class(TInterfacedObject,IVTEditLink)
                   private
                     FEdit:TMVCEdit;                    // a normal custom edit control
                     FTree:TMVCTreeView;               // a back reference to the tree calling
                     FNode:PVirtualNode;               // the node to be edited
                     FColumn:Integer;                  // the column of the node
                   public
                     constructor Create;
                     destructor Destroy; override;

                     function BeginEdit: Boolean; stdcall;
                     function CancelEdit: Boolean; stdcall;
                     function EndEdit: Boolean; stdcall;
                     function GetBounds: TRect; stdcall;
                     function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex): Boolean; stdcall;
                     procedure ProcessMessage(var Message: TMessage); stdcall;
                     procedure SetBounds(R:TRect); stdcall;

                     property Tree:TMVCTreeView read FTree;
                   end;


implementation

{ Let's go }

{ *********************************************************************** }

constructor TMVCNode.Create;
begin
  inherited Create;
  FChildren:=TList.Create;
end;

destructor TMVCNode.Destroy;
begin
  if Assigned(FParent) then
    with FParent do
      RemoveChild(FChildren.IndexOf(Self));
  { When destroying free all children. }
  while ChildCount>0 do DestroyChild(0);

  inherited Destroy;
end;

function TMVCNode.GetImageIndex:integer;
begin
  { Take a close look at your data and return the index of whatever image
    you want next to it. Here we base the choice on the length of the
    caption. No caption, no icon. }
  if Caption=''
    then Result:=-1 else Result:=(Length(Caption) mod 4);
end;

procedure TMVCNode.InvalidateVirtualNode;
var T:TBaseVirtualTree;
begin
  { If the tree has a node that displays this Node then invalidate it. }
  if Assigned(FVirtualNode) then
  begin
    T := TreeFromNode(FVirtualNode);
    T.InvalidateNode(FVirtualNode);
  end;
end;

procedure TMVCNode.SetCheckState(aCheckState:TCheckState);
begin
  { Update the checkstate that is stored in our Node. If the tree has a
    node for the Node then invalidate it. }
  if aCheckState=FCheckstate then exit;
  FCheckState:=aCheckState;
  if Assigned(FVirtualNode) then FVirtualNode.CheckState:=aCheckState;
  InvalidateVirtualNode;
end;

procedure TMVCNode.SetIncidence(aValue:integer);
begin
  { Set the Nodes property Incidence and invalidate the node in the tree
    if there is one. We fix the value into its valid range. }
  if aValue=FIncidence then exit;
  FIncidence:=aValue;
  if FIncidence<0
    then FIncidence:=0
    else
      if FIncidence>63
        then FIncidence:=63;
  InvalidateVirtualNode;
end;

procedure TMVCNode.SetCaption(aCaption:string);
begin
  { Set the Nodes property Caption and invalidate the node in the tree
    if there is one. }
  if aCaption=FCaption then exit;
  FCaption:=aCaption;
  InvalidateVirtualNode;
end;

procedure TMVCNode.SetSubCaption(aSubCaption:string);
begin
  { Set the Nodes property Subcaption and invalidate the node in the tree
    if there is one. }
  if aSubCaption=FSubCaption then exit;
  FSubCaption:=aSubCaption;
  InvalidateVirtualNode;
end;

function TMVCNode.GetChildCount:integer;
begin
  Result:=FChildren.Count;
end;

function TMVCNode.GetChild(n:integer):TMVCNode;
begin
  Result:=TMVCNode(FChildren[n]);
end;

function TMVCNode.CreateChild:TMVCNode;
begin
  Result:=TMVCNode.Create;
  Result.FParent:=Self;
  FChildren.Add(Result);
  if Assigned(FVirtualNode) then
    with TreeFromNode(FVirtualNode) do
      begin
        ReinitNode(FVirtualNode,False);
        InvalidateToBottom(FVirtualNode);
      end;
end;

procedure TMVCNode.RemoveChild(n:integer);
var C:TMVCNode;
begin
  { Remove Child number n from our Children-List and the tree }
  C:=Child[n];
  C.FParent:=NIL;
  FChildren.Delete(n);
  if Assigned(C.FVirtualNode) then
    TreeFromNode(C.FVirtualNode).DeleteNode(C.FVirtualNode);
end;

procedure TMVCNode.DestroyChild(n:integer);
var C:TMVCNode;
begin
  C:=Child[n];
  RemoveChild(n);
  C.Free;
end;

{*************************************************************************}

constructor TMVCTree.Create;
begin
  inherited Create;
  FRoot:=TMVCNode.Create;
end;

destructor TMVCTree.Destroy;
begin
  { Upon destruction we need to break the link to the Viewer and free
    all our Nodes and last the list itself. }
  Viewer:=NIL;
  FRoot.Free;
  FRoot:=NIL;
  inherited Destroy;
end;

procedure TMVCTree.SetViewer(aViewer:TObject);
begin
  { Assign a viewer, De-Assign a viewer (by passing NIL) and assigning
    a different viewer than the one that is already linked. }

  { Prevent recursion when the viewer itself sets this property. }
  if FSettingViewer>0 then exit;

  inc(FSettingViewer);
  try
    { First de-assign any viewer that is already linked. }
    if Assigned(FViewer) then TMVCTreeView(FViewer).Tree:=NIL;
    { Set our field to point to the new viewer. }
    FViewer:=aViewer;
    { Now assign this List to the new viewer. }
    if Assigned(FViewer) then TMVCTreeView(FViewer).Tree:=Self;
  finally
    dec(FSettingViewer);
  end;
end;

procedure TMVCTree.BeginUpdate;
begin
  if Assigned(FViewer) then TMVCTreeView(FViewer).BeginUpdate;
end;

procedure TMVCTree.EndUpdate;
begin
  if Assigned(FViewer) then TMVCTreeView(FViewer).EndUpdate;
end;

{ *********************************************************************** }

{ Now the tree. }

{ The internal node-data assigned to every virtual node consist only of
  a reference to an instance of TMVCNode. }
type PMyNodeData=^TMyNodeData;
     TMyNodeData=packed record Node:TMVCNode end;

destructor TMVCTreeView.Destroy;
begin
  { When destroying the tree, break the link with the list. Note that
    we do NOT set FNodes:=NIL. By using the Set-Method it is made sure
    that the List gets notified of our demise and sets its own reference
    to NIL too. }
  Tree:=NIL;
  inherited Destroy;
end;

procedure TMVCTreeView.SetTree(aTree:TMVCTree);
begin
  if FTree=aTree then exit;

  { If we already have a list, break the link to it. }
  if Assigned(FTree) then FTree.Viewer:=NIL;

  { Now make a link to the new structure: }
  FTree:=aTree;
  if Assigned(FTree)
    then
      begin
        FTree.Viewer:=Self;
        RootNodeCount:=FTree.Root.ChildCount;
        if FTree.Root.ChildCount>0 then ValidateNode(GetFirst, False);
      end
    else RootNodeCount:=0;
end;

function TMVCTreeView.GetMVCNode(VirtualNode:PVirtualNode):TMVCNode;
begin
  { Return the reference to the TMVCNode that is represented by
    Virtualnode }
  if VirtualNode.IsAssigned() then
    Result := PMyNodeData(InternalData(VirtualNode)).Node
  else
    Result := nil;
end;

procedure TMVCTreeView.SetMVCNode(VirtualNode:PVirtualNode;aNode:TMVCNode);
begin
  { Note the relationship between a VirtualNode and the TMVCNode it
    represents in the Nodes data. }
  PMyNodeData(InternalData(VirtualNode)).Node:=aNode;
end;

function TMVCTreeView.DoCreateEditor(Node: PVirtualNode; Column: TColumnIndex): IVTEditLink;
var Link:TMVCEditLink;
begin
  Result:=inherited DoCreateEditor(Node,Column);
  if Result=nil then
  begin
    Link:=TMVCEditLink.Create;
    Result:=Link;
  end;
end;

function TMVCTreeView.DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = nil): Integer;
{ How wide is the the node in pixels. This is interesting if the graphic
  representation includes elements that are not text and whose width needs
  to be calculated. Here we draw a bar whose width corresponds to the
  value of the Incidence-property of the MVCNode. }
var N:TMVCNode;
  Text: string;
begin
  N:=GetMVCNode(Node);
  if Canvas = nil then
    Canvas := Self.Canvas;
  if not Assigned(N)
    then Result:=0
    else
      begin
        Text:=GetNodeText(N, Column);
        Result:=Canvas.TextWidth(Text);
        if Column + 1 in [0, 1] then
          Result := Result + 8 + N.Incidence;
      end;
end;

function TMVCTreeView.GetNodeText(aNode:TMVCNode;aColumn:integer):string;
{ This method returns the text that is to be displayed in aColumn for
  the Node aNode. It is in a separate function so that it can be used
  for the calculation of width and for the actual drawing. You could
  also process the data from the actual Node as stored in your structure
  to give some other text as shown here for the third column. }
begin
  case aColumn of
    -1,0:Result:=aNode.Caption;
       1:Result:=aNode.SubCaption;
       2:case aNode.Incidence of
             0..5:Result:='under 6';
            6..20:Result:='6 to 21';
           21..62:Result:='21 or above';
               63:Result:='Max.';
             else Result:='What?';
         end; (* of case Incidence *)
    else Result:='What *"§ added columns without giving data?';
  end; (* of case aColumn *)
end;

procedure TMVCTreeView.SetNodeText(aNode:TMVCNode;aColumn:integer;aText:string);
{ Set the text for the node and column. This is called by the editor when
  editing has finished. }
begin
  case aColumn of
    -1,0:aNode.Caption:=aText;
       1:aNode.SubCaption:=aText;
    else { Error, this column should not / cannot be edited }
  end; (* of case aColumn *)
end;

procedure TMVCTreeView.DoPaintNode(var PaintInfo: TVTPaintInfo);
{ Here we actually draw the graphical representation of the node. It is
  drawn one cell, i.e. Node/Column at a time. All relevant data is either
  passed as a parameter or we look it up in our TMVCNode-Structure that
  is linked to the Node via the internal data. }

var N:TMVCNode;
    SaveFontColor:TColor;
    Flags:Integer;
    TxtRect:TRect;
    NodeText:string;
    OldBrushColor,OldPenColor:TColor;

  procedure SaveDC;
  begin
    OldBrushColor:=PaintInfo.Canvas.Brush.Color;
    OldPenColor:=PaintInfo.Canvas.Pen.Color;
  end;

  procedure RestoreDC;
  begin
    PaintInfo.Canvas.Brush.Color:=OldBrushColor;
    PaintInfo.Canvas.Pen.Color:=OldPenColor;
  end;

begin
  SaveDC; { No-brainer: We save and restore every canvas-setting, we _ever_
                        change in this method. So initial and final state
                        of the canvas are of no concern. }
  try
    with PaintInfo, Canvas do
      begin
        Font:=Self.Font;

        { Get a reference to our data. If this fails bail out - this
          should not happen anyway. If it does you will notice on screen.
          Paranoics add assertions as you like. }
        N:=MVCNode[Node]; if N=NIL then exit;

        { Get the text-string to be displayed in the column. }
        NodeText:=GetNodeText(N, Column);

        { Some shuffling of feet and rectangles. Try for yourself what
          happens here be adding offsets, changing colors etc.. }
        if (TVTPaintOption.toHotTrack in Self.TreeOptions.PaintOptions) and
           (Node=HotNode)
          then Font.Style:=Font.Style+[fsUnderline]
          else Font.Style:=Font.Style-[fsUnderline];

        if vsSelected in Node.States
          then
            begin
              if Focused
                then (* Selected, focused *)
                  begin
                    Brush.Color:=clHighLight;
                    Font.Color:=clWhite;
                  end
                else (* Selected, non-focused *)
                  begin
                    Brush.Color:=clBtnFace;
                    Font.Color:=Self.Font.Color;
                  end;
              { Fill out the entire rectangle }
              FillRect(ContentRect);
            end
          else (* not selected, see Mikes samples on what is going on... *)
            if Node=DropTargetNode
              then
                begin
                  if LastDropMode=dmOnNode
                    then
                      begin
                        Brush.Color:=clHighLight;
                        Font.Color:=clWhite;
                      end
                    else
                      begin
                        Brush.Style:=bsClear;
                        Font.Color:=Self.Font.Color;
                      end;
                  FillRect(ContentRect);
                end;

        if Focused
           and (FocusedNode=Node) and
           not (TVTSelectionOption.toFullRowSelect in Self.TreeOptions.SelectionOptions)
          then
            begin
              if Self.Color=clGray
                then Brush.Color:=clWhite
                else Brush.Color:=clBlack;
              SaveFontColor:=Font.Color;
              Font.Color:=Self.Color;
              Windows.DrawFocusRect(Handle,ContentRect);
              Font.Color:=SaveFontColor;
            end;

        { Disabled node color overrides all other variants }
        if vsDisabled in Node.States then Font.Color:=clBtnShadow;

        if Column+1 in [0,1] then
          begin
            { Draw the Incidence-Bar }
            Pen.Color:=clBlack;
            Brush.Style:=bsSolid;
            { Mix a color for an incidence-value }
            Brush.Color:= RGB(4 * N.Incidence, 128, 255 - 4 * N.Incidence);
            Rectangle(ContentRect.Left+2,
                      ContentRect.Top+2,
                      ContentRect.Left+2+N.Incidence,
                      ContentRect.Bottom-2);
          end;

        { Paint corresponding text }
        Brush.Color:=Color;
        SetBkMode(Handle,TRANSPARENT);

        TxtRect.Left:=  ContentRect.Left;
        TxtRect.Top:=   ContentRect.Top;
        TxtRect.Right:= ContentRect.Right;
        TxtRect.Bottom:=ContentRect.Bottom;
        if Column+1 in [0,1]
          then TxtRect.Left:=TxtRect.Left+6+N.Incidence;
        Flags:=DT_LEFT or DT_SINGLELINE or DT_VCENTER;
        DrawText(Handle,PChar(NodeText),Length(NodeText),TxtRect,Flags);
      end; { of with Canvas }
  finally
    RestoreDC;
  end;
end;

procedure TMVCTreeView.DoFreeNode(aNode:PVirtualNode);
{ A virtual node is being freed by the tree. Make sure the associated Node
  loses its pointer to the node. }
var N:TMVCNode;
begin
  N:=MVCNode[aNode];
  if Assigned(N) then
    begin
      N.VirtualNode:=NIL;
      SetMVCNode(aNode,NIL);
    end;
  inherited DoFreeNode(aNode);
end;

function TMVCTreeView.DoInitChildren(Node: PVirtualNode; var ChildCount: Cardinal): Boolean;
begin
  inherited DoInitChildren(Node,ChildCount);
  ChildCount:=MVCNode[Node].ChildCount;
  Result := True;
end;

procedure TMVCTreeView.DoInitNode(aParent,aNode:PVirtualNode;
                                  var aInitStates:TVirtualNodeInitStates);
{ The tree has just allocated a new virtual node. Link it to the TMVCNode
  it is to represent. }
var P,I:TMVCNode;
begin
  inherited DoInitNode(aParent,aNode,aInitStates);
  with aNode^ do
    begin
      { Wich MVCNode corresponds to the virtual node being initialized?
        Find the Parent-MVCNode via the Parent-VirtualNode }
      if (aParent=RootNode) or (aParent=NIL)
        then P:=FTree.Root
        else P:=MVCNode[aParent];
      { MVCNode we are looking for is child number aIndex. }
      I:=P.Child[Index];

      { Now set all the data the Treeview needs plus our link to the node }
      SetMVCNode(aNode,I);
      I.VirtualNode:=aNode;

      if I.ChildCount>0
        then Include(aInitStates,ivsHasChildren)
        else Exclude(aInitStates,ivsHasChildren);
      CheckState:=I.CheckState;
    end;
end;

function TMVCTreeView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList;
{ The tree requests the image-index for a Node and column. }
var N:TMVCNode;
begin
  if (Column > 0) or (Kind <> TVTImageKind.ikNormal) then
    exit(nil);
  case Column of
    -1,0:begin
           { We only want Icons in the first column. Ask the node which
             one it wants. }
           N:=MVCNode[Node];
           if N=NIL { No node assigned, this should not happen. }
             then Index:=-1
             else Index:=N.GetImageIndex;
         end;
    else Index:=-1;
  end;
  Result := Images;
end;

procedure TMVCTreeView.DoChecked(aNode:PVirtualNode);
{ In the tree a node has been checked, unchecked or whatever change to
  the CheckState happens. Propagate that to the TMVCNode. }
var N:TMVCNode;
begin
  if Assigned(FTree) then
    begin
      N:=MVCNode[aNode];
      if Assigned(N) then N.CheckState:=aNode^.CheckState;
    end;
  inherited DoChecked(aNode);
end;

function TMVCTreeView.InternalData(Node: PVirtualNode): Pointer;

begin
  if (Node = RootNode) or (Node = nil) then
    Result := nil
  else
    Result := PByte(Node) + FInternalDataOffset;
end;

function TMVCTreeView.InternalDataSize: Cardinal;

begin
  // The size of the internal data this tree class needs.
  Result := SizeOf(TMyNodeData);
end;

constructor TMVCEditLink.Create;
begin
  inherited;
  FEdit := TMVCEdit.Create(Self);
  with FEdit do
  begin
    Visible := False;
    Ctl3D := False;
    BorderStyle := bsSingle;
    AutoSize := False;
  end;
end;

destructor TMVCEditLink.Destroy;
begin
  FEdit.Free;
  inherited;
end;

function TMVCEditLink.BeginEdit: Boolean;
begin
  Result := True;
  FEdit.Show;
  FEdit.SetFocus;
end;

function TMVCEditLink.CancelEdit: Boolean;
begin
  Result := True;
  // to show the kill focus handler that we don't need a second notification for the tree
  FTree:=nil;
  FEdit.Hide;
end;

function TMVCEditLink.EndEdit: Boolean;
var LastTree:TMVCTreeView;
    MVCNode:TMVCNode;
begin
  Result := True;
  try
    if Assigned(FTree) then
    begin
      if FEdit.Modified then
      begin
        MVCNode:=FTree.MVCNode[FNode];
        // keep tree reference because the application might want to change the focuse while
        // processing the NewText event
        LastTree:=FTree;
        FTree:=nil;

        LastTree.SetNodeText(MVCNode,FColumn,FEdit.Caption);
      end;
      FTree:=nil;
    end;
  finally
    FEdit.Hide;
  end;
end;

function TMVCEditLink.GetBounds: TRect;
begin
  Result:=FEdit.BoundsRect;
end;

function TMVCEditLink.PrepareEdit(Tree:TBaseVirtualTree;Node:PVirtualNode;Column:TColumnIndex): Boolean;
// retrieves the true text bounds from the owner tree
var R:TRect;
    MVCNode:TMVCNode;
begin
  Result := True;
  FTree:=Tree as TMVCTreeView;

  FNode:=Node;
  FColumn:=Column;

  MVCNode:=FTree.MVCNode[Node];

  FEdit.Caption:=FTree.GetNodeText(MVCNode,Column);
  FEdit.Parent:=Tree;
  R:=FTree.GetDisplayRect(Node,Column,True);

  { In the primary column there is the "Incidence-Bar". Adjust the left
    side of the rect to exclude it }
  if Column+1 in [0,1] then R.Left:=R.Left+MVCNode.Incidence;

  with R do
    begin
      // set the edit's bounds but make sure there's a minimum width and the right border does not
      // extend beyond the parent's right border
      if Right-Left<50 then Right:=Left+50;
      if Right>FTree.Width then Right:=FTree.Width;
      FEdit.SetBounds(Left,Top,Right-Left,Bottom-Top);
      FEdit.Font:=FTree.Font;
    end;
end;

procedure TMVCEditLink.SetBounds(R: TRect);
begin
  // ignore this one as we get here the entire node rect but want the minimal text bounds
end;

constructor TMVCEdit.Create(Link:TMVCEditLink);
begin
  inherited Create(nil);
  ShowHint:=False;
  ParentShowHint:=False;
  FLink:=Link;
end;

procedure TMVCEdit.WMChar(var Message: TWMChar);
// handle character keys
begin
  // avoid beep
  if Message.CharCode <> VK_ESCAPE then
  begin
    inherited;
    if Message.CharCode > $20 then AutoAdjustSize;
  end;
end;

procedure TMVCEdit.WMKeyDown(var Message: TWMKeyDown);
// handles some control keys (either redirection to tree, edit window size or clipboard handling)
begin
  case Message.CharCode of
    // pretend these keycodes were send to the tree
    VK_ESCAPE,
    VK_UP,
    VK_DOWN:
      FLink.FTree.WndProc(TMessage(Message));
    VK_RETURN:
      FLink.FTree.DoEndEdit;
    // standard clipboard actions,
    // Caution: to make these work you must not use default TAction classes like TEditPaste etc. in the application!
    Ord('C'):
      if (Message.KeyData and MK_CONTROL) <> 0 then CopyToClipboard;
    Ord('X'):
      if (Message.KeyData and MK_CONTROL) <> 0 then
      begin
        CutToClipboard;
        AutoAdjustSize;
      end;
    Ord('V'):
      if (Message.KeyData and MK_CONTROL) <> 0 then
      begin
        PasteFromClipboard;
        AutoAdjustSize;
      end;
  else
    inherited;
    // second level for keys to be passed to its target
    case Message.CharCode of
      VK_BACK,
      VK_DELETE:
        AutoAdjustSize;
    end;
  end;
end;

procedure TMVCEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  // FLink.FTree is set to nil if the link doesn't need to notify the tree (e.g. hiding the edit causes
  // a kill focus message)
  if Assigned(FLink.FTree) then FLink.FTree.DoCancelEdit;
end;

procedure TMVCEdit.AutoAdjustSize;
var
  DC: HDC;
  Size: TSize;
  EditRect,
  TreeRect: TRect;
begin
  DC := GetDc(Handle);
  GetTextExtentPoint32(DC, PChar(Text), Length(Text), Size);
  // determine minimum and maximum sizes
  if Size.cx < 50 then Size.cx := 50;
  EditRect := ClientRect;
  MapWindowPoints(Handle, HWND_DESKTOP, EditRect, 2);
  TreeRect := FLink.FTree.ClientRect;
  MapWindowPoints(FLink.FTree.Handle, HWND_DESKTOP, TreeRect, 2);
  if (EditRect.Left + Size.cx) > TreeRect.Right then Size.cx := TreeRect.Right - EditRect.Left;
  SetWindowPos(Handle, 0, 0, 0, Size.cx, Height, SWP_NOMOVE or SWP_NOOWNERZORDER or SWP_NOZORDER);
  ReleaseDC(Handle, DC);
end;

procedure TMVCEdit.CreateParams(var Params:TCreateParams);
begin
  Ctl3D := False;
  inherited;
end;

procedure TMVCEditLink.ProcessMessage(var Message: TMessage);
begin
  // nothing to do
end;

constructor TMVCTreeView.Create(AOwner: TComponent);
begin
  inherited;
  FInternalDataOffset := AllocateInternalDataArea(SizeOf(Pointer));
end;

function TMVCTreeView.GetOptions: TVirtualTreeOptions;

begin
  Result := inherited TreeOptions as TVirtualTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TMVCTreeView.SetOptions(const Value: TVirtualTreeOptions);

begin
  TreeOptions.Assign(Value);
end;

//----------------------------------------------------------------------------------------------------------------------

function TMVCTreeView.GetOptionsClass: TTreeOptionsClass;

begin
  Result := TVirtualTreeOptions;
end;

//----------------------------------------------------------------------------------------------------------------------

end.
