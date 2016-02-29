unit VirtualTreeWrapper;

// The contents of this file are subject to
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for the
// specific language governing rights and limitations under the License.
//
// Original code released 02-15-2011
//
// Copyright (C) 2011 VUTS Liberec (Jan Rames ramejan@gmail.com)

interface

uses SysUtils, Classes, Controls, VirtualTrees, Generics.Collections,
     RTLConsts, UITypes;

type
  ///<summary>
  ///    Provides basic record wrapper functionality in terms of initializating
  ///    and finalizating the record's members (strings, interfaces) to prevent
  ///    memory leaks
  ///</summary>
  TBaseVirtualTreeWrapper<T : record> = class(TComponent)
  public
	type P = ^T;
	type TFreeProc = reference to procedure(var UserData : T);
  private
	FFreeNode	: TVTFreeNodeEvent;
	FFreeProc	: TFreeProc;
  protected
	///<summary>
	///    Assigns Tree's Properties and Events
	///</summary>
	procedure Init; virtual;
	function Tree : TVirtualStringTree; inline;

	//No virtual functions needed, just reassign particular events in create
	//of descendants
	{procedure InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
	  var InitialStates: TVirtualNodeInitStates); Cannot be relied upon, it'll
	  be called later even after the AddChild returns}
	procedure FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
  public
	constructor Create(AOwner : TComponent); overload; override;
	constructor Create(AOwner : TComponent; FreeProc : TFreeProc); reintroduce; overload;

	///<summary>
	///    Adds a node and initializes its data (do not ever call
	///    TVirtualStringTree.AddChild as it would fail to initialize it)
	///</summary>
	function AddChild(Parent : PVirtualNode) : PVirtualNode; inline;
	function GetUserData(Node : PVirtualNode) : P; inline;

	property Data[Node : PVIrtualNode] : P read GetUserData;
  end;

  TVirtualTreeWrapper<T : record> = class;

  ///<summary>
  ///    Record that stores Item's common data
  ///</summary>
  TVSTData = record
	Caption		: string;
	Hint		: string;
	ImageIndex	: Integer;
  end;
  PVSTData = ^TVSTData;

  TVirtualNodeEnumerator<T : record> = class;
  ///<summary>
  ///    Record that can be used to operate the node with object-like manner
  ///</summary>
  TVirtualNode<T : record> = record
  strict private
	Tree	: TVirtualTreeWrapper<T>;
	FNode	: PVirtualNode;
	FData	: PVSTData;

	function GetData : TBaseVirtualTreeWrapper<T>.P;

	function GetCaption : string;
	procedure SetCaption(const ACaption : string);
	function GetHint : string;
	procedure SetHint(const AHint : string);
	function GetImageIndex : Integer;
	procedure SetImageIndex(const AImageIndex : Integer);
	function GetCheckState : TCheckState;
	procedure SetCheckState(Value: TCheckState);
	function GetCheckType : TCheckType;
	procedure SetCheckType(Value: TCheckType);
	function GetLevel : Integer;
	function GetIndex : Integer;
	function GetItem(Index : Cardinal) : TVirtualNode<T>;
	function GetChildCount : Cardinal;
	function GetMultiLine: Boolean;
	procedure SetMultiLine(const Value: Boolean);
	function GetHasChildren: Boolean;
	procedure SetHasChildren(const Value: Boolean);
  private
	procedure Create(ATree : TVirtualTreeWrapper<T>; ANode : PVirtualNode);
	function GetIsEmpty: Boolean;
  public
	class function Empty : TVirtualNode<T>; static;
	class operator Implicit(const Self: TVirtualNode<T>): PVirtualNode; inline;

	function AddChild : TVirtualNode<T>; overload;
	function AddChild(const ACaption : string) : TVirtualNode<T>; overload;
	procedure Delete;
	procedure MakeVisible(Recursive : Boolean = false);

	function Parent: TVirtualNode<T>;
	function FirstChild : TVirtualNode<T>;
	function NextSibling : TVirtualNode<T>;
	function PrevSiblinng : TVirtualNode<T>;

	function GetEnumerator : TVirtualNodeEnumerator<T>;

	property Node		: PVirtualNode read FNode;
	property Level		: Integer	read GetLevel;
	property Index		: Integer	read GetIndex;
	property ChildCount	: Cardinal	read GetChildCount;
	property Data		: TBaseVirtualTreeWrapper<T>.P read GetData;
	property Caption	: string	read GetCaption		write SetCaption;
	property Hint		: string	read GetHint		write SetHint;
	property ImageIndex	: Integer	read GetImageIndex	write SetImageIndex;
	property CheckState	: TCheckState read GetCheckState write SetCheckState;
	property CheckType	: TCheckType read GetCheckType	write SetCheckType;
	property MultiLine	: Boolean	read GetMultiLine	write SetMultiLine;
	property HasChildren: Boolean	read GetHasChildren	write SetHasChildren;
	property Items[Index : Cardinal]	: TVirtualNode<T> read GetItem; default;
	property IsEmpty	: Boolean	read GetIsEmpty;
  end;

  ///<summary>
  ///    Provides enhanced record wrapper functionality with caption, hint, etc.
  ///    make sure that your record begins with TVSTData:
  ///    record
  ///        Info : TVSTData;
  ///        Data1: Type1;
  ///        Data2: Type2;
  ///        ...
  ///    end;
  ///</summary>
  TVirtualTreeWrapper<T : record> = class(TBaseVirtualTreeWrapper<T>)
  public
	type P = TBaseVirtualTreeWrapper<T>.P;
  private
  	FUpdateCount	: Integer;
	function GetItem(Node: PVirtualNode): TVirtualNode<T>; inline;
  protected
	procedure Init; override;
	function GetData(Node : PVirtualNode) : PVSTData; inline;
	procedure NodeUpdated(Node : PVirtualNode); inline;

	//No virtual functions needed, just reassign particular events in create
	//of descendants

	procedure DoGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
	  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
	procedure DoGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
	  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: UnicodeString);
	procedure DoGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
	  var Ghosted: Boolean; var ImageIndex: TImageIndex);
  public
	constructor Create(AOwner : TComponent); override;

	function AddChild(ParentNode : PVirtualNode) : PVirtualNode; overload; inline;
	function AddChild(ParentNode : PVirtualNode; const Caption : UnicodeString) : PVirtualNode; overload; inline;
	function AddChild(ParentNode : PVirtualNode; const Caption, Hint : UnicodeString) : PVirtualNode; overload; inline;
	procedure DeleteNode(Node: PVirtualNode); inline;

	function GetCaption(Node : PVirtualNode) : string; inline;
	procedure SetCaption(Node : PVirtualNode; const ACaption : string); inline;
	function GetHint(Node : PVirtualNode) : string; inline;
	procedure SetHint(Node : PVirtualNode; const AHint : string); inline;
	function GetImageIndex(Node : PVirtualNode) : Integer; inline;
	procedure SetImageIndex(Node : PVirtualNode; const AImageIndex : Integer); inline;

	///<summary>
	///    Finds node with given Caption returns first node found or nil
	///    (if no match was found). Search is only limited to Childs of
	///    ParentNode. If ParentNode is nil, base of the tree is searched.
    ///    For extended search use Incremental search feature of the VirtualTree
    ///</summary>
	function FindNode(const ACaption : string; ParentNode : PVirtualNode = nil) : PVirtualNode;

	procedure BeginUpdate;
	procedure EndUpdate;

	///<summary>
	///    Returns IVirtualNode nased on PVirtualNode (if set to nil, root is
	///    returned)
    ///</summary>
	property Items[Node : PVirtualNode] : TVirtualNode<T>	read GetItem; default;
  end;

  TVirtualNodeEnumerator<T : record> = class
  strict private
	Node	: PVirtualNode;
	FCurrent : PVirtualNode;
	Tree	: TVirtualTreeWrapper<T>;
	function GetCurrent	: TVirtualNode<T>; inline;
  public
	constructor Create(ATree : TVirtualTreeWrapper<T>; ANode : PVirtualNode);
	function MoveNext : Boolean; inline;
	property Current : TVirtualNode<T> read GetCurrent;
  end;

implementation

{ TBaseVirtualTreeViewWrapper<T> }

function TBaseVirtualTreeWrapper<T>.AddChild(Parent: PVirtualNode): PVirtualNode;
var Ptr	: P;
begin
	Result:=Tree.AddChild(Parent);
	//Treat the node as if it has some initial data which causes calling of
	//OnFreeNode even if the node hasn't been already initialized
	Include(Result^.States, vsOnFreeNodeCallRequired);
	Ptr:=Tree.GetNodeData(Result);
	//Not needed as VirtualTree uses AllocMem which nils (it zeros the entire
	//memory block) all pointers that Initialize nils (basically this is the
	//same thing that SetLength for dynamic arrays does).
	//Initialize(Ptr^);
end;

constructor TBaseVirtualTreeWrapper<T>.Create(AOwner: TComponent; FreeProc : TFreeProc);
begin
	Create(AOwner);
	FFreeProc:=FreeProc;
end;

constructor TBaseVirtualTreeWrapper<T>.Create(AOwner: TComponent);
begin
	inherited Create(AOwner as TVirtualStringTree);	//Make sure Owner is set correctly
	Init;
end;

procedure TBaseVirtualTreeWrapper<T>.FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var P	: ^T;
begin
	if (Assigned(FFreeNode)) then FFreeNode(Sender, Node);

	P:=Sender.GetNodeData(Node);
	if (Assigned(FFreeProc)) then FFreeProc(P^);

	Finalize(P^);
end;

function TBaseVirtualTreeWrapper<T>.GetUserData(Node: PVirtualNode): P;
begin
	Result:=Tree.GetNodeData(Node);
end;

procedure TBaseVirtualTreeWrapper<T>.Init;
begin
	Tree.NodeDataSize:=sizeof(T);

	//Tree.OnInitNode:=InitNode;
	FFreeNode:=Tree.OnFreeNode;
	Tree.OnFreeNode:=FreeNode;
end;

function TBaseVirtualTreeWrapper<T>.Tree: TVirtualStringTree;
begin
	Result:=TVirtualStringTree(Owner);
end;

{ TVirtualTreeViewWrapper<T> }

function TVirtualTreeWrapper<T>.AddChild(ParentNode: PVirtualNode): PVirtualNode;
var Data	: P;
begin
	Result:=inherited AddChild(ParentNode);
	Data:=GetUserData(Result);
	PVSTData(Data)^.ImageIndex:=-1;
end;

function TVirtualTreeWrapper<T>.AddChild(ParentNode: PVirtualNode;
  const Caption: UnicodeString): PVirtualNode;
var Data	: P;
begin
	Result:=inherited AddChild(ParentNode);
	Data:=GetUserData(Result);
	PVSTData(Data)^.Caption:=Caption;
	PVSTData(Data)^.ImageIndex:=-1;
	NodeUpdated(Result);
end;

function TVirtualTreeWrapper<T>.AddChild(ParentNode: PVirtualNode;
  const Caption, Hint: UnicodeString): PVirtualNode;
var Data	: P;
begin
	Result:=inherited AddChild(ParentNode);
	Data:=GetUserData(Result);
	PVSTData(Data)^.Caption:=Caption;
	PVSTData(Data)^.Hint:=Hint;
	PVSTData(Data)^.ImageIndex:=-1;
	NodeUpdated(Result);
end;

procedure TVirtualTreeWrapper<T>.BeginUpdate;
begin
	Inc(FUpdateCount);
	Tree.BeginUpdate;
end;

constructor TVirtualTreeWrapper<T>.Create(AOwner: TComponent);
begin
	inherited;
	FUpdateCount:=0;
end;

procedure TVirtualTreeWrapper<T>.EndUpdate;
begin
	Dec(FUpdateCount);
	Tree.EndUpdate;
	if (FUpdateCount <= 0) then begin
		//Tree.InvalidateChildren(nil, true);
		FUpdateCount:=0;
	end;
end;

function TVirtualTreeWrapper<T>.FindNode(const ACaption: string;
  ParentNode: PVirtualNode = nil): PVirtualNode;
begin
	if (ParentNode = nil) then ParentNode:=Tree.RootNode;

	ParentNode:=ParentNode^.FirstChild;
	Result:=nil;
	while ParentNode <> nil do begin
		if (GetData(ParentNode)^.Caption = ACaption) then Exit(ParentNode);
		ParentNode:=ParentNode.NextSibling;
	end;
end;

function TVirtualTreeWrapper<T>.GetCaption(Node: PVirtualNode): string;
begin
	Result:=GetData(Node)^.Caption;
end;

function TVirtualTreeWrapper<T>.GetData(Node: PVirtualNode): PVSTData;
begin
	Result:=Tree.GetNodeData(Node);
end;

function TVirtualTreeWrapper<T>.GetHint(Node: PVirtualNode): string;
begin
	Result:=GetData(Node)^.Hint;
end;

function TVirtualTreeWrapper<T>.GetImageIndex(Node: PVirtualNode): Integer;
begin
	Result:=GetData(Node)^.ImageIndex;
end;

function TVirtualTreeWrapper<T>.GetItem(Node: PVirtualNode): TVirtualNode<T>;
begin
	Result.Create(Self, Node);
end;

procedure TVirtualTreeWrapper<T>.DeleteNode(Node: PVirtualNode);
begin
	Tree.DeleteNode(Node);
end;

procedure TVirtualTreeWrapper<T>.DoGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: UnicodeString);
begin
	HintText:=GetData(Node)^.Hint;
end;

procedure TVirtualTreeWrapper<T>.DoGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
	ImageIndex:=GetData(Node)^.ImageIndex;
end;

procedure TVirtualTreeWrapper<T>.DoGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
begin
	CellText:=GetData(Node)^.Caption;
end;

procedure TVirtualTreeWrapper<T>.Init;
begin
	inherited;

	Tree.OnGetText:=DoGetText;
	Tree.OnGetHint:=DoGetHint;
	Tree.OnGetImageIndex:=DoGetImageIndex;
end;

procedure TVirtualTreeWrapper<T>.NodeUpdated(Node: PVirtualNode);
begin
	if (FUpdateCount = 0) then Tree.InvalidateNode(Node);
end;

procedure TVirtualTreeWrapper<T>.SetCaption(Node: PVirtualNode;
  const ACaption: string);
begin
	GetData(Node)^.Caption:=ACaption;
	NodeUpdated(Node);
end;

procedure TVirtualTreeWrapper<T>.SetHint(Node: PVirtualNode; const AHint: string);
begin
	GetData(Node)^.Hint:=AHint;
	NodeUpdated(Node);
end;

procedure TVirtualTreeWrapper<T>.SetImageIndex(Node: PVirtualNode;
  const AImageIndex: Integer);
begin
	NodeUpdated(Node);
end;

{ TVirtualNodeImpl }

function TVirtualNode<T>.AddChild: TVirtualNode<T>;
begin
	Result.Create(Tree, Tree.AddChild(FNode));
end;

function TVirtualNode<T>.AddChild(const ACaption: string): TVirtualNode<T>;
begin
	Result.Create(Tree, Tree.AddChild(FNode, ACaption));
end;

procedure TVirtualNode<T>.Create(ATree: TVirtualTreeWrapper<T>;
  ANode: PVirtualNode);
begin
	Tree:=ATree;
	FNode:=ANode;
	if (FNode = nil) then FData:=nil
	else FData:=ATree.GetData(FNode);
end;

function TVirtualNode<T>.GetCaption: string;
begin
	Result:=FData^.Caption;
end;

function TVirtualNode<T>.GetCheckState: TCheckState;
begin
	Result:=Tree.Tree.CheckState[FNode];
end;

function TVirtualNode<T>.GetCheckType: TCheckType;
begin
	Result:=Tree.Tree.CheckType[FNode];
end;

function TVirtualNode<T>.GetChildCount: Cardinal;
begin
	Result:=FNode^.ChildCount;
end;

function TVirtualNode<T>.GetData: TBaseVirtualTreeWrapper<T>.P;
begin
	Result:=Pointer(FData);
end;

function TVirtualNode<T>.GetEnumerator: TVirtualNodeEnumerator<T>;
begin
	Result:=TVirtualNodeEnumerator<T>.Create(Tree, FNode);
end;

function TVirtualNode<T>.GetHasChildren: Boolean;
begin
	Result:=Tree.Tree.HasChildren[FNode];
end;

function TVirtualNode<T>.GetHint: string;
begin
	Result:=FData^.Hint;
end;

function TVirtualNode<T>.GetImageIndex: Integer;
begin
	Result:=FData^.ImageIndex;
end;

function TVirtualNode<T>.GetIndex: Integer;
begin
	Result:=FNode^.Index;
end;

function TVirtualNode<T>.GetIsEmpty: Boolean;
begin
	Result:=FNode = nil;
end;

function TVirtualNode<T>.GetItem(Index: Cardinal): TVirtualNode<T>;
var i		: Integer;
	ANode	: PVirtualNode;
begin
	if (Index >= FNode^.ChildCount) then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

	ANode:=FNode^.FirstChild;
	if (Index > 0) then for i:=0 to Index - 1 do begin
		Assert(ANode <> nil, 'Node shouldn''t be nil, ChildCount incorrect');
		ANode:=ANode^.NextSibling;
	end;
	Result:=Tree.GetItem(ANode);
end;

function TVirtualNode<T>.GetLevel: Integer;
begin
	Result:=Tree.Tree.GetNodeLevel(FNode);
end;

function TVirtualNode<T>.GetMultiLine: Boolean;
begin
	Result:=Tree.Tree.MultiLine[FNode];
end;

class operator TVirtualNode<T>.Implicit(
  const Self: TVirtualNode<T>): PVirtualNode;
begin
	Result:=Self.FNode;
end;

procedure TVirtualNode<T>.MakeVisible(Recursive: Boolean);
var AParent	: PVirtualNode;
	List	: TList<PVirtualNode>;
	i		: Integer;
begin
	with Tree.Tree do begin
		List:=TList<PVirtualNode>.Create;
		try
			AParent:=FNode.Parent;
			// The root node is marked by having its NextSibling (and PrevSibling) pointing to itself.
			while (AParent <> nil) and (AParent^.NextSibling <> AParent) do begin
				if (vsExpanded in AParent^.States) then Break;
				List.Add(AParent);
				AParent:=AParent^.Parent;
			end;
			for i:=List.Count - 1 downto 0 do Expanded[List[i]]:=true;

			if (Recursive) then FullExpand(FNode)
			else Expanded[FNode]:=true;
		finally
			List.Free;
		end;
	end;
end;

function TVirtualNode<T>.NextSibling: TVirtualNode<T>;
begin
	if (FNode^.NextSibling = nil) then Exit(Empty);
	
	Result:=Tree.GetItem(FNode^.NextSibling);
end;

function TVirtualNode<T>.Parent: TVirtualNode<T>;
begin
	Result:=Tree.GetItem(FNode^.Parent);
end;

function TVirtualNode<T>.PrevSiblinng: TVirtualNode<T>;
begin
	if (FNode^.PrevSibling = nil) then Exit(Empty);
	
	Result:=Tree.GetItem(FNode^.PrevSibling);
end;

procedure TVirtualNode<T>.Delete;
begin
	Tree.DeleteNode(FNode);
end;

class function TVirtualNode<T>.Empty: TVirtualNode<T>;
begin
	Result.FNode:=nil;
	Result.FData:=nil;
end;

function TVirtualNode<T>.FirstChild: TVirtualNode<T>;
begin
	if (FNode^.FirstChild = nil) then Exit(Empty);
	
	Result:=Tree.GetItem(FNode^.FirstChild);
end;

procedure TVirtualNode<T>.SetCaption(const ACaption: string);
begin
	FData^.Caption:=ACaption;
	Tree.NodeUpdated(FNode);
end;

procedure TVirtualNode<T>.SetCheckState(Value: TCheckState);
begin
	Tree.Tree.CheckState[FNode]:=Value;
end;

procedure TVirtualNode<T>.SetCheckType(Value: TCheckType);
begin
	Tree.Tree.CheckType[FNode]:=Value;
end;

procedure TVirtualNode<T>.SetHasChildren(const Value: Boolean);
begin
	Tree.Tree.HasChildren[FNode]:=Value;
end;

procedure TVirtualNode<T>.SetHint(const AHint: string);
begin
	FData^.Hint:=AHint;
	Tree.NodeUpdated(FNode);
end;

procedure TVirtualNode<T>.SetImageIndex(const AImageIndex: Integer);
begin
	FData^.ImageIndex:=AImageIndex;
	Tree.NodeUpdated(FNode);
end;

procedure TVirtualNode<T>.SetMultiLine(const Value: Boolean);
begin
	Tree.Tree.MultiLine[FNode]:=Value;
end;

{ TVirtualNodeEnumerator<T> }

constructor TVirtualNodeEnumerator<T>.Create(ATree : TVirtualTreeWrapper<T>; 
	ANode: PVirtualNode);
begin
	Node:=ANode;
	FCurrent:=nil;
	Tree:=ATree;
end;

function TVirtualNodeEnumerator<T>.GetCurrent: TVirtualNode<T>;
begin
	Result:=Tree.GetItem(FCurrent);
end;

function TVirtualNodeEnumerator<T>.MoveNext: Boolean;
begin
	if (FCurrent = nil) then FCurrent:=Node^.FirstChild
	else FCurrent:=FCurrent^.NextSibling;

	Result:=FCurrent <> nil;
end;

end.
