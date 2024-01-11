unit myevents;

{
  IEventPresenter:
  The presenter object that connects to the Virtual Tree View passed
  in Setup method called in Create.
  -- Sets up the columns
  -- Sets up the events and callbacks, including those needed for
     sorting.
  -- Implements the display methods.

  Use of Interfaces:
  -- See OnInitNode for an example of how it sets the interface data
     object IMyEventData as the data for the node.
  -- Interfaced objects are reference-counted, you don't need to free them.
  -- Even this object, IEventPresenter, is designed as an interface for reuse
     by forms that need to display a similar Tree View. This is an
     example of a reuse without making a derived component of the
     Virtual Tree View.

  Written by Joachim Marder, Sanjay Kanade
}

interface

uses Vcl.ImgList, VirtualTrees, System.Classes, System.UITypes, Vcl.Controls, VirtualTrees.Types;

type
  IEventPresenter = interface
    ['{1984E951-24C5-4484-B470-BBADBDA70EEA}']
    procedure displayOnlyStarEvents(aOnlyStarEvents: Boolean); stdcall;
  end;

  //With virtual methods that can be overridden by a derived
  //class.
  TEventPresenter = class(TInterfacedObject, IEventPresenter)
  private
    fVST: TVirtualStringTree;
    fImageList: TImageList; //Just in case we need it

    fOnlyStarEvents: boolean;

  protected
    //events to be hooked up
    procedure doOnInitNode(Sender: TBaseVirtualTree; ParentNode,
          Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates); virtual;
    procedure doOnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string); virtual;
    procedure doOnGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex); virtual;
    procedure doOnCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer); virtual;

    procedure processForStarDisplay(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);

    //column display functions
    function getDisplayDate(aDate: TDateTime): string;
    function getDisplayName(aName: string): string;
    function getDisplayAmount(anAmount: currency): string;
    procedure setup(aVST: TVirtualStringTree; anImageList: TImageList); virtual; stdcall;

  public
    constructor Create(aVST: TVirtualStringTree; anImageList: TImageList);
    destructor Destroy; override;
    procedure displayOnlyStarEvents(aOnlyStarEvents: Boolean); virtual; stdcall;
  end;

implementation

uses myeventdata, System.DateUtils, System.SysUtils, Winapi.Windows, System.Math;

//----------------------------------------------------------------------------------------------------------------------
constructor TEventPresenter.Create(aVST: TVirtualStringTree; anImageList: TImageList);
begin
  inherited Create();
  //initialize
  fOnlyStarEvents := false;
  setup(aVST, anImageList);
end;

//----------------------------------------------------------------------------------------------------------------------
destructor TEventPresenter.Destroy;
begin
  //do cleanup
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.setup(aVST: TVirtualStringTree; anImageList: TImageList); stdcall;
var
  col: TVirtualTreeColumn;
begin
  fVST := aVST;
  fImageList := anImageList;

  //hook up events
  fVST.OnInitNode := doOnInitNode;
  fVST.OnGetText := doOnGetText;
  fVST.OnGetImageIndex := doOnGetImageIndex;
  fVST.OnCompareNodes := doOnCompareNodes;

  //set up columns
  col := fVST.header.Columns.Add;
  col.Text := 'Star Event';
  col.Hint := 'Number of Stars';
  col.Width := 120;
  col := fVST.header.Columns.Add;
  col.Text := 'Date';
  col.Hint := 'The date of the event';
  col.Width := 100;
  col := fVST.header.Columns.Add;
  col.Text := 'Charity Event Name';
  col.Width := 180;
  col := fVST.header.Columns.Add;
  col.Text := 'Amount Collected';
  col.Width := 180;
  col.Alignment := taRightJustify;

  //settings that we want
  fVST.Header.AutoSizeIndex := -1;
  //If the form uses a larger font, the header should use the same
  fVST.Header.Font.Assign(fVST.font);
  fVST.Header.Options := fVST.Header.Options + [TVTHeaderOption.hoVisible, TVTHeaderOption.hoHeaderClickAutoSort];
  fVST.TreeOptions.PaintOptions := fVST.TreeOptions.PaintOptions
            - [TVTPaintOption.toShowRoot, TVTPaintOption.toShowTreeLines];
  fVST.TreeOptions.SelectionOptions := fVST.TreeOptions.SelectionOptions
            + [TVTSelectionOption.toFullRowSelect];
  fVST.TreeOptions.AutoOptions := fVST.TreeOptions.AutoOptions + [TVTAutoOption.toAutoSort];

  //generate 20 events as part of setup
  fVST.RootNodeCount := 20;

  //By default sort descrending on date
  fVST.Header.SortDirection := sdDescending;
  fVST.Header.SortColumn := 1;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.doOnInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  myEvent: IMyEventData;
begin
  //generate a fictitious event for our testing
  myEvent := TMyEventData.Create;
  myEvent.initializeRandom;
  //This is how an interface is set as data for the node
  Node.SetData(myEvent);
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.doOnGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
  myEvent: IMyEventData;
begin
  if (Kind <> ikNormal) and (Kind <> ikSelected) then
    exit;

  myEvent := Sender.GetInterfaceFromNodeData<IMyEventData>(Node);
  if Assigned(myEvent) and (column = 0)
     and myEvent.isStarEvent
  then
    //provide the index of the star image if it is a star event
    ImageIndex := 15;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.doOnCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
var
  myEvent1, myEvent2: IMyEventData;
begin
  myEvent1 := Sender.GetInterfaceFromNodeData<IMyEventData>(Node1);
  myEvent2 := Sender.GetInterfaceFromNodeData<IMyEventData>(Node2);
  case column of
    0:
       result := Floor(myEvent2.amount - myEvent1.amount); //reverse sort on amount
    1:
       if myEvent1.date = myEvent2.date then
         result := 0
       else
       if myEvent1.date > myEvent2.date then
         result := 1
       else
         result := -1;
    2:
       result := comparetext(myEvent1.name, myEvent2.name);
    3:
       result := Floor(myEvent1.amount - myEvent2.amount);
  end;
end;


//----------------------------------------------------------------------------------------------------------------------
//called for each node by next function displayOnlyStarEvents
procedure TEventPresenter.processForStarDisplay(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer; var Abort: Boolean);
var
  myEvent: IMyEventData;
begin
  myEvent := Sender.GetInterfaceFromNodeData<IMyEventData>(Node);
  //the node is visible based on the flag and whether it is a star event
  Sender.IsVisible[Node] := (not fOnlyStarEvents) or myEvent.isStarEvent;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.displayOnlyStarEvents(aOnlyStarEvents: Boolean); stdcall;
begin
  fOnlyStarEvents := aOnlyStarEvents;
  fVST.BeginUpdate;
  try
    //iterate the tree setting the visiblity of the nodes based on the above flag
    fVST.IterateSubtree(nil, processForStarDisplay, nil, [], True);
  finally
    fVST.EndUpdate;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------
function TEventPresenter.getDisplayDate(aDate: TDateTime): string;
begin
  result := FormatDateTime('c', aDate);
end;

//----------------------------------------------------------------------------------------------------------------------
function TEventPresenter.getDisplayName(aName: string): string;
begin
  result := aName;
end;

//----------------------------------------------------------------------------------------------------------------------
function TEventPresenter.getDisplayAmount(anAmount: currency): string;
begin
  result := CurrToStrF(anAmount, ffCurrency, 2, formatSettings);
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TEventPresenter.doOnGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  myEvent: IMyEventData;
begin
  //the format of the display is handled in display methods
  myEvent := Sender.GetInterfaceFromNodeData<IMyEventData>(Node);
  if Assigned(myEvent) then
  case column of
    1: CellText := getDisplayDate(myEvent.date);
    2: CellText := getDisplayName(myEvent.name);
    3: CellText := getDisplayAmount(myEvent.amount);
  end;
end;

end.
