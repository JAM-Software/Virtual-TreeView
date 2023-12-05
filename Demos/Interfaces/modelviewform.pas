unit modelviewform;

{ Demo showing the use of Interfaces for:
  -- Data of each node
  -- A presenter object that sets up and encapsulates the Virtual Tree View logic

  Application overview:
  -- Displays a list of charity events (randomly generated).
  -- Each row displays a date, event name and the amount collected.
  -- Any event collecting more than 300,000 is marked as a Star event,
     shown in the first column.
  -- Click on the buttons to show All or Only Starred events.
  -- Click on column headers to sort on that column.

  Advantage of using Interface for node data object:
  -- One big advantage of interfaces is that you don't have to worry about
     the freeing of objects (automatic garbage collection), especially
     of the node data objects. See unit myevents.pas that creates those
     node objects.
  -- Another advantage is the better extensibility provided by interfaces
    (recall COM objects).

  Uses a presenter Interface object IEventPresenter (unit myevents) that:
  -- deals with the creation of Node Data objects IMyEventData (unit myeventdata)
  -- sets up the proper tree options
  -- handles Virtual Tree events inside it including the call backs for sorting

  This demo serves as an example of Model-View-Presenter where the Presenter
  object is also an interface that can be reused by any form to show the
  same Tree View.
  Note: Such a Presenter can also be made by making a derived component
  based on Virtual Tree View. This is just an alternate example of reuse
  by making the Presenter an interface that is a sort of code-first
  approach.

  Advantage of a Presenter object:
  -- Simplifies the code of the application form where the form
     does not need to handle events. The form simply sets up the
     presenter object IEventPresenter that encapsulates the showing of
     the view (Tree View).
  -- Application also does not have to deal with tree options needed
     as they are handled by the presenter. For example, we deliberately
     set a larger font for this form. The presenter takes care to
     use the same font for the header otherwise it will look odd.
  -- Custom application methods can be implemented in the presenter
     that change the behavior of the view in a model-view paradigm.
     For example, see the use of method displayOnlyStarEvents.
  -- The presenter object IEventPresenter can be reused easily in another
     form that needs to show the same kind of Tree View. This is
     an alternative way of reuse as compared to making a derived
     Virtual Tree View component.

  Written by Joachim Marder, Sanjay Kanade

}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, Vcl.StdCtrls, Vcl.ImgList, myevents,
  System.ImageList, VirtualTrees.BaseAncestorVCL, VirtualTrees.BaseTree,
  VirtualTrees.AncestorVCL;

type
  TFormModelView = class(TForm)
    ImageList1: TImageList;
    btDisplayStars: TButton;
    btDisplayAll: TButton;
    VST: TVirtualStringTree;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btDisplayStarsClick(Sender: TObject);
    procedure btDisplayAllClick(Sender: TObject);
  private
    myEvents: IEventPresenter;
  public
    { Public declarations }
  end;

var
  FormModelView: TFormModelView;

implementation

{$R *.dfm}

uses System.DateUtils;

//----------------------------------------------------------------------------------------------------------------------
procedure TFormModelView.FormCreate(Sender: TObject);
begin
  //set up the presenter object
  myEvents := TEventPresenter.Create(VST, imageList1);
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TFormModelView.btDisplayStarsClick(Sender: TObject);
begin
  //display only star charity events that collected more than
  //a minimum amount
  myEvents.displayOnlyStarEvents(True);
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TFormModelView.btDisplayAllClick(Sender: TObject);
begin
  //display all charity events
  myEvents.displayOnlyStarEvents(False);
end;


end.
