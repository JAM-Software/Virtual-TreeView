unit myeventdata;

{
  IMyEventData:
  The data object for each event. For this particular demo, it also
  includes the method to generate a random event for creating test data.

  Written by Joachim Marder, Sanjay Kanade
}
interface

type
  IMyEventData = interface
    ['{0CDF0FE6-6B98-4541-B644-729513B41044}']
    procedure SetDate(adate: TDateTime); stdcall;
    procedure SetName(aName: string); stdcall;
    procedure SetAmount(anAmount: currency); stdcall;
    function GetDate: TDateTime; stdcall;
    function GetName: string; stdcall;
    function GetAmount: currency; stdcall;

    //Generates a fictitious event for the demo
    procedure initializeRandom; stdcall;

    //properties for raw data
    //date of event
    property date: TDateTime read GetDate write SetDate;
    //name of event
    property name: string read GetName write SetName;
    //amount collected in the event
    property amount: currency read GetAmount write SetAmount;

    function isStarEvent: boolean; stdcall;
  end;

  TMyEventData = class(TInterfacedObject, IMyEventData)
  private
    fdate: TDateTime;
    fname: string;
    famount: currency;
  protected
    procedure SetDate(adate: TDateTime); stdcall;
    procedure SetName(aName: string); stdcall;
    procedure SetAmount(anAmount: currency); stdcall;
    function GetDate: TDateTime; stdcall;
    function GetName: string; stdcall;
    function GetAmount: currency; stdcall;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure initializeRandom; virtual; stdcall;

    property date: TDateTime read GetDate write SetDate;
    property name: string read GetName write SetName;
    property amount: currency read GetAmount write SetAmount;

    function isStarEvent: boolean; virtual; stdcall;
  end;

implementation

uses System.DateUtils, System.SysUtils;

//----------------------------------------------------------------------------------------------------------------------
constructor TMyEventData.Create;
begin
  inherited;
  //initialize
  famount := -1;
end;

//----------------------------------------------------------------------------------------------------------------------
destructor TMyEventData.Destroy;
begin
  //do cleanup
  //To test whether auto cleanup is done
  //showmessage('Cleaning up '+fname);
  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TMyEventData.SetDate(adate: TDateTime); stdcall;
begin
  fdate := adate;
end;

//----------------------------------------------------------------------------------------------------------------------
function TMyEventData.GetDate: TDateTime; stdcall;
begin
  result := fdate;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TMyEventData.SetName(aName: string); stdcall;
begin
  fname := aName;
end;

//----------------------------------------------------------------------------------------------------------------------
function TMyEventData.GetName: string; stdcall;
begin
  result := fname;
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TMyEventData.SetAmount(anAmount: currency); stdcall;
begin
  famount := anAmount;
end;

//----------------------------------------------------------------------------------------------------------------------
function TMyEventData.GetAmount: currency; stdcall;
begin
  result := famount;
end;

//----------------------------------------------------------------------------------------------------------------------
function getRandomDate: TDateTime;
begin
  result := Today;
  result := IncDay(result, -random(365));
end;

//----------------------------------------------------------------------------------------------------------------------
procedure TMyEventData.initializeRandom;
begin
  fdate := getRandomDate;
  fname := Format('Charity event %d', [random(9999)+1]);
  famount := random(500000);
  while famount < 50000 do
    famount := random(500000);
end;

//----------------------------------------------------------------------------------------------------------------------
//An event is a star event if it collected an amount > 300,000.
function TMyEventData.isStarEvent: boolean;
begin
  result := famount > 300000;
end;

end.
