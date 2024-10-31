unit States;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, VirtualTrees.BaseTree;

type
  TStateForm = class(TForm)
    EnableCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox32: TCheckBox;
    GroupBox2: TGroupBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox43: TCheckBox;
    CheckBox44: TCheckBox;
    GroupBox3: TGroupBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    GroupBox4: TGroupBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox37: TCheckBox;
    CheckBox41: TCheckBox;
    CheckBox42: TCheckBox;
    GroupBox5: TGroupBox;
    CheckBox31: TCheckBox;
    CheckBox36: TCheckBox;
    CheckBox39: TCheckBox;
    CheckBox40: TCheckBox;
    GroupBox6: TGroupBox;
    CheckBox2: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    GroupBox7: TGroupBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox33: TCheckBox;
    GroupBox8: TGroupBox;
    CheckBox26: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox34: TCheckBox;
    CheckBox35: TCheckBox;
    CheckBox38: TCheckBox;
    CheckBox45: TCheckBox;
    CheckBox46: TCheckBox;
    procedure EnableCheckBoxClick(Sender: TObject);
  protected
    procedure SetActiveState(CheckBox: TCheckBox; Active: Boolean);
  end;

var
  StateForm: TStateForm;

procedure UpdateStateDisplay(CurrentStates, Enter, Leave: TVirtualTreeStates);

//----------------------------------------------------------------------------------------------------------------------

implementation

{$R *.dfm}

uses
  VirtualTrees.Types;

//----------------------------------------------------------------------------------------------------------------------

procedure UpdateStateDisplay(CurrentStates, Enter, Leave: TVirtualTreeStates);

var
  NewStates: TVirtualTreeStates;

begin
  with StateForm do
    if EnableCheckBox.Checked then
    begin
      // At this point the new states are not yet set. So construct the new set here.
      NewStates := CurrentStates + Enter - Leave;
      SetActiveState(CheckBox1, tsChangePending in NewStates);
      SetActiveState(CheckBox2, tsCollapsing in NewStates);
      SetActiveState(CheckBox3, tsToggleFocusedSelection in NewStates);
      SetActiveState(CheckBox4, tsClearPending in NewStates);
      SetActiveState(CheckBox5, tsClipboardFlushing in NewStates);
      SetActiveState(CheckBox6, tsCopyPending in NewStates);
      SetActiveState(CheckBox7, tsCutPending in NewStates);
      SetActiveState(CheckBox8, tsDrawSelPending in NewStates);
      SetActiveState(CheckBox9, tsDrawSelecting in NewStates);
      SetActiveState(CheckBox10, tsEditing in NewStates);
      SetActiveState(CheckBox11, tsEditPending in NewStates);
      SetActiveState(CheckBox12, tsExpanding in NewStates);
      SetActiveState(CheckBox13, tsHint in NewStates);
      SetActiveState(CheckBox14, tsInAnimation in NewStates);
      SetActiveState(CheckBox15, tsIncrementalSearching in NewStates);
      SetActiveState(CheckBox16, tsIncrementalSearchPending in NewStates);
      SetActiveState(CheckBox17, tsIterating in NewStates);
      SetActiveState(CheckBox19, tsLeftButtonDown in NewStates);
      SetActiveState(CheckBox21, tsMiddleButtonDown in NewStates);
      SetActiveState(CheckBox23, tsNeedRootCountUpdate in NewStates);
      SetActiveState(CheckBox24, tsOLEDragging in NewStates);
      SetActiveState(CheckBox25, tsOLEDragPending in NewStates);
      SetActiveState(CheckBox26, tsPainting in NewStates);
      SetActiveState(CheckBox27, tsRightButtonDown in NewStates);
      SetActiveState(CheckBox28, tsScrolling in NewStates);
      SetActiveState(CheckBox29, tsScrollPending in NewStates);
      SetActiveState(CheckBox30, tsSizing in NewStates);
      SetActiveState(CheckBox31, tsStopValidation in NewStates);
      SetActiveState(CheckBox32, tsStructureChangePending in NewStates);
      SetActiveState(CheckBox33, tsSynchMode in NewStates);
      SetActiveState(CheckBox34, tsThumbTracking in NewStates);
      SetActiveState(CheckBox36, tsUseCache in NewStates);
      SetActiveState(CheckBox37, tsUserDragObject in NewStates);
      SetActiveState(CheckBox38, tsUseThemes in NewStates);
      SetActiveState(CheckBox39, tsValidating in NewStates);
      SetActiveState(CheckBox40, tsValidationNeeded in NewStates);
      SetActiveState(CheckBox41, tsVCLDragging in NewStates);
      SetActiveState(CheckBox42, tsVCLDragPending in NewStates);
      SetActiveState(CheckBox43, tsWheelPanning in NewStates);
      SetActiveState(CheckBox44, tsWheelScrolling in NewStates);
      SetActiveState(CheckBox45, tsWindowCreating in NewStates);
      SetActiveState(CheckBox46, tsPopupMenuShown in NewStates);

      Update;
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStateForm.EnableCheckBoxClick(Sender: TObject);

begin
  CheckBox1.Enabled := EnableCheckBox.Checked;
  CheckBox2.Enabled := EnableCheckBox.Checked;
  CheckBox3.Enabled := EnableCheckBox.Checked;
  CheckBox4.Enabled := EnableCheckBox.Checked;
  CheckBox5.Enabled := EnableCheckBox.Checked;
  CheckBox6.Enabled := EnableCheckBox.Checked;
  CheckBox7.Enabled := EnableCheckBox.Checked;
  CheckBox8.Enabled := EnableCheckBox.Checked;
  CheckBox9.Enabled := EnableCheckBox.Checked;
  CheckBox10.Enabled := EnableCheckBox.Checked;
  CheckBox11.Enabled := EnableCheckBox.Checked;
  CheckBox12.Enabled := EnableCheckBox.Checked;
  CheckBox13.Enabled := EnableCheckBox.Checked;
  CheckBox14.Enabled := EnableCheckBox.Checked;
  CheckBox15.Enabled := EnableCheckBox.Checked;
  CheckBox16.Enabled := EnableCheckBox.Checked;
  CheckBox17.Enabled := EnableCheckBox.Checked;
  CheckBox19.Enabled := EnableCheckBox.Checked;
  CheckBox21.Enabled := EnableCheckBox.Checked;
  CheckBox23.Enabled := EnableCheckBox.Checked;
  CheckBox24.Enabled := EnableCheckBox.Checked;
  CheckBox25.Enabled := EnableCheckBox.Checked;
  CheckBox26.Enabled := EnableCheckBox.Checked;
  CheckBox27.Enabled := EnableCheckBox.Checked;
  CheckBox28.Enabled := EnableCheckBox.Checked;
  CheckBox29.Enabled := EnableCheckBox.Checked;
  CheckBox30.Enabled := EnableCheckBox.Checked;
  CheckBox31.Enabled := EnableCheckBox.Checked;
  CheckBox32.Enabled := EnableCheckBox.Checked;
  CheckBox33.Enabled := EnableCheckBox.Checked;
  CheckBox34.Enabled := EnableCheckBox.Checked;
  CheckBox35.Enabled := EnableCheckBox.Checked;
  CheckBox36.Enabled := EnableCheckBox.Checked;
  CheckBox37.Enabled := EnableCheckBox.Checked;
  CheckBox38.Enabled := EnableCheckBox.Checked;
  CheckBox39.Enabled := EnableCheckBox.Checked;
  CheckBox40.Enabled := EnableCheckBox.Checked;
  CheckBox41.Enabled := EnableCheckBox.Checked;
  CheckBox42.Enabled := EnableCheckBox.Checked;
  CheckBox43.Enabled := EnableCheckBox.Checked;
  CheckBox44.Enabled := EnableCheckBox.Checked;
  CheckBox45.Enabled := EnableCheckBox.Checked;
  CheckBox46.Enabled := EnableCheckBox.Checked;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TStateForm.SetActiveState(CheckBox: TCheckBox; Active: Boolean);

begin
  CheckBox.Checked := Active;
  if Active then
    CheckBox.Font.Style := [fsBold]
  else
    CheckBox.Font.Style := [];
end;

//----------------------------------------------------------------------------------------------------------------------

end.
