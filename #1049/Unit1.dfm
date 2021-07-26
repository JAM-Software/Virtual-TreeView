object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 224
  ClientWidth = 574
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    574
    224)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 452
    Top = 55
    Width = 37
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Position'
  end
  object Label2: TLabel
    Left = 453
    Top = 8
    Width = 27
    Height = 13
    Anchors = [akTop, akRight]
    Caption = 'Name'
  end
  object edIndex: TEdit
    Left = 446
    Top = 72
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 1
  end
  object btnAdd: TButton
    Left = 446
    Top = 99
    Width = 121
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Add'
    TabOrder = 2
    OnClick = btnAddClick
  end
  object btnRemove: TButton
    Left = 446
    Top = 135
    Width = 121
    Height = 30
    Anchors = [akTop, akRight]
    Caption = 'Remove position'
    TabOrder = 3
    OnClick = btnRemoveClick
  end
  object edName: TEdit
    Left = 446
    Top = 28
    Width = 121
    Height = 21
    Anchors = [akTop, akRight]
    TabOrder = 0
  end
  object VirtualStringTree1: TVirtualStringTree
    Left = 8
    Top = 8
    Width = 432
    Height = 208
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    Header.AutoSizeIndex = 0
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoOwnerDraw, hoVisible]
    Header.Style = hsFlatButtons
    TabOrder = 4
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toGridExtensions, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowDropmark, toShowHorzGridLines, toShowTreeLines, toShowVertGridLines, toUseBlendedImages, toUseBlendedSelection]
    TreeOptions.SelectionOptions = [toExtendedFocus, toRightClickSelect]
    OnGetText = VirtualStringTree1GetText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
  end
  object chkBeginEndUpdate: TCheckBox
    Left = 446
    Top = 171
    Width = 120
    Height = 17
    Caption = 'Begin/EndUpdate'
    TabOrder = 5
  end
end
