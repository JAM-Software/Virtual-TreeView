object MainForm: TMainForm
  Left = 717
  Top = 268
  Width = 425
  Height = 481
  BiDiMode = bdLeftToRight
  Caption = 'Simple Virtual Treeview demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  OnCreate = FormCreate
  DesignSize = (
    409
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 111
    Height = 13
    Caption = 'Last operation duration:'
  end
  object VST: TVirtualStringTree
    Left = 8
    Top = 36
    Width = 397
    Height = 301
    Anchors = [akLeft, akTop, akRight, akBottom]
    BiDiMode = bdLeftToRight
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    DragMode = dmAutomatic
    DragType = dtVCL
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    HintAnimation = hatNone
    IncrementalSearch = isAll
    ParentBiDiMode = False
    RootNodeCount = 100
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toEditable, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect, toCenterScrollIntoView]
    OnFreeNode = VSTFreeNode
    OnInitNode = VSTInitNode
    OnStartDrag = VSTStartDrag
    Columns = <>
  end
  object ClearButton: TButton
    Left = 97
    Top = 410
    Width = 129
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Clear tree'
    TabOrder = 1
    OnClick = ClearButtonClick
  end
  object AddOneButton: TButton
    Left = 96
    Top = 350
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add node(s) to root'
    TabOrder = 2
    OnClick = AddButtonClick
  end
  object Edit1: TEdit
    Left = 8
    Top = 366
    Width = 81
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '1'
  end
  object Button1: TButton
    Tag = 1
    Left = 96
    Top = 378
    Width = 130
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Add node(s) as children'
    TabOrder = 4
    OnClick = AddButtonClick
  end
  object CloseButton: TButton
    Left = 330
    Top = 410
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = CloseButtonClick
  end
end
