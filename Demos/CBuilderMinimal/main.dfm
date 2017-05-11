object MainForm: TMainForm
  Left = 732
  Top = 219
  BiDiMode = bdLeftToRight
  Caption = 'Simple Virtual Treeview demo'
  ClientHeight = 443
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  Position = poDesigned
  OnCreate = FormCreate
  DesignSize = (
    409
    443)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 280
    Width = 116
    Height = 13
    Caption = 'Last operation duration:'
  end
  object VST: TVirtualStringTree
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 393
    Height = 262
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alTop
    BiDiMode = bdLeftToRight
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    DragMode = dmAutomatic
    DragType = dtVCL
    Header.AutoSizeIndex = -1
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    HintAnimation = hatNone
    IncrementalSearch = isAll
    ParentBiDiMode = False
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.MiscOptions = [toEditable, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnInitNode = VSTInitNode
    Columns = <
      item
        Position = 0
        Width = 300
        WideText = 'Name'
      end>
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
