object NodeForm: TNodeForm
  Left = 573
  Top = 332
  Caption = 'NodeForm'
  ClientHeight = 504
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    757
    504)
  PixelsPerInch = 96
  TextHeight = 16
  object Label8: TLabel
    Left = 12
    Top = 393
    Width = 716
    Height = 72
    Anchors = [akLeft, akRight, akBottom]
    AutoSize = False
    Caption = 
      'Since Virtual Treeview uses Unicode for text display it is not e' +
      'asy to provide multiline support on Windows 9x/Me systems. Under' +
      ' Windows NT (4.0, 2000, XP) there is support by the operation sy' +
      'stem and so full word breaking is possible there. Otherwise you ' +
      'have to insert line breaks manually to have multiline captions. ' +
      'Of course there is no difference in handling between multiline a' +
      'nd single line nodes (except for the vertical alignment of the l' +
      'atter).'
    ShowAccelChar = False
    WordWrap = True
  end
  object Panel1: TPanel
    Left = 12
    Top = 4
    Width = 716
    Height = 357
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clAppWorkSpace
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      716
      357)
    object MLTree: TVirtualStringTree
      Left = 96
      Top = 8
      Width = 533
      Height = 337
      Anchors = [akLeft, akTop, akRight, akBottom]
      BorderStyle = bsNone
      ClipboardFormats.Strings = (
        'CSV'
        'HTML Format'
        'Plain text'
        'Rich Text Format'
        'Rich Text Format Without Objects'
        'Unicode text'
        'Virtual Tree Data')
      Colors.SelectionRectangleBlendColor = 10539203
      DefaultNodeHeight = 130
      DragMode = dmAutomatic
      Header.AutoSizeIndex = 0
      Header.Font.Charset = DEFAULT_CHARSET
      Header.Font.Color = clWindowText
      Header.Font.Height = -11
      Header.Font.Name = 'Tahoma'
      Header.Font.Style = []
      Header.Options = [hoAutoResize, hoColumnResize, hoDrag]
      HintAnimation = hatNone
      HintMode = hmTooltip
      LineMode = lmBands
      NodeAlignment = naFromTop
      NodeDataSize = 4
      ParentShowHint = False
      RootNodeCount = 30
      SelectionCurveRadius = 10
      ShowHint = True
      TabOrder = 0
      TextMargin = 5
      TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoSpanColumns, toAutoTristateTracking, toAutoDeleteMovedNodes]
      TreeOptions.MiscOptions = [toEditable, toInitOnSave, toReportMode, toToggleOnDblClick, toWheelPanning]
      TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toShowTreeLines, toThemeAware, toUseBlendedImages]
      TreeOptions.SelectionOptions = [toMultiSelect]
      OnEditing = MLTreeEditing
      OnGetText = MLTreeGetText
      OnPaintText = MLTreePaintText
      OnInitNode = MLTreeInitNode
      OnMeasureItem = MLTreeMeasureItem
      OnStateChange = MLTreeStateChange
      Columns = <
        item
          Position = 1
          Width = 466
        end
        item
          Position = 0
        end>
    end
  end
  object AutoAdjustCheckBox: TCheckBox
    Left = 12
    Top = 368
    Width = 329
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Automatically adjust node height to node text.'
    TabOrder = 1
    OnClick = AutoAdjustCheckBoxClick
  end
end
