object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 7
  Margins.Top = 7
  Margins.Right = 7
  Margins.Bottom = 7
  Caption = 'Multiselect Demo'
  ClientHeight = 776
  ClientWidth = 1104
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 216
  TextHeight = 37
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 828
    Height = 776
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alClient
    DefaultNodeHeight = 46
    Header.AutoSizeIndex = 0
    Header.Height = 40
    Header.MaxHeight = 22500
    Header.MinHeight = 23
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Indent = 41
    Margin = 9
    TabOrder = 0
    TextMargin = 9
    TreeOptions.SelectionOptions = [toExtendedFocus, toMultiSelect, toSelectNextNodeOnRemoval]
    OnFreeNode = VirtualStringTree1FreeNode
    OnGetText = VirtualStringTree1GetText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        MaxWidth = 9999
        MinWidth = 9
        Position = 0
        Spacing = 2
        Text = 'table_schema'
        Width = 212
      end
      item
        MaxWidth = 9999
        MinWidth = 9
        Position = 1
        Spacing = 2
        Text = 'table_name'
        Width = 191
      end
      item
        MaxWidth = 9999
        MinWidth = 9
        Position = 2
        Spacing = 2
        Text = 'table_type'
        Width = 204
      end
      item
        MaxWidth = 9999
        MinWidth = 9
        Position = 3
        Spacing = 2
        Text = 'table_version'
        Width = 183
      end>
  end
  object Panel1: TPanel
    Left = 828
    Top = 0
    Width = 276
    Height = 776
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alRight
    Anchors = []
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    DesignSize = (
      276
      776)
    object btnSelect4CellsLeftToRight: TButton
      Left = 44
      Top = 6
      Width = 216
      Height = 90
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select 4 cells'#13#10'left to right'
      TabOrder = 0
      WordWrap = True
      OnClick = btnSelect4CellsLeftToRightClick
    end
    object btnSelect4CellsRightToLeft: TButton
      Left = 44
      Top = 110
      Width = 216
      Height = 90
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select 4 cells'#13#10'right to left'
      TabOrder = 1
      WordWrap = True
      OnClick = btnSelect4CellsRightToLeftClick
    end
    object btnClickRow2Col1: TButton
      Left = 14
      Top = 293
      Width = 252
      Height = 56
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select row 2, cell 1'
      TabOrder = 2
      OnClick = btnClickRow2Col1Click
    end
    object btnClickRow1Col1: TButton
      Left = 14
      Top = 222
      Width = 252
      Height = 57
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select row 1, cell 1'
      TabOrder = 3
      OnClick = btnClickRow1Col1Click
    end
    object btnSelectRow3Col1Row4Col2: TButton
      Left = 14
      Top = 363
      Width = 252
      Height = 118
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select row 3, col 2, row 4, col 3'#13#10'Copy'
      TabOrder = 4
      WordWrap = True
      OnClick = btnSelectRow3Col1Row4Col2Click
    end
    object btnSelectRow2_3_Copy: TButton
      Left = 14
      Top = 495
      Width = 252
      Height = 72
      Margins.Left = 7
      Margins.Top = 7
      Margins.Right = 7
      Margins.Bottom = 7
      Anchors = [akRight]
      Caption = 'Select row 2-3, copy'
      TabOrder = 5
      WordWrap = True
      OnClick = btnSelectRow2_3_CopyClick
    end
  end
end
