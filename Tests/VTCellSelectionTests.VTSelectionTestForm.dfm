object SelectionTestForm: TSelectionTestForm
  Left = 0
  Top = 0
  Caption = 'SelectionTestForm'
  ClientHeight = 843
  ClientWidth = 1424
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  PixelsPerInch = 216
  TextHeight = 37
  object VSTA: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 1424
    Height = 843
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alClient
    ClipboardFormats.Strings = (
      'Plain text'
      'Virtual Tree Data')
    DefaultNodeHeight = 46
    Header.AutoSizeIndex = 0
    Header.Height = 41
    Header.MaxHeight = 22500
    Header.MinHeight = 23
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Indent = 41
    Margin = 9
    TabOrder = 0
    TextMargin = 9
    TreeOptions.SelectionOptions = [toMultiSelect, toSelectNextNodeOnRemoval]
    OnChange = VSTAChange
    OnClick = VSTAClick
    OnDragAllowed = VSTADragAllowed
    OnDragOver = VSTADragOver
    OnDragDrop = VSTADragDrop
    OnFreeNode = VSTAFreeNode
    OnGetText = VSTAGetText
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        MaxWidth = 11250
        MinWidth = 72
        Position = 0
        Text = 'Name'
        Width = 563
      end
      item
        MaxWidth = 11250
        MinWidth = 72
        Position = 1
        Text = 'Desp'
        Width = 150
      end
      item
        MaxWidth = 11250
        MinWidth = 72
        Position = 2
        Text = 'Location'
        Width = 135
      end>
    DefaultText = ''
  end
end
