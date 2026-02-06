object VisibilityForm: TVisibilityForm
  Left = 5
  Top = 5
  Margins.Left = 7
  Margins.Top = 7
  Margins.Right = 7
  Margins.Bottom = 7
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'VisibilityForm'
  ClientHeight = 641
  ClientWidth = 1046
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 216
  DesignSize = (
    1046
    641)
  TextHeight = 37
  object VST1: TVirtualStringTree
    Left = 4
    Top = 12
    Width = 951
    Height = 538
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Anchors = [akLeft, akTop, akRight, akBottom]
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    DefaultNodeHeight = 46
    Header.AutoSizeIndex = 0
    Header.Height = 37
    Header.MainColumn = -1
    Header.MaxHeight = 22500
    Header.MinHeight = 23
    Header.Options = [hoColumnResize, hoDrag]
    HintMode = hmTooltip
    IncrementalSearch = isAll
    Indent = 41
    Margin = 9
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    TextMargin = 9
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoHideButtons, toAutoChangeScale]
    TreeOptions.SelectionOptions = [toMultiSelect]
    OnChange = VST1Change
    OnFreeNode = VST1FreeNode
    OnGetText = VST1GetText
    OnInitChildren = VST1InitChildren
    OnInitNode = VST1InitNode
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <>
    DefaultText = ''
  end
  object btnCheck: TButton
    Left = 786
    Top = 573
    Width = 169
    Height = 56
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Caption = 'btnCheck'
    TabOrder = 1
    OnClick = btnCheckClick
  end
end
