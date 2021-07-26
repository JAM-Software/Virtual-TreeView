object DrawTreeForm: TDrawTreeForm
  Left = 544
  Top = 320
  ClientHeight = 474
  ClientWidth = 710
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Trebuchet MS'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    710
    474)
  PixelsPerInch = 96
  TextHeight = 18
  object Label7: TLabel
    Left = 0
    Top = 0
    Width = 710
    Height = 61
    Align = alTop
    AutoSize = False
    Caption = 
      'A sample for a draw tree, which shows images of all known types ' +
      'as thumbnails. By default this tree uses the image loader librar' +
      'y GraphicEx  to support many common image formats like png, gif ' +
      'etc. (see www.delphi-gems.com for more infos and download).'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 4
    Top = 381
    Width = 247
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = 'Adjust vertical image alignment of nodes:'
  end
  object Label3: TLabel
    Left = 424
    Top = 381
    Width = 22
    Height = 18
    Anchors = [akLeft, akBottom]
    Caption = '50%'
  end
  object VDT1: TVirtualDrawTree
    Left = 10
    Top = 84
    Width = 684
    Height = 278
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoExpandDelay = 200
    AutoScrollDelay = 200
    BevelInner = bvNone
    BevelKind = bkTile
    BorderStyle = bsNone
    ClipboardFormats.Strings = (
      'Virtual Tree Data')
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    Colors.TreeLineColor = clBtnFace
    DefaultNodeHeight = 32
    Header.AutoSizeIndex = -1
    Header.Background = clBtnHighlight
    Header.Height = 22
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoRestrictDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsPlates
    HintMode = hmHint
    Images = SystemImages
    IncrementalSearch = isAll
    Indent = 20
    LineMode = lmBands
    ParentShowHint = False
    RootNodeCount = 10
    ScrollBarOptions.VerticalIncrement = 32
    ShowHint = True
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes, toAutoChangeScale]
    TreeOptions.PaintOptions = [toShowBackground, toShowButtons, toShowDropmark, toShowHorzGridLines, toShowRoot, toShowTreeLines, toShowVertGridLines, toThemeAware]
    TreeOptions.SelectionOptions = [toFullRowSelect]
    OnCompareNodes = VDT1CompareNodes
    OnDrawHint = VDT1DrawHint
    OnDrawNode = VDT1DrawNode
    OnFreeNode = VDT1FreeNode
    OnGetHintSize = VDT1GetHintSize
    OnGetImageIndex = VDT1GetImageIndex
    OnGetNodeWidth = VDT1GetNodeWidth
    OnHeaderClick = VDT1HeaderClick
    OnInitChildren = VDT1InitChildren
    OnInitNode = VDT1InitNode
    OnStateChange = VDT1StateChange
    Touch.InteractiveGestures = [igPan, igPressAndTap]
    Touch.InteractiveGestureOptions = [igoPanSingleFingerHorizontal, igoPanSingleFingerVertical, igoPanInertia, igoPanGutter, igoParentPassthrough]
    Columns = <
      item
        BiDiMode = bdLeftToRight
        Options = [coAllowClick, coEnabled, coParentColor, coResizable, coShowDropMark, coVisible]
        Position = 0
        Text = 'Image file name'
        Width = 217
      end
      item
        Position = 1
        Text = 'Thumbnail'
        Width = 200
      end
      item
        Position = 2
        Text = 'Properties'
        Width = 160
      end>
  end
  object TrackBar1: TTrackBar
    Left = 264
    Top = 379
    Width = 157
    Height = 21
    Anchors = [akLeft, akBottom]
    Max = 100
    Position = 50
    TabOrder = 1
    ThumbLength = 15
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
  object SystemImages: TImageList
    Left = 668
    Top = 404
  end
end
