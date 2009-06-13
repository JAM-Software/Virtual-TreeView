object StateForm: TStateForm
  Left = 74
  Top = 44
  BorderStyle = bsToolWindow
  Caption = 'Watch Virtual Treeview at work:'
  ClientHeight = 589
  ClientWidth = 478
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Arial Narrow'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 120
  TextHeight = 17
  object EnableCheckBox: TCheckBox
    Left = 9
    Top = 14
    Width = 160
    Height = 19
    Caption = 'Enable state tracking'
    TabOrder = 0
    OnClick = EnableCheckBoxClick
  end
  object GroupBox1: TGroupBox
    Left = 18
    Top = 41
    Width = 228
    Height = 96
    Caption = ' Changes: '
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 9
      Top = 18
      Width = 159
      Height = 19
      Caption = 'Change pending'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 9
      Top = 36
      Width = 159
      Height = 20
      Caption = 'Toggle focus selection'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 9
      Top = 54
      Width = 159
      Height = 20
      Caption = 'Clear pending'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox32: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'Structure change pending'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 18
    Top = 245
    Width = 228
    Height = 169
    Caption = ' Mouse actions: '
    TabOrder = 2
    object CheckBox8: TCheckBox
      Left = 9
      Top = 127
      Width = 159
      Height = 19
      Caption = 'Draw selection pending'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox9: TCheckBox
      Left = 9
      Top = 145
      Width = 159
      Height = 19
      Caption = 'Draw selecting'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox19: TCheckBox
      Left = 9
      Top = 18
      Width = 159
      Height = 19
      Caption = 'Left mouse button down'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox20: TCheckBox
      Left = 9
      Top = 36
      Width = 159
      Height = 20
      Caption = 'Mouse check pending'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox21: TCheckBox
      Left = 9
      Top = 54
      Width = 173
      Height = 20
      Caption = 'Middle mouse button down'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox27: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'Right mouse button down'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox43: TCheckBox
      Left = 9
      Top = 91
      Width = 146
      Height = 19
      Caption = 'Mouse wheel panning'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox44: TCheckBox
      Left = 9
      Top = 109
      Width = 146
      Height = 19
      Caption = 'Mouse wheel scrolling'
      Enabled = False
      TabOrder = 7
    end
  end
  object GroupBox3: TGroupBox
    Left = 258
    Top = 313
    Width = 210
    Height = 114
    Caption = ' Keyboard actions: '
    TabOrder = 3
    object CheckBox10: TCheckBox
      Left = 9
      Top = 36
      Width = 159
      Height = 20
      Caption = 'Editing'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox11: TCheckBox
      Left = 9
      Top = 54
      Width = 159
      Height = 20
      Caption = 'Edit pending'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox15: TCheckBox
      Left = 9
      Top = 91
      Width = 187
      Height = 19
      Caption = 'Incremental search in progress'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox16: TCheckBox
      Left = 9
      Top = 73
      Width = 173
      Height = 19
      Caption = 'Incremental search pending'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox18: TCheckBox
      Left = 9
      Top = 18
      Width = 159
      Height = 19
      Caption = 'Key check pending'
      Enabled = False
      TabOrder = 4
    end
  end
  object GroupBox4: TGroupBox
    Left = 258
    Top = 41
    Width = 210
    Height = 168
    Caption = ' Clipboard and drag'#39'n drop actions: '
    TabOrder = 4
    object CheckBox5: TCheckBox
      Left = 9
      Top = 109
      Width = 159
      Height = 19
      Caption = 'Clipboard flushing'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox6: TCheckBox
      Left = 9
      Top = 127
      Width = 159
      Height = 19
      Caption = 'Clipboard copy pending'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox7: TCheckBox
      Left = 9
      Top = 145
      Width = 159
      Height = 19
      Caption = 'Clipboard cut pending'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox24: TCheckBox
      Left = 9
      Top = 91
      Width = 178
      Height = 19
      Caption = 'OLE drag'#39'n drop in progress'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox25: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'OLE drag'#39'n drop pending'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox37: TCheckBox
      Left = 9
      Top = 54
      Width = 183
      Height = 20
      Caption = 'VCL dd with app. drag object'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox41: TCheckBox
      Left = 9
      Top = 18
      Width = 173
      Height = 19
      Caption = 'VCL drag'#39'n drop in progress'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox42: TCheckBox
      Left = 9
      Top = 36
      Width = 159
      Height = 20
      Caption = 'VCL drag'#39'n drop pending'
      Enabled = False
      TabOrder = 7
    end
  end
  object GroupBox5: TGroupBox
    Left = 18
    Top = 141
    Width = 228
    Height = 96
    Caption = ' Tree cache: '
    TabOrder = 5
    object CheckBox31: TCheckBox
      Left = 9
      Top = 18
      Width = 210
      Height = 19
      Caption = 'Tree cache validation stop request'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox36: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'Tree cache valid'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox39: TCheckBox
      Left = 9
      Top = 36
      Width = 183
      Height = 20
      Caption = 'Tree cache is being validated'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox40: TCheckBox
      Left = 9
      Top = 54
      Width = 183
      Height = 20
      Caption = 'Tree cache invalid or unused'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox6: TGroupBox
    Left = 258
    Top = 209
    Width = 210
    Height = 105
    Caption = ' Collapse/Expand/Scroll: '
    TabOrder = 6
    object CheckBox2: TCheckBox
      Left = 9
      Top = 18
      Width = 151
      Height = 19
      Caption = 'Full collapse in progress'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox12: TCheckBox
      Left = 9
      Top = 36
      Width = 146
      Height = 20
      Caption = 'Full expand in progress'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox28: TCheckBox
      Left = 9
      Top = 54
      Width = 69
      Height = 20
      Caption = 'Scrolling'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox29: TCheckBox
      Left = 9
      Top = 73
      Width = 125
      Height = 19
      Caption = 'Auto scroll pending'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox7: TGroupBox
    Left = 258
    Top = 431
    Width = 210
    Height = 141
    Caption = ' Miscellanous: '
    TabOrder = 7
    object CheckBox13: TCheckBox
      Left = 9
      Top = 36
      Width = 187
      Height = 20
      Caption = 'Last hint window was from VT'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox14: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'In animation'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox17: TCheckBox
      Left = 9
      Top = 91
      Width = 159
      Height = 19
      Caption = 'Iterating'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox23: TCheckBox
      Left = 9
      Top = 54
      Width = 159
      Height = 20
      Caption = 'Need root count update'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox33: TCheckBox
      Left = 9
      Top = 18
      Width = 159
      Height = 19
      Caption = 'Synchronous mode active'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox46: TCheckBox
      Left = 9
      Top = 109
      Width = 159
      Height = 19
      Caption = 'Popup menu shown'
      Enabled = False
      TabOrder = 5
    end
  end
  object GroupBox8: TGroupBox
    Left = 18
    Top = 422
    Width = 228
    Height = 150
    Caption = ' Window related actions: '
    TabOrder = 8
    object CheckBox22: TCheckBox
      Left = 9
      Top = 127
      Width = 159
      Height = 19
      Caption = 'Default node height scale'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox26: TCheckBox
      Left = 9
      Top = 73
      Width = 159
      Height = 19
      Caption = 'Tree painting'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox30: TCheckBox
      Left = 9
      Top = 54
      Width = 159
      Height = 20
      Caption = 'Window resizing'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox34: TCheckBox
      Left = 9
      Top = 91
      Width = 159
      Height = 19
      Caption = 'Tumb tracking (scrollbar)'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox35: TCheckBox
      Left = 9
      Top = 109
      Width = 159
      Height = 19
      Caption = 'Updates locked'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox38: TCheckBox
      Left = 9
      Top = 18
      Width = 210
      Height = 19
      Caption = 'Windows XP Theme support in use'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox45: TCheckBox
      Left = 9
      Top = 36
      Width = 205
      Height = 20
      Caption = 'Treewindow is under construction'
      Enabled = False
      TabOrder = 6
    end
  end
end
