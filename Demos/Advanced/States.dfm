object StateForm: TStateForm
  Left = 74
  Top = 44
  BorderStyle = bsToolWindow
  Caption = 'Watch Virtual Treeview at work:'
  ClientHeight = 545
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object EnableCheckBox: TCheckBox
    Left = 10
    Top = 8
    Width = 127
    Height = 17
    Caption = 'Enable state tracking'
    TabOrder = 0
    OnClick = EnableCheckBoxClick
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 31
    Width = 207
    Height = 92
    Caption = ' Changes: '
    TabOrder = 1
    object CheckBox1: TCheckBox
      Left = 10
      Top = 16
      Width = 190
      Height = 17
      Caption = 'Change pending'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox3: TCheckBox
      Left = 10
      Top = 33
      Width = 190
      Height = 17
      Caption = 'Toggle focus selection'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox4: TCheckBox
      Left = 10
      Top = 50
      Width = 190
      Height = 17
      Caption = 'Clear pending'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox32: TCheckBox
      Left = 10
      Top = 67
      Width = 190
      Height = 17
      Caption = 'Structure change pending'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 228
    Width = 209
    Height = 160
    Caption = ' Mouse actions: '
    TabOrder = 2
    object CheckBox8: TCheckBox
      Left = 10
      Top = 101
      Width = 190
      Height = 17
      Caption = 'Draw selection pending'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox9: TCheckBox
      Left = 10
      Top = 118
      Width = 190
      Height = 17
      Caption = 'Draw selecting'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox19: TCheckBox
      Left = 10
      Top = 16
      Width = 190
      Height = 17
      Caption = 'Left mouse button down'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox21: TCheckBox
      Left = 10
      Top = 33
      Width = 190
      Height = 17
      Caption = 'Middle mouse button down'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox27: TCheckBox
      Left = 10
      Top = 50
      Width = 190
      Height = 17
      Caption = 'Right mouse button down'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox43: TCheckBox
      Left = 10
      Top = 67
      Width = 190
      Height = 17
      Caption = 'Mouse wheel panning'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox44: TCheckBox
      Left = 10
      Top = 84
      Width = 190
      Height = 17
      Caption = 'Mouse wheel scrolling'
      Enabled = False
      TabOrder = 6
    end
  end
  object GroupBox3: TGroupBox
    Left = 223
    Top = 295
    Width = 190
    Height = 109
    Caption = ' Keyboard actions: '
    TabOrder = 3
    object CheckBox10: TCheckBox
      Left = 10
      Top = 33
      Width = 174
      Height = 17
      Caption = 'Editing'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox11: TCheckBox
      Left = 10
      Top = 50
      Width = 174
      Height = 17
      Caption = 'Edit pending'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox15: TCheckBox
      Left = 10
      Top = 16
      Width = 174
      Height = 17
      Caption = 'Incremental search in progress'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox16: TCheckBox
      Left = 10
      Top = 67
      Width = 174
      Height = 17
      Caption = 'Incremental search pending'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox4: TGroupBox
    Left = 223
    Top = 31
    Width = 190
    Height = 160
    Caption = ' Clipboard and drag'#39'n drop actions: '
    TabOrder = 4
    object CheckBox5: TCheckBox
      Left = 10
      Top = 101
      Width = 174
      Height = 17
      Caption = 'Clipboard flushing'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox6: TCheckBox
      Left = 10
      Top = 118
      Width = 174
      Height = 17
      Caption = 'Clipboard copy pending'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox7: TCheckBox
      Left = 10
      Top = 135
      Width = 174
      Height = 17
      Caption = 'Clipboard cut pending'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox24: TCheckBox
      Left = 10
      Top = 84
      Width = 174
      Height = 17
      Caption = 'OLE drag'#39'n drop in progress'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox25: TCheckBox
      Left = 10
      Top = 67
      Width = 174
      Height = 17
      Caption = 'OLE drag'#39'n drop pending'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox37: TCheckBox
      Left = 10
      Top = 50
      Width = 174
      Height = 17
      Caption = 'VCL dd with app. drag object'
      Enabled = False
      TabOrder = 5
    end
    object CheckBox41: TCheckBox
      Left = 10
      Top = 16
      Width = 174
      Height = 17
      Caption = 'VCL drag'#39'n drop in progress'
      Enabled = False
      TabOrder = 6
    end
    object CheckBox42: TCheckBox
      Left = 10
      Top = 33
      Width = 174
      Height = 17
      Caption = 'VCL drag'#39'n drop pending'
      Enabled = False
      TabOrder = 7
    end
  end
  object GroupBox5: TGroupBox
    Left = 8
    Top = 130
    Width = 209
    Height = 92
    Caption = ' Tree cache: '
    TabOrder = 5
    object CheckBox31: TCheckBox
      Left = 10
      Top = 16
      Width = 190
      Height = 17
      Caption = 'Tree cache validation stop request'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox36: TCheckBox
      Left = 10
      Top = 67
      Width = 190
      Height = 17
      Caption = 'Tree cache valid'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox39: TCheckBox
      Left = 10
      Top = 33
      Width = 190
      Height = 17
      Caption = 'Tree cache is being validated'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox40: TCheckBox
      Left = 10
      Top = 50
      Width = 190
      Height = 17
      Caption = 'Tree cache invalid or unused'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox6: TGroupBox
    Left = 223
    Top = 197
    Width = 190
    Height = 92
    Caption = ' Collapse/Expand/Scroll: '
    TabOrder = 6
    object CheckBox2: TCheckBox
      Left = 10
      Top = 16
      Width = 174
      Height = 17
      Caption = 'Full collapse in progress'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox12: TCheckBox
      Left = 10
      Top = 33
      Width = 174
      Height = 17
      Caption = 'Full expand in progress'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox28: TCheckBox
      Left = 10
      Top = 50
      Width = 174
      Height = 17
      Caption = 'Scrolling'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox29: TCheckBox
      Left = 10
      Top = 67
      Width = 174
      Height = 17
      Caption = 'Auto scroll pending'
      Enabled = False
      TabOrder = 3
    end
  end
  object GroupBox7: TGroupBox
    Left = 223
    Top = 411
    Width = 190
    Height = 126
    Caption = ' Miscellanous: '
    TabOrder = 7
    object CheckBox13: TCheckBox
      Left = 10
      Top = 33
      Width = 174
      Height = 17
      Caption = 'Last hint window was from VT'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox14: TCheckBox
      Left = 10
      Top = 67
      Width = 174
      Height = 17
      Caption = 'In animation'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox17: TCheckBox
      Left = 10
      Top = 84
      Width = 174
      Height = 17
      Caption = 'Iterating'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox23: TCheckBox
      Left = 10
      Top = 50
      Width = 174
      Height = 17
      Caption = 'Need root count update'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox33: TCheckBox
      Left = 10
      Top = 16
      Width = 174
      Height = 17
      Caption = 'Synchronous mode active'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox46: TCheckBox
      Left = 10
      Top = 101
      Width = 174
      Height = 17
      Caption = 'Popup menu shown'
      Enabled = False
      TabOrder = 5
    end
  end
  object GroupBox8: TGroupBox
    Left = 8
    Top = 394
    Width = 209
    Height = 143
    Caption = ' Window related actions: '
    TabOrder = 8
    object CheckBox26: TCheckBox
      Left = 10
      Top = 67
      Width = 190
      Height = 17
      Caption = 'Tree painting'
      Enabled = False
      TabOrder = 0
    end
    object CheckBox30: TCheckBox
      Left = 10
      Top = 50
      Width = 190
      Height = 17
      Caption = 'Window resizing'
      Enabled = False
      TabOrder = 1
    end
    object CheckBox34: TCheckBox
      Left = 10
      Top = 84
      Width = 190
      Height = 17
      Caption = 'Tumb tracking (scrollbar)'
      Enabled = False
      TabOrder = 2
    end
    object CheckBox35: TCheckBox
      Left = 10
      Top = 101
      Width = 190
      Height = 17
      Caption = 'Updates locked'
      Enabled = False
      TabOrder = 3
    end
    object CheckBox38: TCheckBox
      Left = 10
      Top = 16
      Width = 190
      Height = 17
      Caption = 'Windows XP Theme support in use'
      Enabled = False
      TabOrder = 4
    end
    object CheckBox45: TCheckBox
      Left = 10
      Top = 33
      Width = 190
      Height = 17
      Caption = 'Treewindow is under construction'
      Enabled = False
      TabOrder = 5
    end
  end
end
