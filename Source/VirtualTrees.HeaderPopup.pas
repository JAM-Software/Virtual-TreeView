unit VirtualTrees.HeaderPopup;

//----------------------------------------------------------------------------------------------------------------------
//
// Version 4.7.0
//
// The contents of this file are subject to the Mozilla Public License
// Version 1.1 (the "License"); you may not use this file except in
// compliance with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under the terms of the
// GNU Lesser General Public License as published by the Free Software Foundation;
// either version 2.1 of the License, or (at your option) any later version.
// You may obtain a copy of the LGPL at http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS"
// basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
// License for the specific language governing rights and limitations
// under the License.
//
// The Original Code is VTHeaderPopup.pas.
//
// The Initial Developer of the Original Code is Ralf Junker <delphi@zeitungsjunge.de>. All Rights Reserved.
//
// September 2004:
//  - Bug fix: TVTHeaderPopupMenu.OnMenuItemClick used the wrong Tag member for the event.
// 
// Modified 12 Dec 2003 by Ralf Junker <delphi@zeitungsjunge.de>.
//   - Added missing default storage specifier for Options property.
//   - To avoid mixing up image lists of different trees sharing the same header 
//     popup, set the popup's image list to nil if hoShowImages is not in the 
//     tree's header options.
//   - Added an additional check for the PopupComponent property before casting 
//     it hardly to a Virtual Treeview in OnMenuItemClick. See entry 31 Mar 2003.
//
// Modified 14 Sep 2003 by Mike Lischke <public@delphi-gems.com>.
//   - Renamed event type name to be consistent with other event types (e.g. used in VT).
//   - Added event for hiding/showing columns.
//   - DoXXX method are now virtual.
//   - Conditional code rearrangement to get back Ctrl+Shift+Up/Down navigation.
//
// Modified 31 Mar 2003 by Mike Lischke <public@soft-gems.net>.
//   - Added a check for the PopupComponent property before casting it hardly to 
//     a Virtual Treeview. People might (accidentally) misuse the header popup.
//
// Modified 20 Oct 2002 by Borut Maricic <borut.maricic@pobox.com>.
//   - Added the possibility to use Troy Wolbrink's Unicode aware popup menu. 
//     Define the compiler symbol TNT to enable it. You can get Troy's Unicode
//     controls collection from http://home.ccci.org/wolbrink/tnt/delphi_unicode_controls.htm.
//
// Modified 24 Feb 2002 by Ralf Junker <delphi@zeitungsjunge.de>.
//   - Fixed a bug where the OnAddHeaderPopupItem would interfere with 
//     poAllowHideAll options.
//   - All column indexes now consistently use TColumnIndex (instead of Integer).
//
// Modified 23 Feb 2002 by Ralf Junker <delphi@zeitungsjunge.de>.
//   - Added option to show menu items in the same order as the columns or in 
//     original order.
//   - Added option to prevent the user to hide all columns.
//
// Modified 17 Feb 2002 by Jim Kueneman <jimdk@mindspring.com>.
//   - Added the event to filter the items as they are added to the menu.
//----------------------------------------------------------------------------------------------------------------------

interface

//{$DEFINE VT_FMX}
{$IFNDEF VT_FMX}
  {$DEFINE VT_VCL}
{$ENDIF}

uses
{$IFDEF VT_FMX}
  System.Classes,
  FMX.Menus,
  VirtualTrees,
  VirtualTrees.FMX;
{$ELSE}
  System.Classes,
  Vcl.Menus,
  VirtualTrees;
{$ENDIF}


type
  TVTHeaderPopupOption = (
    poOriginalOrder, // Show menu items in original column order as they were added to the tree.
    poAllowHideAll,   // Allows to hide all columns, including the last one.
    poResizeToFitItem // Adds an item which, if clicks, resizes all columns to fit by callung TVTHeader.AutoFitColumns
  );
  TVTHeaderPopupOptions = set of TVTHeaderPopupOption;

  TColumnChangeEvent = procedure(const Sender: TBaseVirtualTree; const Column: TColumnIndex; Visible: Boolean) of object;

  TVTHeaderPopupMenu = class(TPopupMenu)
  strict private
    FOptions: TVTHeaderPopupOptions;

    FOnHeaderAddPopupItem: TVTHeaderAddPopupItemEvent;
    FOnColumnChange: TColumnChangeEvent;
    procedure ResizeColumnToFit(Sender: TObject);
    procedure ResizeToFit(Sender: TObject);
  strict protected
    procedure DoAddHeaderPopupItem(const Column: TColumnIndex; out Cmd: TAddPopupItemType); virtual;
    procedure DoColumnChange(Column: TColumnIndex; Visible: Boolean); virtual;
    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Popup(x, y: {$IFDEF VT_FMX}Single{$ELSE}Integer{$ENDIF}); override;
  published
    property Options: TVTHeaderPopupOptions read FOptions write FOptions default [poResizeToFitItem];

    property OnAddHeaderPopupItem: TVTHeaderAddPopupItemEvent read FOnHeaderAddPopupItem write FOnHeaderAddPopupItem;
    property OnColumnChange: TColumnChangeEvent read FOnColumnChange write FOnColumnChange;
  end;

//----------------------------------------------------------------------------------------------------------------------

implementation

uses
{$IFDEF VT_FMX}
  FMX.Types;
{$ELSE}
  Winapi.Windows, System.Types;
{$ENDIF}
const
  cResizeToFitMenuItemName = 'VT_ResizeToFitMenuItem';


resourcestring
  sResizeColumnToFit = 'Size &Column to Fit';
  sResizeToFit = 'Size &All Columns to Fit';

type
  TVTMenuItem = class(TMenuItem)
  public
    constructor Create(AOwner: TComponent; const ACaption: string; AClickHandler: TNotifyEvent = nil); reintroduce;
  end;

//----------------- TVTHeaderPopupMenu ---------------------------------------------------------------------------------

constructor TVTHeaderPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  FOptions := [poResizeToFitItem];
end;

procedure TVTHeaderPopupMenu.DoAddHeaderPopupItem(const Column: TColumnIndex; out Cmd: TAddPopupItemType);

begin
  Cmd := apNormal;
  if Assigned(FOnHeaderAddPopupItem) then
    FOnHeaderAddPopupItem((PopupComponent as TBaseVirtualTree), Column, Cmd);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeaderPopupMenu.DoColumnChange(Column: TColumnIndex; Visible: Boolean);

begin
  if Assigned(FOnColumnChange) then
    FOnColumnChange((PopupComponent as TBaseVirtualTree), Column, Visible);
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVTHeaderPopupMenu.OnMenuItemClick(Sender: TObject);

begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then begin
    with TBaseVirtualTree(PopupComponent).Header.Columns.Items[TVTMenuItem(Sender).Tag] do
    begin
      if TVTMenuItem(Sender).{$IFDEF VT_FMX}IsChecked{$ELSE}Checked{$ENDIF} then
        Options := Options - [coVisible]
      else
        Options := Options + [coVisible];
    end;
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

{$IFDEF VT_FMX}
function NewItem(const ACaption: string; AShortCut: TShortCut;
  AChecked, AEnabled: Boolean; AOnClick: TNotifyEvent; hCtx: THelpContext;
  const AName: string): TVTMenuItem;
begin
  Result := TVTMenuItem.Create(nil, ACaption, AOnClick);
  with Result do
  begin
    Text := ACaption;
    ShortCut := AShortCut;
    OnClick := AOnClick;
    HelpContext := hCtx;
    IsChecked := AChecked;
    Enabled := AEnabled;
    Name := AName;
  end;
end;

function NewLine: TMenuItem;
begin
  Result := TMenuItem.Create(nil);
  Result.Text := '-';
end;
{$ENDIF}
procedure TVTHeaderPopupMenu.Popup(x, y: {$IFDEF VT_FMX}Single{$ELSE}Integer{$ENDIF});
var
  ColPos: TColumnPosition;
  ColIdx: TColumnIndex;

  NewMenuItem: TVTMenuItem;
  Cmd: TAddPopupItemType;

  VisibleCounter: Cardinal;
  VisibleItem: TVTMenuItem;

  i: Integer;
begin
  if Assigned(PopupComponent) and (PopupComponent is TBaseVirtualTree) then
  begin
    // Delete existing menu items.
{$IFDEF VT_FMX}
    while ItemsCount > 0 do
      Items[0].Free;

    if poResizeToFitItem in Self.Options then begin
      NewMenuItem := NewItem(sResizeToFit, 0, False, True, OnMenuItemClick, 0, cResizeToFitMenuItemName);
      InsertObject(ItemsCount-1{???}, NewMenuItem);
      InsertObject(ItemsCount-1{???}, NewLine());
      {if IsHandleValid(Handle) then
        Platform.UpdateMenuItem(mnuFile);  }
    end;//poResizeToFitItem
{$ELSE}
    for i := Items.Count -1 downto 0 do begin
      if Items[i] is TVTMenuItem then
        Items[i].Free;
    end;//for i

    if poResizeToFitItem in Self.Options then
    begin
      Items.Add(TVTMenuItem.Create(Self, sResizeColumnToFit, ResizeColumnToFit));
      Items.Add(TVTMenuItem.Create(Self, sResizeToFit, ResizeToFit));
      Items.Add(TVTMenuItem.Create(Self, cLineCaption));
    end;//poResizeToFitItem
{$ENDIF}


    // Add column menu items.
    with (PopupComponent as TBaseVirtualTree).Header do
    begin
      if hoShowImages in Options then
        Self.Images := Images
      else
        // Remove a possible reference to image list of another tree previously assigned.
        Self.Images := nil;
      VisibleItem := nil;
      VisibleCounter := 0;
      for ColPos := 0 to Columns.Count - 1 do
      begin
        if poOriginalOrder in FOptions then
          ColIdx := ColPos
        else
          ColIdx := Columns.ColumnFromPosition(ColPos);

        with Columns[ColIdx] do
        begin
          if coVisible in Options then
            System.Inc(VisibleCounter);
          DoAddHeaderPopupItem(ColIdx, Cmd);
          if Cmd <> apHidden then
          begin
            NewMenuItem := TVTMenuItem.Create(Self, Text, OnMenuItemClick);
            NewMenuItem.Tag := ColIdx;
            NewMenuItem.{$IFDEF VT_FMX}Text{$ELSE}Caption{$ENDIF} := Text;
            NewMenuItem.Hint := Hint;
            NewMenuItem.ImageIndex := ImageIndex;
            NewMenuItem.{$IFDEF VT_FMX}IsChecked{$ELSE}Checked{$ENDIF} := coVisible in Options;
            if Cmd = apDisabled then
              NewMenuItem.Enabled := False
            else
              if coVisible in Options then
                VisibleItem := NewMenuItem;
{$IFDEF VT_FMX}
            InsertObject(Self.ItemsCount-1{???}, NewMenuItem);
{$ELSE}
            Items.Add(NewMenuItem);
{$ENDIF}
          end;
        end;
      end;

      // Conditionally disable menu item of last enabled column.
      if (VisibleCounter = 1) and (VisibleItem <> nil) and not (poAllowHideAll in FOptions) then
        VisibleItem.Enabled := False;
    end;
  end;

  inherited;
end;

procedure TVTHeaderPopupMenu.ResizeColumnToFit(Sender: TObject);
var
  P: TPoint;
  Column: TColumnIndex;
begin
  P := Point(PopupPoint.X, PopupPoint.Y + TBaseVirtualTree(PopupComponent).Header.Height);
  P := TBaseVirtualTree(PopupComponent).ScreenToClient(P);
  Column := TBaseVirtualTree(PopupComponent).Header.Columns.ColumnFromPosition(P);
  if Column <> InvalidColumn then
    TBaseVirtualTree(PopupComponent).Header.AutoFitColumns(True, smaUseColumnOption, Column, Column);
end;

procedure TVTHeaderPopupMenu.ResizeToFit(Sender: TObject);
begin
  TBaseVirtualTree(PopupComponent).Header.AutoFitColumns();
end;

//----------------------------------------------------------------------------------------------------------------------

{ TVTMenuItem }

constructor TVTMenuItem.Create(AOwner: TComponent; const ACaption: string; AClickHandler: TNotifyEvent);
begin
  Inherited Create(AOwner);
  {$IFDEF VT_FMX}Text{$ELSE}Caption{$ENDIF} := ACaption;
  OnClick := AClickHandler;
end;

end.

