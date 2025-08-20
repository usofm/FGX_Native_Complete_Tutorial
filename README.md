# FGX Native Complete Tutorial

**A Comprehensive Guide to Cross-Platform Mobile Development with FGX Native**

Based on the extensive sample collection in the FGX Native Samples directory, this tutorial provides complete coverage of the framework's capabilities for building iOS and Android applications using Delphi.

## Table of Contents

1. [Introduction to FGX Native](#introduction-to-fgx-native)
2. [Project Structure and Setup](#project-structure-and-setup)
3. [Core UI Components](#core-ui-components)
4. [Layout and Positioning](#layout-and-positioning)
5. [Data Display and Collections](#data-display-and-collections)
6. [Form Management](#form-management)
7. [Input Controls](#input-controls)
8. [Graphics and Canvas](#graphics-and-canvas)
9. [Animations and Effects](#animations-and-effects)
10. [Platform Integration](#platform-integration)
11. [Media and Camera](#media-and-camera)
12. [Maps and Location](#maps-and-location)
13. [Services and Authentication](#services-and-authentication)
14. [Web Browser Integration](#web-browser-integration)
15. [Advanced UI Components](#advanced-ui-components)
16. [Push Notifications](#push-notifications)
17. [Advertising Integration](#advertising-integration)
18. [Android-Specific Features](#android-specific-features)
19. [Advanced Topics](#advanced-topics)
20. [Best Practices](#best-practices)

---

## 1. Introduction to FGX Native

FGX Native is a Delphi-based cross-platform mobile development framework that enables you to create native iOS and Android applications using a single codebase. The framework provides:

- **Cross-Platform Compatibility**: Write once, run on both iOS and Android
- **Native Performance**: Compiled to native code for optimal performance
- **Rich Component Library**: Comprehensive set of UI controls and services
- **Material Design Support**: Built-in theming following Material Design principles
- **Platform Integration**: Access to device-specific features and APIs

### Key Features
- **MVVM Architecture Support**: Modern architectural patterns with LiveData
- **Asset Management**: Automated asset handling with compile-time safety
- **Responsive Design**: Auto-sizing and flexible layouts
- **Theme System**: Light/Dark theme support with system integration
- **Memory Optimization**: Efficient resource management and virtual components

---

## 2. Project Structure and Setup

### Basic Project Structure

Every FGX Native project follows a standard structure:

```
ProjectName/
├── ProjectName.dpr              # Main program file
├── ProjectName.dproj            # MSBuild project file
├── Form.Main.pas               # Main form implementation
├── Form.Main.xfm               # Form definition file
├── Assets.Consts.pas           # Auto-generated asset constants
├── description.json            # Project metadata
├── Assets/                     # Asset directory
│   ├── config.json            # Asset configuration
│   ├── config-android.json    # Android-specific config
│   └── config-ios.json        # iOS-specific config
└── Platform Templates/         # Platform-specific files
    ├── info.plist.TemplateiOS.xml
    └── AndroidManifest.template.xml
```

### Sample: Basic Project Setup

**File: ProjectName.dpr**
```pascal
program ProjectName;

uses
  FGX.Application,
  Form.Main in 'Form.Main.pas' {FormMain};

{$R *.res}

begin
  TfgApplication.Initialize;
  TfgApplication.CreateForm(TFormMain, FormMain);
  TfgApplication.Run;
end.
```

**File: Form.Main.pas**
```pascal
unit Form.Main;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, FGX.Forms, FGX.Controls, FGX.Layout;

type
  TFormMain = class(TfgForm)
    // Form controls declared here
  private
    // Private members
  public
    // Public members
  end;

var
  FormMain: TFormMain;

implementation

{$R *.xfm}

// Implementation code here

end.
```

---

## 3. Core UI Components

### 3.1 Activity Indicator (Loading Spinner)

**Sample Location**: `ActivityIndicator - Tint`

The `TfgActivityIndicator` provides visual feedback during loading operations with customizable colors.

**Key Features**:
- Start/stop animation control
- Custom color tinting
- Auto-hide when stopped
- Cross-platform consistency

**Example Implementation**:
```pascal
type
  TFormMain = class(TfgForm)
    aiDefault: TfgActivityIndicator;
    aiBlue: TfgActivityIndicator;
    aiRed: TfgActivityIndicator;
    aiAutoHidden: TfgActivityIndicator;
    btnControl: TfgButton;
    procedure btnControlTap(Sender: TObject);
  private
    FIsAnimating: Boolean;
  end;

procedure TFormMain.btnControlTap(Sender: TObject);
begin
  FIsAnimating := not FIsAnimating;
  
  // Control all indicators
  aiDefault.IsAnimating := FIsAnimating;
  aiBlue.IsAnimating := FIsAnimating;
  aiRed.IsAnimating := FIsAnimating;
  aiAutoHidden.IsAnimating := FIsAnimating;
  
  // Update button text
  if FIsAnimating then
    btnControl.Text := 'Stop'
  else
    btnControl.Text := 'Start';
end;
```

**Form Configuration (.xfm)**:
```xml
<TfgActivityIndicator Name="aiBlue">
  <Tint>#0066CC</Tint>
  <IsAnimating>True</IsAnimating>
</TfgActivityIndicator>
```

### 3.2 Labels with Auto-sizing

**Sample Location**: `Label - Autosize`

The `TfgLabel` component provides text display with automatic sizing capabilities.

**Key Features**:
- Auto-width and auto-height sizing
- Text wrapping
- Rich text formatting
- Alignment options

**Example**:
```pascal
procedure TFormMain.UpdateLabels;
begin
  // Auto-width label
  lblAutoWidth.AutoSize := TfgAutoSize.Width;
  lblAutoWidth.Text := 'This label auto-sizes its width based on content';
  
  // Auto-height label with wrapping
  lblAutoHeight.AutoSize := TfgAutoSize.Height;
  lblAutoHeight.Text := 'This is a longer text that will wrap and auto-size height';
  lblAutoHeight.Size.Width := 200;
end;
```

### 3.3 Buttons and Interactions

**Sample Location**: Multiple samples demonstrate button usage

Basic button implementation with event handling:

```pascal
type
  TFormMain = class(TfgForm)
    btnPrimary: TfgButton;
    btnSecondary: TfgButton;
    procedure btnPrimaryTap(Sender: TObject);
  end;

procedure TFormMain.btnPrimaryTap(Sender: TObject);
begin
  TfgDialogs.ShowMessage('Button tapped!');
end;
```

### 3.4 Advanced Card System

**Sample Location**: `Card - Base`

The `TfgCardPanel` provides a complete Material Design card system with elevation, corner radius, and interactive features.

#### Basic Card Configuration

```pascal
type
  TFormMain = class(TfgForm)
    fgCardPanel1: TfgCardPanel;
    tbElevation: TfgTrackBar;
    tbCornerRadius: TfgTrackBar;
    procedure tbElevationChanging(Sender: TObject);
    procedure tbCornerRadiusChanging(Sender: TObject);
  end;

procedure TFormMain.SetupBasicCard;
begin
  // Basic Material Design card setup
  fgCardPanel1.Elevation := 2;           // Material Design elevation
  fgCardPanel1.CornerRadius := 8;        // Rounded corners
  fgCardPanel1.BackgroundColor := TAlphaColors.White;
  fgCardPanel1.Margins.SetBounds(16, 16, 16, 16); // Standard spacing
  
  // Interactive elevation changes
  tbElevation.Min := 0;
  tbElevation.Max := 24;
  tbElevation.Value := 2;
  tbElevation.OnChanging := tbElevationChanging;
  
  // Corner radius controls
  tbCornerRadius.Min := 0;
  tbCornerRadius.Max := 20;
  tbCornerRadius.Value := 8;
  tbCornerRadius.OnChanging := tbCornerRadiusChanging;
end;

procedure TFormMain.tbElevationChanging(Sender: TObject);
begin
  fgCardPanel1.Elevation := tbElevation.Value;
  lblElevationValue.Text := Format('Elevation: %.0f dp', [tbElevation.Value]);
end;

procedure TFormMain.tbCornerRadiusChanging(Sender: TObject);
begin
  fgCardPanel1.CornerRadius := tbCornerRadius.Value;
  lblCornerRadiusValue.Text := Format('Corner Radius: %.0f dp', [tbCornerRadius.Value]);
end;
```

#### Material Design Card Types

```pascal
// Different Material Design card elevations
procedure TFormMain.SetupMaterialDesignCards;
begin
  // Resting elevation (0-1dp) - Default state
  cardResting.Elevation := 1;
  cardResting.BackgroundColor := TAlphaColors.White;
  
  // Raised elevation (1-8dp) - Hover/Focus state  
  cardRaised.Elevation := 4;
  cardRaised.BackgroundColor := TAlphaColors.White;
  
  // Dragged elevation (8-24dp) - Dragging state
  cardDragged.Elevation := 16;
  cardDragged.BackgroundColor := TAlphaColors.White;
  
  // Apply Material Design color system
  ApplyMaterialColorsToCards;
end;

procedure TFormMain.ApplyMaterialColorsToCards;
var
  surfaceColor: TAlphaColor;
  onSurfaceColor: TAlphaColor;
begin
  // Get theme-appropriate colors
  if TfgApplication.Theme = TfgTheme.Dark then
  begin
    surfaceColor := TAlphaColorRec.Gray.WithAlpha(240);
    onSurfaceColor := TAlphaColors.White;
  end
  else
  begin
    surfaceColor := TAlphaColors.White;
    onSurfaceColor := TAlphaColorRec.Black.WithAlpha(220);
  end;
  
  // Apply to all cards
  cardResting.BackgroundColor := surfaceColor;
  cardRaised.BackgroundColor := surfaceColor;
  cardDragged.BackgroundColor := surfaceColor;
  
  // Update text colors
  UpdateCardTextColors(onSurfaceColor);
end;
```

#### Interactive Card Behaviors

```pascal
// Card with interactive states
type
  TInteractiveCard = class(TfgCardPanel)
  private
    FDefaultElevation: Single;
    FHoveredElevation: Single;
    FPressedElevation: Single;
    FAnimationRunning: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    
    property DefaultElevation: Single read FDefaultElevation write FDefaultElevation;
    property HoveredElevation: Single read FHoveredElevation write FHoveredElevation;
    property PressedElevation: Single read FPressedElevation write FPressedElevation;
  end;

constructor TInteractiveCard.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultElevation := 2;
  FHoveredElevation := 8;
  FPressedElevation := 16;
  FAnimationRunning := False;
end;

procedure TInteractiveCard.MouseEnter;
begin
  inherited;
  if not FAnimationRunning then
    AnimateElevation(FHoveredElevation);
end;

procedure TInteractiveCard.MouseLeave;
begin
  inherited;
  if not FAnimationRunning then
    AnimateElevation(FDefaultElevation);
end;

procedure TInteractiveCard.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  AnimateElevation(FPressedElevation);
end;

procedure TInteractiveCard.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  AnimateElevation(FHoveredElevation);
end;

procedure TInteractiveCard.AnimateElevation(const ATargetElevation: Single);
begin
  if FAnimationRunning then Exit;
  
  FAnimationRunning := True;
  TfgAnimationHelper.AnimateFloat(
    Self,
    'Elevation',
    Elevation,
    ATargetElevation,
    0.2,
    procedure
    begin
      FAnimationRunning := False;
    end);
end;
```

#### Advanced Card Layouts

```pascal
// Card with complex content layout
procedure TFormMain.CreateContentCard;
var
  cardLayout: TfgLayout;
  headerLayout: TfgLayout;
  contentLayout: TfgLayout;
  actionLayout: TfgLayout;
begin
  // Main card container
  cardContent := TfgCardPanel.Create(Self);
  cardContent.Parent := Self;
  cardContent.Elevation := 2;
  cardContent.CornerRadius := 12;
  cardContent.Margins.SetBounds(16, 16, 16, 16);
  
  // Main layout inside card
  cardLayout := TfgLayout.Create(cardContent);
  cardLayout.Parent := cardContent;
  cardLayout.Align := TfgAlign.Client;
  cardLayout.Padding.SetBounds(16, 16, 16, 16);
  
  // Header section
  headerLayout := CreateCardHeader(cardLayout);
  
  // Content section  
  contentLayout := CreateCardContent(cardLayout);
  
  // Action buttons section
  actionLayout := CreateCardActions(cardLayout);
end;

function TFormMain.CreateCardHeader(AParent: TfgControl): TfgLayout;
var
  headerLayout: TfgLayout;
  avatarImage: TfgImage;
  titleLabel: TfgLabel;
  subtitleLabel: TfgLabel;
  menuButton: TfgButton;
begin
  headerLayout := TfgLayout.Create(AParent);
  headerLayout.Parent := AParent;
  headerLayout.Align := TfgAlign.Top;
  headerLayout.Size.Height := 72;
  
  // Avatar
  avatarImage := TfgImage.Create(headerLayout);
  avatarImage.Parent := headerLayout;
  avatarImage.Align := TfgAlign.Left;
  avatarImage.Size.Width := 40;
  avatarImage.Size.Height := 40;
  avatarImage.Margins.SetBounds(0, 16, 16, 16);
  avatarImage.ImageMode := TfgImageMode.FitCrop;
  avatarImage.CornerRadius := 20; // Circular avatar
  
  // Title and subtitle
  titleLabel := TfgLabel.Create(headerLayout);
  titleLabel.Parent := headerLayout;
  titleLabel.Align := TfgAlign.Client;
  titleLabel.Margins.SetBounds(0, 8, 16, 0);
  titleLabel.Text := 'Card Title';
  titleLabel.Font.Size := 16;
  titleLabel.Font.Style := [TFontStyle.fsBold];
  
  subtitleLabel := TfgLabel.Create(headerLayout);
  subtitleLabel.Parent := headerLayout;
  subtitleLabel.Align := TfgAlign.Client;
  subtitleLabel.Margins.SetBounds(0, 32, 16, 8);
  subtitleLabel.Text := 'Subtitle';
  subtitleLabel.Font.Size := 14;
  subtitleLabel.TextColor := TAlphaColorRec.Gray;
  
  // Menu button
  menuButton := TfgButton.Create(headerLayout);
  menuButton.Parent := headerLayout;
  menuButton.Align := TfgAlign.Right;
  menuButton.Size.Width := 48;
  menuButton.Size.Height := 48;
  menuButton.StyleKind := TfgButtonStyleKind.Icon;
  menuButton.Icon := TfgAssets.GetBitmap('more_vert');
  
  Result := headerLayout;
end;

function TFormMain.CreateCardContent(AParent: TfgControl): TfgLayout;
var
  contentLayout: TfgLayout;
  contentImage: TfgImage;
  contentText: TfgLabel;
begin
  contentLayout := TfgLayout.Create(AParent);
  contentLayout.Parent := AParent;
  contentLayout.Align := TfgAlign.Client;
  
  // Featured image
  contentImage := TfgImage.Create(contentLayout);
  contentImage.Parent := contentLayout;
  contentImage.Align := TfgAlign.Top;
  contentImage.Size.Height := 200;
  contentImage.Margins.SetBounds(0, 8, 0, 16);
  contentImage.ImageMode := TfgImageMode.FitCrop;
  contentImage.CornerRadius := 8;
  
  // Content text
  contentText := TfgLabel.Create(contentLayout);
  contentText.Parent := contentLayout;
  contentText.Align := TfgAlign.Client;
  contentText.Text := 'Card content description goes here. This can be multiple lines of text that provide details about the card content.';
  contentText.AutoSize := TfgAutoSize.Height;
  contentText.WordWrap := True;
  contentText.Font.Size := 14;
  contentText.LineHeight := 1.4;
  
  Result := contentLayout;
end;

function TFormMain.CreateCardActions(AParent: TfgControl): TfgLayout;
var
  actionLayout: TfgLayout;
  flexLayout: TfgFlex;
  btnShare: TfgButton;
  btnFavorite: TfgButton;
  btnMore: TfgButton;
begin
  actionLayout := TfgLayout.Create(AParent);
  actionLayout.Parent := AParent;
  actionLayout.Align := TfgAlign.Bottom;
  actionLayout.Size.Height := 52;
  actionLayout.Margins.SetBounds(0, 8, 0, 0);
  
  // Flex layout for buttons
  flexLayout := TfgFlex.Create(actionLayout);
  flexLayout.Parent := actionLayout;
  flexLayout.Align := TfgAlign.Client;
  flexLayout.FlexDirection := TfgFlexDirection.Row;
  flexLayout.JustifyContent := TfgJustifyContent.FlexEnd;
  flexLayout.AlignItems := TfgAlignItems.Center;
  
  // Action buttons
  btnShare := CreateActionButton(flexLayout, 'share', 'Share');
  btnFavorite := CreateActionButton(flexLayout, 'favorite', 'Like');
  btnMore := CreateActionButton(flexLayout, 'more_horiz', 'More');
  
  Result := actionLayout;
end;

function TFormMain.CreateActionButton(AParent: TfgControl; const AIcon, AText: string): TfgButton;
begin
  Result := TfgButton.Create(AParent);
  Result.Parent := AParent;
  Result.StyleKind := TfgButtonStyleKind.Text;
  Result.Text := AText;
  Result.Icon := TfgAssets.GetBitmap(AIcon);
  Result.Margins.SetBounds(8, 0, 0, 0);
  Result.Size.Width := 80;
  Result.Size.Height := 36;
end;
```

#### Card Collection Patterns

```pascal
// Cards in CollectionView for efficient scrolling
procedure TFormMain.SetupCardCollection;
begin
  cardCollection.Columns := 1;
  cardCollection.ItemSpacing := 16;
  cardCollection.Padding.SetBounds(16, 16, 16, 16);
  cardCollection.OnBindItem := CardCollectionBindItem;
  cardCollection.OnGetItemCount := CardCollectionGetItemCount;
end;

procedure TFormMain.CardCollectionBindItem(Sender: TObject; const AIndex: Integer; 
  const AStyle: string; const AItem: TfgItemWrapper);
var
  card: TfgCardPanel;
  data: TCardData;
begin
  card := AItem.GetControlByLookupName<TfgCardPanel>('card');
  data := FCardData[AIndex];
  
  // Configure card appearance
  card.Elevation := 2;
  card.CornerRadius := 12;
  
  // Bind data to card content
  AItem.GetControlByLookupName<TfgLabel>('title').Text := data.Title;
  AItem.GetControlByLookupName<TfgLabel>('subtitle').Text := data.Subtitle;
  AItem.GetControlByLookupName<TfgImage>('image').LoadFromURL(data.ImageURL);
  
  // Apply theme colors
  ApplyThemeToCard(card, AItem);
end;

// Staggered card layout for Pinterest-like effect
procedure TFormMain.SetupStaggeredCardLayout;
begin
  staggeredCollection.LayoutMode := TfgCollectionViewLayout.Staggered;
  staggeredCollection.Columns := 2;
  staggeredCollection.ItemSpacing := 8;
  staggeredCollection.OnGetItemHeight := StaggeredCollectionGetItemHeight;
end;

function TFormMain.StaggeredCollectionGetItemHeight(Sender: TObject; const AIndex: Integer): Single;
var
  data: TCardData;
  aspectRatio: Single;
begin
  data := FCardData[AIndex];
  aspectRatio := data.ImageHeight / data.ImageWidth;
  
  // Base height + dynamic content height
  Result := 200 + (aspectRatio * 150) + (data.ContentLines * 20);
end;
```

### 3.5 Page Control and Tabs

**Sample Location**: `PageControl - Tint`

The `TfgPageControl` provides tabbed navigation with customizable appearance:

```pascal
type
  TFormMain = class(TfgForm)
    fgPageControl1: TfgPageControl;
    fgPage1: TfgPage;
    fgPage2: TfgPage;
    fgPage3: TfgPage;
    fgPage4: TfgPage;
    procedure fgPageControl1Changed(Sender: TObject);
  end;

procedure TFormMain.SetupPageControl;
begin
  // Configure page control appearance
  fgPageControl1.TintColor := TAlphaColors.Blue;
  fgPageControl1.SelectedTintColor := TAlphaColors.White;
  fgPageControl1.BackgroundColor := TAlphaColors.Lightgray;
  fgPageControl1.Position := TfgPageControlPosition.Bottom;
  
  // Setup pages
  fgPage1.Title := 'Home';
  fgPage1.Icon := TfgAssets.GetBitmap('home_icon');
  
  fgPage2.Title := 'Search';
  fgPage2.Icon := TfgAssets.GetBitmap('search_icon');
  
  fgPage3.Title := 'Profile';
  fgPage3.Icon := TfgAssets.GetBitmap('profile_icon');
  
  // Set initial page
  fgPageControl1.ActivePageIndex := 0;
end;

procedure TFormMain.fgPageControl1Changed(Sender: TObject);
begin
  case fgPageControl1.ActivePageIndex of
    0: LoadHomePage;
    1: LoadSearchPage;
    2: LoadProfilePage;
  end;
  
  // Update navigation title
  fgNavigationBar1.Title := fgPageControl1.ActivePage.Title;
end;

// Advanced tab management
procedure TFormMain.AddDynamicTab(const ATitle, AIcon: string);
var
  newPage: TfgPage;
begin
  newPage := TfgPage.Create(fgPageControl1);
  newPage.Title := ATitle;
  newPage.Icon := TfgAssets.GetBitmap(AIcon);
  newPage.Parent := fgPageControl1;
  
  // Add content to the new page
  CreatePageContent(newPage);
  
  // Switch to the new page
  fgPageControl1.ActivePage := newPage;
end;
```

### 3.6 Image Display Modes

**Sample Location**: `Image - Modes`

Advanced image display with different scaling modes:

```pascal
type
  TFormMain = class(TfgForm)
    fgImage1: TfgImage;
    cbxImageMode: TfgComboBox;
    cbxMaskMode: TfgComboBox;
    chkUseMask: TfgCheckBox;
    procedure cbxImageModeChanged(Sender: TObject);
    procedure cbxMaskModeChanged(Sender: TObject);
    procedure chkUseMaskChanged(Sender: TObject);
  end;

procedure TFormMain.fgFormCreate(Sender: TObject);
begin
  // Fill image mode options
  FillImageModes(cbxImageMode);
  FillMaskModes(cbxMaskMode);
  
  // Set initial mode
  fgImage1.ImageMode := TfgImageMode.Fit;
  cbxImageMode.ItemIndex := Ord(TfgImageMode.Fit);
end;

procedure TFormMain.FillImageModes(const AComboBox: TfgComboBox);
var
  mode: TfgImageMode;
  modeNames: array[TfgImageMode] of string = (
    'Original', 'Stretch', 'Fit', 'FitCrop', 'Fill', 'Center'
  );
begin
  AComboBox.Items.Clear;
  for mode := Low(TfgImageMode) to High(TfgImageMode) do
    AComboBox.Items.Add(modeNames[mode]);
end;

procedure TFormMain.cbxImageModeChanged(Sender: TObject);
var
  selectedMode: TfgImageMode;
begin
  selectedMode := TfgImageMode(cbxImageMode.ItemIndex);
  fgImage1.ImageMode := selectedMode;
  
  // Update description
  case selectedMode of
    TfgImageMode.Original: lblDescription.Text := 'Image at original size';
    TfgImageMode.Stretch: lblDescription.Text := 'Image stretched to fit';
    TfgImageMode.Fit: lblDescription.Text := 'Image scaled to fit while maintaining aspect ratio';
    TfgImageMode.FitCrop: lblDescription.Text := 'Image scaled and cropped to fill';
    TfgImageMode.Fill: lblDescription.Text := 'Image fills the entire area';
    TfgImageMode.Center: lblDescription.Text := 'Image centered without scaling';
  end;
end;

procedure TFormMain.chkUseMaskChanged(Sender: TObject);
begin
  if chkUseMask.IsChecked then
  begin
    fgImage1.MaskImage := TfgAssets.GetBitmap('mask_circle');
    fgImage1.MaskMode := TfgMaskMode(cbxMaskMode.ItemIndex);
    cbxMaskMode.Enabled := True;
  end
  else
  begin
    fgImage1.MaskImage := nil;
    cbxMaskMode.Enabled := False;
  end;
end;

// Advanced image loading with progress
procedure TFormMain.LoadImageWithProgress(const AImageURL: string);
begin
  activityIndicator.IsAnimating := True;
  
  TfgHTTPClient.GetAsync(AImageURL,
    procedure(const AResponse: TfgHTTPResponse)
    var
      bitmap: TfgBitmap;
    begin
      activityIndicator.IsAnimating := False;
      
      if AResponse.StatusCode = 200 then
      begin
        bitmap := TfgBitmap.CreateFromStream(AResponse.ContentStream);
        try
          fgImage1.Bitmap.Assign(bitmap);
          lblStatus.Text := Format('Image loaded: %dx%d', [bitmap.Width, bitmap.Height]);
        finally
          bitmap.Free;
        end;
      end
      else
        lblStatus.Text := 'Failed to load image: ' + AResponse.StatusText;
    end);
end;
```

---

## 4. Layout and Positioning

### 4.1 Flex Layout System

**Sample Locations**: 
- `Flex - Auto wrap`
- `Flex - Automatic scaling with FlexGrow`
- `Flex - Justify content`
- `Flex - Navigation buttons`
- `Flex - Relative position`
- `Flex - Stacks`

The `TfgFlex` component provides powerful FlexBox-style layouts.

#### Auto Wrap Example

```pascal
procedure TFormMain.SetupAutoWrapFlex;
begin
  flexContainer.FlexDirection := TfgFlexDirection.Row;
  flexContainer.FlexWrap := TfgFlexWrap.Wrap;
  flexContainer.JustifyContent := TfgJustifyContent.SpaceEvenly;
  
  // Add items that will wrap automatically
  for i := 1 to 10 do
  begin
    btn := TfgButton.Create(flexContainer);
    btn.Text := Format('Item %d', [i]);
    btn.Size.Width := 80;
    btn.Size.Height := 40;
    btn.Parent := flexContainer;
  end;
end;
```

#### FlexGrow Scaling

```pascal
procedure TFormMain.SetupFlexGrow;
begin
  // Container setup
  flexMain.FlexDirection := TfgFlexDirection.Row;
  
  // Fixed size item
  btnFixed.FlexOptions.FlexGrow := 0;
  btnFixed.Size.Width := 100;
  
  // Growing items
  btnGrow1.FlexOptions.FlexGrow := 1;
  btnGrow2.FlexOptions.FlexGrow := 2; // Takes twice the space
  btnGrow3.FlexOptions.FlexGrow := 1;
end;
```

#### Justify Content Options

```pascal
procedure TFormMain.DemoJustifyContent(AJustification: TfgJustifyContent);
begin
  flexDemo.JustifyContent := AJustification;
  
  case AJustification of
    TfgJustifyContent.FlexStart: lblDescription.Text := 'Items aligned to start';
    TfgJustifyContent.FlexEnd: lblDescription.Text := 'Items aligned to end';
    TfgJustifyContent.Center: lblDescription.Text := 'Items centered';
    TfgJustifyContent.SpaceBetween: lblDescription.Text := 'Space between items';
    TfgJustifyContent.SpaceAround: lblDescription.Text := 'Space around items';
    TfgJustifyContent.SpaceEvenly: lblDescription.Text := 'Even space distribution';
  end;
end;
```

### 4.2 Navigation Layouts

**Sample Location**: `Flex - Navigation buttons`

Creating responsive navigation bars:

```pascal
procedure TFormMain.SetupNavigation;
begin
  // Navigation container
  flexNav.FlexDirection := TfgFlexDirection.Row;
  flexNav.JustifyContent := TfgJustifyContent.SpaceBetween;
  flexNav.AlignItems := TfgAlignItems.Center;
  
  // Back button (fixed)
  btnBack.FlexOptions.FlexGrow := 0;
  
  // Title (flexible)
  lblTitle.FlexOptions.FlexGrow := 1;
  lblTitle.TextAlign := TfgTextAlign.Center;
  
  // Menu button (fixed)
  btnMenu.FlexOptions.FlexGrow := 0;
end;
```

### 4.3 Relative Positioning

**Sample Location**: `Flex - Relative position`

Advanced positioning with offsets:

```pascal
procedure TFormMain.SetupRelativePositioning;
begin
  // Floating action button
  btnFAB.FlexOptions.Position := TfgPosition.Absolute;
  btnFAB.FlexOptions.Bottom := 20;
  btnFAB.FlexOptions.Right := 20;
  
  // Overlay notification
  pnlNotification.FlexOptions.Position := TfgPosition.Absolute;
  pnlNotification.FlexOptions.Top := 50;
  pnlNotification.FlexOptions.Right := 10;
  pnlNotification.FlexOptions.Left := 10;
end;
```

---

## 5. Data Display and Collections

### 5.1 Simple List Implementation

**Sample Location**: `CollectionView - Simple list`

Basic text list using `TfgCollectionView`:

```pascal
type
  TFormMain = class(TfgForm)
    fgCollectionView1: TfgCollectionView;
    function fgCollectionView1GetItemCount(Sender: TObject): Integer;
    procedure fgCollectionView1BindItem(Sender: TObject; const AIndex: Integer; 
      const AStyle: string; const AItem: TfgItemWrapper);
  end;

function TFormMain.fgCollectionView1GetItemCount(Sender: TObject): Integer;
begin
  Result := 1000; // Number of items
end;

procedure TFormMain.fgCollectionView1BindItem(Sender: TObject; 
  const AIndex: Integer; const AStyle: string; const AItem: TfgItemWrapper);
begin
  // Bind data to item controls
  AItem.GetControlByLookupName<TfgLabel>('title').Text := Format('Item %d', [AIndex]);
end;
```

### 5.2 Multi-Column Layout

**Sample Location**: `CollectionView - Columns`

Grid-style layout with multiple columns:

```pascal
procedure TFormMain.SetupColumns;
begin
  collectionView.Columns := 2; // Two columns
  collectionView.ItemSpacing := 8;
  collectionView.ItemAspectRatio := 1.0; // Square items
end;

procedure TFormMain.BindPhotoItem(const AIndex: Integer; const AItem: TfgItemWrapper);
var
  imgPhoto: TfgImage;
  lblCaption: TfgLabel;
begin
  imgPhoto := AItem.GetControlByLookupName<TfgImage>('photo');
  lblCaption := AItem.GetControlByLookupName<TfgLabel>('caption');
  
  // Load photo asynchronously
  imgPhoto.LoadFromAsset(Format('photo_%d.jpg', [AIndex + 1]));
  lblCaption.Text := Format('Photo %d', [AIndex + 1]);
end;
```

### 5.3 Advanced Selection

**Sample Location**: `CollectionView - Advanced selection`

Multi-selection with visual feedback:

```pascal
type
  TFormMain = class(TfgForm)
    procedure collectionViewSelectionChanged(Sender: TObject);
  private
    FSelectedItems: TList<Integer>;
  end;

procedure TFormMain.collectionViewSelectionChanged(Sender: TObject);
var
  i: Integer;
begin
  FSelectedItems.Clear;
  
  // Get all selected indices
  for i := 0 to collectionView.GetSelectedItemsCount - 1 do
    FSelectedItems.Add(collectionView.GetSelectedItem(i));
    
  // Update UI
  lblSelectionCount.Text := Format('%d items selected', [FSelectedItems.Count]);
  btnDelete.Enabled := FSelectedItems.Count > 0;
end;

procedure TFormMain.BindSelectableItem(const AIndex: Integer; const AItem: TfgItemWrapper);
var
  chkSelect: TfgCheckBox;
  isSelected: Boolean;
begin
  chkSelect := AItem.GetControlByLookupName<TfgCheckBox>('select');
  isSelected := FSelectedItems.Contains(AIndex);
  
  chkSelect.IsChecked := isSelected;
  AItem.Selected := isSelected;
end;
```

### 5.4 Dynamic Loading

**Sample Location**: `CollectionView - Dynamic loading`

Load data on demand for performance:

```pascal
type
  TDataProvider = class
    function GetItemCount: Integer;
    function GetItem(AIndex: Integer): TDataItem;
    procedure LoadMoreItems;
  end;

procedure TFormMain.SetupDynamicLoading;
begin
  collectionView.OnNeedMoreItems := CollectionViewNeedMoreItems;
  collectionView.LoadMoreThreshold := 10; // Load when 10 items from end
end;

procedure TFormMain.CollectionViewNeedMoreItems(Sender: TObject);
begin
  // Load more data in background
  TTask.Run(
    procedure
    begin
      FDataProvider.LoadMoreItems;
      
      // Update UI on main thread
      TThread.Synchronize(nil,
        procedure
        begin
          collectionView.RefreshData;
        end);
    end);
end;
```

## 6. Navigation and Drawer Systems

FGX Native provides comprehensive navigation solutions including traditional navigation bars and slide-out drawer panels for complex app hierarchies.

### 6.1 Navigation Bar

**Sample Location**: `NavigationBar - Autosize`

The TfgNavigationBar component provides platform-appropriate top navigation:

```pascal
type
  TFormMain = class(TfgForm)
    fgNavigationBar1: TfgNavigationBar;
    procedure NavigationBarLeftButtonTap(Sender: TObject);
    procedure NavigationBarRightButtonTap(Sender: TObject);
  end;

// Basic navigation bar setup
procedure TFormMain.SetupNavigationBar;
begin
  fgNavigationBar1.Title := 'My App';
  fgNavigationBar1.ShowBackButton := True;
  fgNavigationBar1.LeftButton.Text := 'Menu';
  fgNavigationBar1.RightButton.Text := 'Settings';
end;
```

### 6.2 Drawer Layout System

**Sample Location**: `Drawer - Base`, `Assets - Dark and Light themes`

The TfgDrawerLayout provides slide-out navigation panels ideal for apps with multiple top-level sections:

```pascal
type
  TFormMain = class(TfgForm)
    fgDrawerLayout1: TfgDrawerLayout;
    fgDrawerLayout1_Drawer: TfgDrawer;
    fgDrawerLayout1_Content: TfgMainContent;
    fgListMenu: TfgListMenu;
    procedure fgDrawerLayout1BeginOpen(Sender: TObject);
    procedure fgDrawerLayout1Opened(Sender: TObject);
    procedure fgDrawerLayout1BeginClose(Sender: TObject);
    procedure fgDrawerLayout1Closed(Sender: TObject);
    procedure fgDrawerLayout1StateChanged(Sender: TObject);
    procedure fgDrawerLayout1SlideChanged(Sender: TObject);
  end;

// Drawer configuration
procedure TFormMain.SetupDrawer;
begin
  // Enable/disable drawer
  fgDrawerLayout1.Enabled := True;
  
  // Set toggle control (e.g., hamburger menu button)
  fgDrawerLayout1.ToggleControl := btnMenu;
  
  // Manual drawer control
  fgDrawerLayout1.Open;  // or Close
end;

// Event handlers for drawer state tracking
procedure TFormMain.fgDrawerLayout1StateChanged(Sender: TObject);
begin
  case fgDrawerLayout1.State of
    TfgDrawerState.Opened: HandleDrawerOpened;
    TfgDrawerState.Closed: HandleDrawerClosed;
  end;
end;
```

## 7. Picker Components

FGX Native provides platform-native picker components for common input scenarios.

### 7.1 Date Picker

**Sample Location**: `Pickers - Date`

```pascal
uses FGX.Pickers.Date, FGX.Toasts;

// Quick date picker
procedure TFormMain.ShowDatePicker;
begin
  TfgPickerDateFactory.PickDate(Self, EncodeDate(2020, 1, 1), 
    procedure (const ASelectedDate: TDate)
    begin
      TfgToastFactory.Show('Selected: ' + DateToStr(ASelectedDate));
    end);
end;

// Advanced date picker with constraints
procedure TFormMain.ShowConstrainedDatePicker;
var
  datePicker: TfgPickerDate;
begin
  datePicker := TfgPickerDateFactory.CreatePicker(Self);
  datePicker.Date := EncodeDate(2020, 1, 15);
  datePicker.MinDate := EncodeDate(2020, 1, 10);
  datePicker.MaxDate := EncodeDate(2020, 1, 25);
  
  datePicker.OnDateSelectedCallback := procedure (const ASelectedDate: TDate)
  begin
    ProcessSelectedDate(ASelectedDate);
  end;
  
  datePicker.Show;
end;
```

### 7.2 Photo Picker

**Sample Location**: `Pickers - Photo`

```pascal
uses FGX.Pickers.Photo, FGX.Pickers.Media;

// Take photo from camera
procedure TFormMain.TakePhotoFromCamera;
begin
  TfgPickerPhotoFactory.PickPhotoFromCamera(Self,
    procedure(const AFileNames: TfgMediaFiles)
    begin
      LoadImages(AFileNames);
    end,
    procedure(const AProgress: Single)
    begin
      ShowProgress(AProgress);
    end,
    HideProgress);
end;

// Pick multiple photos from library
procedure TFormMain.PickPhotosFromLibrary;
begin
  TfgPickerPhotoFactory.PickPhotosFromLibrary(Self,
    procedure(const AFileNames: TfgMediaFiles)
    begin
      ProcessSelectedPhotos(AFileNames);
    end);
end;
```

## 8. Progress Indicators

### 8.1 Activity Indicator

For operations with unknown duration:

```pascal
uses FGX.ActivityIndicator;

procedure TFormMain.ShowLoadingIndicator;
begin
  indProgress.Visible := True;
  indProgress.Start;
  indProgress.TintColorName := R.Color.COLORS_BLUE400;
end;

procedure TFormMain.HideLoadingIndicator;
begin
  indProgress.Stop;
  indProgress.Visible := False;
end;
```

### 8.2 Progress Bar

For operations with known progress:

```pascal
uses FGX.ProgressBar;

procedure TFormMain.SetupProgressBar;
begin
  progressBar.Min := 0;
  progressBar.Max := 100;
  progressBar.Progress := 0;
  progressBar.TintColorName := R.Color.COLORS_GREEN400;
end;

procedure TFormMain.UpdateProgress(const AValue: Integer);
begin
  progressBar.Progress := AValue;
end;
```

## 9. Input Controls (Sliders and Switches)

### 9.1 Slider Component

Sliders allow users to select values from a defined range:

```pascal
uses FGX.TrackBar;

type
  TFormMain = class(TfgForm)
    tbVolume: TfgTrackBar;
    tbBrightness: TfgTrackBar;
    procedure tbVolumeChanging(Sender: TObject);
    procedure tbVolumeChanged(Sender: TObject);
  end;

procedure TFormMain.SetupSliders;
begin
  // Volume slider (0-100)
  tbVolume.Min := 0;
  tbVolume.Max := 100;
  tbVolume.Value := 50;
  
  // Brightness slider (0.0-1.0)
  tbBrightness.Min := 0;
  tbBrightness.Max := 100;
  tbBrightness.Value := 75;
end;

// Real-time value tracking
procedure TFormMain.tbVolumeChanging(Sender: TObject);
begin
  // Update UI during drag
  lblVolumeValue.Text := Format('Volume: %d%%', [Round(tbVolume.Value)]);
end;

// Final value when dragging stops
procedure TFormMain.tbVolumeChanged(Sender: TObject);
begin
  // Apply the setting
  SetSystemVolume(tbVolume.Value / 100.0);
  SaveVolumePreference(Round(tbVolume.Value));
end;

// Custom slider styling
procedure TFormMain.CustomizeSliderAppearance;
begin
  tbVolume.TintColorName := R.Color.COLORS_BLUE400;
  tbVolume.Track.ColorName := R.Color.COLORS_GREY300;
  tbVolume.Thumb.ColorName := R.Color.COLORS_BLUE600;
end;
```

### 9.2 Switch Component

Switches provide mobile-friendly toggle controls:

```pascal
uses FGX.Switch;

type
  TFormMain = class(TfgForm)
    swNotifications: TfgSwitch;
    swDarkMode: TfgSwitch;
    swLocationTracking: TfgSwitch;
    procedure swNotificationsChanged(Sender: TObject);
    procedure swDarkModeChanged(Sender: TObject);
  end;

procedure TFormMain.SetupSwitches;
begin
  // Load saved preferences
  swNotifications.IsChecked := GetNotificationPreference;
  swDarkMode.IsChecked := GetThemePreference = 'Dark';
  swLocationTracking.IsChecked := GetLocationPermission;
  
  // Customize appearance
  swNotifications.TintColorName := R.Color.COLORS_GREEN400;
  swDarkMode.TintColorName := R.Color.COLORS_PURPLE400;
  swLocationTracking.TintColorName := R.Color.COLORS_ORANGE400;
end;

procedure TFormMain.swNotificationsChanged(Sender: TObject);
begin
  if swNotifications.IsChecked then
  begin
    EnableNotifications;
    ShowToast('Notifications enabled');
  end
  else
  begin
    DisableNotifications;
    ShowToast('Notifications disabled');
  end;
  
  SaveNotificationPreference(swNotifications.IsChecked);
end;

procedure TFormMain.swDarkModeChanged(Sender: TObject);
begin
  if swDarkMode.IsChecked then
    ThemeName := 'Dark'
  else
    ThemeName := 'Light';
    
  SaveThemePreference(ThemeName);
end;

// Switch groups for radio-like behavior
type
  TSwitchGroup = class
  private
    FSwitches: TArray<TfgSwitch>;
    FAllowNone: Boolean;
  public
    constructor Create(const ASwitches: TArray<TfgSwitch>; AAllowNone: Boolean = False);
    procedure SwitchChanged(ASender: TfgSwitch);
  end;

constructor TSwitchGroup.Create(const ASwitches: TArray<TfgSwitch>; AAllowNone: Boolean);
var
  sw: TfgSwitch;
begin
  FSwitches := ASwitches;
  FAllowNone := AAllowNone;
  
  // Set change handlers
  for sw in FSwitches do
    sw.OnChanged := SwitchChanged;
end;

procedure TSwitchGroup.SwitchChanged(ASender: TfgSwitch);
var
  sw: TfgSwitch;
begin
  if ASender.IsChecked then
  begin
    // Turn off other switches
    for sw in FSwitches do
      if sw <> ASender then
        sw.IsChecked := False;
  end
  else if not FAllowNone then
  begin
    // Ensure at least one is selected
    ASender.IsChecked := True;
  end;
end;

// Usage example
procedure TFormMain.SetupViewModeGroup;
var
  switchGroup: TSwitchGroup;
begin
  switchGroup := TSwitchGroup.Create([swListView, swGridView, swCardView], False);
  swListView.IsChecked := True; // Default selection
end;
```

### 9.3 Advanced Input Patterns

```pascal
// Slider with custom labels and stepped values
procedure TFormMain.SetupCustomSlider;
var
  i: Integer;
  qualityLabels: TArray<string>;
begin
  qualityLabels := ['Low', 'Medium', 'High', 'Ultra'];
  
  tbQuality.Min := 0;
  tbQuality.Max := Length(qualityLabels) - 1;
  tbQuality.Value := 2; // Default to High
  
  // Display current quality level
  tbQuality.OnChanged := procedure(Sender: TObject)
  begin
    lblQualityLevel.Text := qualityLabels[Round(tbQuality.Value)];
    ApplyQualitySettings(Round(tbQuality.Value));
  end;
end;

// Switch with confirmation dialog
procedure TFormMain.swDangerousModeChanged(Sender: TObject);
begin
  if swDangerousMode.IsChecked then
  begin
    TfgDialogFactory.ShowConfirmation('Enable Dangerous Mode?',
      'This will enable advanced features that may cause data loss.',
      procedure(const AResult: TfgDialogResult)
      begin
        if AResult = TfgDialogResult.Yes then
        begin
          EnableDangerousMode;
        end
        else
        begin
          swDangerousMode.IsChecked := False;
        end;
      end);
  end
  else
  begin
    DisableDangerousMode;
  end;
end;
```

## 10. Tab Controls and Page Management

FGX Native provides sophisticated tab control through TfgPageControl for organizing content across multiple screens.

### 10.1 Basic Page Control

**Sample Location**: `PageControl - Tint`, `PageControl - Advanced`

```pascal
uses FGX.PageControl;

type
  TFormMain = class(TfgForm)
    fgPageControl1: TfgPageControl;
    fgPage1: TfgPage;
    fgPage2: TfgPage;
    fgPage3: TfgPage;
    fgPage4: TfgPage;
    procedure fgPageControl1Changed(Sender: TObject);
    procedure fgPageControl1PageSelected(Sender: TObject; const APage: TfgPage);
    procedure fgPageControl1PageSelecting(Sender: TObject; const APage: TfgPage; var ACanSelect: Boolean);
  end;

// Basic tab setup
procedure TFormMain.SetupTabs;
begin
  // Configure page control
  fgPageControl1.TabsHeight := 48;
  fgPageControl1.SwipeEnabled := True;
  fgPageControl1.TabsPosition := TfgTabsPosition.Top;
  
  // Setup individual pages
  fgPage1.Text := 'Home';
  fgPage1.IconName := R.Bitmap.HOME_ICON;
  
  fgPage2.Text := 'Profile';
  fgPage2.IconName := R.Bitmap.PROFILE_ICON;
  
  fgPage3.Text := 'Settings';
  fgPage3.IconName := R.Bitmap.SETTINGS_ICON;
  
  // Theming
  fgPageControl1.TintColorName := R.Color.COLORS_BLUE400;
end;

// Handle page changes
procedure TFormMain.fgPageControl1Changed(Sender: TObject);
var
  tintColorName: string;
begin
  // Dynamic tint color based on active page
  case fgPageControl1.PageIndex of
    0: tintColorName := R.Color.COLORS_BLUE400;
    1: tintColorName := R.Color.COLORS_GREEN400;
    2: tintColorName := R.Color.COLORS_ORANGE400;
    3: tintColorName := R.Color.COLORS_PURPLE400;
  end;
  
  fgPageControl1.TintColorName := tintColorName;
  UpdatePageContent(fgPageControl1.PageIndex);
end;

procedure TFormMain.fgPageControl1PageSelecting(Sender: TObject; const APage: TfgPage; var ACanSelect: Boolean);
begin
  // Prevent switching to certain pages based on conditions
  if (APage = fgPage3) and (not UserHasPermission) then
  begin
    ACanSelect := False;
    ShowToast('Access denied to settings');
  end;
end;
```

### 10.2 Advanced Tab Configuration

```pascal
// Custom tab appearance
procedure TFormMain.SetupAdvancedTabs;
begin
  // Hide tab bar but keep swipe functionality
  fgPageControl1.TabsVisible := False;
  fgPageControl1.SwipeEnabled := True;
  
  // Custom navigation buttons
  btnPrevious.OnClick := procedure(Sender: TObject)
  begin
    if fgPageControl1.PageIndex > 0 then
      fgPageControl1.PageIndex := fgPageControl1.PageIndex - 1;
  end;
  
  btnNext.OnClick := procedure(Sender: TObject)
  begin
    if fgPageControl1.PageIndex < fgPageControl1.PageCount - 1 then
      fgPageControl1.PageIndex := fgPageControl1.PageIndex + 1;
  end;
end;

// Tab badges for notifications
procedure TFormMain.SetupTabBadges;
begin
  // Add badge to messages tab
  fgPage2.BadgeText := '3';
  fgPage2.ShowBadge := True;
  
  // Update badge dynamically
  TTimer.CreateTimer(5000, procedure
  begin
    var unreadCount := GetUnreadMessageCount;
    if unreadCount > 0 then
    begin
      fgPage2.BadgeText := IntToStr(unreadCount);
      fgPage2.ShowBadge := True;
    end
    else
    begin
      fgPage2.ShowBadge := False;
    end;
  end);
end;

// Programmatic tab navigation
procedure TFormMain.NavigateToTab(const ATabIndex: Integer; const AAnimate: Boolean = True);
begin
  if AAnimate then
    fgPageControl1.SetPageIndex(ATabIndex, TfgPageTransition.Slide)
  else
    fgPageControl1.PageIndex := ATabIndex;
end;
```

## 11. Media Components

### 11.1 Video Player

FGX Native provides embedded video playback capabilities for both local files and streaming content:

```pascal
uses FGX.VideoPlayer;

type
  TFormMain = class(TfgForm)
    videoPlayer: TfgVideoPlayer;
    btnPlay: TfgButton;
    btnPause: TfgButton;
    btnStop: TfgButton;
    btnFullscreen: TfgButton;
    trackBarPosition: TfgTrackBar;
    trackBarVolume: TfgTrackBar;
    lblDuration: TfgLabel;
    lblCurrentTime: TfgLabel;
    procedure videoPlayerStateChanged(Sender: TObject);
    procedure videoPlayerPositionChanged(Sender: TObject);
    procedure videoPlayerError(Sender: TObject; const AError: string);
  end;

// Basic video setup
procedure TFormMain.SetupVideoPlayer;
begin
  // Load video from file
  videoPlayer.FileName := TPath.Combine(TPath.GetDocumentsPath, 'sample.mp4');
  
  // Or load from URL
  // videoPlayer.URL := 'https://example.com/video.mp4';
  
  // Configure playback
  videoPlayer.AutoPlay := False;
  videoPlayer.ShowControls := True;
  videoPlayer.Volume := 0.8;
  
  // Setup progress tracking
  trackBarPosition.Min := 0;
  trackBarVolume.Min := 0;
  trackBarVolume.Max := 100;
  trackBarVolume.Value := 80;
end;

// Playback controls
procedure TFormMain.btnPlayClick(Sender: TObject);
begin
  videoPlayer.Play;
end;

procedure TFormMain.btnPauseClick(Sender: TObject);
begin
  videoPlayer.Pause;
end;

procedure TFormMain.btnStopClick(Sender: TObject);
begin
  videoPlayer.Stop;
end;

procedure TFormMain.btnFullscreenClick(Sender: TObject);
begin
  videoPlayer.Fullscreen := not videoPlayer.Fullscreen;
end;

// Event handlers
procedure TFormMain.videoPlayerStateChanged(Sender: TObject);
begin
  case videoPlayer.State of
    TfgVideoPlayerState.Playing:
    begin
      btnPlay.Enabled := False;
      btnPause.Enabled := True;
      btnStop.Enabled := True;
    end;
    TfgVideoPlayerState.Paused:
    begin
      btnPlay.Enabled := True;
      btnPause.Enabled := False;
      btnStop.Enabled := True;
    end;
    TfgVideoPlayerState.Stopped:
    begin
      btnPlay.Enabled := True;
      btnPause.Enabled := False;
      btnStop.Enabled := False;
      trackBarPosition.Value := 0;
    end;
  end;
end;

procedure TFormMain.videoPlayerPositionChanged(Sender: TObject);
var
  position, duration: Single;
begin
  position := videoPlayer.Position;
  duration := videoPlayer.Duration;
  
  if duration > 0 then
  begin
    trackBarPosition.Max := Round(duration);
    trackBarPosition.Value := Round(position);
    
    lblCurrentTime.Text := FormatTime(position);
    lblDuration.Text := FormatTime(duration);
  end;
end;

procedure TFormMain.videoPlayerError(Sender: TObject; const AError: string);
begin
  TfgToastFactory.Show('Video Error: ' + AError);
  TfgLog.Error('Video playback error: %s', [AError]);
end;

// Position and volume control
procedure TFormMain.trackBarPositionChanging(Sender: TObject);
begin
  if not videoPlayer.IsSeeking then
    videoPlayer.Position := trackBarPosition.Value;
end;

procedure TFormMain.trackBarVolumeChanging(Sender: TObject);
begin
  videoPlayer.Volume := trackBarVolume.Value / 100.0;
end;

// Helper function for time formatting
function TFormMain.FormatTime(const ASeconds: Single): string;
var
  totalSeconds, minutes, seconds: Integer;
begin
  totalSeconds := Round(ASeconds);
  minutes := totalSeconds div 60;
  seconds := totalSeconds mod 60;
  Result := Format('%d:%02d', [minutes, seconds]);
end;
```

### 11.2 Advanced Video Features

```pascal
// Video player with custom overlay controls
type
  TCustomVideoPlayer = class(TfgLayout)
  private
    FVideoPlayer: TfgVideoPlayer;
    FControlsOverlay: TfgLayout;
    FAutoHideTimer: TTimer;
    procedure SetupCustomControls;
    procedure ShowControls;
    procedure HideControls;
    procedure AutoHideControls;
  public
    constructor Create(AOwner: TComponent); override;
    property VideoPlayer: TfgVideoPlayer read FVideoPlayer;
  end;

constructor TCustomVideoPlayer.Create(AOwner: TComponent);
begin
  inherited;
  
  // Create video player
  FVideoPlayer := TfgVideoPlayer.Create(Self);
  FVideoPlayer.Parent := Self;
  FVideoPlayer.Align := TfgAlignLayout.Contents;
  FVideoPlayer.ShowControls := False; // We'll use custom controls
  
  // Create controls overlay
  FControlsOverlay := TfgLayout.Create(Self);
  FControlsOverlay.Parent := Self;
  FControlsOverlay.Align := TfgAlignLayout.Contents;
  
  SetupCustomControls;
  
  // Auto-hide timer
  FAutoHideTimer := TTimer.Create(Self);
  FAutoHideTimer.Interval := 3000; // 3 seconds
  FAutoHideTimer.OnTimer := procedure(Sender: TObject)
  begin
    HideControls;
  end;
  
  // Tap to show/hide controls
  FVideoPlayer.OnTap := procedure(Sender: TObject)
  begin
    if FControlsOverlay.Visible then
      HideControls
    else
      ShowControls;
  end;
end;

procedure TCustomVideoPlayer.SetupCustomControls;
var
  playButton: TfgButton;
  progressBar: TfgProgressBar;
  timeLabel: TfgLabel;
begin
  // Semi-transparent background
  var bgRect := TfgRectangle.Create(FControlsOverlay);
  bgRect.Parent := FControlsOverlay;
  bgRect.Align := TfgAlignLayout.Contents;
  bgRect.Fill.Color := $80000000;
  
  // Play/pause button in center
  playButton := TfgButton.Create(FControlsOverlay);
  playButton.Parent := FControlsOverlay;
  playButton.Align := TfgAlignLayout.Center;
  playButton.Size.Size := TfgSizeF.Create(80, 80);
  playButton.StyleName := 'transparent';
  playButton.OnClick := procedure(Sender: TObject)
  begin
    if FVideoPlayer.State = TfgVideoPlayerState.Playing then
      FVideoPlayer.Pause
    else
      FVideoPlayer.Play;
  end;
  
  // Progress bar at bottom
  progressBar := TfgProgressBar.Create(FControlsOverlay);
  progressBar.Parent := FControlsOverlay;
  progressBar.Align := TfgAlignLayout.Bottom;
  progressBar.Margins.SetBounds(20, 0, 20, 20);
end;

// Picture-in-picture mode (if supported)
procedure TFormMain.EnablePictureInPicture;
begin
  if videoPlayer.SupportsPictureInPicture then
  begin
    videoPlayer.PictureInPictureMode := True;
    videoPlayer.OnPictureInPictureChanged := procedure(Sender: TObject; const AEnabled: Boolean)
    begin
      if AEnabled then
        TfgLog.Info('Entered picture-in-picture mode')
      else
        TfgLog.Info('Exited picture-in-picture mode');
    end;
  end;
end;
```

## 12. Web Browser Integration

### 12.1 Basic Web Browser

```pascal
uses FGX.WebBrowser;

type
  TFormMain = class(TfgForm)
    webBrowser: TfgWebBrowser;
    edtURL: TfgEdit;
    btnGo: TfgButton;
    btnBack: TfgButton;
    btnForward: TfgButton;
    btnRefresh: TfgButton;
    progressBar: TfgProgressBar;
    procedure webBrowserStartLoading(Sender: TObject);
    procedure webBrowserFinishLoading(Sender: TObject);
    procedure webBrowserErrorLoading(Sender: TObject; const AError: string);
    procedure webBrowserDecideLoadUrl(Sender: TObject; const AURL: string; var ACanLoad: Boolean);
  end;

// Basic web browser setup
procedure TFormMain.SetupWebBrowser;
begin
  // Load initial page
  webBrowser.URL := 'https://www.example.com';
  
  // Or load local HTML
  // webBrowser.LoadHTML('<html><body><h1>Hello World</h1></body></html>');
  
  // Enable JavaScript
  webBrowser.JavaScriptEnabled := True;
  
  // Setup navigation buttons
  btnBack.OnClick := procedure(Sender: TObject)
  begin
    if webBrowser.CanGoBack then
      webBrowser.GoBack;
  end;
  
  btnForward.OnClick := procedure(Sender: TObject)
  begin
    if webBrowser.CanGoForward then
      webBrowser.GoForward;
  end;
  
  btnRefresh.OnClick := procedure(Sender: TObject)
  begin
    webBrowser.Reload;
  end;
end;

// Navigation
procedure TFormMain.btnGoClick(Sender: TObject);
var
  url: string;
begin
  url := edtURL.Text;
  if not url.StartsWith('http') then
    url := 'https://' + url;
    
  webBrowser.URL := url;
end;

// Loading events
procedure TFormMain.webBrowserStartLoading(Sender: TObject);
begin
  progressBar.Visible := True;
  progressBar.Progress := 0;
  btnRefresh.Enabled := False;
end;

procedure TFormMain.webBrowserFinishLoading(Sender: TObject);
begin
  progressBar.Visible := False;
  btnRefresh.Enabled := True;
  
  // Update navigation buttons
  btnBack.Enabled := webBrowser.CanGoBack;
  btnForward.Enabled := webBrowser.CanGoForward;
  
  // Update URL bar
  edtURL.Text := webBrowser.URL;
end;

procedure TFormMain.webBrowserErrorLoading(Sender: TObject; const AError: string);
begin
  progressBar.Visible := False;
  TfgToastFactory.Show('Failed to load page: ' + AError);
  TfgLog.Error('WebBrowser loading error: %s', [AError]);
end;

// URL filtering
procedure TFormMain.webBrowserDecideLoadUrl(Sender: TObject; const AURL: string; var ACanLoad: Boolean);
begin
  // Block certain URLs
  if AURL.Contains('malicious-site.com') then
  begin
    ACanLoad := False;
    TfgToastFactory.Show('Blocked unsafe site');
    Exit;
  end;
  
  // Handle custom protocols
  if AURL.StartsWith('myapp://') then
  begin
    ACanLoad := False;
    HandleCustomProtocol(AURL);
    Exit;
  end;
  
  ACanLoad := True;
end;
```

### 12.2 JavaScript Integration

```pascal
// JavaScript execution and communication
procedure TFormMain.JavaScriptExamples;
var
  result: string;
begin
  // Execute JavaScript
  webBrowser.ExecuteJavaScript('document.body.style.backgroundColor = "lightblue";');
  
  // Get result from JavaScript
  result := webBrowser.EvaluateJavaScript('document.title');
  TfgLog.Info('Page title: %s', [result]);
  
  // Get current URL via JavaScript
  result := webBrowser.EvaluateJavaScript('window.location.href');
  edtURL.Text := result;
  
  // Inject custom JavaScript functions
  webBrowser.ExecuteJavaScript(
    'function sendMessageToApp(message) {' +
    '  window.location = "myapp://message/" + encodeURIComponent(message);' +
    '}');
end;

// Handle custom protocol messages from JavaScript
procedure TFormMain.HandleCustomProtocol(const AURL: string);
var
  protocol, action, data: string;
begin
  // Parse URL: myapp://action/data
  protocol := AURL.Substring(0, AURL.IndexOf('://'));
  action := AURL.Substring(AURL.IndexOf('://') + 3);
  
  if action.StartsWith('message/') then
  begin
    data := TNetEncoding.URL.Decode(action.Substring(8));
    TfgToastFactory.Show('Message from web: ' + data);
  end
  else if action = 'close' then
  begin
    Close;
  end;
end;

// Scroll control
procedure TFormMain.ScrollWebView;
begin
  // Scroll to top
  webBrowser.ScrollTo(0, 0);
  
  // Scroll by offset
  webBrowser.ScrollBy(0, 100);
  
  // Get scroll position
  var scrollX := webBrowser.EvaluateJavaScript('window.pageXOffset');
  var scrollY := webBrowser.EvaluateJavaScript('window.pageYOffset');
  TfgLog.Info('Scroll position: %s, %s', [scrollX, scrollY]);
end;
```

## 13. Logging System

FGX Native provides a comprehensive logging system for debugging and monitoring applications.

### 13.1 Basic Logging

```pascal
uses FGX.Log;

// Basic logging examples
procedure TFormMain.DemonstrateLogging;
begin
  // Different log levels
  TfgLog.Debug('Debug message - detailed information');
  TfgLog.Info('Info message - general information');
  TfgLog.Warning('Warning message - potential issues');
  TfgLog.Error('Error message - non-fatal errors');
  TfgLog.Fatal('Fatal message - critical errors');
  
  // Formatted logging
  TfgLog.Debug('User %s logged in at %s', [username, TimeToStr(Now)]);
  TfgLog.Warning('Control [%s:%s] being removed with uncleared references', 
    [Control.Name, Control.ClassName]);
end;

// Configure logging levels
procedure TFormMain.ConfigureLogging;
begin
  // Set minimum log level (filters out lower levels)
  TfgLog.MinimumLevel := TfgLogLevel.Info; // Will exclude Debug messages
  
  // In debug builds, show all levels
  {$IFDEF DEBUG}
  TfgLog.MinimumLevel := TfgLogLevel.Debug;
  {$ELSE}
  TfgLog.MinimumLevel := TfgLogLevel.Info;
  {$ENDIF}
end;
```

### 13.2 Advanced Logging Patterns

```pascal
// Custom logging helper class
type
  TAppLogger = class
  private
    class var FInstance: TAppLogger;
    FSessionId: string;
  public
    constructor Create;
    class function Instance: TAppLogger;
    
    procedure LogUserAction(const AAction, AScreen: string);
    procedure LogPerformance(const AOperation: string; const AStartTime: TDateTime);
    procedure LogError(const AError: Exception; const AContext: string = '');
  end;

constructor TAppLogger.Create;
begin
  inherited;
  FSessionId := TGuid.NewGuid.ToString;
end;

class function TAppLogger.Instance: TAppLogger;
begin
  if not Assigned(FInstance) then
    FInstance := TAppLogger.Create;
  Result := FInstance;
end;

procedure TAppLogger.LogUserAction(const AAction, AScreen: string);
begin
  TfgLog.Info('[USER_ACTION] Session: %s, Screen: %s, Action: %s', 
    [FSessionId, AScreen, AAction]);
end;

procedure TAppLogger.LogPerformance(const AOperation: string; const AStartTime: TDateTime);
var
  duration: Int64;
begin
  duration := MilliSecondsBetween(Now, AStartTime);
  TfgLog.Info('[PERFORMANCE] Operation: %s, Duration: %dms', [AOperation, duration]);
end;

procedure TAppLogger.LogError(const AError: Exception; const AContext: string);
begin
  TfgLog.Error('[EXCEPTION] Context: %s, Type: %s, Message: %s', 
    [AContext, AError.ClassName, AError.Message]);
end;

// Usage examples
procedure TFormMain.ExampleLoggingUsage;
var
  startTime: TDateTime;
begin
  // Log user actions
  TAppLogger.Instance.LogUserAction('ButtonTap', 'MainForm', 'LoginButton');
  
  // Performance logging
  startTime := Now;
  try
    // Some operation
    LoadDataFromServer;
  finally
    TAppLogger.Instance.LogPerformance('LoadDataFromServer', startTime);
  end;
  
  // Error logging
  try
    ProcessUserData;
  except
    on E: Exception do
      TAppLogger.Instance.LogError(E, 'ProcessUserData');
  end;
end;

// Conditional logging for sensitive data
procedure TFormMain.LogWithPrivacyFilter(const AMessage: string; const AUserData: string);
begin
  {$IFDEF DEBUG}
  TfgLog.Debug('%s - UserData: %s', [AMessage, AUserData]);
  {$ELSE}
  TfgLog.Debug('%s - UserData: [FILTERED]', [AMessage]);
  {$ENDIF}
end;
```

## 14. Firebase Push Notifications

FGX Native provides comprehensive Firebase push notification support for cross-platform mobile applications.

### 14.1 Firebase Setup and Configuration

**Requirements**: FGX Native 1.15.2.0 or higher

```pascal
uses FGX.PushNotificationService;

type
  TFormMain = class(TfgForm)
    fgPushNotificationService1: TfgPushNotificationService;
    procedure fgPushNotificationService1DeviceTokenChanged(Sender: TObject; const ADeviceToken: string);
    procedure fgPushNotificationService1PushNotificationReceived(Sender: TObject; const ANotification: TfgPushNotification);
    procedure fgPushNotificationService1AuthorizationChanged(Sender: TObject; const AAuthorization: TfgPushNotificationAuthorization);
  end;

// Basic push notification setup
procedure TFormMain.SetupPushNotifications;
begin
  // Configure Firebase Cloud Messaging
  fgPushNotificationService1.ServiceName := 'FCM'; // Firebase Cloud Messaging
  
  // Request permission from user
  fgPushNotificationService1.RequestAuthorization([
    TfgPushNotificationPermission.Alert,
    TfgPushNotificationPermission.Badge,
    TfgPushNotificationPermission.Sound
  ]);
  
  // Start the service
  fgPushNotificationService1.StartService;
end;

// Handle device token registration
procedure TFormMain.fgPushNotificationService1DeviceTokenChanged(Sender: TObject; const ADeviceToken: string);
begin
  TfgLog.Info('Device token received: %s', [ADeviceToken]);
  
  // Send token to your backend server
  SendTokenToServer(ADeviceToken);
  
  // Store token locally for debugging
  TfgPreferencesService.GetDefaultPreferences.SetValue('FCM_TOKEN', ADeviceToken);
end;

// Handle incoming push notifications
procedure TFormMain.fgPushNotificationService1PushNotificationReceived(Sender: TObject; const ANotification: TfgPushNotification);
begin
  TfgLog.Info('Push notification received: %s', [ANotification.AlertBody]);
  
  // Display notification content
  TfgToastFactory.Show(ANotification.AlertBody);
  
  // Handle notification data
  ProcessNotificationData(ANotification);
  
  // Update badge count
  if ANotification.Badge > 0 then
    fgPushNotificationService1.SetBadgeCount(ANotification.Badge);
end;

// Handle authorization changes
procedure TFormMain.fgPushNotificationService1AuthorizationChanged(Sender: TObject; const AAuthorization: TfgPushNotificationAuthorization);
begin
  case AAuthorization of
    TfgPushNotificationAuthorization.Authorized:
    begin
      TfgLog.Info('Push notifications authorized');
      lblNotificationStatus.Text := 'Notifications enabled';
    end;
    TfgPushNotificationAuthorization.Denied:
    begin
      TfgLog.Warning('Push notifications denied by user');
      lblNotificationStatus.Text := 'Notifications disabled';
      ShowNotificationSettings;
    end;
    TfgPushNotificationAuthorization.NotDetermined:
    begin
      TfgLog.Info('Push notification authorization not determined');
      lblNotificationStatus.Text := 'Pending authorization';
    end;
  end;
end;
```

### 14.2 Firebase Project Configuration

**iOS Configuration Steps:**

1. **Create Firebase Project**:
   - Go to Firebase Console (https://console.firebase.google.com)
   - Create new project or select existing one
   - Add iOS app to your Firebase project

2. **Download Configuration File**:
   - Download `GoogleService-Info.plist`
   - Add file to your FGX Native iOS project
   - Ensure it's included in deployment

3. **Enable Push Notifications**:
   - In Firebase Console, go to Project Settings
   - Navigate to Cloud Messaging tab
   - Upload your iOS APNs certificate or key

**Android Configuration Steps:**

1. **Download Configuration File**:
   - Download `google-services.json`
   - Add to your Android project directory

2. **Configure Package Name**:
   - Ensure package name matches your Android app identifier

### 14.3 Advanced Push Notification Handling

```pascal
// Custom notification processor
type
  TNotificationProcessor = class
  private
    FPendingNotifications: TList<TfgPushNotification>;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure ProcessNotification(const ANotification: TfgPushNotification);
    procedure ProcessPendingNotifications;
    procedure HandleDeepLink(const ADeepLink: string);
  end;

constructor TNotificationProcessor.Create;
begin
  inherited;
  FPendingNotifications := TList<TfgPushNotification>.Create;
end;

destructor TNotificationProcessor.Destroy;
begin
  FPendingNotifications.Free;
  inherited;
end;

procedure TNotificationProcessor.ProcessNotification(const ANotification: TfgPushNotification);
var
  notificationType: string;
  deepLink: string;
begin
  // Extract custom data from notification
  notificationType := ANotification.CustomData.Values['type'];
  deepLink := ANotification.CustomData.Values['deep_link'];
  
  TfgLog.Info('Processing notification type: %s', [notificationType]);
  
  case notificationType of
    'message':
    begin
      // Handle message notification
      ShowMessageScreen(ANotification.CustomData.Values['message_id']);
    end;
    'promotion':
    begin
      // Handle promotional notification
      ShowPromotionDialog(ANotification.AlertBody, ANotification.CustomData.Values['promo_code']);
    end;
    'update':
    begin
      // Handle app update notification
      ShowUpdateDialog;
    end;
  end;
  
  // Handle deep links
  if not deepLink.IsEmpty then
    HandleDeepLink(deepLink);
end;

procedure TNotificationProcessor.HandleDeepLink(const ADeepLink: string);
var
  segments: TArray<string>;
begin
  // Parse deep link: myapp://screen/action?params
  segments := ADeepLink.Split(['/']);
  
  if Length(segments) >= 3 then
  begin
    case segments[2] of
      'profile':
        NavigateToProfile(segments[3]);
      'product':
        NavigateToProduct(segments[3]);
      'settings':
        NavigateToSettings;
    end;
  end;
end;

// Background notification handling
procedure TFormMain.fgPushNotificationService1BackgroundNotificationReceived(Sender: TObject; const ANotification: TfgPushNotification);
begin
  // Handle silent notifications (background processing)
  TfgLog.Info('Background notification received');
  
  // Perform background tasks
  if ANotification.CustomData.Values['action'] = 'sync_data' then
  begin
    // Trigger data synchronization
    TTask.Run(procedure
    begin
      SyncDataWithServer;
    end);
  end;
end;
```

### 14.4 Topic Subscriptions and User Segmentation

```pascal
// Topic subscription management
procedure TFormMain.ManageTopicSubscriptions;
begin
  // Subscribe to topics
  fgPushNotificationService1.SubscribeToTopic('news');
  fgPushNotificationService1.SubscribeToTopic('sports');
  fgPushNotificationService1.SubscribeToTopic('technology');
  
  // Unsubscribe from topics
  fgPushNotificationService1.UnsubscribeFromTopic('promotions');
end;

// User-specific topic management
procedure TFormMain.SetupUserTopics(const AUserId: string; const AUserPreferences: TUserPreferences);
begin
  // Subscribe based on user preferences
  if AUserPreferences.NewsEnabled then
    fgPushNotificationService1.SubscribeToTopic('news_' + AUserId);
    
  if AUserPreferences.LocationEnabled then
    fgPushNotificationService1.SubscribeToTopic('location_' + GetUserRegion(AUserId));
    
  if AUserPreferences.CategoryEnabled then
    fgPushNotificationService1.SubscribeToTopic('category_' + AUserPreferences.Category);
end;

// Analytics integration
procedure TFormMain.TrackNotificationEvents(const ANotification: TfgPushNotification);
begin
  // Track notification opened
  TfgAnalyticsService.TrackEvent('notification_opened', [
    TfgAnalyticsParam.Create('notification_id', ANotification.CustomData.Values['id']),
    TfgAnalyticsParam.Create('notification_type', ANotification.CustomData.Values['type']),
    TfgAnalyticsParam.Create('campaign_id', ANotification.CustomData.Values['campaign_id'])
  ]);
end;
```

### 14.5 Local Notifications

```pascal
// Schedule local notifications
procedure TFormMain.ScheduleLocalNotifications;
var
  notification: TfgLocalNotification;
begin
  // Create reminder notification
  notification := TfgLocalNotification.Create;
  try
    notification.AlertBody := 'Don''t forget to check your daily tasks!';
    notification.AlertTitle := 'Daily Reminder';
    notification.FireDate := Now + 1; // Tomorrow
    notification.Badge := 1;
    notification.SoundName := 'default';
    
    // Custom data
    notification.CustomData.Values['type'] := 'reminder';
    notification.CustomData.Values['action'] := 'open_tasks';
    
    // Schedule notification
    fgPushNotificationService1.ScheduleLocalNotification(notification);
  finally
    notification.Free;
  end;
end;

// Cancel local notifications
procedure TFormMain.CancelLocalNotifications;
begin
  // Cancel all local notifications
  fgPushNotificationService1.CancelAllLocalNotifications;
  
  // Or cancel specific notification by identifier
  fgPushNotificationService1.CancelLocalNotification('reminder_1');
end;
```

## 15. Advanced Theme Management (v1.19.0.0)

FGX Native 1.19.0.0 introduces comprehensive theme management with automatic system theme synchronization and accent color customization.

### 15.1 Theme Configuration

```pascal
uses FGX.Application, FGX.Theme;

// Application-level theme setup
procedure TFormMain.SetupApplicationTheme;
begin
  // Configure theme settings
  Application.ThemeSettings.Kind := TfgThemeKind.SystemDefault;
  Application.ThemeSettings.AccentColor := TfgColor.Blue;
  Application.ThemeSettings.AutoSyncWithSystem := True;
  
  // Handle system theme changes
  Application.OnSystemThemeChanged := ApplicationSystemThemeChanged;
end;

// Handle system theme changes
procedure TFormMain.ApplicationSystemThemeChanged(Sender: TObject; const AAppearance: TfgSystemAppearance);
begin
  TfgLog.Info('System theme changed to: %s', [AAppearance.ThemeKind.ToString]);
  
  // Update UI elements that don't automatically adapt
  UpdateCustomControls(AAppearance.ThemeKind);
  
  // Save user preference
  TfgPreferencesService.GetDefaultPreferences.SetValue('last_theme', Ord(AAppearance.ThemeKind));
end;

// Manual theme switching
procedure TFormMain.SetTheme(const AThemeKind: TfgThemeKind);
begin
  Application.ThemeSettings.Kind := AThemeKind;
  
  // Update theme-dependent UI
  case AThemeKind of
    TfgThemeKind.Light:
    begin
      // Light theme customizations
      fgNavigationBar1.Style := 'light';
      lblStatus.TextColor := TfgColor.Black;
    end;
    TfgThemeKind.Dark:
    begin
      // Dark theme customizations
      fgNavigationBar1.Style := 'dark';
      lblStatus.TextColor := TfgColor.White;
    end;
  end;
end;
```

### 15.2 Accent Color Management

```pascal
// Accent color configuration
procedure TFormMain.SetupAccentColors;
begin
  // Set primary accent color
  Application.ThemeSettings.AccentColor := TfgColor.Create($FF2196F3); // Material Blue
  
  // Configure secondary accent color
  Application.ThemeSettings.SecondaryAccentColor := TfgColor.Create($FF4CAF50); // Material Green
  
  // Apply to components
  ApplyAccentColors;
end;

procedure TFormMain.ApplyAccentColors;
var
  accentColor: TfgColor;
begin
  accentColor := Application.ThemeSettings.AccentColor;
  
  // Apply to navigation elements
  fgPageControl1.TintColor := accentColor;
  fgNavigationBar1.TintColor := accentColor;
  
  // Apply to input controls
  fgSwitch1.TintColor := accentColor;
  fgTrackBar1.TintColor := accentColor;
  
  // Apply to buttons
  fgButton1.TintColor := accentColor;
end;

// Color picker for accent selection
procedure TFormMain.ShowAccentColorPicker;
var
  colorOptions: TArray<TfgColor>;
  i: Integer;
begin
  colorOptions := [
    TfgColor.Create($FF2196F3), // Blue
    TfgColor.Create($FF4CAF50), // Green
    TfgColor.Create($FFFF9800), // Orange
    TfgColor.Create($FF9C27B0), // Purple
    TfgColor.Create($FFF44336), // Red
    TfgColor.Create($FF607D8B)  // Blue Grey
  ];
  
  // Create color selection dialog
  var colorDialog := TfgColorSelectionDialog.Create(Self);
  try
    colorDialog.Colors := colorOptions;
    colorDialog.SelectedColor := Application.ThemeSettings.AccentColor;
    colorDialog.OnColorSelected := procedure(const AColor: TfgColor)
    begin
      Application.ThemeSettings.AccentColor := AColor;
      ApplyAccentColors;
      SaveAccentColorPreference(AColor);
    end;
    
    colorDialog.Show;
  finally
    colorDialog.Free;
  end;
end;
```

### 15.3 Custom Theme Resources

```pascal
// Custom theme resource management
type
  TCustomThemeManager = class
  private
    FThemeResources: TDictionary<string, TfgColor>;
    FCurrentTheme: TfgThemeKind;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure LoadThemeResources(const AThemeKind: TfgThemeKind);
    function GetThemeColor(const AColorName: string): TfgColor;
    procedure RegisterCustomColor(const AName: string; const ALightColor, ADarkColor: TfgColor);
  end;

constructor TCustomThemeManager.Create;
begin
  inherited;
  FThemeResources := TDictionary<string, TfgColor>.Create;
  LoadThemeResources(TfgThemeKind.Light);
end;

destructor TCustomThemeManager.Destroy;
begin
  FThemeResources.Free;
  inherited;
end;

procedure TCustomThemeManager.LoadThemeResources(const AThemeKind: TfgThemeKind);
begin
  FCurrentTheme := AThemeKind;
  FThemeResources.Clear;
  
  case AThemeKind of
    TfgThemeKind.Light:
    begin
      FThemeResources.Add('primary', TfgColor.Create($FF2196F3));
      FThemeResources.Add('surface', TfgColor.Create($FFFFFFFF));
      FThemeResources.Add('background', TfgColor.Create($FFFAFAFA));
      FThemeResources.Add('on_surface', TfgColor.Create($FF000000));
      FThemeResources.Add('on_background', TfgColor.Create($FF000000));
    end;
    TfgThemeKind.Dark:
    begin
      FThemeResources.Add('primary', TfgColor.Create($FF64B5F6));
      FThemeResources.Add('surface', TfgColor.Create($FF121212));
      FThemeResources.Add('background', TfgColor.Create($FF000000));
      FThemeResources.Add('on_surface', TfgColor.Create($FFFFFFFF));
      FThemeResources.Add('on_background', TfgColor.Create($FFFFFFFF));
    end;
  end;
end;

function TCustomThemeManager.GetThemeColor(const AColorName: string): TfgColor;
begin
  if not FThemeResources.TryGetValue(AColorName, Result) then
    Result := TfgColor.Gray; // Default fallback
end;

procedure TCustomThemeManager.RegisterCustomColor(const AName: string; const ALightColor, ADarkColor: TfgColor);
begin
  case FCurrentTheme of
    TfgThemeKind.Light: FThemeResources.AddOrSetValue(AName, ALightColor);
    TfgThemeKind.Dark: FThemeResources.AddOrSetValue(AName, ADarkColor);
  end;
end;

// Usage example
procedure TFormMain.ApplyCustomTheme;
var
  themeManager: TCustomThemeManager;
begin
  themeManager := TCustomThemeManager.Create;
  try
    // Register custom colors
    themeManager.RegisterCustomColor('card_background', 
      TfgColor.Create($FFFFFFFF), // Light theme
      TfgColor.Create($FF1E1E1E)  // Dark theme
    );
    
    // Apply theme colors
    fgCardPanel1.Color := themeManager.GetThemeColor('card_background');
    fgLabel1.TextColor := themeManager.GetThemeColor('on_surface');
  finally
    themeManager.Free;
  end;
end;
```

### 15.4 Theme Persistence and User Preferences

```pascal
// Theme preferences management
type
  TThemePreferences = record
    ThemeKind: TfgThemeKind;
    AccentColor: TfgColor;
    AutoSync: Boolean;
    CustomThemeName: string;
  end;

procedure TFormMain.SaveThemePreferences(const APreferences: TThemePreferences);
var
  preferences: TfgPreferences;
begin
  preferences := TfgPreferencesService.GetDefaultPreferences;
  
  preferences.SetValue('theme_kind', Ord(APreferences.ThemeKind));
  preferences.SetValue('accent_color', APreferences.AccentColor.ToHex);
  preferences.SetValue('auto_sync', APreferences.AutoSync);
  preferences.SetValue('custom_theme', APreferences.CustomThemeName);
end;

function TFormMain.LoadThemePreferences: TThemePreferences;
var
  preferences: TfgPreferences;
begin
  preferences := TfgPreferencesService.GetDefaultPreferences;
  
  Result.ThemeKind := TfgThemeKind(preferences.GetValue('theme_kind', Ord(TfgThemeKind.SystemDefault)));
  Result.AccentColor := TfgColor.FromHex(preferences.GetValue('accent_color', '#2196F3'));
  Result.AutoSync := preferences.GetValue('auto_sync', True);
  Result.CustomThemeName := preferences.GetValue('custom_theme', '');
end;

// Apply saved preferences on app start
procedure TFormMain.ApplySavedThemePreferences;
var
  savedPreferences: TThemePreferences;
begin
  savedPreferences := LoadThemePreferences;
  
  Application.ThemeSettings.Kind := savedPreferences.ThemeKind;
  Application.ThemeSettings.AccentColor := savedPreferences.AccentColor;
  Application.ThemeSettings.AutoSyncWithSystem := savedPreferences.AutoSync;
  
  if not savedPreferences.CustomThemeName.IsEmpty then
    LoadCustomTheme(savedPreferences.CustomThemeName);
end;
```

## 16. Authentication Services

FGX Native provides comprehensive authentication services supporting major platforms including Apple ID, Google Sign-In, and Facebook Login.

### 16.1 Authentication Service Manager

All authentication services are managed by `TfgAuthenticationServiceManager` and implement the `IFGXAuthenticationService` interface:

```pascal
uses FGX.AuthenticationService, FGX.AuthenticationService.Manager;

type
  TFormMain = class(TfgForm)
    fgAuthenticationServiceManager1: TfgAuthenticationServiceManager;
    procedure fgAuthenticationServiceManager1AuthenticationCompleted(Sender: TObject; const AService: IFGXAuthenticationService; const AAccount: TfgAccount);
    procedure fgAuthenticationServiceManager1AuthenticationFailed(Sender: TObject; const AService: IFGXAuthenticationService; const AError: string);
  end;

// Basic authentication setup
procedure TFormMain.SetupAuthenticationServices;
begin
  // Configure available authentication services
  fgAuthenticationServiceManager1.AddService(TfgAppleIdAuthenticationClient.Create);
  fgAuthenticationServiceManager1.AddService(TfgGoogleSignInAuthenticationClient.Create);
  fgAuthenticationServiceManager1.AddService(TfgFacebookLoginAuthenticationClient.Create);
end;

// Handle successful authentication
procedure TFormMain.fgAuthenticationServiceManager1AuthenticationCompleted(Sender: TObject; const AService: IFGXAuthenticationService; const AAccount: TfgAccount);
begin
  TfgLog.Info('Authentication successful with %s', [AService.ServiceName]);
  
  // Process account information
  ProcessUserAccount(AAccount);
  
  // Store authentication state
  SaveAuthenticationState(AService.ServiceName, AAccount);
  
  // Navigate to main application
  NavigateToMainScreen;
end;

// Handle authentication failure
procedure TFormMain.fgAuthenticationServiceManager1AuthenticationFailed(Sender: TObject; const AService: IFGXAuthenticationService; const AError: string);
begin
  TfgLog.Error('Authentication failed with %s: %s', [AService.ServiceName, AError]);
  
  // Show error to user
  TfgToastFactory.Show('Authentication failed: ' + AError);
  
  // Reset UI state
  ResetAuthenticationUI;
end;
```

### 16.2 Apple ID Authentication (iOS 13+)

**Requirements**: FGX Native 1.13.4.0+, iOS 13+, Paid Apple Developer Account

**Server Configuration:**
1. Apple Developer Console setup
2. Create App ID with "Sign In with Apple" capability
3. Generate unique Bundle ID

**Client Configuration:**
```pascal
uses FGX.AuthenticationService.AppleId;

type
  TFormMain = class(TfgForm)
    fgAppleIdAuthenticationClient1: TfgAppleIdAuthenticationClient;
    btnSignInWithApple: TfgButton;
    procedure btnSignInWithAppleClick(Sender: TObject);
    procedure fgAppleIdAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
  end;

// Apple ID sign-in implementation
procedure TFormMain.btnSignInWithAppleClick(Sender: TObject);
begin
  if fgAppleIdAuthenticationClient1.IsAvailable then
  begin
    // Request specific user information scopes
    fgAppleIdAuthenticationClient1.RequestedScopes := [
      TfgAppleIdScope.FullName,
      TfgAppleIdScope.Email
    ];
    
    // Initiate authentication
    fgAppleIdAuthenticationClient1.Authenticate;
  end
  else
  begin
    TfgToastFactory.Show('Apple ID Sign-In not available on this device');
  end;
end;

procedure TFormMain.fgAppleIdAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
var
  appleAccount: TfgAppleIdAccount;
begin
  TfgLog.Info('Apple ID authentication successful');
  
  // Cast to Apple-specific account type
  if AAccount is TfgAppleIdAccount then
  begin
    appleAccount := TfgAppleIdAccount(AAccount);
    
    // Access Apple-specific properties
    lblUserId.Text := appleAccount.AccountId.ToString;
    lblUserName.Text := appleAccount.FullName.GivenName + ' ' + appleAccount.FullName.FamilyName;
    lblUserEmail.Text := appleAccount.Email;
    
    // Check authentication state
    case appleAccount.RealUserStatus of
      TfgAppleIdRealUserStatus.LikelyReal: TfgLog.Info('User likely real');
      TfgAppleIdRealUserStatus.Unknown: TfgLog.Info('User status unknown');
      TfgAppleIdRealUserStatus.Unsupported: TfgLog.Info('Real user check unsupported');
    end;
  end;
  
  // Store authentication token securely
  StoreAppleIdCredentials(appleAccount);
end;

// Verify existing Apple ID credentials
procedure TFormMain.VerifyAppleIdCredentials;
var
  storedAccountId: TfgAccountId;
begin
  storedAccountId := LoadStoredAppleIdAccountId;
  
  if not storedAccountId.IsEmpty then
  begin
    fgAppleIdAuthenticationClient1.GetCredentialState(storedAccountId,
      procedure(const ACredentialState: TfgAppleIdCredentialState)
      begin
        case ACredentialState of
          TfgAppleIdCredentialState.Authorized:
          begin
            TfgLog.Info('Apple ID credentials still valid');
            AutoSignInUser(storedAccountId);
          end;
          TfgAppleIdCredentialState.Revoked:
          begin
            TfgLog.Warning('Apple ID credentials revoked');
            ClearStoredCredentials;
            ShowSignInScreen;
          end;
          TfgAppleIdCredentialState.NotFound:
          begin
            TfgLog.Info('Apple ID credentials not found');
            ShowSignInScreen;
          end;
        end;
      end);
  end;
end;
```

### 16.3 Google Sign-In Authentication

**Server Configuration:**
1. Google Developer Console setup
2. Create OAuth 2.0 client for Android
3. Configure package name and SHA-1 certificate fingerprint

**Certificate Fingerprint Retrieval:**
```bash
keytool -list -v -alias androiddebugkey -keystore "C:\Users\%USERNAME%\AppData\Roaming\Embarcadero\BDS\22.0\debug.keystore"
```

**Client Configuration:**
```pascal
uses FGX.AuthenticationService.GoogleSignIn;

type
  TFormMain = class(TfgForm)
    fgGoogleSignInAuthenticationClient1: TfgGoogleSignInAuthenticationClient;
    btnSignInWithGoogle: TfgButton;
    procedure btnSignInWithGoogleClick(Sender: TObject);
    procedure fgGoogleSignInAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
  end;

// Google Sign-In implementation
procedure TFormMain.btnSignInWithGoogleClick(Sender: TObject);
begin
  // Configure requested scopes
  fgGoogleSignInAuthenticationClient1.RequestedScopes := [
    'profile',
    'email',
    'https://www.googleapis.com/auth/drive.readonly'
  ];
  
  // Set hosted domain (optional, for G Suite accounts)
  fgGoogleSignInAuthenticationClient1.HostedDomain := 'example.com';
  
  // Initiate authentication
  fgGoogleSignInAuthenticationClient1.Authenticate;
end;

procedure TFormMain.fgGoogleSignInAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
var
  googleAccount: TfgGoogleAccount;
begin
  TfgLog.Info('Google Sign-In authentication successful');
  
  if AAccount is TfgGoogleAccount then
  begin
    googleAccount := TfgGoogleAccount(AAccount);
    
    // Access Google-specific properties
    lblUserId.Text := googleAccount.UserId;
    lblUserName.Text := googleAccount.DisplayName;
    lblUserEmail.Text := googleAccount.Email;
    
    // Load profile image
    if not googleAccount.PhotoUrl.IsEmpty then
      LoadProfileImage(googleAccount.PhotoUrl);
    
    // Access authentication tokens
    var idToken := googleAccount.IdToken;
    var accessToken := googleAccount.AccessToken;
    
    // Send tokens to your backend for verification
    VerifyGoogleTokens(idToken, accessToken);
  end;
end;

// Silent sign-in for returning users
procedure TFormMain.AttemptSilentGoogleSignIn;
begin
  fgGoogleSignInAuthenticationClient1.SilentSignIn(
    procedure(const AAccount: TfgAccount)
    begin
      // Silent sign-in successful
      TfgLog.Info('Silent Google Sign-In successful');
      ProcessUserAccount(AAccount);
    end,
    procedure(const AError: string)
    begin
      // Silent sign-in failed, show manual sign-in options
      TfgLog.Info('Silent Google Sign-In failed: %s', [AError]);
      ShowSignInOptions;
    end);
end;

// Sign out from Google
procedure TFormMain.SignOutFromGoogle;
begin
  fgGoogleSignInAuthenticationClient1.SignOut(
    procedure
    begin
      TfgLog.Info('Google Sign-Out successful');
      ClearUserSession;
      NavigateToSignInScreen;
    end);
end;
```

### 16.4 Facebook Login Authentication

**Server Configuration:**
1. Facebook Developer Console setup
2. Create Facebook App
3. Configure iOS platform settings
4. Obtain App ID and Client Token

**Client Configuration:**
```pascal
uses FGX.AuthenticationService.FacebookLogin;

type
  TFormMain = class(TfgForm)
    fgFacebookLoginAuthenticationClient1: TfgFacebookLoginAuthenticationClient;
    btnSignInWithFacebook: TfgButton;
    procedure btnSignInWithFacebookClick(Sender: TObject);
    procedure fgFacebookLoginAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
  end;

// Facebook Login implementation
procedure TFormMain.btnSignInWithFacebookClick(Sender: TObject);
begin
  // Configure requested permissions
  fgFacebookLoginAuthenticationClient1.RequestedPermissions := [
    'public_profile',
    'email',
    'user_friends'
  ];
  
  // Set login behavior
  fgFacebookLoginAuthenticationClient1.LoginBehavior := TfgFacebookLoginBehavior.SystemAccount;
  
  // Initiate authentication
  fgFacebookLoginAuthenticationClient1.Authenticate;
end;

procedure TFormMain.fgFacebookLoginAuthenticationClient1AuthenticationCompleted(Sender: TObject; const AAccount: TfgAccount);
var
  facebookAccount: TfgFacebookAccount;
begin
  TfgLog.Info('Facebook Login authentication successful');
  
  if AAccount is TfgFacebookAccount then
  begin
    facebookAccount := TfgFacebookAccount(AAccount);
    
    // Access Facebook-specific properties
    lblUserId.Text := facebookAccount.UserId;
    lblUserName.Text := facebookAccount.Name;
    lblUserEmail.Text := facebookAccount.Email;
    
    // Access granted permissions
    var grantedPermissions := facebookAccount.GrantedPermissions;
    var declinedPermissions := facebookAccount.DeclinedPermissions;
    
    // Load profile picture
    if not facebookAccount.PictureUrl.IsEmpty then
      LoadProfileImage(facebookAccount.PictureUrl);
    
    // Access authentication token
    var accessToken := facebookAccount.AccessToken;
    SendFacebookTokenToBackend(accessToken);
  end;
end;

// Request additional permissions
procedure TFormMain.RequestAdditionalFacebookPermissions;
begin
  fgFacebookLoginAuthenticationClient1.RequestAdditionalPermissions([
    'user_location',
    'user_birthday'
  ],
  procedure(const AGrantedPermissions, ADeclinedPermissions: TArray<string>)
  begin
    TfgLog.Info('Additional permissions granted: %s', [string.Join(',', AGrantedPermissions)]);
    TfgLog.Info('Additional permissions declined: %s', [string.Join(',', ADeclinedPermissions)]);
    
    // Update UI based on new permissions
    UpdateUIBasedOnPermissions(AGrantedPermissions);
  end);
end;

// Facebook Graph API calls
procedure TFormMain.MakeFacebookGraphAPICall;
var
  accessToken: string;
begin
  accessToken := GetCurrentFacebookAccessToken;
  
  // Example: Get user's friends
  TfgHttpClient.Get('https://graph.facebook.com/me/friends?access_token=' + accessToken,
    procedure(const AResponse: TfgHttpResponse)
    begin
      if AResponse.StatusCode = 200 then
      begin
        ProcessFriendsList(AResponse.Content);
      end
      else
      begin
        TfgLog.Error('Facebook Graph API error: %d', [AResponse.StatusCode]);
      end;
    end);
end;
```

### 16.5 Multi-Provider Authentication UI

```pascal
// Universal authentication screen
type
  TAuthenticationScreen = class(TfgForm)
  private
    FAuthenticationManager: TfgAuthenticationServiceManager;
    FAvailableServices: TList<IFGXAuthenticationService>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    procedure SetupAuthenticationOptions;
    procedure ShowAuthenticationProvider(const AProvider: string);
  end;

constructor TAuthenticationScreen.Create(AOwner: TComponent);
begin
  inherited;
  
  FAuthenticationManager := TfgAuthenticationServiceManager.Create(Self);
  FAvailableServices := TList<IFGXAuthenticationService>.Create;
  
  SetupAuthenticationOptions;
end;

procedure TAuthenticationScreen.SetupAuthenticationOptions;
begin
  // Add available authentication services
  {$IFDEF IOS}
  if TOSVersion.Check(13) then
  begin
    var appleIdService := TfgAppleIdAuthenticationClient.Create;
    FAvailableServices.Add(appleIdService);
    FAuthenticationManager.AddService(appleIdService);
    
    // Show Apple ID button (required to be first on iOS)
    btnAppleId.Visible := True;
    btnAppleId.BringToFront;
  end;
  {$ENDIF}
  
  {$IFDEF ANDROID}
  var googleService := TfgGoogleSignInAuthenticationClient.Create;
  FAvailableServices.Add(googleService);
  FAuthenticationManager.AddService(googleService);
  btnGoogleSignIn.Visible := True;
  {$ENDIF}
  
  // Facebook Login available on both platforms
  var facebookService := TfgFacebookLoginAuthenticationClient.Create;
  FAvailableServices.Add(facebookService);
  FAuthenticationManager.AddService(facebookService);
  btnFacebookLogin.Visible := True;
  
  // Setup manager events
  FAuthenticationManager.OnAuthenticationCompleted := AuthenticationCompleted;
  FAuthenticationManager.OnAuthenticationFailed := AuthenticationFailed;
end;

procedure TAuthenticationScreen.ShowAuthenticationProvider(const AProvider: string);
var
  service: IFGXAuthenticationService;
begin
  // Find and authenticate with specific provider
  for service in FAvailableServices do
  begin
    if service.ServiceName = AProvider then
    begin
      service.Authenticate;
      Break;
    end;
  end;
end;

// Unified authentication result handling
procedure TAuthenticationScreen.AuthenticationCompleted(Sender: TObject; const AService: IFGXAuthenticationService; const AAccount: TfgAccount);
begin
  // Log authentication success
  TfgLog.Info('Authentication successful with %s for user %s', [AService.ServiceName, AAccount.DisplayName]);
  
  // Store authentication state
  var authState := TAuthenticationState.Create;
  authState.Provider := AService.ServiceName;
  authState.AccountId := AAccount.AccountId.ToString;
  authState.DisplayName := AAccount.DisplayName;
  authState.Email := AAccount.Email;
  authState.AuthenticatedAt := Now;
  
  SaveAuthenticationState(authState);
  
  // Navigate to main application
  TfgNavigationController.Instance.NavigateToMain;
end;

procedure TAuthenticationScreen.AuthenticationFailed(Sender: TObject; const AService: IFGXAuthenticationService; const AError: string);
begin
  TfgLog.Error('Authentication failed with %s: %s', [AService.ServiceName, AError]);
  
  // Show appropriate error message
  case AService.ServiceName of
    'AppleId': TfgToastFactory.Show('Apple ID sign-in failed');
    'GoogleSignIn': TfgToastFactory.Show('Google sign-in failed');
    'FacebookLogin': TfgToastFactory.Show('Facebook login failed');
  end;
  
  // Reset UI state
  HideLoadingIndicator;
  EnableAuthenticationButtons;
end;
```

### 16.6 Authentication State Management

```pascal
// Authentication state persistence
type
  TAuthenticationState = class
  public
    Provider: string;
    AccountId: string;
    DisplayName: string;
    Email: string;
    AccessToken: string;
    RefreshToken: string;
    ExpiresAt: TDateTime;
    AuthenticatedAt: TDateTime;
  end;

procedure TFormMain.SaveAuthenticationState(const AState: TAuthenticationState);
var
  prefs: TfgPreferences;
  stateJson: string;
begin
  prefs := TfgPreferencesService.GetDefaultPreferences;
  
  // Serialize state to JSON (simplified)
  stateJson := Format('{"provider":"%s","accountId":"%s","displayName":"%s","email":"%s","authenticatedAt":"%s"}',
    [AState.Provider, AState.AccountId, AState.DisplayName, AState.Email, DateTimeToStr(AState.AuthenticatedAt)]);
  
  prefs.SetValue('auth_state', stateJson);
  prefs.SetValue('auth_provider', AState.Provider);
  prefs.SetValue('is_authenticated', True);
end;

function TFormMain.LoadAuthenticationState: TAuthenticationState;
var
  prefs: TfgPreferences;
  stateJson: string;
begin
  prefs := TfgPreferencesService.GetDefaultPreferences;
  
  Result := TAuthenticationState.Create;
  
  if prefs.GetValue('is_authenticated', False) then
  begin
    stateJson := prefs.GetValue('auth_state', '');
    // Deserialize from JSON (simplified)
    Result.Provider := prefs.GetValue('auth_provider', '');
  end;
end;

procedure TFormMain.ClearAuthenticationState;
var
  prefs: TfgPreferences;
begin
  prefs := TfgPreferencesService.GetDefaultPreferences;
  
  prefs.SetValue('is_authenticated', False);
  prefs.SetValue('auth_state', '');
  prefs.SetValue('auth_provider', '');
end;

// Auto sign-in on app launch
procedure TFormMain.AttemptAutoSignIn;
var
  authState: TAuthenticationState;
begin
  authState := LoadAuthenticationState;
  
  if not authState.Provider.IsEmpty then
  begin
    case authState.Provider of
      'AppleId': VerifyAppleIdCredentials;
      'GoogleSignIn': AttemptSilentGoogleSignIn;
      'FacebookLogin': VerifyFacebookTokenValidity;
    end;
  end
  else
  begin
    // No stored authentication, show sign-in screen
    ShowAuthenticationScreen;
  end;
end;
```

## 17. Advanced Build and Deployment

FGX Native provides advanced build system features and deployment optimizations for professional development workflows.

### 17.1 Automatic Classes.dex Generation (Android)

**Available since FGX Native 1.1.6.0**

The automatic classes.dex generation system simplifies Android library integration and third-party JAR management:

#### Managing Android Libraries

Access the library management dialog via:
1. **Main Menu**: `Project → FGX Android Libraries`
2. **Project Panel**: `Target Platforms → Android (32/64 bits) → Libraries → Setup Android Libraries`

#### Library Integration Workflow

```pascal
// Example: Adding custom JAR libraries to your project
procedure TFormMain.ConfigureAndroidLibraries;
begin
  // The build system automatically generates classes.dex from:
  
  // 1. Default FGX Native libraries (managed automatically)
  // 2. User-added JAR files in "All modules" section
  // 3. Third-party libraries in "User" section
  
  TfgLog.Info('Android libraries will be automatically integrated during build');
end;

// Using third-party Android libraries in code
procedure TFormMain.UseThirdPartyLibrary;
begin
  // After adding JAR to library manager, use Java2Delphi to generate headers
  // Example: Using a custom analytics library
  
  {$IFDEF ANDROID}
  var customAnalytics := TJCustomAnalyticsManager.JavaClass.getInstance;
  customAnalytics.trackEvent('app_started');
  {$ENDIF}
end;
```

#### Library Categories

1. **Default Libraries**: Core FGX Native and Android framework libraries
2. **All Modules**: Project-wide libraries available to all modules
3. **User Libraries**: Custom third-party JAR files
4. **Automatic Integration**: Libraries added via Maven coordinates

#### Best Practices for Library Management

```pascal
// Conditional library usage based on platform
{$IFDEF ANDROID}
uses
  // Android-specific units
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.GraphicsContentViewText,
  // Custom library units (generated via Java2Delphi)
  CustomLibrary.Analytics,
  CustomLibrary.Utils;
{$ENDIF}

type
  TLibraryManager = class
  private
    {$IFDEF ANDROID}
    FAnalyticsInstance: JCustomAnalytics;
    {$ENDIF}
  public
    procedure InitializeLibraries;
    procedure TrackEvent(const AEventName: string; const AParameters: TArray<string> = []);
  end;

procedure TLibraryManager.InitializeLibraries;
begin
  {$IFDEF ANDROID}
  try
    FAnalyticsInstance := TJCustomAnalytics.JavaClass.getInstance;
    FAnalyticsInstance.initialize(StringToJString('your-api-key'));
    TfgLog.Info('Third-party analytics library initialized');
  except
    on E: Exception do
      TfgLog.Error('Failed to initialize analytics library: %s', [E.Message]);
  end;
  {$ENDIF}
end;

procedure TLibraryManager.TrackEvent(const AEventName: string; const AParameters: TArray<string>);
begin
  {$IFDEF ANDROID}
  if Assigned(FAnalyticsInstance) then
  begin
    // Convert parameters to Java format
    var javaParams := TJavaObjectArray<JString>.Create(Length(AParameters));
    for var i := 0 to High(AParameters) do
      javaParams.Items[i] := StringToJString(AParameters[i]);
      
    FAnalyticsInstance.trackEvent(StringToJString(AEventName), javaParams);
  end;
  {$ENDIF}
  
  // Cross-platform logging
  TfgLog.Info('Event tracked: %s', [AEventName]);
end;
```

### 17.2 Maven Library Integration

FGX Native supports automatic Maven dependency resolution using Group:Artifact:Version format:

```pascal
// Example Maven library integration
// Add to Android Libraries dialog: "com.google.android.material:material:1.9.0"

{$IFDEF ANDROID}
uses
  // Generated headers for Material Design Components
  Androidapi.JNI.Material,
  Androidapi.JNI.MaterialComponents;
{$ENDIF}

procedure TFormMain.UseMaterialDesignComponents;
begin
  {$IFDEF ANDROID}
  // Example: Using Material Design Bottom Sheet
  var bottomSheetBehavior := TJBottomSheetBehavior.JavaClass.from(materialBottomSheet);
  bottomSheetBehavior.setState(TJBottomSheetBehavior.JavaClass.STATE_EXPANDED);
  {$ENDIF}
end;
```

### 17.3 Java2Delphi Header Generation

Convert Java libraries to Delphi headers for seamless integration:

#### Workflow Steps:
1. **Add JAR to project** via Android Libraries dialog
2. **Generate headers** using Java2Delphi utility
3. **Include generated units** in your project
4. **Use Java classes** via JNI bridge

#### Example Header Usage:

```pascal
// Generated header example: CustomLibrary.pas
unit CustomLibrary;

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Embarcadero;

type
  JCustomLibraryClass = interface;
  JCustomLibrary = interface;

  JCustomLibraryClass = interface(JObjectClass)
    ['{12345678-1234-1234-1234-123456789ABC}']
    function getInstance: JCustomLibrary; cdecl;
  end;

  [JavaSignature('com/example/CustomLibrary')]
  JCustomLibrary = interface(JObject)
    ['{12345678-1234-1234-1234-123456789DEF}']
    procedure initialize(apiKey: JString); cdecl;
    procedure trackEvent(eventName: JString; parameters: JString); cdecl;
  end;

  TJCustomLibrary = class(TJavaGenericImport<JCustomLibraryClass, JCustomLibrary>)
  end;

implementation

end.

// Usage in your application:
procedure TFormMain.UseGeneratedHeader;
var
  customLib: JCustomLibrary;
begin
  customLib := TJCustomLibrary.JavaClass.getInstance;
  customLib.initialize(StringToJString('api-key-here'));
  customLib.trackEvent(StringToJString('user_action'), StringToJString('button_click'));
end;
```

### 17.4 Build Optimization Strategies

#### Conditional Compilation for Performance

```pascal
// Optimize builds with conditional compilation
{$IFDEF RELEASE}
  // Release-specific optimizations
  {$DEFINE DISABLE_DEBUG_LOGGING}
  {$DEFINE ENABLE_CRASH_REPORTING}
{$ELSE}
  // Debug-specific features
  {$DEFINE ENABLE_DEBUG_UI}
  {$DEFINE VERBOSE_LOGGING}
{$ENDIF}

procedure TFormMain.OptimizedInitialization;
begin
  {$IFDEF ENABLE_CRASH_REPORTING}
  SetupCrashReporting;
  {$ENDIF}
  
  {$IFDEF ENABLE_DEBUG_UI}
  SetupDebugOverlay;
  {$ENDIF}
  
  {$IFNDEF DISABLE_DEBUG_LOGGING}
  TfgLog.Debug('Application initialized in debug mode');
  {$ENDIF}
end;
```

#### Asset Optimization

```pascal
// Asset loading optimization
type
  TAssetOptimizer = class
  public
    class procedure OptimizeImageAssets;
    class procedure PreloadCriticalAssets;
    class procedure CleanupUnusedAssets;
  end;

class procedure TAssetOptimizer.OptimizeImageAssets;
begin
  // Load appropriate resolution assets based on device
  var screenDensity := TfgScreen.GetScreenDensity;
  var assetSuffix: string;
  
  case screenDensity of
    TfgScreenDensity.LDPI: assetSuffix := '_ldpi';
    TfgScreenDensity.MDPI: assetSuffix := '_mdpi';
    TfgScreenDensity.HDPI: assetSuffix := '_hdpi';
    TfgScreenDensity.XHDPI: assetSuffix := '_xhdpi';
    TfgScreenDensity.XXHDPI: assetSuffix := '_xxhdpi';
    TfgScreenDensity.XXXHDPI: assetSuffix := '_xxxhdpi';
  else
    assetSuffix := '_hdpi'; // Default fallback
  end;
  
  // Load optimized assets
  TfgAssetsManager.Current.LoadImageSet('main_background' + assetSuffix);
end;

class procedure TAssetOptimizer.PreloadCriticalAssets;
begin
  // Preload assets needed for app startup
  TfgAssetsManager.Current.PreloadAsset(R.Bitmap.SPLASH_LOGO);
  TfgAssetsManager.Current.PreloadAsset(R.Bitmap.APP_ICON);
  TfgAssetsManager.Current.PreloadAsset(R.Color.PRIMARY_COLOR);
end;
```

### 17.5 Deployment Pipeline Integration

#### Automated Build Scripts

```pascal
// Build automation helper
type
  TBuildAutomation = class
  public
    class procedure RunPreBuildChecks;
    class procedure OptimizeForRelease;
    class procedure GenerateReleaseNotes;
    class procedure ValidateAssets;
  end;

class procedure TBuildAutomation.RunPreBuildChecks;
begin
  // Validate project configuration
  if not TFile.Exists(TPath.Combine(GetProjectPath, 'Assets.Consts.pas')) then
    raise Exception.Create('Assets.Consts.pas not found - run Asset Manager first');
    
  // Check for required certificates (iOS)
  {$IFDEF IOS}
  if not HasValidCodesignCertificate then
    raise Exception.Create('Valid code signing certificate required for iOS build');
  {$ENDIF}
  
  // Validate Android configuration
  {$IFDEF ANDROID}
  if not HasValidKeystore then
    TfgLog.Warning('No keystore configured - using debug certificate');
  {$ENDIF}
end;

class procedure TBuildAutomation.OptimizeForRelease;
begin
  // Enable compiler optimizations
  SetCompilerDefine('RELEASE');
  SetCompilerDefine('OPTIMIZE');
  
  // Disable debug features
  UnsetCompilerDefine('DEBUG');
  UnsetCompilerDefine('ENABLE_DEBUG_UI');
  
  // Configure logging level
  TfgLog.MinimumLevel := TfgLogLevel.Warning;
end;
```

#### Continuous Integration Support

```pascal
// CI/CD integration helpers
procedure TFormMain.ConfigureForCI;
begin
  // Setup headless build environment
  if IsRunningInCI then
  begin
    // Disable UI-dependent features
    Application.ShowMainForm := False;
    
    // Configure automated testing
    SetupAutomatedTesting;
    
    // Enable crash reporting for CI builds
    SetupCrashReporting;
  end;
end;

function TFormMain.IsRunningInCI: Boolean;
begin
  // Check for CI environment variables
  Result := (GetEnvironmentVariable('CI') = 'true') or
            (GetEnvironmentVariable('CONTINUOUS_INTEGRATION') = 'true') or
            (GetEnvironmentVariable('GITHUB_ACTIONS') = 'true');
end;

procedure TFormMain.SetupAutomatedTesting;
begin
  // Configure for automated UI testing
  Application.OnException := HandleCIException;
  
  // Setup test data
  LoadTestData;
  
  // Enable test mode features
  EnableTestMode;
end;
```

### 5.5 Pull to Refresh

**Sample Location**: `CollectionView - Pull to refresh`

Implement pull-to-refresh functionality:

```pascal
procedure TFormMain.SetupPullToRefresh;
begin
  collectionView.PullToRefresh.Enabled := True;
  collectionView.OnPullToRefresh := CollectionViewPullToRefresh;
end;

procedure TFormMain.CollectionViewPullToRefresh(Sender: TObject);
begin
  // Show loading indicator
  activityIndicator.IsAnimating := True;
  
  // Refresh data
  TTask.Run(
    procedure
    begin
      // Simulate network request
      Sleep(2000);
      
      // Update data
      FDataProvider.RefreshData;
      
      // Update UI
      TThread.Synchronize(nil,
        procedure
        begin
          collectionView.RefreshData;
          collectionView.PullToRefresh.EndRefresh;
          activityIndicator.IsAnimating := False;
        end);
    end);
end;
```

### 5.6 Item Filtering

**Sample Location**: `CollectionView - Filtering Items`

Real-time search and filtering:

```pascal
type
  TFormMain = class(TfgForm)
    editSearch: TfgEdit;
    procedure editSearchChangeText(Sender: TObject);
  private
    FFilterText: string;
    FFilteredData: TList<TDataItem>;
    procedure ApplyFilter(const AFilter: string);
  end;

procedure TFormMain.editSearchChangeText(Sender: TObject);
begin
  FFilterText := editSearch.Text.ToLower;
  ApplyFilter(FFilterText);
end;

procedure TFormMain.ApplyFilter(const AFilter: string);
var
  item: TDataItem;
begin
  FFilteredData.Clear;
  
  if AFilter.IsEmpty then
    FFilteredData.AddRange(FAllData)
  else
  begin
    for item in FAllData do
      if item.Title.ToLower.Contains(AFilter) or 
         item.Description.ToLower.Contains(AFilter) then
        FFilteredData.Add(item);
  end;
  
  collectionView.RefreshData;
end;

function TFormMain.GetFilteredItemCount: Integer;
begin
  Result := FFilteredData.Count;
end;
```

---

## 6. Form Management

### 6.1 Safe Area Padding

**Sample Location**: `Form - Safe area padding`

Handle device safe areas (notches, home indicators):

```pascal
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Apply safe area padding
  ApplySafeAreaPadding([TfgSafeAreaSide.Top, TfgSafeAreaSide.Bottom]);
end;

procedure TFormMain.ApplySafeAreaPadding(const ASides: TfgSafeAreaSides);
var
  safeArea: TRectF;
begin
  safeArea := GetSafeAreaInsets;
  
  if TfgSafeAreaSide.Top in ASides then
    pnlContent.Margins.Top := safeArea.Top;
    
  if TfgSafeAreaSide.Bottom in ASides then
    pnlContent.Margins.Bottom := safeArea.Bottom;
    
  if TfgSafeAreaSide.Left in ASides then
    pnlContent.Margins.Left := safeArea.Left;
    
  if TfgSafeAreaSide.Right in ASides then
    pnlContent.Margins.Right := safeArea.Right;
end;
```

### 6.2 System Status Bar

**Sample Location**: `Form - System status bar`

Control the appearance of the system status bar:

```pascal
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Configure status bar
  SystemStatusBar.BackgroundColor := TAlphaColors.Darkblue;
  SystemStatusBar.LightContent := True;
  SystemStatusBar.Visible := True;
end;

procedure TFormMain.ToggleStatusBarStyle;
begin
  if SystemStatusBar.LightContent then
  begin
    SystemStatusBar.LightContent := False;
    SystemStatusBar.BackgroundColor := TAlphaColors.White;
  end
  else
  begin
    SystemStatusBar.LightContent := True;
    SystemStatusBar.BackgroundColor := TAlphaColors.Black;
  end;
end;
```

### 6.3 Dialog Forms

**Sample Location**: `Form - Dialog`

Create custom dialog forms:

```pascal
// Dialog.Standard.pas
type
  TDialogStandard = class(TfgForm)
    lblTitle: TfgLabel;
    lblMessage: TfgLabel;
    btnOK: TfgButton;
    btnCancel: TfgButton;
    procedure btnOKTap(Sender: TObject);
    procedure btnCancelTap(Sender: TObject);
  private
    FResult: TModalResult;
  public
    class function ShowDialog(const ATitle, AMessage: string): TModalResult;
    property ModalResult: TModalResult read FResult;
  end;

class function TDialogStandard.ShowDialog(const ATitle, AMessage: string): TModalResult;
var
  dialog: TDialogStandard;
begin
  dialog := TDialogStandard.Create(nil);
  try
    dialog.lblTitle.Text := ATitle;
    dialog.lblMessage.Text := AMessage;
    dialog.ShowModal;
    Result := dialog.ModalResult;
  finally
    dialog.Free;
  end;
end;
```

### 6.4 Frame-Based Architecture

**Sample Location**: `Form - Frames`

Use frames for reusable UI components:

```pascal
// Frame.Child1.pas
type
  TFrameChild1 = class(TfgFrame)
    lblTitle: TfgLabel;
    btnAction: TfgButton;
    procedure btnActionTap(Sender: TObject);
  public
    procedure SetData(const AData: TFrameData);
  end;

// Form.Main.pas
procedure TFormMain.SwitchToFrame(AFrameClass: TfgFrameClass);
var
  frame: TfgFrame;
begin
  // Clear existing frame
  if Assigned(FCurrentFrame) then
    FCurrentFrame.Free;
    
  // Create new frame
  frame := AFrameClass.Create(pnlFrameContainer);
  frame.Parent := pnlFrameContainer;
  frame.Align := TfgAlign.Client;
  
  FCurrentFrame := frame;
end;
```

---

## 7. Input Controls

### 7.1 ComboBox with Custom Items

**Sample Location**: `ComboBox - Flags`

ComboBox with images and custom styling:

```pascal
type
  TCountryItem = record
    Name: string;
    Code: string;
    FlagAsset: string;
  end;

procedure TFormMain.SetupCountryComboBox;
var
  countries: TArray<TCountryItem>;
begin
  countries := [
    TCountryItem.Create('United States', 'US', 'flag_us.png'),
    TCountryItem.Create('United Kingdom', 'UK', 'flag_uk.png'),
    TCountryItem.Create('Germany', 'DE', 'flag_de.png')
  ];
  
  comboCountries.OnBindItem := ComboCountriesBindItem;
  comboCountries.ItemsCount := Length(countries);
end;

procedure TFormMain.ComboCountriesBindItem(Sender: TObject; const AIndex: Integer; 
  const AItem: TfgItemWrapper);
var
  imgFlag: TfgImage;
  lblName: TfgLabel;
  country: TCountryItem;
begin
  country := FCountries[AIndex];
  
  imgFlag := AItem.GetControlByLookupName<TfgImage>('flag');
  lblName := AItem.GetControlByLookupName<TfgLabel>('name');
  
  imgFlag.LoadFromAsset(country.FlagAsset);
  lblName.Text := country.Name;
end;
```

### 7.2 Date and Time Pickers

**Sample Locations**: 
- `DatePicker - Base`
- `DateTimeEdit - Edit form`
- `Pickers - Date`

Date selection components:

```pascal
procedure TFormMain.SetupDatePickers;
begin
  // Date picker
  datePicker.Date := Now;
  datePicker.MinDate := EncodeDate(1900, 1, 1);
  datePicker.MaxDate := EncodeDate(2100, 12, 31);
  datePicker.OnDateChanged := DatePickerChanged;
  
  // DateTime edit
  dtEdit.DateTime := Now;
  dtEdit.ShowTime := True;
  dtEdit.ShowDate := True;
end;

procedure TFormMain.DatePickerChanged(Sender: TObject);
begin
  lblSelectedDate.Text := FormatDateTime('dddd, mmmm d, yyyy', datePicker.Date);
end;

procedure TFormMain.ShowDatePickerDialog;
begin
  TfgPickerService.ShowDatePicker(
    Now,
    procedure(const ASelectedDate: TDateTime; const ACancelled: Boolean)
    begin
      if not ACancelled then
      begin
        dtEdit.DateTime := ASelectedDate;
        lblResult.Text := DateTimeToStr(ASelectedDate);
      end;
    end);
end;
```

### 7.3 Text Input and Memos

**Sample Location**: `Memo - Autosize`

Auto-sizing text input:

```pascal
procedure TFormMain.SetupMemo;
begin
  memo.AutoSize := TfgAutoSize.Height;
  memo.MaxLines := 5;
  memo.OnChangeText := MemoChangeText;
  memo.Placeholder := 'Enter your text here...';
end;

procedure TFormMain.MemoChangeText(Sender: TObject);
begin
  lblCharCount.Text := Format('%d characters', [memo.Text.Length]);
  
  // Auto-expand memo height
  if memo.Lines.Count > 3 then
    memo.MaxLines := memo.Lines.Count + 1;
end;
```

---

## 8. Graphics and Canvas

### 8.1 Custom Drawing

**Sample Location**: `Canvas - Draw simple text`

Custom graphics rendering:

```pascal
type
  TFormMain = class(TfgForm)
    paintBox: TfgPaintBox;
    procedure paintBoxPaint(Sender: TObject; const ACanvas: TfgCanvas; const ARect: TRectF);
  end;

procedure TFormMain.paintBoxPaint(Sender: TObject; const ACanvas: TfgCanvas; const ARect: TRectF);
var
  textRect: TRectF;
  font: TfgFont;
begin
  // Clear background
  ACanvas.Fill.Color := TAlphaColors.White;
  ACanvas.FillRect(ARect);
  
  // Setup font
  font := TfgFont.Create;
  try
    font.Family := 'Arial';
    font.Size := 24;
    font.Style := [TFontStyle.fsBold];
    
    // Draw text
    ACanvas.Font.Assign(font);
    ACanvas.Fill.Color := TAlphaColors.Blue;
    
    textRect := TRectF.Create(10, 10, ARect.Width - 10, ARect.Height - 10);
    ACanvas.FillText(textRect, 'Hello FGX Native!', False, 1, [], TfgTextAlign.Center, TfgTextAlign.Center);
  finally
    font.Free;
  end;
end;
```

### 8.2 Bitmap Operations

**Sample Location**: `Canvas - BitmapData`

Working with bitmap data:

```pascal
procedure TFormMain.ProcessBitmap;
var
  bitmap: TfgBitmap;
  bitmapData: TfgBitmapData;
  x, y: Integer;
  color: TAlphaColor;
begin
  bitmap := TfgBitmap.Create(200, 200);
  try
    // Map bitmap for pixel access
    if bitmap.Map(TfgMapAccess.Write, bitmapData) then
    try
      // Modify pixels
      for y := 0 to bitmap.Height - 1 do
        for x := 0 to bitmap.Width - 1 do
        begin
          // Create gradient effect
          color := TAlphaColorRec.ColorToRGB(
            MakeColor(x / bitmap.Width, y / bitmap.Height, 0.5));
          bitmapData.SetPixel(x, y, color);
        end;
    finally
      bitmap.Unmap(bitmapData);
    end;
    
    // Display result
    imgResult.Bitmap.Assign(bitmap);
  finally
    bitmap.Free;
  end;
end;
```

### 8.3 Control Screenshots

**Sample Location**: `Canvas - Control screenshot`

Capture control as image:

```pascal
procedure TFormMain.CaptureControlScreenshot;
var
  bitmap: TfgBitmap;
begin
  bitmap := pnlContent.CreateScreenshot;
  try
    // Save to photo library
    TfgPhotoLibraryService.SaveImageToPhotos(bitmap,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if ASuccess then
          TfgDialogs.ShowMessage('Screenshot saved to photos')
        else
          TfgDialogs.ShowMessage('Error: ' + AError);
      end);
  finally
    bitmap.Free;
  end;
end;
```

---

## 9. Animations and Effects

### 9.1 Fade Animations

**Sample Location**: `Animation - Fading controls`

Smooth fade in/out effects:

```pascal
procedure TFormMain.FadeControls;
begin
  // Fade out first image
  TfgAnimationHelper.FadeOut(img1, 0.5,
    procedure
    begin
      // Fade in second image when first completes
      TfgAnimationHelper.FadeIn(img2, 0.5);
    end);
end;

procedure TFormMain.CrossFadeImages;
begin
  // Simultaneous fade
  TfgAnimationHelper.FadeOut(imgCurrent, 0.3);
  TfgAnimationHelper.FadeIn(imgNext, 0.3);
end;
```

### 9.2 Form Transitions

**Sample Location**: `Animation - Switching Forms`

Animated form transitions:

```pascal
procedure TFormMain.SwitchToSecondForm;
begin
  TfgAnimationHelper.SlideTransition(Self, FormSecond, TfgSlideDirection.Left, 0.4,
    procedure
    begin
      // Transition complete
      FormSecond.Show;
      Self.Hide;
    end);
end;

procedure TFormMain.ShowModalWithAnimation;
begin
  TfgAnimationHelper.ShowModal(FormModal, TfgModalAnimation.SlideUp, 0.3);
end;
```

### 9.3 Animation Templates

**Sample Location**: `Animation - Templates`

Pre-built animation patterns:

```pascal
procedure TFormMain.ApplyAnimationTemplates;
begin
  // Bounce animation
  TfgAnimationHelper.Bounce(btnAction, 0.6);
  
  // Shake animation
  TfgAnimationHelper.Shake(pnlError, 0.3);
  
  // Scale animation
  TfgAnimationHelper.Scale(imgIcon, 1.2, 0.2,
    procedure
    begin
      TfgAnimationHelper.Scale(imgIcon, 1.0, 0.2);
    end);
  
  // Slide in from edge
  TfgAnimationHelper.SlideIn(pnlNotification, TfgSlideDirection.Top, 0.4);
end;
```

---

## 10. Platform Integration

### 10.1 Device Information

**Sample Location**: `DeviceInfo - Base`

Access device characteristics:

```pascal
procedure TFormMain.DisplayDeviceInfo;
begin
  lblDeviceName.Text := TfgDeviceInfo.DeviceName;
  lblOSVersion.Text := TfgDeviceInfo.OSVersion;
  lblScreenSize.Text := Format('%.0fx%.0f', [
    TfgDeviceInfo.ScreenSize.Width, 
    TfgDeviceInfo.ScreenSize.Height
  ]);
  lblScreenDensity.Text := Format('%.1f', [TfgDeviceInfo.ScreenDensity]);
  lblUniqueID.Text := TfgDeviceInfo.UniqueDeviceID;
  
  // Platform-specific info
  {$IFDEF ANDROID}
  lblAPILevel.Text := IntToStr(TfgDeviceInfo.AndroidAPILevel);
  {$ENDIF}
  
  {$IFDEF IOS}
  lblDeviceType.Text := TfgDeviceInfo.iOSDeviceType;
  {$ENDIF}
end;
```

### 10.2 Permissions Handling

**Sample Location**: `Permissions - Request`

Request runtime permissions:

```pascal
procedure TFormMain.RequestCameraPermission;
begin
  TfgPermissionsService.RequestPermissions([TfgPermission.Camera],
    procedure(const APermissions: TArray<TfgPermission>; const AGrantResults: TArray<TfgPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and (AGrantResults[0] = TfgPermissionStatus.Granted) then
      begin
        // Permission granted - open camera
        OpenCamera;
      end
      else
      begin
        // Permission denied
        TfgDialogs.ShowMessage('Camera permission is required to take photos');
      end;
    end);
end;

procedure TFormMain.RequestMultiplePermissions;
var
  permissions: TArray<TfgPermission>;
begin
  permissions := [
    TfgPermission.Camera,
    TfgPermission.ReadExternalStorage,
    TfgPermission.WriteExternalStorage
  ];
  
  TfgPermissionsService.RequestPermissions(permissions,
    procedure(const APermissions: TArray<TfgPermission>; const AGrantResults: TArray<TfgPermissionStatus>)
    var
      i: Integer;
      allGranted: Boolean;
    begin
      allGranted := True;
      for i := 0 to High(AGrantResults) do
        if AGrantResults[i] <> TfgPermissionStatus.Granted then
        begin
          allGranted := False;
          Break;
        end;
        
      if allGranted then
        InitializeFeatures
      else
        ShowPermissionExplanation;
    end);
end;
```

### 10.3 Phone Integration

**Sample Location**: `Phone - Call`

Make phone calls:

```pascal
procedure TFormMain.MakePhoneCall;
var
  phoneNumber: string;
begin
  phoneNumber := editPhoneNumber.Text;
  
  if TfgPhoneDialer.CanMakeCall then
  begin
    TfgPhoneDialer.Call(phoneNumber,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if not ASuccess then
          TfgDialogs.ShowMessage('Failed to make call: ' + AError);
      end);
  end
  else
    TfgDialogs.ShowMessage('Phone calls not supported on this device');
end;
```

### 10.4 Clipboard Operations

**Sample Location**: `Clipboard - Text`

Text clipboard management:

```pascal
procedure TFormMain.CopyToClipboard;
begin
  TfgClipboard.SetText(editSource.Text);
  lblStatus.Text := 'Text copied to clipboard';
end;

procedure TFormMain.PasteFromClipboard;
begin
  if TfgClipboard.HasText then
    editDestination.Text := TfgClipboard.GetText
  else
    lblStatus.Text := 'No text in clipboard';
end;

procedure TFormMain.MonitorClipboard;
begin
  // Monitor clipboard changes
  TfgClipboard.OnChanged := ClipboardChanged;
end;

procedure TFormMain.ClipboardChanged(Sender: TObject);
begin
  lblClipboardStatus.Text := 'Clipboard updated: ' + 
    TfgClipboard.GetText.Substring(0, Min(50, TfgClipboard.GetText.Length));
end;
```

---

## 11. Media and Camera

### 11.1 Camera Preview

**Sample Location**: `Camera - Camera preview`

Live camera preview:

```pascal
type
  TFormMain = class(TfgForm)
    cameraPreview: TfgCameraPreview;
    btnCapture: TfgButton;
    btnSwitchCamera: TfgButton;
    procedure btnCaptureTap(Sender: TObject);
    procedure btnSwitchCameraTap(Sender: TObject);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Initialize camera
  cameraPreview.Camera := TfgCameraKind.Back;
  cameraPreview.Quality := TfgCameraQuality.High;
  cameraPreview.StartPreview;
end;

procedure TFormMain.btnCaptureTap(Sender: TObject);
begin
  cameraPreview.TakePhoto(
    procedure(const ABitmap: TfgBitmap; const ASuccess: Boolean)
    begin
      if ASuccess then
      begin
        // Save photo
        TfgPhotoLibraryService.SaveImageToPhotos(ABitmap,
          procedure(const ASaved: Boolean; const AError: string)
          begin
            if ASaved then
              TfgDialogs.ShowMessage('Photo saved successfully')
            else
              TfgDialogs.ShowMessage('Error saving photo: ' + AError);
          end);
      end;
    end);
end;

procedure TFormMain.btnSwitchCameraTap(Sender: TObject);
begin
  if cameraPreview.Camera = TfgCameraKind.Back then
    cameraPreview.Camera := TfgCameraKind.Front
  else
    cameraPreview.Camera := TfgCameraKind.Back;
end;
```

### 11.2 Photo Picker

**Sample Location**: `Pickers - Photo`

Select photos from gallery:

```pascal
procedure TFormMain.PickPhotoFromGallery;
begin
  TfgPhotoLibraryService.PickImage(
    procedure(const ABitmap: TfgBitmap; const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
      begin
        imgSelected.Bitmap.Assign(ABitmap);
        lblStatus.Text := 'Photo selected successfully';
      end
      else
        lblStatus.Text := 'Error: ' + AError;
    end);
end;

procedure TFormMain.PickMultiplePhotos;
begin
  TfgPhotoLibraryService.PickImages(5, // Max 5 images
    procedure(const ABitmaps: TArray<TfgBitmap>; const ASuccess: Boolean; const AError: string)
    var
      i: Integer;
    begin
      if ASuccess then
      begin
        // Display selected images
        for i := 0 to High(ABitmaps) do
        begin
          if i < FImageControls.Count then
            FImageControls[i].Bitmap.Assign(ABitmaps[i]);
        end;
        lblStatus.Text := Format('%d photos selected', [Length(ABitmaps)]);
      end
      else
        lblStatus.Text := 'Error: ' + AError;
    end);
end;
```

### 11.3 Video Recording

**Sample Location**: `Pickers - Video`

Video capture and playback:

```pascal
procedure TFormMain.RecordVideo;
begin
  TfgCameraService.RecordVideo(30, // Max 30 seconds
    procedure(const AVideoPath: string; const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
      begin
        // Play recorded video
        videoPlayer.VideoPath := AVideoPath;
        videoPlayer.Play;
        lblStatus.Text := 'Video recorded successfully';
      end
      else
        lblStatus.Text := 'Error: ' + AError;
    end);
end;
```

### 11.4 Lottie Animations

**Sample Location**: `LottieAnimation - Player`

Play Lottie animations:

```pascal
procedure TFormMain.LoadLottieAnimation;
begin
  // Load from assets
  lottiePlayer.LoadFromAsset('animation.json');
  
  // Configure playback
  lottiePlayer.AutoPlay := True;
  lottiePlayer.Loop := True;
  lottiePlayer.Speed := 1.0;
end;

procedure TFormMain.ControlLottiePlayback;
begin
  if lottiePlayer.IsPlaying then
    lottiePlayer.Pause
  else
    lottiePlayer.Play;
end;

procedure TFormMain.SetupLottieEvents;
begin
  lottiePlayer.OnAnimationStart := LottieAnimationStart;
  lottiePlayer.OnAnimationFinish := LottieAnimationFinish;
  lottiePlayer.OnAnimationLooped := LottieAnimationLooped;
end;
```

---

## 12. Maps and Location

### 12.1 Basic Map Setup

**Sample Location**: `Map - Stylization`

Google Maps integration:

```pascal
procedure TFormMain.InitializeMap;
begin
  // Basic map setup
  map.MapType := TfgMapType.Normal;
  map.ShowUserLocation := True;
  map.ShowZoomControls := True;
  map.ShowCompass := True;
  
  // Set initial location
  map.Location := TfgCoordinate.Create(37.7749, -122.4194); // San Francisco
  map.Zoom := 12;
end;

procedure TFormMain.ApplyMapStyle;
var
  styleJSON: string;
begin
  // Load custom style from assets
  styleJSON := TfgAssets.GetText('map_style_dark.json');
  map.CustomStyle := styleJSON;
end;
```

### 12.2 Map Objects and Markers

**Sample Location**: `Map - Objects`

Add markers and shapes to map:

```pascal
procedure TFormMain.AddMapMarkers;
var
  marker: TfgMapMarker;
  polyline: TfgMapPolyline;
  polygon: TfgMapPolygon;
  circle: TfgMapCircle;
begin
  // Add marker
  marker := map.AddMarker(TfgCoordinate.Create(37.7849, -122.4094));
  marker.Title := 'Custom Marker';
  marker.Snippet := 'This is a custom marker';
  marker.Icon := TfgAssets.GetBitmap('marker_icon.png');
  
  // Add polyline (route)
  polyline := map.AddPolyline([
    TfgCoordinate.Create(37.7749, -122.4194),
    TfgCoordinate.Create(37.7849, -122.4094),
    TfgCoordinate.Create(37.7949, -122.3994)
  ]);
  polyline.Color := TAlphaColors.Blue;
  polyline.Width := 5;
  
  // Add polygon (area)
  polygon := map.AddPolygon([
    TfgCoordinate.Create(37.7649, -122.4294),
    TfgCoordinate.Create(37.7749, -122.4194),
    TfgCoordinate.Create(37.7649, -122.4094)
  ]);
  polygon.FillColor := TAlphaColorRec.Blue.WithAlpha(100);
  polygon.StrokeColor := TAlphaColors.Blue;
  
  // Add circle
  circle := map.AddCircle(TfgCoordinate.Create(37.7549, -122.4394), 1000); // 1km radius
  circle.FillColor := TAlphaColorRec.Red.WithAlpha(50);
end;
```

### 12.3 Map Controls

**Sample Location**: `Map - Controls`

Customize map controls:

```pascal
procedure TFormMain.ConfigureMapControls;
begin
  // Zoom controls
  map.ShowZoomControls := chkZoomControls.IsChecked;
  map.ZoomControlsPosition := TfgMapControlPosition.BottomRight;
  
  // My Location button
  map.ShowMyLocationButton := chkMyLocation.IsChecked;
  map.MyLocationButtonPosition := TfgMapControlPosition.TopRight;
  
  // Compass
  map.ShowCompass := chkCompass.IsChecked;
  map.CompassPosition := TfgMapControlPosition.TopLeft;
  
  // Map type controls
  map.ShowMapTypeControls := chkMapType.IsChecked;
  
  // Gestures
  map.EnableZoomGestures := chkZoomGestures.IsChecked;
  map.EnablePanGestures := chkPanGestures.IsChecked;
  map.EnableRotateGestures := chkRotateGestures.IsChecked;
  map.EnableTiltGestures := chkTiltGestures.IsChecked;
end;
```

### 12.4 Custom Tile Providers

**Sample Location**: `Map - Tile Provider`

Use custom map tiles:

```pascal
procedure TFormMain.SetupCustomTileProvider;
var
  tileProvider: TfgCustomTileProvider;
begin
  tileProvider := TfgCustomTileProvider.Create;
  tileProvider.OnGetTileURL := GetCustomTileURL;
  map.TileProvider := tileProvider;
end;

function TFormMain.GetCustomTileURL(const AX, AY, AZoom: Integer): string;
begin
  // Return URL for custom tile server
  Result := Format('https://tile.openstreetmap.org/%d/%d/%d.png', [AZoom, AX, AY]);
end;
```

---

## 13. Services and Authentication

### 13.1 Biometric Authentication

**Sample Location**: `Authentication - Biometric`

Touch ID / Face ID authentication:

```pascal
procedure TFormMain.SetupBiometricAuth;
begin
  if TfgBiometricAuthentication.IsSupported then
  begin
    lblBiometricSupport.Text := 'Biometric authentication supported';
    btnAuthenticate.Enabled := True;
    
    // Check available biometric types
    if TfgBiometricAuthentication.HasTouchID then
      lblType.Text := 'Touch ID available';
    if TfgBiometricAuthentication.HasFaceID then
      lblType.Text := 'Face ID available';
  end
  else
  begin
    lblBiometricSupport.Text := 'Biometric authentication not supported';
    btnAuthenticate.Enabled := False;
  end;
end;

procedure TFormMain.AuthenticateWithBiometrics;
begin
  TfgBiometricAuthentication.Authenticate('Please authenticate to continue',
    procedure(const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
      begin
        lblResult.Text := 'Authentication successful';
        // Proceed with authenticated operations
        UnlockSecureFeatures;
      end
      else
      begin
        lblResult.Text := 'Authentication failed: ' + AError;
      end;
    end);
end;
```

### 13.2 Local Notifications

**Sample Location**: `Local Notification - Base`

Schedule local notifications:

```pascal
procedure TFormMain.ScheduleNotification;
var
  notification: TfgLocalNotification;
begin
  notification := TfgLocalNotification.Create;
  try
    notification.Title := 'Reminder';
    notification.Body := 'Don''t forget to check the app!';
    notification.Badge := 1;
    notification.Sound := TfgNotificationSound.Default;
    notification.FireDate := Now + (1/24/60); // 1 minute from now
    
    TfgLocalNotificationService.ScheduleNotification(notification,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if ASuccess then
          lblStatus.Text := 'Notification scheduled'
        else
          lblStatus.Text := 'Error: ' + AError;
      end);
  finally
    notification.Free;
  end;
end;

procedure TFormMain.SetupNotificationHandling;
begin
  // Handle notification tap when app is in background
  TfgLocalNotificationService.OnNotificationReceived := NotificationReceived;
end;

procedure TFormMain.NotificationReceived(const ANotification: TfgLocalNotification);
begin
  TfgDialogs.ShowMessage(Format('Notification received: %s', [ANotification.Body]));
end;
```

### 13.3 Barcode Scanner

**Sample Location**: `Barcode Scanner - Base`

Scan QR codes and barcodes:

```pascal
procedure TFormMain.StartBarcodeScanning;
begin
  barcodeScanner.ScanFormats := [TfgBarcodeFormat.QRCode, TfgBarcodeFormat.Code128];
  barcodeScanner.ShowScanBounds := True;
  barcodeScanner.OnCodeScanned := CodeScanned;
  barcodeScanner.StartScanning;
end;

procedure TFormMain.CodeScanned(Sender: TObject; const ACode: string; const AFormat: TfgBarcodeFormat);
begin
  // Stop scanning
  barcodeScanner.StopScanning;
  
  // Process scanned code
  lblScannedCode.Text := ACode;
  lblCodeFormat.Text := TfgBarcodeFormatHelper.ToString(AFormat);
  
  // Handle different code types
  case AFormat of
    TfgBarcodeFormat.QRCode: ProcessQRCode(ACode);
    TfgBarcodeFormat.Code128: ProcessBarcode(ACode);
  end;
end;

procedure TFormMain.ScanBitmapBarcode;
var
  bitmap: TfgBitmap;
begin
  // Load image from assets or camera
  bitmap := TfgAssets.GetBitmap('test_qr.png');
  
  TfgBarcodeScanner.ScanBitmap(bitmap, [TfgBarcodeFormat.QRCode],
    procedure(const ACodes: TArray<TfgScannedCode>; const ASuccess: Boolean)
    var
      code: TfgScannedCode;
    begin
      if ASuccess and (Length(ACodes) > 0) then
      begin
        code := ACodes[0];
        lblResult.Text := Format('Found: %s (%s)', [code.Text, TfgBarcodeFormatHelper.ToString(code.Format)]);
      end
      else
        lblResult.Text := 'No codes found in image';
    end);
end;
```

---

## 14. Advanced Topics

### 14.1 Bottom Sheet Layouts

**Sample Locations**: 
- `BottomSheetLayout - Base`
- `BottomSheetLayout - Float`

Sliding bottom panels:

```pascal
procedure TFormMain.SetupBottomSheet;
begin
  bottomSheet.PeekHeight := 100;
  bottomSheet.State := TfgBottomSheetState.Collapsed;
  bottomSheet.OnStateChanged := BottomSheetStateChanged;
  
  // Add content to bottom sheet
  SetupBottomSheetContent;
end;

procedure TFormMain.BottomSheetStateChanged(Sender: TObject; const AState: TfgBottomSheetState);
begin
  case AState of
    TfgBottomSheetState.Collapsed: lblState.Text := 'Collapsed';
    TfgBottomSheetState.Expanded: lblState.Text := 'Expanded';
    TfgBottomSheetState.Hidden: lblState.Text := 'Hidden';
  end;
end;

procedure TFormMain.ToggleBottomSheet;
begin
  if bottomSheet.State = TfgBottomSheetState.Collapsed then
    bottomSheet.State := TfgBottomSheetState.Expanded
  else
    bottomSheet.State := TfgBottomSheetState.Collapsed;
end;
```

### 14.2 Gesture Recognition

**Sample Locations**:
- `Gestures - LongTap`
- `Gestures - Zoom`

Advanced touch handling:

```pascal
procedure TFormMain.SetupGestureRecognition;
begin
  // Long tap gesture
  gestureManager.AddLongTapGesture(pnlTarget, LongTapDetected);
  
  // Zoom gesture
  gestureManager.AddZoomGesture(imgZoomable, ZoomDetected);
  
  // Pan gesture
  gestureManager.AddPanGesture(pnlMovable, PanDetected);
end;

procedure TFormMain.LongTapDetected(Sender: TObject; const APosition: TPointF);
begin
  // Show context menu
  ShowContextMenuAt(APosition);
end;

procedure TFormMain.ZoomDetected(Sender: TObject; const AScale: Single; const ACenter: TPointF);
begin
  // Apply zoom to image
  imgZoomable.Scale.X := AScale;
  imgZoomable.Scale.Y := AScale;
  imgZoomable.Position.Point := ACenter;
end;

procedure TFormMain.PanDetected(Sender: TObject; const ADelta: TPointF);
begin
  // Move panel
  pnlMovable.Position.X := pnlMovable.Position.X + ADelta.X;
  pnlMovable.Position.Y := pnlMovable.Position.Y + ADelta.Y;
end;
```

### 14.3 Advanced Theme System

**Sample Location**: `Assets - Dark and Light themes`

FGX Native provides a comprehensive theming system that supports Material Design 2.0 principles with automatic light/dark theme switching.

#### Theme Architecture Overview

```pascal
type
  // Theme types supported by FGX Native
  TfgTheme = (Light, Dark, System);
  TfgSystemAppearance = (Light, Dark, Unspecified);
  
  // Theme color system following Material Design 2.0
  TfgThemeColors = record
    Primary: TAlphaColor;           // Primary brand color
    PrimaryVariant: TAlphaColor;    // Darker variant of primary
    Secondary: TAlphaColor;         // Secondary brand color  
    SecondaryVariant: TAlphaColor;  // Darker variant of secondary
    Background: TAlphaColor;        // Screen background
    Surface: TAlphaColor;           // Component backgrounds
    Error: TAlphaColor;            // Error states
    OnPrimary: TAlphaColor;        // Text/icons on primary
    OnSecondary: TAlphaColor;      // Text/icons on secondary
    OnBackground: TAlphaColor;     // Text/icons on background
    OnSurface: TAlphaColor;        // Text/icons on surface
    OnError: TAlphaColor;          // Text/icons on error
  end;
```

#### Complete Theme Implementation

```pascal
type
  TFormMain = class(TfgForm)
    rbLightTheme: TfgRadioButton;
    rbDarkTheme: TfgRadioButton;
    rbSystemTheme: TfgRadioButton;
    procedure ChangeTheme(Sender: TObject);
    procedure fgFormSystemThemeChanged(Sender: TObject; const AAppearance: TfgSystemAppearance);
  private
    FThemeManager: TfgThemeManager;
    procedure UpdateThemeUI;
    procedure ApplyCustomColors;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  
  // Initialize theme manager
  FThemeManager := TfgThemeManager.Create;
  
  // Set up system theme monitoring
  OnSystemThemeChanged := fgFormSystemThemeChanged;
  
  // Apply initial theme based on system preference
  if TfgPlatform.SystemAppearance = TfgSystemAppearance.Dark then
    ApplyTheme(TfgTheme.Dark)
  else
    ApplyTheme(TfgTheme.Light);
end;

procedure TFormMain.ChangeTheme(Sender: TObject);
var
  selectedTheme: TfgTheme;
begin
  // Determine selected theme
  if rbLightTheme.IsChecked then
    selectedTheme := TfgTheme.Light
  else if rbDarkTheme.IsChecked then
    selectedTheme := TfgTheme.Dark
  else
    selectedTheme := TfgTheme.System;
    
  ApplyTheme(selectedTheme);
end;

procedure TFormMain.ApplyTheme(const ATheme: TfgTheme);
begin
  // Set application theme
  TfgApplication.Theme := ATheme;
  
  // Load theme-specific assets
  LoadThemeAssets(ATheme);
  
  // Apply custom colors
  ApplyCustomColors;
  
  // Update UI elements
  UpdateThemeUI;
  
  // Refresh all forms
  TfgApplication.RefreshTheme;
  
  // Save theme preference
  TfgSettings.SetValue('user_theme', Ord(ATheme));
end;

procedure TFormMain.LoadThemeAssets(const ATheme: TfgTheme);
begin
  case ATheme of
    TfgTheme.Light:
      begin
        // Load light theme assets
        TfgAssets.LoadThemeVariant('light');
        
        // Set light theme specific icons
        btnMenu.Icon := TfgAssets.GetBitmap('menu_dark'); // Dark icon on light background
        btnSearch.Icon := TfgAssets.GetBitmap('search_dark');
      end;
      
    TfgTheme.Dark:
      begin
        // Load dark theme assets
        TfgAssets.LoadThemeVariant('dark');
        
        // Set dark theme specific icons
        btnMenu.Icon := TfgAssets.GetBitmap('menu_light'); // Light icon on dark background
        btnSearch.Icon := TfgAssets.GetBitmap('search_light');
      end;
      
    TfgTheme.System:
      begin
        // Use system theme
        if TfgPlatform.SystemAppearance = TfgSystemAppearance.Dark then
          LoadThemeAssets(TfgTheme.Dark)
        else
          LoadThemeAssets(TfgTheme.Light);
      end;
  end;
end;

procedure TFormMain.ApplyCustomColors;
var
  themeColors: TfgThemeColors;
begin
  // Get current theme colors
  themeColors := TfgThemeManager.GetCurrentColors;
  
  // Apply to navigation bar
  fgNavigationBar1.TintColor := themeColors.Primary;
  fgNavigationBar1.BackgroundColor := themeColors.Surface;
  fgNavigationBar1.TextColor := themeColors.OnSurface;
  
  // Apply to cards
  cardExample.BackgroundColor := themeColors.Surface;
  cardExample.ShadowColor := TAlphaColorRec.Black.WithAlpha(
    IfThen(TfgApplication.Theme = TfgTheme.Dark, 100, 50)
  );
  
  // Apply to buttons
  btnPrimary.TintColor := themeColors.Primary;
  btnSecondary.TintColor := themeColors.Secondary;
  
  // Apply to collection view
  fgCollectionView1.BackgroundColor := themeColors.Background;
  
  // Apply to drawer
  fgDrawerLayout1_Drawer.BackgroundColor := themeColors.Surface;
  fgListMenu.BackgroundColor := themeColors.Surface;
  fgListMenu.ItemTextColor := themeColors.OnSurface;
  fgListMenu.SelectedItemColor := themeColors.Primary.WithAlpha(50);
end;

procedure TFormMain.fgFormSystemThemeChanged(Sender: TObject; const AAppearance: TfgSystemAppearance);
begin
  // Respond to system theme changes
  if rbSystemTheme.IsChecked then
  begin
    case AAppearance of
      TfgSystemAppearance.Light: ApplyTheme(TfgTheme.Light);
      TfgSystemAppearance.Dark: ApplyTheme(TfgTheme.Dark);
    end;
  end;
end;
```

#### Advanced Theme Customization

```pascal
// Custom theme builder
type
  TCustomThemeBuilder = class
  public
    class function BuildMaterialTheme(const APrimaryColor: TAlphaColor): TfgThemeColors;
    class function BuildBrandTheme(const ABrandColors: TArray<TAlphaColor>): TfgThemeColors;
    class function BuildAccessibleTheme(const AHighContrast: Boolean): TfgThemeColors;
  end;

class function TCustomThemeBuilder.BuildMaterialTheme(const APrimaryColor: TAlphaColor): TfgThemeColors;
begin
  // Generate full Material Design color palette from primary color
  Result.Primary := APrimaryColor;
  Result.PrimaryVariant := DarkenColor(APrimaryColor, 0.2);
  Result.Secondary := GenerateSecondaryColor(APrimaryColor);
  Result.SecondaryVariant := DarkenColor(Result.Secondary, 0.2);
  
  // Apply Material Design color algorithm
  Result.Background := TAlphaColors.White;
  Result.Surface := TAlphaColors.White;
  Result.Error := TAlphaColorRec.Red;
  
  // Calculate "on" colors for contrast
  Result.OnPrimary := GetContrastColor(Result.Primary);
  Result.OnSecondary := GetContrastColor(Result.Secondary);
  Result.OnBackground := TAlphaColorRec.Black.WithAlpha(220);
  Result.OnSurface := TAlphaColorRec.Black.WithAlpha(220);
  Result.OnError := TAlphaColors.White;
end;

// Theme animation system
procedure TFormMain.AnimateThemeTransition(const AFromTheme, AToTheme: TfgTheme);
begin
  // Animate theme transition
  TfgAnimationHelper.AnimateColorTransition(
    Self,
    GetThemeColors(AFromTheme).Background,
    GetThemeColors(AToTheme).Background,
    0.3,
    procedure(const AColor: TAlphaColor)
    begin
      BackgroundColor := AColor;
    end);
    
  // Animate component colors
  TfgAnimationHelper.AnimateColorTransition(
    fgNavigationBar1,
    GetThemeColors(AFromTheme).Surface,
    GetThemeColors(AToTheme).Surface,
    0.3,
    procedure(const AColor: TAlphaColor)
    begin
      fgNavigationBar1.BackgroundColor := AColor;
    end);
end;
```

#### Theme Resource Management

```pascal
// Theme-aware asset loading
procedure TFormMain.LoadThemeAwareAssets;
begin
  // Automatic theme suffix loading
  imgLogo.LoadFromAsset('logo'); // Automatically loads logo_light.png or logo_dark.png
  
  // Manual theme-specific loading
  case TfgApplication.Theme of
    TfgTheme.Light:
      begin
        imgBackground.LoadFromAsset('bg_light');
        SetIconTints(TAlphaColorRec.Black.WithAlpha(180));
      end;
    TfgTheme.Dark:
      begin
        imgBackground.LoadFromAsset('bg_dark');
        SetIconTints(TAlphaColorRec.White.WithAlpha(200));
      end;
  end;
end;

// Component theme inheritance
procedure TFormMain.ApplyThemeToCustomComponents;
var
  i: Integer;
  control: TfgControl;
begin
  // Apply theme to all child controls recursively
  for i := 0 to ControlsCount - 1 do
  begin
    control := Controls[i];
    
    if control is TfgThemeAware then
      (control as TfgThemeAware).ApplyTheme(TfgApplication.Theme);
      
    // Apply theme to nested controls
    ApplyThemeToChildren(control);
  end;
end;
```

#### Theme Persistence and Configuration

```pascal
// Theme settings management
type
  TThemeSettings = class
  private
    class var FInstance: TThemeSettings;
  public
    UserThemePreference: TfgTheme;
    AccentColor: TAlphaColor;
    FollowSystemTheme: Boolean;
    HighContrastMode: Boolean;
    
    class function Instance: TThemeSettings;
    procedure SaveSettings;
    procedure LoadSettings;
    procedure ResetToDefaults;
  end;

procedure TThemeSettings.SaveSettings;
begin
  TfgSettings.SetValue('theme_preference', Ord(UserThemePreference));
  TfgSettings.SetValue('accent_color', AccentColor);
  TfgSettings.SetValue('follow_system', FollowSystemTheme);
  TfgSettings.SetValue('high_contrast', HighContrastMode);
end;

procedure TThemeSettings.LoadSettings;
begin
  UserThemePreference := TfgTheme(TfgSettings.GetValue('theme_preference', Ord(TfgTheme.System)));
  AccentColor := TfgSettings.GetValue('accent_color', TAlphaColorRec.Blue);
  FollowSystemTheme := TfgSettings.GetValue('follow_system', True);
  HighContrastMode := TfgSettings.GetValue('high_contrast', False);
end;

// Theme preview system
procedure TFormMain.ShowThemePreview(const ATheme: TfgTheme);
var
  previewForm: TThemePreviewForm;
begin
  previewForm := TThemePreviewForm.Create(nil);
  try
    previewForm.PreviewTheme := ATheme;
    previewForm.ShowModal(
      procedure(const AResult: TModalResult)
      begin
        if AResult = mrOK then
          ApplyTheme(ATheme);
      end);
  finally
    previewForm.Free;
  end;
end;
```

### 14.4 Asset Management

**Sample Locations**:
- `Assets - Asynch image loading`
- `Assets - Custom Font Awesome`
- `Assets - Custom text file`
- `Assets - Releasing from memory`

Efficient asset handling:

```pascal
procedure TFormMain.LoadAssetsAsynchronously;
begin
  // Load images asynchronously
  TfgAssets.LoadImageAsync('large_image.jpg',
    procedure(const ABitmap: TfgBitmap; const ASuccess: Boolean)
    begin
      if ASuccess then
      begin
        imgDisplay.Bitmap.Assign(ABitmap);
        activityIndicator.IsAnimating := False;
      end;
    end);
end;

procedure TFormMain.LoadCustomFont;
begin
  // Load custom font
  TfgAssets.LoadFont('FontAwesome.ttf');
  
  // Use in labels
  lblIcon.Font.Family := 'FontAwesome';
  lblIcon.Text := #$F007; // Font Awesome user icon
end;

procedure TFormMain.LoadTextAsset;
var
  content: string;
begin
  content := TfgAssets.GetText('data.json');
  
  // Parse and use content
  ProcessJSONData(content);
end;

procedure TFormMain.ManageAssetMemory;
begin
  // Release unused assets
  TfgAssets.ReleaseUnusedAssets;
  
  // Preload critical assets
  TfgAssets.PreloadAssets(['icon.png', 'logo.png', 'background.jpg']);
end;
```

---

## 14. Web Browser Integration

### 14.1 Basic Web Browser

**Sample Location**: `WebBrowser - Base`

The `TfgWebBrowser` component provides full web browser functionality:

```pascal
procedure TFormMain.SetupWebBrowser;
begin
  webBrowser.OnDidFinishLoad := WebBrowserDidFinishLoad;
  webBrowser.OnDidFailLoad := WebBrowserDidFailLoad;
  webBrowser.OnShouldStartLoad := WebBrowserShouldStartLoad;
  
  // Load initial URL
  webBrowser.Navigate('https://www.example.com');
end;

procedure TFormMain.WebBrowserDidFinishLoad(Sender: TObject);
begin
  lblStatus.Text := 'Page loaded successfully';
  activityIndicator.IsAnimating := False;
end;

procedure TFormMain.WebBrowserDidFailLoad(Sender: TObject; const AError: string);
begin
  lblStatus.Text := 'Error loading page: ' + AError;
  activityIndicator.IsAnimating := False;
end;

function TFormMain.WebBrowserShouldStartLoad(Sender: TObject; const AURL: string): Boolean;
begin
  // Control which URLs can be loaded
  Result := not AURL.Contains('blocked-domain.com');
  
  if not Result then
    TfgDialogs.ShowMessage('URL blocked by policy');
end;
```

### 14.2 JavaScript Execution

**Sample Location**: `WebBrowser - Javascript`

Execute JavaScript and get results:

```pascal
procedure TFormMain.ExecuteJavaScript;
var
  script: string;
begin
  script := 'document.title + " - " + window.location.href';
  
  webBrowser.EvaluateJavaScript(script,
    procedure(const AResult: string; const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
        lblResult.Text := AResult
      else
        lblError.Text := 'JavaScript error: ' + AError;
    end);
end;

procedure TFormMain.InjectCustomJavaScript;
var
  customScript: string;
begin
  customScript := '''
    function highlightElement(id) {
      var element = document.getElementById(id);
      if (element) {
        element.style.backgroundColor = 'yellow';
        return 'Element highlighted: ' + id;
      }
      return 'Element not found: ' + id;
    }
    
    function getFormData() {
      var form = document.querySelector('form');
      if (form) {
        var formData = new FormData(form);
        var result = {};
        for (var pair of formData.entries()) {
          result[pair[0]] = pair[1];
        }
        return JSON.stringify(result);
      }
      return '{}';
    }
  ''';
  
  webBrowser.EvaluateJavaScript(customScript,
    procedure(const AResult: string; const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
        lblStatus.Text := 'Custom JavaScript injected successfully'
      else
        lblStatus.Text := 'JavaScript injection failed: ' + AError;
    end);
end;

procedure TFormMain.CallInjectedFunction;
begin
  webBrowser.EvaluateJavaScript('highlightElement("header")',
    procedure(const AResult: string; const ASuccess: Boolean; const AError: string)
    begin
      lblFunctionResult.Text := AResult;
    end);
end;
```

### 14.3 Local HTML Content

**Sample Location**: `WebBrowser - Local HTML`

Load local HTML content:

```pascal
procedure TFormMain.LoadLocalHTML;
var
  htmlContent: string;
begin
  // Load HTML from assets
  htmlContent := TfgAssets.GetText('local_page.html');
  webBrowser.LoadHTML(htmlContent);
end;

procedure TFormMain.CreateDynamicHTML;
var
  htmlContent: string;
begin
  htmlContent := Format('''
  <!DOCTYPE html>
  <html>
  <head>
    <title>Dynamic Content</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
      body { font-family: Arial, sans-serif; padding: 20px; }
      .card { border: 1px solid #ddd; padding: 15px; margin: 10px 0; border-radius: 5px; }
      .button { background: #007AFF; color: white; padding: 10px 20px; border: none; border-radius: 5px; }
    </style>
  </head>
  <body>
    <h1>Hello from FGX Native!</h1>
    <div class="card">
      <h3>User Info</h3>
      <p>Name: %s</p>
      <p>Device: %s</p>
      <p>Time: %s</p>
    </div>
    <button class="button" onclick="sendMessageToApp()">Send Message to App</button>
    
    <script>
      function sendMessageToApp() {
        // This will be intercepted by the app
        window.location = 'app://message?text=Hello from WebView';
      }
    </script>
  </body>
  </html>
  ''', [
    'John Doe',
    TfgDeviceInfo.DeviceName,
    FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
  ]);
  
  webBrowser.LoadHTML(htmlContent);
end;
```

### 14.4 Basic Authentication

**Sample Location**: `WebBrowser - Basic Authentification`

Handle HTTP Basic Authentication:

```pascal
procedure TFormMain.SetupBasicAuth;
begin
  webBrowser.OnAuthenticationChallenge := WebBrowserAuthChallenge;
end;

procedure TFormMain.WebBrowserAuthChallenge(Sender: TObject; const ARealm: string; 
  var AUsername, APassword: string; var AHandled: Boolean);
begin
  // Show custom authentication dialog
  ShowAuthDialog(ARealm,
    procedure(const AUser, APass: string; const ACancelled: Boolean)
    begin
      if not ACancelled then
      begin
        AUsername := AUser;
        APassword := APass;
        AHandled := True;
      end;
    end);
end;

procedure TFormMain.ShowAuthDialog(const ARealm: string; 
  ACallback: TProc<string, string, Boolean>);
var
  authForm: TAuthenticationForm;
begin
  authForm := TAuthenticationForm.Create(nil);
  try
    authForm.lblRealm.Text := Format('Authentication required for: %s', [ARealm]);
    authForm.ShowModal(
      procedure(const AModalResult: TModalResult)
      begin
        if AModalResult = mrOK then
          ACallback(authForm.editUsername.Text, authForm.editPassword.Text, False)
        else
          ACallback('', '', True);
      end);
  finally
    authForm.Free;
  end;
end;
```

### 14.5 SSL Error Handling

**Sample Location**: `WebBrowser - SSL Error handling`

Handle SSL certificate errors:

```pascal
procedure TFormMain.SetupSSLErrorHandling;
begin
  webBrowser.OnSSLError := WebBrowserSSLError;
end;

procedure TFormMain.WebBrowserSSLError(Sender: TObject; const AError: string; 
  var AIgnore: Boolean);
begin
  // Show SSL error dialog
  TfgDialogs.ShowConfirmation(
    'SSL Certificate Error',
    Format('SSL Error: %s'#13#10#13#10'Do you want to continue anyway?', [AError]),
    ['Continue', 'Cancel'],
    procedure(const AResult: TModalResult)
    begin
      AIgnore := (AResult = mrOK);
    end);
end;
```

---

## 15. Advanced UI Components

### 15.1 Virtual Pager Layout

**Sample Locations**:
- `VirtualPagerLayout - Base`
- `VirtualPagerLayout - Pages pool`
- `VirtualPagerLayout - Photo slider`

The `TfgVirtualPagerLayout` provides efficient paging with memory management:

#### Photo Slider Implementation

```pascal
type
  TFormMain = class(TfgForm)
    vplPhotoSlider: TfgVirtualPagerLayout;
    tPhoto: TfgTimer;
    swAutoSlide: TfgSwitch;
    procedure vplPhotoSliderGetPageCount(Sender: TObject; var ACount: Integer);
    procedure vplPhotoSliderLoadPage(Sender: TObject; const AItemIndex: Integer; var APage: TfgControl);
    procedure vplPhotoSliderUnloadPage(Sender: TObject; const AItemIndex: Integer; const APage: TfgControl);
  private
    FPhotos: TList<TfgAssetName>;
    FAcquiredPages: TList<TfgImage>;
    FReleasedPages: TList<TfgImage>;
  end;

constructor TFormMain.Create(AOwner: TComponent);
var
  PhotoIndex: Integer;
begin
  inherited;
  FAcquiredPages := TList<TfgImage>.Create;
  FReleasedPages := TList<TfgImage>.Create;
  FPhotos := TList<TfgAssetName>.Create;
  
  // Fill photo list
  for PhotoIndex := 1 to 11 do
    FPhotos.Add(Format('Photos\%d', [PhotoIndex]));
end;

procedure TFormMain.vplPhotoSliderGetPageCount(Sender: TObject; var ACount: Integer);
begin
  ACount := FPhotos.Count;
end;

procedure TFormMain.vplPhotoSliderLoadPage(Sender: TObject; const AItemIndex: Integer; var APage: TfgControl);
var
  imageControl: TfgImage;
begin
  // Try to reuse existing image control
  if FReleasedPages.Count > 0 then
  begin
    imageControl := FReleasedPages.ExtractAt(0);
  end
  else
  begin
    // Create new image control
    imageControl := TfgImage.Create(vplPhotoSlider);
    imageControl.Align := TfgAlign.Client;
  end;
  
  // Load image from assets
  imageControl.LoadFromAsset(FPhotos[AItemIndex]);
  
  // Track acquired page
  FAcquiredPages.Add(imageControl);
  APage := imageControl;
end;

procedure TFormMain.vplPhotoSliderUnloadPage(Sender: TObject; const AItemIndex: Integer; const APage: TfgControl);
var
  imageControl: TfgImage;
begin
  imageControl := APage as TfgImage;
  
  // Remove from acquired list
  FAcquiredPages.Remove(imageControl);
  
  // Add to released pool for reuse
  FReleasedPages.Add(imageControl);
  
  // Clear the image to free memory
  imageControl.Bitmap := nil;
end;

// Auto-slide functionality
procedure TFormMain.swAutoSlideChanged(Sender: TObject);
begin
  tPhoto.Enabled := swAutoSlide.IsChecked;
end;

procedure TFormMain.tPhotoTimer(Sender: TObject);
var
  nextIndex: Integer;
begin
  nextIndex := vplPhotoSlider.ItemIndex + 1;
  if nextIndex >= FPhotos.Count then
    nextIndex := 0;
    
  vplPhotoSlider.ItemIndex := nextIndex;
end;
```

### 15.2 Signature Capture

**Sample Location**: `Signature - Base`

Capture handwritten signatures:

```pascal
type
  TFormMain = class(TfgForm)
    signature: TfgSignature;
    btnClear: TfgButton;
    btnSave: TfgButton;
    imgPreview: TfgImage;
    procedure btnClearTap(Sender: TObject);
    procedure btnSaveTap(Sender: TObject);
  end;

procedure TFormMain.btnClearTap(Sender: TObject);
begin
  signature.Clear;
  imgPreview.Bitmap := nil;
end;

procedure TFormMain.btnSaveTap(Sender: TObject);
var
  signatureBitmap: TfgBitmap;
begin
  // Create bitmap from signature
  signatureBitmap := signature.CreateBitmap(Round(imgPreview.Width), Round(imgPreview.Height));
  try
    // Display preview
    imgPreview.Bitmap.Assign(signatureBitmap);
    
    // Save to photo library
    TfgPhotoLibraryService.SaveImageToPhotos(signatureBitmap,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if ASuccess then
          TfgDialogs.ShowMessage('Signature saved successfully')
        else
          TfgDialogs.ShowMessage('Error saving signature: ' + AError);
      end);
  finally
    signatureBitmap.Free;
  end;
end;

// Advanced signature configuration
procedure TFormMain.ConfigureSignature;
begin
  signature.StrokeColor := TAlphaColors.Blue;
  signature.StrokeWidth := 3.0;
  signature.BackgroundColor := TAlphaColors.White;
  
  // Event handlers
  signature.OnStrokeBegin := SignatureStrokeBegin;
  signature.OnStrokeEnd := SignatureStrokeEnd;
end;

procedure TFormMain.SignatureStrokeBegin(Sender: TObject);
begin
  lblStatus.Text := 'Drawing...';
end;

procedure TFormMain.SignatureStrokeEnd(Sender: TObject);
begin
  lblStatus.Text := 'Ready';
  btnSave.Enabled := not signature.IsEmpty;
end;
```

### 15.3 Scratch Overlay

**Sample Location**: `ScratchOverlay - Base`

Scratch-off overlay effect:

```pascal
type
  TFormMain = class(TfgForm)
    scratchOverlay: TfgScratchOverlay;
    imgHidden: TfgImage;
    procedure scratchOverlayProgress(Sender: TObject; const AProgress: Single);
    procedure scratchOverlayComplete(Sender: TObject);
  end;

procedure TFormMain.SetupScratchOverlay;
begin
  // Configure scratch overlay
  scratchOverlay.ScratchRadius := 20;
  scratchOverlay.RevealThreshold := 0.7; // 70% scratched to complete
  scratchOverlay.OnProgress := scratchOverlayProgress;
  scratchOverlay.OnComplete := scratchOverlayComplete;
  
  // Set overlay image
  scratchOverlay.OverlayImage := TfgAssets.GetBitmap('scratch_surface.png');
end;

procedure TFormMain.scratchOverlayProgress(Sender: TObject; const AProgress: Single);
begin
  lblProgress.Text := Format('Scratched: %.1f%%', [AProgress * 100]);
  progressBar.Value := AProgress;
end;

procedure TFormMain.scratchOverlayComplete(Sender: TObject);
begin
  lblProgress.Text := 'Revealed!';
  
  // Animate the reveal
  TfgAnimationHelper.FadeOut(scratchOverlay, 0.5,
    procedure
    begin
      scratchOverlay.Visible := False;
    end);
end;
```

### 15.4 Region Picker

**Sample Location**: `RegionPicker - Crop Photo`

Interactive photo cropping:

```pascal
type
  TFormMain = class(TfgForm)
    regionPicker: TfgRegionPicker;
    imgSource: TfgImage;
    imgCropped: TfgImage;
    btnCrop: TfgButton;
    procedure btnCropTap(Sender: TObject);
    procedure btnLoadPhotoTap(Sender: TObject);
  end;

procedure TFormMain.SetupRegionPicker;
begin
  regionPicker.AspectRatio := 1.0; // Square crop
  regionPicker.MinRegionSize := TPointF.Create(50, 50);
  regionPicker.SelectionColor := TAlphaColors.Blue;
  regionPicker.SelectionWidth := 2;
end;

procedure TFormMain.btnLoadPhotoTap(Sender: TObject);
begin
  TfgPhotoLibraryService.PickImage(
    procedure(const ABitmap: TfgBitmap; const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
      begin
        imgSource.Bitmap.Assign(ABitmap);
        regionPicker.SourceBitmap := ABitmap;
        btnCrop.Enabled := True;
      end;
    end);
end;

procedure TFormMain.btnCropTap(Sender: TObject);
var
  croppedBitmap: TfgBitmap;
  cropRect: TRectF;
begin
  cropRect := regionPicker.SelectedRegion;
  croppedBitmap := regionPicker.CropBitmap(cropRect);
  try
    imgCropped.Bitmap.Assign(croppedBitmap);
    
    // Save cropped image
    TfgPhotoLibraryService.SaveImageToPhotos(croppedBitmap,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if ASuccess then
          TfgDialogs.ShowMessage('Cropped image saved')
        else
          TfgDialogs.ShowMessage('Error: ' + AError);
      end);
  finally
    croppedBitmap.Free;
  end;
end;
```

### 15.5 Virtual List Picker

**Sample Location**: `VirtualListPicker - Base`

Efficient scrollable picker for large datasets:

```pascal
type
  TFormMain = class(TfgForm)
    virtualListPicker: TfgVirtualListPicker;
    procedure virtualListPickerGetItemCount(Sender: TObject; var ACount: Integer);
    procedure virtualListPickerBindItem(Sender: TObject; const AIndex: Integer; const AItem: TfgItemWrapper);
    procedure virtualListPickerSelectionChanged(Sender: TObject);
  private
    FCountries: TArray<string>;
  end;

procedure TFormMain.SetupVirtualListPicker;
begin
  // Load country list
  FCountries := LoadCountriesFromAsset;
  
  // Configure picker
  virtualListPicker.ItemHeight := 44;
  virtualListPicker.VisibleItemsCount := 7;
  virtualListPicker.LoopingEnabled := True;
  
  // Set events
  virtualListPicker.OnGetItemCount := virtualListPickerGetItemCount;
  virtualListPicker.OnBindItem := virtualListPickerBindItem;
  virtualListPicker.OnSelectionChanged := virtualListPickerSelectionChanged;
end;

procedure TFormMain.virtualListPickerGetItemCount(Sender: TObject; var ACount: Integer);
begin
  ACount := Length(FCountries);
end;

procedure TFormMain.virtualListPickerBindItem(Sender: TObject; const AIndex: Integer; const AItem: TfgItemWrapper);
var
  lblCountry: TfgLabel;
begin
  lblCountry := AItem.GetControlByLookupName<TfgLabel>('country_name');
  lblCountry.Text := FCountries[AIndex];
end;

procedure TFormMain.virtualListPickerSelectionChanged(Sender: TObject);
begin
  lblSelected.Text := FCountries[virtualListPicker.SelectedIndex];
end;
```

---

## 16. Push Notifications

### 16.1 Firebase Push Notifications

**Sample Location**: `PushNotification - Receive`

Receive push notifications via Firebase:

```pascal
type
  TFormMain = class(TfgForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure RegisterForPushNotifications;
    procedure PushNotificationReceived(const ANotification: TfgPushNotification);
    procedure TokenReceived(const AToken: string);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  RegisterForPushNotifications;
end;

procedure TFormMain.RegisterForPushNotifications;
begin
  // Request permission for notifications
  TfgPushNotificationService.RequestPermission(
    procedure(const AGranted: Boolean)
    begin
      if AGranted then
      begin
        // Register for push notifications
        TfgPushNotificationService.RegisterForNotifications;
        
        // Set event handlers
        TfgPushNotificationService.OnTokenReceived := TokenReceived;
        TfgPushNotificationService.OnNotificationReceived := PushNotificationReceived;
      end
      else
        TfgDialogs.ShowMessage('Push notification permission denied');
    end);
end;

procedure TFormMain.TokenReceived(const AToken: string);
begin
  lblToken.Text := 'Token: ' + AToken;
  
  // Send token to your server
  SendTokenToServer(AToken);
end;

procedure TFormMain.PushNotificationReceived(const ANotification: TfgPushNotification);
begin
  // Handle notification based on app state
  if TfgApplication.State = TfgApplicationState.Active then
  begin
    // App is in foreground - show custom alert
    ShowCustomNotificationAlert(ANotification);
  end
  else
  begin
    // App was opened from notification
    HandleNotificationAction(ANotification);
  end;
end;

procedure TFormMain.ShowCustomNotificationAlert(const ANotification: TfgPushNotification);
var
  message: string;
begin
  message := Format('Title: %s'#13#10'Body: %s', [ANotification.Title, ANotification.Body]);
  
  TfgDialogs.ShowConfirmation('Push Notification', message, ['View', 'Dismiss'],
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrOK then
        HandleNotificationAction(ANotification);
    end);
end;

procedure TFormMain.HandleNotificationAction(const ANotification: TfgPushNotification);
var
  actionType: string;
begin
  // Extract custom data
  if ANotification.Data.TryGetValue('action', actionType) then
  begin
    if actionType = 'open_chat' then
      OpenChatScreen(ANotification.Data['chat_id'])
    else if actionType = 'open_product' then
      OpenProductScreen(ANotification.Data['product_id'])
    else if actionType = 'open_url' then
      OpenWebBrowser(ANotification.Data['url']);
  end;
end;

procedure TFormMain.SendTokenToServer(const AToken: string);
var
  httpClient: TfgHTTPClient;
  requestBody: string;
begin
  httpClient := TfgHTTPClient.Create;
  try
    requestBody := Format('{"device_token":"%s","platform":"%s"}', [
      AToken,
      {$IFDEF ANDROID}'android'{$ELSE}'ios'{$ENDIF}
    ]);
    
    httpClient.PostAsync('https://yourserver.com/api/register-device', requestBody,
      procedure(const AResponse: TfgHTTPResponse)
      begin
        if AResponse.StatusCode = 200 then
          lblStatus.Text := 'Device registered successfully'
        else
          lblStatus.Text := 'Registration failed: ' + AResponse.StatusText;
      end);
  finally
    httpClient.Free;
  end;
end;
```

### 16.2 RuStore Push Notifications

**Sample Location**: `PushNotification - RuStore`

Integration with RuStore push notification service:

```pascal
procedure TFormMain.SetupRuStorePush;
begin
  // Configure RuStore push notifications
  TfgRuStorePushService.Configure('your_project_id');
  TfgRuStorePushService.OnTokenReceived := RuStoreTokenReceived;
  TfgRuStorePushService.OnNotificationReceived := RuStoreNotificationReceived;
  
  // Register for notifications
  TfgRuStorePushService.Register;
end;

procedure TFormMain.RuStoreTokenReceived(const AToken: string);
begin
  lblRuStoreToken.Text := 'RuStore Token: ' + AToken;
  
  // Send to your backend
  RegisterRuStoreDevice(AToken);
end;

procedure TFormMain.RuStoreNotificationReceived(const ANotification: TfgRuStorePushNotification);
begin
  // Handle RuStore-specific notification format
  lblLastNotification.Text := Format('RuStore: %s - %s', [
    ANotification.Title,
    ANotification.Body
  ]);
end;
```

---

## 17. Advertising Integration

### 17.1 Google AdMob Banner Ads

**Sample Location**: `Advertising Google - Banner`

Display banner advertisements:

```pascal
type
  TFormMain = class(TfgForm)
    bannerAd: TfgBannerAd;
    procedure FormCreate(Sender: TObject);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  SetupBannerAd;
end;

procedure TFormMain.SetupBannerAd;
begin
  // Configure AdMob banner
  bannerAd.AdUnitID := 'ca-app-pub-3940256099942544/6300978111'; // Test ID
  bannerAd.AdSize := TfgAdSize.Banner;
  bannerAd.Position := TfgAdPosition.Bottom;
  
  // Set event handlers
  bannerAd.OnAdLoaded := BannerAdLoaded;
  bannerAd.OnAdFailedToLoad := BannerAdFailedToLoad;
  bannerAd.OnAdClicked := BannerAdClicked;
  
  // Load the ad
  bannerAd.LoadAd;
end;

procedure TFormMain.BannerAdLoaded(Sender: TObject);
begin
  lblAdStatus.Text := 'Banner ad loaded successfully';
  bannerAd.Show;
end;

procedure TFormMain.BannerAdFailedToLoad(Sender: TObject; const AError: string);
begin
  lblAdStatus.Text := 'Failed to load banner ad: ' + AError;
end;

procedure TFormMain.BannerAdClicked(Sender: TObject);
begin
  // Track ad click analytics
  TrackAdClick('banner', bannerAd.AdUnitID);
end;
```

### 17.2 Interstitial Ads

**Sample Location**: `Advertising Google - InterstitialAd`

Full-screen interstitial advertisements:

```pascal
type
  TFormMain = class(TfgForm)
    interstitialAd: TfgInterstitialAd;
    btnShowAd: TfgButton;
    procedure btnShowAdTap(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  SetupInterstitialAd;
end;

procedure TFormMain.SetupInterstitialAd;
begin
  // Configure interstitial ad
  interstitialAd.AdUnitID := 'ca-app-pub-3940256099942544/1033173712'; // Test ID
  
  // Set event handlers
  interstitialAd.OnAdLoaded := InterstitialAdLoaded;
  interstitialAd.OnAdFailedToLoad := InterstitialAdFailedToLoad;
  interstitialAd.OnAdDismissed := InterstitialAdDismissed;
  interstitialAd.OnAdShown := InterstitialAdShown;
  
  // Preload the ad
  interstitialAd.LoadAd;
end;

procedure TFormMain.btnShowAdTap(Sender: TObject);
begin
  if interstitialAd.IsLoaded then
    interstitialAd.Show
  else
  begin
    lblStatus.Text := 'Ad not ready, loading...';
    interstitialAd.LoadAd;
  end;
end;

procedure TFormMain.InterstitialAdLoaded(Sender: TObject);
begin
  lblStatus.Text := 'Interstitial ad ready';
  btnShowAd.Enabled := True;
end;

procedure TFormMain.InterstitialAdFailedToLoad(Sender: TObject; const AError: string);
begin
  lblStatus.Text := 'Failed to load interstitial: ' + AError;
  btnShowAd.Enabled := False;
end;

procedure TFormMain.InterstitialAdShown(Sender: TObject);
begin
  lblStatus.Text := 'Interstitial ad shown';
  
  // Track impression
  TrackAdImpression('interstitial', interstitialAd.AdUnitID);
end;

procedure TFormMain.InterstitialAdDismissed(Sender: TObject);
begin
  lblStatus.Text := 'Interstitial ad dismissed';
  
  // Preload next ad
  interstitialAd.LoadAd;
  
  // Continue with app flow
  ProceedWithGameLevel;
end;
```

### 17.3 Yandex Advertising

**Sample Location**: `Advertising Yandex - Banner`

Yandex advertising integration for Russian market:

```pascal
procedure TFormMain.SetupYandexAd;
begin
  yandexBanner.AdUnitID := 'your_yandex_ad_unit_id';
  yandexBanner.AdSize := TfgYandexAdSize.Banner_320x50;
  
  yandexBanner.OnAdLoaded := YandexAdLoaded;
  yandexBanner.OnAdFailedToLoad := YandexAdFailedToLoad;
  
  yandexBanner.LoadAd;
end;

procedure TFormMain.YandexAdLoaded(Sender: TObject);
begin
  lblYandexStatus.Text := 'Yandex ad loaded';
  yandexBanner.Show;
end;

procedure TFormMain.YandexAdFailedToLoad(Sender: TObject; const AError: string);
begin
  lblYandexStatus.Text := 'Yandex ad failed: ' + AError;
end;
```

---

## 18. Android-Specific Features

### 18.1 Alarm Manager

**Sample Location**: `Android - Alarm Manager`

Schedule alarms and background tasks:

```pascal
{$IFDEF ANDROID}
uses
  FGX.Platform.Android.AlarmManager;
{$ENDIF}

procedure TFormMain.ScheduleAlarm;
{$IFDEF ANDROID}
var
  alarmTime: TDateTime;
  alarmIntent: TfgAlarmIntent;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  alarmTime := Now + (1/24); // 1 hour from now
  
  alarmIntent := TfgAlarmIntent.Create;
  try
    alarmIntent.Action := 'com.myapp.ALARM_ACTION';
    alarmIntent.Title := 'Scheduled Reminder';
    alarmIntent.Message := 'This is your scheduled reminder';
    
    TfgAlarmManager.ScheduleAlarm(alarmTime, alarmIntent);
    lblStatus.Text := 'Alarm scheduled for ' + DateTimeToStr(alarmTime);
  finally
    alarmIntent.Free;
  end;
  {$ELSE}
  TfgDialogs.ShowMessage('Alarm Manager is Android-only feature');
  {$ENDIF}
end;

procedure TFormMain.CancelAlarm;
{$IFDEF ANDROID}
var
  alarmIntent: TfgAlarmIntent;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  alarmIntent := TfgAlarmIntent.Create;
  try
    alarmIntent.Action := 'com.myapp.ALARM_ACTION';
    TfgAlarmManager.CancelAlarm(alarmIntent);
    lblStatus.Text := 'Alarm cancelled';
  finally
    alarmIntent.Free;
  end;
  {$ENDIF}
end;
```

### 18.2 Broadcast Receiver

**Sample Location**: `Android - Broadcast receiver`

Listen for system broadcasts:

```pascal
{$IFDEF ANDROID}
uses
  FGX.Platform.Android.BroadcastReceiver;
{$ENDIF}

type
  TFormMain = class(TfgForm)
  private
    {$IFDEF ANDROID}
    FBroadcastReceiver: TfgBroadcastReceiver;
    {$ENDIF}
    procedure OnBroadcastReceived(const AAction: string; const AExtras: TfgBundle);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  {$IFDEF ANDROID}
  SetupBroadcastReceiver;
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TFormMain.SetupBroadcastReceiver;
begin
  FBroadcastReceiver := TfgBroadcastReceiver.Create;
  FBroadcastReceiver.OnReceive := OnBroadcastReceived;
  
  // Register for system broadcasts
  FBroadcastReceiver.RegisterAction('android.intent.action.BATTERY_LOW');
  FBroadcastReceiver.RegisterAction('android.intent.action.BATTERY_OKAY');
  FBroadcastReceiver.RegisterAction('android.intent.action.POWER_CONNECTED');
  FBroadcastReceiver.RegisterAction('android.intent.action.POWER_DISCONNECTED');
  FBroadcastReceiver.RegisterAction('android.net.conn.CONNECTIVITY_CHANGE');
end;
{$ENDIF}

procedure TFormMain.OnBroadcastReceived(const AAction: string; const AExtras: TfgBundle);
begin
  case AAction of
    'android.intent.action.BATTERY_LOW':
      lblBroadcast.Text := 'Battery is low';
    'android.intent.action.BATTERY_OKAY':
      lblBroadcast.Text := 'Battery level is okay';
    'android.intent.action.POWER_CONNECTED':
      lblBroadcast.Text := 'Power connected';
    'android.intent.action.POWER_DISCONNECTED':
      lblBroadcast.Text := 'Power disconnected';
    'android.net.conn.CONNECTIVITY_CHANGE':
      CheckNetworkConnectivity;
  end;
end;

procedure TFormMain.CheckNetworkConnectivity;
{$IFDEF ANDROID}
var
  connected: Boolean;
{$ENDIF}
begin
  {$IFDEF ANDROID}
  connected := TfgNetworkInfo.IsConnected;
  lblNetworkStatus.Text := 'Network: ' + IfThen(connected, 'Connected', 'Disconnected');
  {$ENDIF}
end;
```

### 18.3 Immersive Mode

**Sample Location**: `Android - Immersive Mode`

Full-screen immersive experience:

```pascal
{$IFDEF ANDROID}
uses
  FGX.Platform.Android.ImmersiveMode;
{$ENDIF}

procedure TFormMain.EnableImmersiveMode;
begin
  {$IFDEF ANDROID}
  TfgImmersiveMode.Enable([
    TfgImmersiveFlag.HideNavigation,
    TfgImmersiveFlag.HideStatusBar,
    TfgImmersiveFlag.Sticky
  ]);
  lblStatus.Text := 'Immersive mode enabled';
  {$ENDIF}
end;

procedure TFormMain.DisableImmersiveMode;
begin
  {$IFDEF ANDROID}
  TfgImmersiveMode.Disable;
  lblStatus.Text := 'Immersive mode disabled';
  {$ENDIF}
end;

procedure TFormMain.ToggleImmersiveMode;
begin
  {$IFDEF ANDROID}
  if TfgImmersiveMode.IsEnabled then
    DisableImmersiveMode
  else
    EnableImmersiveMode;
  {$ENDIF}
end;
```

### 18.4 Intent Handling

**Sample Location**: `Android - Receiving intent`

Handle incoming intents:

```pascal
{$IFDEF ANDROID}
uses
  FGX.Platform.Android.Intent;
{$ENDIF}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  {$IFDEF ANDROID}
  CheckForLaunchIntent;
  TfgApplication.OnNewIntent := OnNewIntent;
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TFormMain.CheckForLaunchIntent;
var
  intent: TfgIntent;
begin
  intent := TfgApplication.LaunchIntent;
  if Assigned(intent) then
    ProcessIntent(intent);
end;

procedure TFormMain.OnNewIntent(const AIntent: TfgIntent);
begin
  ProcessIntent(AIntent);
end;

procedure TFormMain.ProcessIntent(const AIntent: TfgIntent);
var
  action: string;
  data: string;
  textShared: string;
begin
  action := AIntent.Action;
  data := AIntent.DataString;
  
  case action of
    'android.intent.action.VIEW':
      begin
        // Handle deep link
        lblIntent.Text := 'Deep link: ' + data;
        HandleDeepLink(data);
      end;
    'android.intent.action.SEND':
      begin
        // Handle shared content
        if AIntent.GetStringExtra('android.intent.extra.TEXT', textShared) then
        begin
          lblIntent.Text := 'Shared text: ' + textShared;
          HandleSharedText(textShared);
        end;
      end;
    'android.intent.action.SEND_MULTIPLE':
      begin
        // Handle multiple shared items
        HandleMultipleSharedItems(AIntent);
      end;
  end;
end;

procedure TFormMain.HandleDeepLink(const AURL: string);
var
  uri: TURI;
begin
  uri := TURI.Create(AURL);
  
  // Parse deep link parameters
  if uri.Host = 'product' then
    OpenProduct(uri.Params.Values['id'])
  else if uri.Host = 'user' then
    OpenUserProfile(uri.Params.Values['id']);
end;
{$ENDIF}
```

### 18.5 Animated Splash Screen

**Sample Location**: `Android - Animated Splash Screen`

Create animated splash screens:

```pascal
{$IFDEF ANDROID}
uses
  FGX.Platform.Android.SplashScreen;
{$ENDIF}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  {$IFDEF ANDROID}
  ConfigureSplashScreen;
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TFormMain.ConfigureSplashScreen;
begin
  // Configure animated splash screen
  TfgSplashScreen.SetAnimatedIcon('splash_icon.xml');
  TfgSplashScreen.SetBackgroundColor($FF2196F3); // Material Blue
  TfgSplashScreen.SetIconAnimation('scale_up');
  TfgSplashScreen.SetDuration(2000); // 2 seconds
  
  // Set callback for when splash is ready to hide
  TfgSplashScreen.OnReadyToHide := SplashScreenReadyToHide;
end;

procedure TFormMain.SplashScreenReadyToHide;
begin
  // Perform initialization
  InitializeApplication;
  
  // Hide splash screen
  TfgSplashScreen.Hide;
end;
{$ENDIF}
```

---

## 19. Advanced Topics

### 19.1 Multi-Touch and Touch Handling

**Sample Location**: `Touches - MultiTouches`

Advanced multi-touch gesture handling:

```pascal
type
  TFormMain = class(TfgForm)
    procedure FormMultiTouch(Sender: TObject; const ATouches: TArray<TfgTouch>; 
      const AAction: TfgTouchAction);
  private
    FTouches: TDictionary<Integer, TfgTouch>;
    procedure HandlePinchGesture(const ATouches: TArray<TfgTouch>);
    procedure HandleRotationGesture(const ATouches: TArray<TfgTouch>);
  end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FTouches := TDictionary<Integer, TfgTouch>.Create;
end;

procedure TFormMain.FormMultiTouch(Sender: TObject; const ATouches: TArray<TfgTouch>; 
  const AAction: TfgTouchAction);
var
  touch: TfgTouch;
begin
  case AAction of
    TfgTouchAction.Down:
      begin
        for touch in ATouches do
          FTouches.AddOrSetValue(touch.ID, touch);
      end;
    TfgTouchAction.Move:
      begin
        for touch in ATouches do
          FTouches.AddOrSetValue(touch.ID, touch);
          
        // Handle gestures based on touch count
        case FTouches.Count of
          2: HandlePinchGesture(ATouches);
          3: HandleRotationGesture(ATouches);
        end;
      end;
    TfgTouchAction.Up:
      begin
        for touch in ATouches do
          FTouches.Remove(touch.ID);
      end;
  end;
  
  lblTouchCount.Text := Format('%d active touches', [FTouches.Count]);
end;

procedure TFormMain.HandlePinchGesture(const ATouches: TArray<TfgTouch>);
var
  distance: Single;
  midPoint: TPointF;
begin
  if Length(ATouches) >= 2 then
  begin
    distance := ATouches[0].Location.Distance(ATouches[1].Location);
    midPoint := TPointF.Create(
      (ATouches[0].Location.X + ATouches[1].Location.X) / 2,
      (ATouches[0].Location.Y + ATouches[1].Location.Y) / 2
    );
    
    // Apply zoom based on distance
    imgTarget.Scale.X := distance / 100;
    imgTarget.Scale.Y := distance / 100;
    imgTarget.Position.Point := midPoint;
  end;
end;
```

### 19.2 Timer Services

**Sample Location**: `Timer`

Efficient timer management:

```pascal
type
  TFormMain = class(TfgForm)
    timer1: TfgTimer;
    timer2: TfgTimer;
    procedure timer1Timer(Sender: TObject);
    procedure timer2Timer(Sender: TObject);
  private
    FStartTime: TDateTime;
    FUpdateCount: Integer;
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  inherited;
  SetupTimers;
end;

procedure TFormMain.SetupTimers;
begin
  // High frequency timer for animations
  timer1.Interval := 16; // ~60 FPS
  timer1.OnTimer := timer1Timer;
  
  // Low frequency timer for periodic updates
  timer2.Interval := 1000; // 1 second
  timer2.OnTimer := timer2Timer;
  
  FStartTime := Now;
end;

procedure TFormMain.timer1Timer(Sender: TObject);
begin
  Inc(FUpdateCount);
  
  // Smooth animation updates
  UpdateAnimations;
end;

procedure TFormMain.timer2Timer(Sender: TObject);
var
  elapsed: TDateTime;
begin
  elapsed := Now - FStartTime;
  lblElapsed.Text := FormatDateTime('hh:nn:ss', elapsed);
  lblFPS.Text := Format('Updates: %d', [FUpdateCount]);
  FUpdateCount := 0;
end;

procedure TFormMain.UpdateAnimations;
var
  progress: Single;
begin
  progress := Frac(Now * 24 * 60); // Cycle every minute
  
  // Rotate an element
  imgRotating.RotationAngle := progress * 360;
  
  // Pulse effect
  btnPulse.Scale.X := 1 + Sin(progress * 2 * Pi) * 0.1;
  btnPulse.Scale.Y := btnPulse.Scale.X;
end;
```

### 19.3 Sharing Services

**Sample Location**: `Share - Base`

Content sharing functionality:

```pascal
type
  TFormMain = class(TfgForm)
    btnShareText: TfgButton;
    btnShareImage: TfgButton;
    btnShareFile: TfgButton;
    procedure btnShareTextTap(Sender: TObject);
    procedure btnShareImageTap(Sender: TObject);
    procedure btnShareFileTap(Sender: TObject);
  end;

procedure TFormMain.btnShareTextTap(Sender: TObject);
var
  shareText: string;
begin
  shareText := 'Check out this amazing app built with FGX Native!';
  
  TfgShareService.ShareText(shareText,
    procedure(const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
        lblStatus.Text := 'Text shared successfully'
      else
        lblStatus.Text := 'Share failed: ' + AError;
    end);
end;

procedure TFormMain.btnShareImageTap(Sender: TObject);
var
  bitmap: TfgBitmap;
begin
  // Create or get image to share
  bitmap := imgToShare.Bitmap;
  
  TfgShareService.ShareImage(bitmap, 'Amazing screenshot from FGX Native app',
    procedure(const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
        lblStatus.Text := 'Image shared successfully'
      else
        lblStatus.Text := 'Share failed: ' + AError;
    end);
end;

procedure TFormMain.btnShareFileTap(Sender: TObject);
var
  filePath: string;
begin
  filePath := TfgAssets.GetAssetPath('sample_document.pdf');
  
  TfgShareService.ShareFile(filePath, 'application/pdf',
    procedure(const ASuccess: Boolean; const AError: string)
    begin
      if ASuccess then
        lblStatus.Text := 'File shared successfully'
      else
        lblStatus.Text := 'Share failed: ' + AError;
    end);
end;

// Advanced sharing with multiple items
procedure TFormMain.ShareMultipleItems;
var
  shareData: TfgShareData;
begin
  shareData := TfgShareData.Create;
  try
    shareData.Text := 'Check out these amazing photos!';
    shareData.Subject := 'Photos from FGX Native App';
    shareData.Images.Add(img1.Bitmap);
    shareData.Images.Add(img2.Bitmap);
    shareData.Files.Add('document1.pdf');
    shareData.Files.Add('document2.txt');
    
    TfgShareService.ShareMultiple(shareData,
      procedure(const ASuccess: Boolean; const AError: string)
      begin
        if ASuccess then
          lblStatus.Text := 'Multiple items shared successfully'
        else
          lblStatus.Text := 'Share failed: ' + AError;
      end);
  finally
    shareData.Free;
  end;
end;
```

### 19.4 Additional UI Controls

#### Switch Controls

**Sample Location**: `Switch - Base`

```pascal
procedure TFormMain.SetupSwitches;
begin
  // Basic switch
  switch1.IsChecked := True;
  switch1.OnChanged := Switch1Changed;
  
  // Customized switch
  switch2.TintColor := TAlphaColors.Green;
  switch2.ThumbTintColor := TAlphaColors.White;
  switch2.OnTintColor := TAlphaColors.Lightgreen;
end;

procedure TFormMain.Switch1Changed(Sender: TObject);
begin
  lblSwitchStatus.Text := 'Switch 1: ' + IfThen(switch1.IsChecked, 'ON', 'OFF');
  
  // Toggle dependent controls
  panel1.Visible := switch1.IsChecked;
end;
```

#### TrackBar Controls

**Sample Location**: `Trackbar - Base`

```pascal
procedure TFormMain.SetupTrackBars;
begin
  // Volume control
  trackBarVolume.Min := 0;
  trackBarVolume.Max := 100;
  trackBarVolume.Value := 50;
  trackBarVolume.OnTracking := TrackBarVolumeTracking;
  trackBarVolume.OnChange := TrackBarVolumeChange;
  
  // Brightness control
  trackBarBrightness.Min := 0.1;
  trackBarBrightness.Max := 1.0;
  trackBarBrightness.Value := 0.8;
  trackBarBrightness.OnChange := TrackBarBrightnessChange;
end;

procedure TFormMain.TrackBarVolumeTracking(Sender: TObject);
begin
  lblVolumeValue.Text := Format('Volume: %.0f%%', [trackBarVolume.Value]);
end;

procedure TFormMain.TrackBarVolumeChange(Sender: TObject);
begin
  // Apply volume setting
  SetSystemVolume(trackBarVolume.Value / 100);
  lblVolumeValue.Text := Format('Volume: %.0f%%', [trackBarVolume.Value]);
end;

procedure TFormMain.TrackBarBrightnessChange(Sender: TObject);
begin
  // Apply brightness to UI
  pnlContent.Opacity := trackBarBrightness.Value;
  lblBrightnessValue.Text := Format('Brightness: %.0f%%', [trackBarBrightness.Value * 100]);
end;
```

#### Time Picker

**Sample Location**: `TimePicker - Base`

```pascal
procedure TFormMain.SetupTimePicker;
begin
  timePicker.Time := Time;
  timePicker.Format := TfgTimeFormat.Hour24;
  timePicker.OnTimeChanged := TimePickerChanged;
end;

procedure TFormMain.TimePickerChanged(Sender: TObject);
begin
  lblSelectedTime.Text := TimeToStr(timePicker.Time);
  
  // Schedule notification for selected time
  ScheduleNotificationAt(timePicker.Time);
end;

procedure TFormMain.ShowTimePickerDialog;
begin
  TfgPickerService.ShowTimePicker(Time,
    procedure(const ASelectedTime: TTime; const ACancelled: Boolean)
    begin
      if not ACancelled then
      begin
        timePicker.Time := ASelectedTime;
        lblResult.Text := 'Selected: ' + TimeToStr(ASelectedTime);
      end;
    end);
end;
```

### 19.5 Popup and Menu Systems

**Sample Location**: `Popup - Base`

Advanced popup and menu implementations:

```pascal
type
  TFormMain = class(TfgForm)
    btnShowPopup: TfgButton;
    popup: TfgPopup;
    frameList: TfgFrameList;
    procedure btnShowPopupTap(Sender: TObject);
    procedure PopupItemSelected(Sender: TObject; const AIndex: Integer);
  end;

procedure TFormMain.SetupPopup;
begin
  popup.PlacementTarget := btnShowPopup;
  popup.Placement := TfgPopupPlacement.Bottom;
  popup.OffsetX := 0;
  popup.OffsetY := 5;
  
  // Setup list frame in popup
  frameList.OnItemTap := PopupItemSelected;
  frameList.Items.Add('Copy');
  frameList.Items.Add('Paste');
  frameList.Items.Add('Delete');
  frameList.Items.Add('Properties');
end;

procedure TFormMain.btnShowPopupTap(Sender: TObject);
var
  popupPoint: TPointF;
begin
  // Calculate popup position
  popupPoint := btnShowPopup.LocalToAbsolute(TPointF.Create(0, btnShowPopup.Height));
  
  popup.ShowAt(popupPoint);
end;

procedure TFormMain.PopupItemSelected(Sender: TObject; const AIndex: Integer);
begin
  popup.Hide;
  
  case AIndex of
    0: ExecuteCopy;
    1: ExecutePaste;
    2: ExecuteDelete;
    3: ShowProperties;
  end;
end;

// Context menu system
procedure TFormMain.ShowContextMenu(const APosition: TPointF);
var
  contextMenu: TfgContextMenu;
begin
  contextMenu := TfgContextMenu.Create(Self);
  try
    contextMenu.AddItem('Cut', CutAction);
    contextMenu.AddItem('Copy', CopyAction);
    contextMenu.AddItem('Paste', PasteAction);
    contextMenu.AddSeparator;
    contextMenu.AddItem('Select All', SelectAllAction);
    
    contextMenu.ShowAt(APosition);
  finally
    contextMenu.Free;
  end;
end;
```

---

## 20. Advanced Architectural Patterns

### 20.1 MVVM Architecture with LiveData

Based on advanced samples, FGX Native supports modern MVVM patterns:

```pascal
// ViewModel.Base.pas - Base ViewModel class
type
  TViewModelBase = class
  private
    FPropertyChanged: TNotifyEvent;
  protected
    procedure NotifyPropertyChanged;
    property OnPropertyChanged: TNotifyEvent read FPropertyChanged write FPropertyChanged;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

// Usage in forms with data binding
type
  TFormMain = class(TfgForm)
  private
    FViewModel: TMainViewModel;
    procedure BindViewModel;
    procedure ViewModelPropertyChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TFormMain.BindViewModel;
begin
  // Bind CollectionView to ViewModel
  collectionView.ItemsSource := FViewModel.Items;
  collectionView.OnBindItem := 
    procedure(Sender: TObject; const AIndex: Integer; const AItem: TfgItemWrapper)
    var
      dataItem: TDataItem;
    begin
      dataItem := FViewModel.Items[AIndex];
      AItem.GetControlByLookupName<TfgLabel>('title').Text := dataItem.Title;
      AItem.GetControlByLookupName<TfgLabel>('description').Text := dataItem.Description;
    end;
end;
```

### 20.2 Service Layer Architecture

```pascal
// Service.Container.pas - Dependency injection
type
  TServiceContainer = class
  private
    class var FInstance: TServiceContainer;
    FServices: TDictionary<TGUID, TObject>;
  public
    class function Instance: TServiceContainer;
    procedure RegisterSingleton<T: IInterface>(const AInstance: T);
    function Resolve<T: IInterface>: T;
  end;

// Application setup
procedure TFormMain.ConfigureServices;
begin
  TServiceContainer.Instance.RegisterSingleton<IDataService>(TDataService.Create);
  TServiceContainer.Instance.RegisterSingleton<INetworkService>(TNetworkService.Create);
end;
```

### 20.3 Performance Optimization Techniques

```pascal
// Memory management optimizations
procedure TFormMain.OptimizeMemoryUsage;
begin
  // Release unused assets
  TfgAssets.ReleaseUnusedAssets;
  
  // Use virtual components for large datasets
  collectionView.VirtualMode := True;
  
  // Optimize image loading
  imgLarge.LoadAsync := True;
end;
```

---

## 21. Best Practices and Guidelines

**Recommended folder structure for complex projects**:

```
Project/
├── Source/
│   ├── Forms/                  # All form files
│   │   ├── Form.Main.pas
│   │   ├── Form.Settings.pas
│   │   └── Form.About.pas
│   ├── Frames/                 # Reusable frames
│   │   ├── Frame.ProductCard.pas
│   │   └── Frame.UserProfile.pas
│   ├── Data/                   # Data layer
│   │   ├── Data.Repository.pas
│   │   ├── Data.Models.pas
│   │   └── Data.Services.pas
│   ├── Utils/                  # Utility classes
│   │   ├── Utils.Helper.pas
│   │   └── Utils.Constants.pas
│   └── ViewModels/             # MVVM ViewModels
│       ├── ViewModel.Main.pas
│       └── ViewModel.Settings.pas
├── Assets/                     # Asset files
│   ├── Images/
│   ├── Fonts/
│   ├── Data/
│   └── Animations/
└── Platform/                   # Platform-specific files
    ├── Android/
    └── iOS/
```

### 15.2 Performance Optimization

```pascal
// Use virtual lists for large datasets
procedure TFormMain.OptimizeCollectionView;
begin
  collectionView.VirtualMode := True;
  collectionView.CacheSize := 50;
  collectionView.OnNeedItem := CollectionViewNeedItem;
end;

// Lazy load images
procedure TFormMain.LazyLoadImages;
begin
  // Only load images when they become visible
  collectionView.OnItemVisible := ItemBecameVisible;
end;

procedure TFormMain.ItemBecameVisible(Sender: TObject; const AIndex: Integer);
var
  item: TfgItemWrapper;
  img: TfgImage;
begin
  item := collectionView.GetItem(AIndex);
  img := item.GetControlByLookupName<TfgImage>('photo');
  
  if not Assigned(img.Bitmap) then
    LoadImageAsync(AIndex, img);
end;

// Memory management
procedure TFormMain.ManageMemory;
begin
  // Release unused resources
  TfgAssets.ReleaseUnusedAssets;
  
  // Use weak references for callbacks
  WeakSelf := TWeakReference<TFormMain>.Create(Self);
  
  // Clean up in FormDestroy
end;
```

### 15.3 Error Handling

```pascal
procedure TFormMain.SafeOperation;
begin
  try
    // Risky operation
    PerformNetworkRequest;
  except
    on E: Exception do
    begin
      // Log error
      TfgLog.Error('Operation failed: ' + E.Message);
      
      // Show user-friendly message
      TfgDialogs.ShowMessage('Something went wrong. Please try again.');
    end;
  end;
end;

// Async error handling
procedure TFormMain.SafeAsyncOperation;
begin
  TTask.Run(
    procedure
    begin
      try
        // Background work
        PerformLongRunningTask;
        
        // Update UI on main thread
        TThread.Synchronize(nil,
          procedure
          begin
            UpdateUI;
          end);
      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              HandleError(E.Message);
            end);
        end;
      end;
    end);
end;
```

### 15.4 Testing

```pascal
// Unit testing forms
unit Test.Form.Main;

interface

uses
  DUnitX.TestFramework, Form.Main;

type
  [TestFixture]
  TFormMainTests = class
  private
    FForm: TFormMain;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;
    [Test]
    procedure TestButtonClick;
    [Test]
    procedure TestDataBinding;
  end;

implementation

procedure TFormMainTests.Setup;
begin
  FForm := TFormMain.Create(nil);
end;

procedure TFormMainTests.TearDown;
begin
  FForm.Free;
end;

procedure TFormMainTests.TestButtonClick;
begin
  // Arrange
  FForm.Show;
  
  // Act
  FForm.btnTest.OnTap(FForm.btnTest);
  
  // Assert
  Assert.AreEqual('Expected Text', FForm.lblResult.Text);
end;
```

### 15.5 Platform-Specific Code

```pascal
// Conditional compilation for platform differences
procedure TFormMain.PlatformSpecificCode;
begin
  {$IFDEF ANDROID}
  // Android-specific implementation
  SetupAndroidFeatures;
  {$ENDIF}
  
  {$IFDEF IOS}
  // iOS-specific implementation
  SetupIOSFeatures;
  {$ENDIF}
end;

// Feature detection
procedure TFormMain.SetupConditionalFeatures;
begin
  if TfgPlatform.HasTouchID then
    EnableBiometricLogin;
    
  if TfgPlatform.HasCamera then
    ShowCameraButton;
    
  if TfgPlatform.HasGPS then
    EnableLocationFeatures;
end;
```

---

## Conclusion

This comprehensive tutorial covers the extensive capabilities of the FGX Native framework for cross-platform mobile development, based on analysis of **105+ sample applications** that demonstrate every aspect of the framework.

## Sample Coverage Summary

### Core UI Components (25+ samples)
- ActivityIndicator, Labels, Buttons, Cards, Images, Navigation
- Form management, Drawers, Menus, Input controls
- Pickers, ComboBoxes, Switches, TrackBars, TimePickers

### Data and Collections (15+ samples)  
- CollectionView variations (simple lists, columns, filtering, selection, reordering)
- Virtual components for performance optimization
- Pull-to-refresh, dynamic loading, multiple item styles

### Layout Systems (10+ samples)
- Flexible FlexBox layouts with auto-wrap, grow, positioning
- Navigation structures, responsive design patterns
- Safe area handling, screen adaptation for keyboards

### Platform Integration (20+ samples)
- Android-specific features (Alarm Manager, Broadcast Receiver, Immersive Mode, Intent handling)
- iOS features (biometric authentication, status bar control, safe areas)
- Device information, permissions management, phone integration

### Media and Graphics (15+ samples)
- Camera preview and photo/video capture
- Photo/video pickers, signature capture, region picking
- Canvas operations, bitmap manipulation, control screenshots
- Lottie animations, scratch overlays

### Services and Communication (15+ samples)
- Push notifications (Firebase, RuStore)
- Web browser integration with JavaScript execution
- Biometric authentication, local notifications
- Barcode scanning, sharing services, advertising integration

### Advanced Features (15+ samples)
- Virtual pager layouts with efficient memory management
- Multi-touch handling, gesture recognition systems
- Timer services, popup and menu systems
- Theme management, asset optimization

## Key Architectural Patterns

### ItemStyle + BindItem Pattern
The cornerstone of FGX Native's data-driven UI approach:
```pascal
// Template-based UI with type-safe data binding
procedure TFormMain.BindItem(const AIndex: Integer; const AItem: TfgItemWrapper);
begin
  AItem.GetControlByLookupName<TfgLabel>('title').Text := FData[AIndex].Title;
end;
```

### Advanced Architecture Support
- **MVVM with LiveData**: Reactive UI patterns for modern apps
- **Repository Pattern**: Clean data layer architecture  
- **Service Registration**: Dependency injection for maintainable code
- **Asset Management**: Automated resource handling with compile-time safety
- **Theme System**: Dynamic light/dark themes with system integration

## Production-Ready Features

### Performance Optimization
- Virtual lists for datasets of any size
- Lazy loading and efficient memory management
- Background task processing with proper threading
- Asset caching and release strategies

### Cross-Platform Excellence
- Single codebase targeting iOS and Android
- Platform-specific features with conditional compilation
- Native performance with modern UI patterns
- Consistent behavior across all supported platforms

### Professional Development Features
- Comprehensive error handling patterns
- Unit testing framework integration
- Build system optimization for multiple platforms
- Deployment configuration for app stores

## Learning Path Recommendations

### Beginner (Weeks 1-2)
Start with basic UI components and simple layouts:
- ActivityIndicator, Labels, Buttons (basic interaction)
- Simple CollectionView lists (data display)
- Form management and navigation

### Intermediate (Weeks 3-6)
Add data integration and platform features:
- Advanced CollectionView patterns (filtering, selection)
- Camera integration and media handling
- Permission management and device features

### Advanced (Weeks 7-12)
Implement complex features and optimizations:
- Custom animations and theme systems
- Platform-specific features and services
- Performance optimization techniques

### Expert (Ongoing)
Build production applications:
- Complex application architecture patterns
- Advanced platform integration
- Custom component development

## Framework Strengths

1. **Comprehensive Component Library**: 50+ UI components covering all mobile app needs
2. **Elegant Data Binding**: The ItemStyle + BindItem pattern provides clean separation of UI and data
3. **Platform Abstraction**: Write once, run natively on iOS and Android
4. **Performance Focus**: Virtual components and memory management for smooth UX
5. **Modern Patterns**: Support for MVVM, reactive programming, and dependency injection
6. **Extensive Documentation**: 105+ working samples covering every feature
7. **Professional Tooling**: Asset management, theming, testing, and deployment support

## Next Steps

1. **Start Building**: Begin with simple component samples to understand the framework
2. **Master Data Display**: Learn CollectionView patterns for data-driven applications  
3. **Add Platform Features**: Integrate camera, notifications, and device-specific functionality
4. **Optimize Performance**: Apply virtual components and memory management techniques
5. **Create Production Apps**: Use advanced architecture patterns for scalable applications

The FGX Native framework, supported by this comprehensive sample collection, provides everything needed for professional cross-platform mobile development. From rapid prototyping to production deployment, the framework's combination of powerful components, elegant architecture patterns, and extensive documentation makes it an excellent choice for modern mobile app development.

The **ItemStyle + BindItem pattern**, **comprehensive platform integration**, and **performance-focused design** create a development experience that is both productive and maintainable, enabling teams to build sophisticated mobile applications with confidence.
