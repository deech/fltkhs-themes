{-# LANGUAGE RecursiveDo, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, TypeFamilies, ImplicitParams #-}
module Graphics.UI.FLTK.Theme.Simple
  (
    Graphics.UI.FLTK.Theme.Simple.Browser.browserNew,
    Graphics.UI.FLTK.Theme.Simple.Browser.fileBrowserNew,
    Graphics.UI.FLTK.Theme.Simple.Browser.selectBrowserNew,
    Graphics.UI.FLTK.Theme.Simple.Button.buttonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.checkButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.lightButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.menuButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.returnButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.roundButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Button.toggleButtonNew,
    Graphics.UI.FLTK.Theme.Simple.Clock.clockNew,
    Graphics.UI.FLTK.Theme.Simple.ColorChooser.colorChooserNew,
    Graphics.UI.FLTK.Theme.Simple.ColorChooser.flcColorChooser,
    Graphics.UI.FLTK.Theme.Simple.Common.Assets,
    Graphics.UI.FLTK.Theme.Simple.Common.loadAssets,
    Graphics.UI.FLTK.Theme.Simple.Common.dejaVuSans,
    Graphics.UI.FLTK.Theme.Simple.Counter.counterNew,
    Graphics.UI.FLTK.Theme.Simple.Dial.dialNew,
    Graphics.UI.FLTK.Theme.Simple.Dial.fillDialNew,
    Graphics.UI.FLTK.Theme.Simple.Dial.lineDialNew,
    Graphics.UI.FLTK.Theme.Simple.Group.groupNew,
    Graphics.UI.FLTK.Theme.Simple.Group.scrolledNew,
    Graphics.UI.FLTK.Theme.Simple.Input.fileInputNew,
    Graphics.UI.FLTK.Theme.Simple.Input.inputNew,
    Graphics.UI.FLTK.Theme.Simple.Input.outputNew,
    Graphics.UI.FLTK.Theme.Simple.Menu.choiceNew,
    Graphics.UI.FLTK.Theme.Simple.Menu.sysMenuBarNew,
    Graphics.UI.FLTK.Theme.Simple.Positioner.positionerNew,
    Graphics.UI.FLTK.Theme.Simple.Progress.progressNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.fillSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.gaugeSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.horFillSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.horNiceSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.horSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.horValueSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.sliderNew,
    Graphics.UI.FLTK.Theme.Simple.Slider.valueSliderNew,
    Graphics.UI.FLTK.Theme.Simple.Spinner.spinnerNew,
    Graphics.UI.FLTK.Theme.Simple.Tabs.tabsNew,
    Graphics.UI.FLTK.Theme.Simple.Tree.treeNew,
    Graphics.UI.FLTK.Theme.Simple.Valuator.adjusterNew,
    Graphics.UI.FLTK.Theme.Simple.Valuator.valueInputNew,
    Graphics.UI.FLTK.Theme.Simple.Valuator.valueOutputNew,
    Graphics.UI.FLTK.Theme.Simple.Window.doubleWindowNew,
    Graphics.UI.FLTK.Theme.Simple.Window.singleWindowNew,
    Graphics.UI.FLTK.Theme.Simple.Window.windowNew,
    -- * Ignore the rest ...
    module Graphics.UI.FLTK.LowLevel.FLTKHS
  )
where
import Graphics.UI.FLTK.LowLevel.FLTKHS hiding
  (
    adjusterNew
  , browserNew
  , buttonNew
  , checkButtonNew
  , choiceNew
  , clockNew
  , colorChooserNew
  , counterNew
  , dialNew
  , doubleWindowNew
  , fileInputNew
  , fileBrowserNew
  , fillDialNew
  , fillSliderNew
  , flcColorChooser
  , groupNew
  , horFillSliderNew
  , horNiceSliderNew
  , horSliderNew
  , horValueSliderNew
  , inputNew
  , lightButtonNew
  , lineDialNew
  , menuButtonNew
  , outputNew
  , positionerNew
  , progressNew
  , returnButtonNew
  , roundButtonNew
  , selectBrowserNew
  , scrolledNew
  , singleWindowNew
  , sliderNew
  , spinnerNew
  , sysMenuBarNew
  , tabsNew
  , toggleButtonNew
  , treeNew
  , valueInputNew
  , valueOutputNew
  , valueSliderNew
  , windowNew
  )
import Graphics.UI.FLTK.Theme.Simple.Browser
import Graphics.UI.FLTK.Theme.Simple.Button
import Graphics.UI.FLTK.Theme.Simple.Clock
import Graphics.UI.FLTK.Theme.Simple.ColorChooser
import Graphics.UI.FLTK.Theme.Simple.Common
import Graphics.UI.FLTK.Theme.Simple.Counter
import Graphics.UI.FLTK.Theme.Simple.Dial
import Graphics.UI.FLTK.Theme.Simple.Group
import Graphics.UI.FLTK.Theme.Simple.Input
import Graphics.UI.FLTK.Theme.Simple.Menu
import Graphics.UI.FLTK.Theme.Simple.Positioner
import Graphics.UI.FLTK.Theme.Simple.Progress
import Graphics.UI.FLTK.Theme.Simple.Slider
import Graphics.UI.FLTK.Theme.Simple.Spinner
import Graphics.UI.FLTK.Theme.Simple.Tabs
import Graphics.UI.FLTK.Theme.Simple.Tree
import Graphics.UI.FLTK.Theme.Simple.Valuator
import Graphics.UI.FLTK.Theme.Simple.Window
  
