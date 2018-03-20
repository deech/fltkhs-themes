{-# LANGUAGE ImplicitParams, OverloadedStrings, AllowAmbiguousTypes, GADTs, CPP, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Graphics.UI.FLTK.Theme.Simple.Button
  (
    ButtonFillSpec(..),
    buttonFillSpec,
    buttonIsHidden,
    buttonNew,
    buttonSetup,
    cancelButton,
    checkButtonNew,
    drawCheck,
    drawIndicatorButton,
    drawLight,
    drawMenuButton,
    drawRegularButton,
    drawRound,
    fillButton,
    lightButtonNew,
    makeFillSpec,
    menuButtonNew,
    okButton,
    returnButtonNew,
    roundButtonNew,
    toggleButtonNew
  )
where
import Control.Exception
import Control.Monad
import Graphics.UI.FLTK.LowLevel.Dispatch
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Simple.Common
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as LowLevel
import qualified Graphics.UI.FLTK.LowLevel.Dispatch
data ButtonFillSpec =
  ButtonFillSpec
  {
    buttonBounds :: Rectangle
  , buttonTopColor :: Color
  , buttonTopSelectionColor :: Color
  , buttonTopFillPercentage :: Int
  , buttonBottomColor :: Color
  , buttonBottomSelectionColor :: Color
  , buttonBottomFillPercentage :: Int
  , buttonRadius :: Int
  , buttonBorderColor :: Color
  , buttonBorderFocusColor :: Color
  , buttonBorderHovered :: Color
  , buttonLabelFontSize :: FontSize
  }

buttonIsHidden ::
  (
    Parent orig LowLevel.Button,
    Match a ~ FindOp orig orig (LowLevel.GetType_ ()),
    Op (LowLevel.GetType_ ()) a orig (IO ButtonType)
  ) => Ref orig -> IO Bool
buttonIsHidden b = do
  t <- LowLevel.getType_ b
  case t of
    HiddenButtonType -> return True
    _ -> return False

borderButton :: (Parent a LowLevel.Widget) => ButtonFillSpec -> Ref a -> IO ()
borderButton spec b = do
  oldColor <- LowLevel.flcColor
  focused <- isWidget b FL.focus
  hovering <- isWidget b FL.belowmouse
  if focused
    then LowLevel.flcSetColor (buttonBorderFocusColor spec)
    else if hovering
         then LowLevel.flcSetColor (buttonBorderHovered spec)
         else LowLevel.flcSetColor (buttonBorderColor spec)
  LowLevel.flcBeginLine
  mapM_ (LowLevel.flcVertex . toPrecisePosition) (roundedBoxPoints (buttonBounds spec) (Just (buttonRadius spec)) Nothing)
  LowLevel.flcEndLine
  LowLevel.flcLineStyle (LowLevel.LineDrawStyle (Just LowLevel.LineStyleSolid) Nothing Nothing) Nothing Nothing
  LowLevel.flcSetColor oldColor

fillButton :: ButtonFillSpec -> Bool -> IO ()
fillButton spec pressed = do
  if pressed
    then drawButton (buttonTopSelectionColor spec) (buttonBottomSelectionColor spec)
    else drawButton (buttonTopColor spec) (buttonBottomColor spec)
  where
    drawButton topColor bottomColor = do
      let diameter = (buttonRadius spec) * 2
          quarterCircle x y a1 a2 = do
            fillColor <- LowLevel.flcColor
            LowLevel.flcPie (toRectangle (x,y,diameter,diameter)) a1 a2
            LowLevel.flcSetColor fillColor
      oldColor <- LowLevel.flcColor
      -- top left/right
      let (x,y,w,h) = fromRectangle (buttonBounds spec)
          insideW = w - 1
          insideH = h - 1
          topFillFraction :: Double
          topFillFraction = (fromIntegral (buttonTopFillPercentage spec)) / 100.0
          topHeight = truncate (fromIntegral insideH * topFillFraction)
      LowLevel.flcSetColor topColor
      quarterCircle x y (PreciseAngle 90.0) (PreciseAngle 180.0)
      quarterCircle (x + (insideW - diameter)) y (PreciseAngle 0) (PreciseAngle 90.0)
      LowLevel.flcRectf (toRectangle (x + (buttonRadius spec), y, insideW - diameter, (buttonRadius spec)))
      LowLevel.flcRectf (toRectangle (x, y + (buttonRadius spec), insideW , topHeight - (buttonRadius spec)))
      -- bottom left/right
      let bottomFillFraction :: Double
          bottomFillFraction = (fromIntegral (buttonBottomFillPercentage spec)) / 100.0
          bottomHeight = truncate (fromIntegral insideH * bottomFillFraction)
      LowLevel.flcSetColor bottomColor
      quarterCircle x (y + (insideH - diameter)) (PreciseAngle 180.0) (PreciseAngle 270.0)
      quarterCircle (x + (insideW - diameter)) (y + (insideH - diameter)) (PreciseAngle 270.0) (PreciseAngle 360.0)
      LowLevel.flcRectf (toRectangle (x + (buttonRadius spec), y + h - (buttonRadius spec), insideW - (diameter), (buttonRadius spec)))
      LowLevel.flcRectf (toRectangle (x, y + bottomHeight, insideW, h - bottomHeight - (buttonRadius spec)))
      -- middle gradient
      let middleFraction = fromIntegral (buttonBottomFillPercentage spec - buttonTopFillPercentage spec) / 100.0
          totalSteps :: Double
          totalSteps = fromIntegral h * middleFraction
          stepSize :: Double
          stepSize = 1.0 / totalSteps
      mapM_
        (
          \step -> do
            let weight = 1.0 - (stepSize * fromIntegral step)
                blendColor =
                  colorAverage
                    topColor
                    bottomColor
                    weight
            LowLevel.flcSetColor blendColor
            LowLevel.flcXyline (toPosition (x,y + topHeight + step)) (X (x + insideW))
        )
        [0 .. truncate totalSteps - 1]
      LowLevel.flcSetColor oldColor

drawRegularButton ::  ButtonFillSpec -> Ref LowLevel.Button -> IO ()
drawRegularButton spec b = do
  buttonType <- LowLevel.getType_ b
  case buttonType of
    HiddenButtonType -> return ()
    _ -> do
      pressed <- LowLevel.getValue b
      fillButton spec pressed
      borderButton spec b
      LowLevel.drawLabel b Nothing

drawIndicatorButton ::
  (
    Parent orig LowLevel.Widget,
    Match b ~ FindOp orig orig (LowLevel.ActiveR ()),
    Op (LowLevel.ActiveR ()) b orig (IO Bool),
    Match c ~ FindOp orig orig (LowLevel.GetAlign ()),
    Op (LowLevel.GetAlign ()) c orig (IO Alignments),
    Match d ~ FindOp orig orig (LowLevel.GetImage ()),
    Op (LowLevel.GetImage ()) d orig (IO (Maybe (Ref LowLevel.Image))),
    Match e ~ FindOp orig orig (LowLevel.GetSelectionColor ()),
    Op (LowLevel.GetSelectionColor ()) e orig (IO Color),
    Match g ~ FindOp orig orig (LowLevel.GetRectangle ()),
    Op (LowLevel.GetRectangle ()) g orig (IO Rectangle),
    Match h ~ FindOp orig orig (LowLevel.GetColor ()),
    Op (LowLevel.GetColor ()) h orig (IO Color),
    Match i ~ FindOp orig orig (LowLevel.GetLabelsize ()),
    Op (LowLevel.GetLabelsize ()) i orig (IO FontSize),
    Match j ~ FindOp orig orig (LowLevel.DrawLabel ()),
    Op (LowLevel.DrawLabel ()) j orig (Maybe (Rectangle,Alignments) ->  IO ()),
    Match k ~ FindOp orig orig (LowLevel.GetBox ()),
    Op (LowLevel.GetBox ()) k orig (IO (Boxtype)),
    Match l ~ FindOp orig orig (LowLevel.GetParent ()),
    Op (LowLevel.GetParent ()) l orig (IO (Maybe (Ref LowLevel.Group))),
    Match m ~ FindOp orig orig (LowLevel.GetColor ()),
    Op (LowLevel.GetColor ()) m orig (IO (Color)),
    Match n ~ FindOp orig orig (LowLevel.SetColor ()),
    Op (LowLevel.SetColor ()) n orig (Color -> IO ())
  ) => (ButtonFillSpec -> Bool -> Color -> IO Rectangle) -> Bool -> Bool -> Bool -> Ref orig -> IO ()
drawIndicatorButton indicator pressed transparent hidden b =
  if hidden then return ()
    else do
      spec <- buttonFillSpec b
      boxType <- LowLevel.getBox b
      active <- LowLevel.activeR b
      if (not transparent)
        then case boxType of
               NoBox -> return ()
               _ -> do
                 fillButton spec pressed
                 borderButton spec b
        else do
          maybeParent <- LowLevel.getParent b
          case maybeParent of
            Just (p :: Ref LowLevel.Group) -> do
              c <- LowLevel.getColor p
              () <- LowLevel.setColor b c
              borderButton (spec { buttonBorderColor = lightBackground }) b
            Nothing -> return ()
      let (x,y,w,h) = fromRectangle (buttonBounds spec)
      color <- LowLevel.getSelectionColor b
      indicatorBounds <- indicator spec (pressed && active) color
      let (indicatorX, _, indicatorW, _) = fromRectangle indicatorBounds
      (alignments :: Alignments) <- LowLevel.getAlign b
      (imageM :: Maybe (Ref LowLevel.Image)) <- LowLevel.getImage b
      let labelBounds = case imageM of
            Nothing -> toRectangle (x + buttonRadius spec, y, w - (buttonRadius spec * 2), h)
            Just _ ->
              let widthLeftCut = indicatorX + indicatorW - x
                  widthRightCut = buttonRadius spec
              in toRectangle (indicatorX + indicatorW, y, w - (widthLeftCut + widthRightCut),h)
      LowLevel.drawLabel b (Just (labelBounds, alignments))

drawDownArrow :: ButtonFillSpec -> Color -> IO Rectangle
drawDownArrow spec arrowColor =
  let arrowSize :: Int
      arrowSize =
        case (buttonLabelFontSize spec) of
          FontSize arrowSize -> truncate (fromIntegral arrowSize / 2)
      (x,y,w,h) = fromRectangle (buttonBounds spec)
      arrowX = x + w - arrowSize - arrowSize
      arrowY :: Int
      arrowY = y + (truncate (fromIntegral h / 2.0)) - (truncate (fromIntegral arrowSize / 2))
      arrowBoundingBox = toRectangle (arrowX, arrowY, arrowSize, arrowSize)
  in do
  oldColor <- LowLevel.flcColor
  LowLevel.flcSetColor arrowColor
  LowLevel.flcBeginPolygon
  LowLevel.flcVertex (PrecisePosition (PreciseX (fromIntegral arrowX)) (PreciseY (fromIntegral arrowY)))
  LowLevel.flcVertex (PrecisePosition (PreciseX (fromIntegral (arrowX+arrowSize))) (PreciseY (fromIntegral arrowY)))
  LowLevel.flcVertex (PrecisePosition (PreciseX ((fromIntegral arrowX) + (fromIntegral arrowSize)/2)) (PreciseY (fromIntegral (arrowY + arrowSize))))
  LowLevel.flcVertex (PrecisePosition (PreciseX (fromIntegral arrowX)) (PreciseY (fromIntegral arrowY)))
  LowLevel.flcEndPolygon
  return arrowBoundingBox

drawMenuButton :: Ref LowLevel.MenuButton -> IO ()
drawMenuButton m = do
  t <- LowLevel.getType_ m
  b <- LowLevel.getBox m
  let noBox = case b of { NoBox -> True; _ -> False }
  if (t /= 0 || noBox) then return ()
    else do
      fillSpec <- buttonFillSpec m
      itemIndex <- LowLevel.getValue m
      drawIndicatorButton (\spec _ color -> drawDownArrow spec color) (maybe False (const True) itemIndex) False False m

drawLight :: ButtonFillSpec -> Bool -> Color -> IO Rectangle
drawLight spec lightOn lightColor =
  let (x,y,w,h) = fromRectangle (buttonBounds spec)
      lightX = x + 3
      lightY = y + (truncate (fromIntegral h / 2.0))
      (FontSize lightDiameter) = buttonLabelFontSize spec
      lightRadius = (fromIntegral lightDiameter / 2.0)
      lightBoundingBox = toRectangle (lightX, (lightY - truncate lightRadius), fromIntegral lightDiameter, fromIntegral lightDiameter)
  in do
  oldColor <- LowLevel.flcColor
  when lightOn
    (do
      LowLevel.flcSetColor lightColor
      LowLevel.flcPie lightBoundingBox (PreciseAngle 0.0) (PreciseAngle 360.0)
    )
  LowLevel.flcSetColor (buttonBorderColor spec)
  LowLevel.flcCircle (PrecisePosition (PreciseX (fromIntegral (lightX + truncate lightRadius))) (PreciseY (fromIntegral lightY))) lightRadius
  LowLevel.flcSetColor oldColor
  return lightBoundingBox

drawRound :: ButtonFillSpec -> Bool -> Color -> IO Rectangle
drawRound spec radioOn radioColor =
  let (x,y,w,h) = fromRectangle (buttonBounds spec)
      radioBoxX = x + 3
      radioBoxY = fromIntegral y + (fromIntegral h / 2.0)
      (FontSize fs) = buttonLabelFontSize spec
      radioBoxWidth :: Int
      radioBoxWidth = fromIntegral fs
      radioBoxHalfway :: Double
      radioBoxHalfway = (fromIntegral radioBoxWidth) / 2.0
      radioBoundingBox =
        toRectangle (radioBoxX, truncate (radioBoxY - radioBoxHalfway), radioBoxWidth, radioBoxWidth)
      circleSvg =
        "<svg width=\"%d\" height=\"%d\" viewBox=\" %f %f %d %d\">\n" ++
        "<circle cx=\"0\" cy=\"0\" r=\"%f\" fill=\"%s\" stroke=\"%s\"/>" ++
        "</svg>\n"
      radioRadius = (fromIntegral radioBoxWidth) / 4.5
      radioDiameter = radioRadius * 2
      radioX = fromIntegral radioBoxX + radioBoxHalfway - radioRadius
      radioY = radioBoxY - radioRadius
  in do
  (borderColorR, borderColorG, borderColorB) <- FL.getColorRgb (buttonBorderColor spec)
  (radioColorR, radioColorG, radioColorB) <- FL.getColorRgb (if radioOn then radioColor else (buttonTopColor spec))
  let svg =
        "<svg>\n" ++
        (printf circleSvg
          radioBoxWidth radioBoxWidth (-radioBoxHalfway) (-radioBoxHalfway) radioBoxWidth radioBoxWidth
          (radioBoxHalfway-1.0) ("none" :: String) ("rgb(" ++ show borderColorR ++ "," ++ show borderColorG ++ "," ++ show borderColorB ++ ")")) ++
        (printf circleSvg
          radioBoxWidth radioBoxWidth (-radioBoxHalfway) (-radioBoxHalfway) radioBoxWidth radioBoxWidth
          (radioRadius-1.0) ("rgb(" ++ show radioColorR ++ "," ++ show radioColorG ++ "," ++ show radioColorB ++ ")") ("rgb(" ++ show radioColorR ++ "," ++ show radioColorG ++ "," ++ show radioColorB ++ ")")) ++
        "</svg>\n"
  iE <- LowLevel.svgImageNew ((TE.encodeUtf8 . T.pack) svg)
  case iE of
    Left _ -> throwIO (userError ("The generated SVG is invalid: \n" ++ svg))
    Right i -> do
      LowLevel.draw i (toPosition (radioBoxX,truncate (radioBoxY - radioBoxHalfway)))
      LowLevel.destroy i
  return radioBoundingBox

drawCheck :: ButtonFillSpec -> Bool -> Color -> IO Rectangle
drawCheck spec checked checkColor =
  let (x,y,w,h) = fromRectangle (buttonBounds spec)
      checkBoxWidth :: Int = case (buttonLabelFontSize spec) of { (FontSize w) -> fromIntegral w; }
      checkBoxHeight = checkBoxWidth
      checkBoxX = x + 3
      checkBoxMiddle = fromIntegral checkBoxHeight / 2.0
      checkBoxFourth = fromIntegral checkBoxHeight / 4.0
      yOffset = truncate (fromIntegral (h - fromIntegral checkBoxHeight) / 2.0)
      pad = 1
      checkThickness = 2
      checkBoxY = y + yOffset
      checkBoxRectangle = toRectangle (checkBoxX, checkBoxY, checkBoxWidth, checkBoxHeight)
  in do
  when checked
    (let
        downstrokeStart = Position (X (checkBoxX + pad)) (Y (checkBoxY + truncate checkBoxMiddle))
        upstrokeStart = Position (X (checkBoxX + pad + truncate checkBoxFourth)) (Y (checkBoxY - pad + checkBoxHeight))
     in do
     LowLevel.flcSetColor checkColor
     LowLevel.flcPolygonWith4Sides
       downstrokeStart
       (Position (X (checkBoxX + pad + checkThickness)) (Y (checkBoxY + truncate checkBoxMiddle)))
       (Position (X (checkBoxX + pad + truncate checkBoxFourth + checkThickness))  (Y (checkBoxY - pad + checkBoxHeight)))
       (Position (X (checkBoxX + pad + truncate checkBoxFourth)) (Y (checkBoxY - pad + checkBoxHeight)))
     LowLevel.flcPolygonWith4Sides
       upstrokeStart
       (Position (X (checkBoxX + checkBoxWidth - pad - checkThickness)) (Y (checkBoxY + pad)))
       (Position (X (checkBoxX + checkBoxWidth - pad)) (Y (checkBoxY + pad)))
       (Position (X (checkBoxX + pad + truncate checkBoxFourth + checkThickness)) (Y (checkBoxY - pad + checkBoxHeight)))
    )
  oldColor <- LowLevel.flcColor
  LowLevel.flcSetColor (buttonBorderColor spec)
  LowLevel.flcRect checkBoxRectangle
  LowLevel.flcSetColor oldColor
  return checkBoxRectangle

buttonFillSpec ::
  (
    Match w ~ FindOp orig orig (LowLevel.GetRectangle ()),
    Op (LowLevel.GetRectangle ()) w orig (IO Rectangle),
    Match x ~ FindOp orig orig (LowLevel.GetColor ()),
    Op (LowLevel.GetColor ()) x orig (IO Color),
    Match y ~ FindOp orig orig (LowLevel.GetLabelsize ()),
    Op (LowLevel.GetLabelsize ()) y orig (IO FontSize),
    Match z ~ FindOp orig orig (LowLevel.GetSelectionColor ()),
    Op (LowLevel.GetSelectionColor ()) z orig (IO Color)
  ) => Ref orig -> IO ButtonFillSpec
buttonFillSpec b = do
  rect <- LowLevel.getRectangle b
  color <- LowLevel.getColor b
  selectionColor <- LowLevel.getSelectionColor b
  fontSize <- LowLevel.getLabelsize b
  makeFillSpec rect color selectionColor fontSize

makeFillSpec :: Rectangle -> Color -> Color -> FontSize -> IO ButtonFillSpec
makeFillSpec rect color selectionColor fontSize = do
  (colorR, colorG, colorB) <- FL.getColorRgb color
  pressedColor <- rgbColorWithRgb (colorR - 21, colorG - 21, colorB - 21)
  let slightlyDarker = colorAverage color blackColor 0.93
  let slightlyLighter = colorAverage whiteColor pressedColor 0.93
  hoverColor <- rgbColorWithRgb (0xBB, 0xBB, 0xBB)
  let darkerSelectionColor = darker selectionColor
  return
    ButtonFillSpec
      {
        buttonBounds = rect,
        buttonTopColor = color,
        buttonTopSelectionColor = pressedColor,
        buttonTopFillPercentage = 30,
        buttonBottomColor = slightlyDarker,
        buttonBottomSelectionColor = slightlyLighter,
        buttonBottomFillPercentage = 70,
        buttonRadius = 2,
        buttonBorderColor = slightlyDarker,
        buttonBorderFocusColor = darkerSelectionColor,
        buttonBorderHovered = hoverColor,
        buttonLabelFontSize = fontSize
      }

buttonSetup ::
  (
    ?assets :: Assets,
    Match w ~ FindOp orig orig (LowLevel.SetColor ()),
    Op (LowLevel.SetColor ()) w orig (Color -> IO ()),
    Match x ~ FindOp orig orig (LowLevel.SetLabelfont ()),
    Op (LowLevel.SetLabelfont ()) x orig (Font -> IO ()),
    Match y ~ FindOp orig orig (LowLevel.SetLabelsize ()),
    Op (LowLevel.SetLabelsize ()) y orig (FontSize -> IO ()),
    Match z ~ FindOp orig orig (LowLevel.SetAlign ()),
    Op (LowLevel.SetAlign ()) z orig (Alignments -> IO ())
  ) => Ref orig -> IO ()
buttonSetup b = do
  color <- commonColor
  () <- LowLevel.setColor b color
  () <- LowLevel.setLabelfont b commonFont
  () <- LowLevel.setLabelsize b commonFontSize
  LowLevel.setAlign b (Alignments [AlignTypeInside, AlignTypeCenter , AlignTypeImageNextToText])

buttonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.Button)
buttonNew rectangle label = do
  b <- LowLevel.buttonCustom
         rectangle
         label
         (Just (\b -> do
                   spec <- buttonFillSpec b
                   drawRegularButton spec b))
         (Just (LowLevel.defaultCustomWidgetFuncs { LowLevel.handleCustom = Just handleHover }))
  buttonSetup b
  return b

toggleButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.ToggleButton)
toggleButtonNew rectangle label = do
  b <- LowLevel.buttonCustom
         rectangle
         label
         (Just (\b -> do
                   spec <- buttonFillSpec b
                   drawRegularButton spec b))
         (Just (LowLevel.defaultCustomWidgetFuncs { LowLevel.handleCustom = (Just handleHover ) }))
  LowLevel.setType b ToggleButtonType
  buttonSetup b
  return (LowLevel.castTo b)

lightButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.LightButton)
lightButtonNew rectangle label = do
  b <- LowLevel.lightButtonCustom
         rectangle
         label
         (Just (\b -> do
                   pressed <- LowLevel.getValue b
                   hidden <- buttonIsHidden b
                   drawIndicatorButton drawLight pressed False hidden b))
         (Just (LowLevel.defaultCustomWidgetFuncs { LowLevel.handleCustom = (Just handleHover) }))
  buttonSetup b
  LowLevel.setSelectionColor b yellowColor
  LowLevel.setAlign b (Alignments [AlignTypeInside, AlignTypeCenter, AlignTypeImageNextToText])
  return b

checkButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.CheckButton)
checkButtonNew rectangle label = do
  b <- LowLevel.checkButtonCustom
         rectangle
         label
         (Just (\b -> do
                   pressed <- LowLevel.getValue b
                   hidden <- buttonIsHidden b
                   drawIndicatorButton drawCheck pressed True hidden b))
         Nothing
  buttonSetup b
  c <- commonSelectionColor
  LowLevel.setSelectionColor b c
  return b

roundButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.RoundButton)
roundButtonNew rectangle label = do
  b <- LowLevel.roundButtonCustom
         rectangle
         label
         (Just (\b -> do
                   pressed <- LowLevel.getValue b
                   hidden <- buttonIsHidden b
                   drawIndicatorButton drawRound pressed True hidden b))
         Nothing
  buttonSetup b
  c <- commonSelectionColor
  LowLevel.setSelectionColor b c
  return b

menuButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.MenuButton)
menuButtonNew rect l = do
  m <- LowLevel.menuButtonCustom rect l (Just drawMenuButton) Nothing
  buttonSetup m
  LowLevel.setBox m BorderBox
  LowLevel.setTextfont m commonFont
  LowLevel.setTextsize m commonFontSize
  color <- commonSelectionColor
  LowLevel.setSelectionColor m color
  return m

returnButtonNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.ReturnButton)
returnButtonNew rect l = do
  b <- LowLevel.returnButtonCustom rect l
         (Just ((\b -> do
                   spec <- buttonFillSpec b
                   drawRegularButton spec b) . safeCast))
         (Just (LowLevel.defaultCustomWidgetFuncs { LowLevel.handleCustom = Just handleHover }))
  buttonSetup b
  let (_,_,w',h') = fromRectangle rect
  LowLevel.setAlign b (Alignments [AlignTypeTextNextToImage])
  iM <- LowLevel.copy returnButtonImage Nothing
  case iM of
    Nothing -> return ()
    Just i -> do
      LowLevel.scale i (Size (Width (truncate ((fromIntegral w')/3))) (Height (truncate ((fromIntegral h')/2)))) Nothing Nothing
      LowLevel.setImage b iM
  return b

okButton :: (?assets :: Assets) => Rectangle -> IO (Ref LowLevel.Button)
okButton rect = do
  b <- buttonNew rect (Just "OK")
  iM <- LowLevel.copy okButtonImage Nothing
  let (_,_,w',h') = fromRectangle rect
  case iM of
    Nothing -> return ()
    Just i -> do
      LowLevel.scale i (Size (Width (truncate ((fromIntegral w')/3))) (Height (truncate ((fromIntegral h')/2)))) Nothing Nothing
      LowLevel.setImage b iM
  return b

cancelButton :: (?assets :: Assets) => Rectangle -> IO (Ref LowLevel.Button)
cancelButton rect = do
  b <- buttonNew rect (Just "Cancel")
  iM <- LowLevel.copy cancelButtonImage Nothing
  let (_,_,w',h') = fromRectangle rect
  case iM of
    Nothing -> return ()
    Just i -> do
      LowLevel.scale i (Size (Width (truncate ((fromIntegral w')/3))) (Height (truncate ((fromIntegral h')/2)))) Nothing Nothing
      LowLevel.setImage b iM
  return b
