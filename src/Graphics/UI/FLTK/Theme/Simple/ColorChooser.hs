{-# LANGUAGE ImplicitParams, FlexibleContexts, TypeFamilies, RecursiveDo, ScopedTypeVariables, OverloadedStrings #-}
module Graphics.UI.FLTK.Theme.Simple.ColorChooser
  (
    ColorChooserComponentBounds(..),
    ColorChooserComponentLayout(..),
    ColorChooserState(..),
    colorChooserNew,
    colorDialCallback,
    drawCurrentColor,
    drawHueBox,
    drawPreviewColor,
    drawSaturationSlider,
    flcColorChooser,
    gaugeImageRectangle,
    generateImage,
    generateVImage,
    handleHueBox,
    hueBoxBounds,
    makeColorChooserComponentBounds,
    makeColorChooserComponentLayout,
    maxSliderWidth,
    modeMenuCallback,
    setValuators,
    sliderCallback,
    updateHsv,
    updateRgb
  )
where
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.Fixed
import Data.IORef
import Data.Ratio
import Data.STRef
import Foreign.C.Types
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations as Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Dispatch
import Graphics.UI.FLTK.Theme.Simple.Common
import Graphics.UI.FLTK.Theme.Simple.Dial
import Graphics.UI.FLTK.Theme.Simple.Group
import Graphics.UI.FLTK.Theme.Simple.Menu
import Graphics.UI.FLTK.Theme.Simple.Slider
import Graphics.UI.FLTK.Theme.Simple.Window
import Graphics.UI.FLTK.Theme.Simple.Group
import Graphics.UI.FLTK.Theme.Simple.Button
import Numeric
import Text.Printf as Printf
import qualified Data.ByteString.Char8 as BChar
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as LowLevel

data ColorChooserComponentBounds =
  ColorChooserComponentBounds
  {
    colorChooserHueBoxBounds :: Rectangle
  , colorChooserSliderBounds :: Rectangle
  , colorChooserPreviewPaneBounds :: Rectangle
  } deriving Show


data ColorChooserComponentLayout =
  ColorChooserComponentLayout
  {
    paddingWidthPercentage :: Double
  , paddingHeightPercentage :: Double
  , betweenPaddingPercent :: Double
  , hueBoxWidthPercentage :: Double
  , sliderWidthPercentage :: Double
  , hueBoxHandleRadius :: Double
  , previewPanePercentage :: Double
  } deriving Show


data ColorChooserState =
  ColorChooserState
  {
    colorChooserValue :: Between0And1
  , colorChooserR :: Between0And1
  , colorChooserG :: Between0And1
  , colorChooserB :: Between0And1
  , colorChooserHue :: Between0And6
  , colorChooserSaturation :: Between0And1
  , colorChooserHueImage :: Ref LowLevel.RGBImage
  , colorChooserMode :: ColorChooserMode
  } deriving Show

maxSliderWidth :: Width
maxSliderWidth = Width 30

makeColorChooserComponentLayout :: Size -> Width -> Int -> ColorChooserComponentLayout
makeColorChooserComponentLayout (Size (Width w) (Height h)) (Width maxSliderWidth) gaugeHeight =
  let calculatedSliderWidth = 0.15 `percentOf` w
      gaugeStart = truncate ((fromIntegral gaugeHeight)/2)
      maxSliderWidthPercentage :: Double
      maxSliderWidthPercentage = (fromIntegral maxSliderWidth) / (fromIntegral w)
      _sliderWidthPercentage :: Double
      _sliderWidthPercentage =
        if calculatedSliderWidth > maxSliderWidth
        then maxSliderWidthPercentage
        else 0.15
      _betweenPaddingPercent = 0.02
      hueBoxHandleRadius =
        let r = (fromIntegral (if (w<h) then w else h))*0.03
        in if (r>(fromIntegral gaugeStart)) then (fromIntegral gaugeStart) else r
      _previewPanePercentage = 0.25
  in
  ColorChooserComponentLayout
  {
    paddingWidthPercentage = 0.01
  , paddingHeightPercentage = 0.01
  , betweenPaddingPercent = _betweenPaddingPercent
  , hueBoxWidthPercentage = 1.0 - (_betweenPaddingPercent + _sliderWidthPercentage + _previewPanePercentage)
  , sliderWidthPercentage = _sliderWidthPercentage
  , hueBoxHandleRadius = hueBoxHandleRadius
  , previewPanePercentage = _previewPanePercentage
  }

hueBoxHandleSvg :: String
hueBoxHandleSvg =
  "<svg width=\"%f\" height=\"%f\" viewBox=\"%f %f %f %f\" >"
  ++ "<circle cx=\"0\" cy=\"0\" r=\"%f\" fill-opacity=\"0.0\" stroke=\"black\" />"
  ++ "</svg>"

drawHueBox :: Double -> Int -> IORef ColorChooserState -> Ref LowLevel.ColorChooser -> Ref LowLevel.Box ->IO ()
drawHueBox handleRadius gaugeHeight stateRef c b = do
  state <- readIORef stateRef
  bounds <- LowLevel.getRectangle b
  let hueBox = hueBoxBounds bounds handleRadius gaugeHeight
  color <- commonColor
  let slightlyDarker = Enumerations.colorAverage color blackColor 0.80
  let spec =
        BorderBoxSpec
          {
            borderBoxHoveringColor = slightlyDarker
          , borderBoxColor = slightlyDarker
          , borderBoxFocusedColor = slightlyDarker
          , borderBoxFillColor = lightBackground
          , borderBoxBounds = hueBox
          }
  let (x',y',w',h') = fromRectangle hueBox
  LowLevel.flcRectfWithColor bounds (borderBoxFillColor spec)
  drawBorderBox b spec True
  iM <- LowLevel.copy (colorChooserHueImage state) (Just (Size (Width (w'-2)) (Height (h'-2))))
  maybe
    (return ())
    (\i -> do
        LowLevel.draw i (Position (X (x'+1)) (Y (y'+1)))
        LowLevel.destroy i)
    iM
  damages <- LowLevel.getDamage b
  let svg =
       Printf.printf hueBoxHandleSvg
         (handleRadius*2) (handleRadius*2) (-handleRadius) (-handleRadius) (handleRadius*2) (handleRadius*2)
         handleRadius
  iE <- LowLevel.svgImageNew ((TE.encodeUtf8 . T.pack) svg)
  case iE of
    Left _ -> throwIO (userError ("drawHueBox: the generated SVG is invalid: \n" ++ svg))
    Right i -> do
      hue <- LowLevel.getHue c
      saturation <- LowLevel.getSaturation c
      case hue of
        Left _ -> throwIO (userError ("drawHueBox: hue out of range, must be between 0 and 6."))
        Right (Between0And6 hue) ->
          case saturation of
            Left _ -> throwIO (userError ("drawHueBox: saturation out of range, must be between 0 and 1."))
            Right (Between0And1 saturation) ->
              let (PrecisePosition (PreciseX xp') (PreciseY yp')) =
                    fromHs (hue, saturation) (PreciseSize (PreciseWidth (fromIntegral w')) (PreciseHeight (fromIntegral h')))
                  pos = (Position (X (x'+(truncate (xp'-handleRadius)))) (Y (y'+(truncate (yp'-handleRadius)))))
              in do
              LowLevel.draw i pos
              LowLevel.destroy i
              LowLevel.drawLabel b Nothing

hueBoxBounds :: Rectangle -> Double -> Int -> Rectangle
hueBoxBounds hueBoxOuterRect handleRadius gaugeHeight =
  let (x',y',w',h') = fromRectangle hueBoxOuterRect
  in
    toRectangle
      (
        x'+(truncate handleRadius)
      , y'+(truncate ((fromIntegral gaugeHeight)/2))
      , w'-(truncate (handleRadius*2.0))
      , h'-gaugeHeight
      )

makeColorChooserComponentBounds :: ColorChooserComponentLayout -> Rectangle -> Int -> ColorChooserComponentBounds
makeColorChooserComponentBounds layout rect gaugeHeight =
  let (x',y',w',h') = fromRectangle rect
      paddingWidth = (paddingWidthPercentage layout) `percentOf` w'
      paddingHeight =  (paddingHeightPercentage layout) `percentOf` h'
      gaugeStart = truncate ((fromIntegral gaugeHeight)/2)
      innerRectWidth = w'-(paddingWidth*2)
      innerRectHeight = h'-(paddingHeight*2)
      betweenPadding = (betweenPaddingPercent layout) `percentOf` innerRectWidth
      outerHueBoxWidth = (hueBoxWidthPercentage layout) `percentOf` innerRectWidth
      hueBoxWidth = outerHueBoxWidth - (truncate (hueBoxHandleRadius layout*2.0))
      hueBoxHeight = innerRectHeight-gaugeHeight
      sliderWidth = (sliderWidthPercentage layout) `percentOf` innerRectWidth
      innerRect = (x'+paddingWidth,y'+paddingHeight,innerRectWidth,innerRectHeight)
      previewPaneWidth = (previewPanePercentage layout) `percentOf` innerRectWidth
  in
    ColorChooserComponentBounds
    {
      colorChooserSliderBounds =
        toRectangle (x'+paddingWidth+outerHueBoxWidth,y'+paddingHeight,sliderWidth,innerRectHeight)
    , colorChooserHueBoxBounds =
        toRectangle (x'+paddingWidth,y'+paddingHeight,outerHueBoxWidth,innerRectHeight)
    , colorChooserPreviewPaneBounds =
        toRectangle (x'+paddingWidth+outerHueBoxWidth+sliderWidth+betweenPadding, y'+(if (gaugeStart > paddingHeight) then gaugeStart else paddingHeight), previewPaneWidth, innerRectHeight)
    }

fromHs :: (Double,Double) -> PreciseSize -> PrecisePosition
fromHs (hue,saturation) (PreciseSize (PreciseWidth w) (PreciseHeight h)) =
  let x = hue/6.0*w
      y = (1.0-saturation)*h
  in
    (PrecisePosition
       (PreciseX (if (x<0) then 0.0 else if (x>w) then w else x))
       (PreciseY (if (y<0) then 0.0 else if (y>h) then h else y)))

toHs :: PrecisePosition -> (Double,Double)
toHs (PrecisePosition (PreciseX x) (PreciseY y)) =
  let h = 6.0*x `mod'` 6.0
      s = 1.0 - y
  in
    (
      if (h < 0.0)
      then 0.0
      else h
    ,
      if (s < 0.0)
      then 0.0
      else
        if (s > 1.0)
        then 1.0
        else s
    )

generateImage :: Size -> IO (Ref LowLevel.RGBImage)
generateImage size@(Size (Width w') (Height h')) =
  let toRgb :: Double -> CUChar
      toRgb v = toEnum (truncate (255*v+0.5))
  in do
    m <- DVSM.replicate (w'*h'*3) ((toEnum 0) :: CUChar)
    mapM_ (\r ->
            let yIncrement :: Double
                yIncrement = (fromIntegral r / (fromIntegral h'))
            in
             mapM_
               (\i ->
                  let offset = (w'*r+i)*3
                      xIncrement :: Double
                      xIncrement = ((fromIntegral i) / (fromIntegral w'))
                      (h,s) = toHs (PrecisePosition (PreciseX xIncrement) (PreciseY yIncrement))
                  in do
                    rgbM <- LowLevel.hsv2Rgb (Between0And6 h, Between0And1 s, Between0And1 1.0)
                    case rgbM of
                      Nothing -> throwIO (userError (Printf.printf "Could not convert hsv (%d,%d,%d) to rgb." h s (1.0 :: Double)))
                      Just (Between0And1 r, Between0And1 g, Between0And1 b) -> do
                        DVSM.write m offset (toRgb r)
                        DVSM.write m (offset+1) (toRgb g)
                        DVSM.write m (offset+2) (toRgb b)
               )
               [0 .. w'-1])
          [0 .. h'-1]
    im <- DVS.unsafeFreeze m
    LowLevel.rgbImageNew im size Nothing Nothing

vImageSvg :: String
vImageSvg =
  "<svg width=\"%d\" height=\"%d\"/>\n" ++
  "<defs>\n" ++
  "<linearGradient id=\"saturationGradient\" x1=\"25\" y1=\"180\" x2=\"25\" >\n" ++
  "<stop offset=\"0\" stop-color=\"black\" />\n" ++
  "<stop offset=\"1\" stop-color=\"%s\"/>\n" ++
  "</linearGradient>\n" ++
  "</defs>\n" ++
  "<rect width=\"%d\" height=\"%d\" fill=\"url(#saturationGradient)\"/>\n" ++
  "</svg>"

generateVImage :: RGB -> Size -> IO (Ref LowLevel.SVGImage)
generateVImage (r',g',b') size@(Size (Width w') (Height h')) =
  let svgString :: String
      svgString =
        "<svg width=\"" ++ (show w') ++ "\" height=\"" ++ (show h') ++ "\"/>\n" ++
        "<defs>\n" ++
        "<linearGradient id=\"saturationGradient\" x1=\"25\" y1=\"180\" x2=\"25\" >\n" ++
        "<stop offset=\"0\" stop-color=\"black\" />\n" ++
        "<stop offset=\"1\" stop-color=\"" ++ "rgb(" ++ (show r') ++ "," ++ (show g') ++ "," ++ (show b') ++ ")\"/>\n"  ++
        "</linearGradient>\n" ++
        "</defs>\n" ++
        "<rect width=\"" ++ (show w') ++ "\" height=\"" ++ (show h') ++ "\" fill=\"url(#saturationGradient)\"/>\n" ++
        "</svg>"
  in do
    iE <- LowLevel.svgImageNew (BChar.pack svgString)
    case iE of
      Left _ -> throwIO (userError ("generateVImage': the generated SVG is invalid: \n" ++ svgString))
      Right i -> return i

handleHueBox :: Double -> Int -> Ref LowLevel.ColorChooser -> Ref LowLevel.Box -> Event -> IO (Either UnknownEvent ())
handleHueBox handleRadius gaugeHeight c b e = do
  rect <- LowLevel.getRectangle b
  (Right (Between0And6 currH)) <- LowLevel.getHue c
  (Right (Between0And1 currS)) <- LowLevel.getSaturation c
  (Right (Between0And1 v)) <- LowLevel.getValue c
  let focusMe = do
        vf <- FL.visibleFocus
        when vf (LowLevel.redraw b)
      hueBoxRect = hueBoxBounds rect handleRadius gaugeHeight
      updateHsv =
        let (x',y',w',h') = fromRectangle hueBoxRect
        in do
        vf <- FL.visibleFocus
        when vf ((FL.setFocus b) >> LowLevel.redraw b)
        eventPos@(Position (X ex)(Y ey)) <- FL.eventPosition
        let xDiff = ex-x'
            xIncrement =
              if (xDiff <= 0) then 0.0
              else
                fromIntegral (if (xDiff > w'-1) then w'-1 else xDiff) / fromIntegral w'
            yIncrement = fromIntegral (ey-y') / fromIntegral h'
            (h,s) = toHs (PrecisePosition (PreciseX xIncrement) (PreciseY yIncrement))
        ctrlPressed <- FL.containsEventState Kb_CtrlState
        r <- LowLevel.setHsv c
              (
                Between0And6 (if ctrlPressed then currH else h)
              , Between0And1 s
              , Between0And1 v
              )
        case r of
          Left NoChange -> return ()
          Right _ -> LowLevel.doCallback c
  case e of
    Push -> updateHsv >> return (Right ())
    Drag -> updateHsv >> return (Right ())
    Focus -> focusMe >>  return (Right ())
    Unfocus -> focusMe >> return (Right ())
    Keydown ->
        let (PrecisePosition (PreciseX x) (PreciseY y)) = fromHs (currH,currS) (toPreciseSize (rectangleSize rect))
        in do
        key <- FL.eventKey
        let newXY = case key of
              SpecialKeyType Kb_Up -> Just (x,y-3)
              SpecialKeyType Kb_Down -> Just (x,y+3)
              SpecialKeyType Kb_Left -> Just (x-3,y)
              SpecialKeyType Kb_Right -> Just (x+3,y)
              _ -> Nothing
        case newXY of
          Nothing -> return (Left UnknownEvent)
          Just (newX,newY) ->
            let (_,_,w',h') = fromRectangle hueBoxRect
                (h,s) = toHs (PrecisePosition (PreciseX (newX/(fromIntegral w'))) (PreciseY (newY/(fromIntegral h'))))
            in do
            r <- LowLevel.setHsv c (Between0And6 h, Between0And1 s, Between0And1 v)
            case r of
              Left NoChange -> return ()
              Right _ -> LowLevel.doCallback c
            return (Right ())
    _ -> return (Left UnknownEvent)

drawSaturationSlider :: IORef ColorChooserState -> Ref LowLevel.Slider -> IO ()
drawSaturationSlider stateRef s = do
  bounds <- LowLevel.getRectangle s
  state <- readIORef stateRef
  damages <- LowLevel.getDamage s
  let gaugeImageBounds = gaugeImageRectangle mkGaugeSliderSpec bounds
  when True -- (DamageExpose `elem` damages)
    (do
       c <- LowLevel.getColor s
       LowLevel.flcRectfWithColor gaugeImageBounds c
       rgbM <- LowLevel.hsv2Rgb (colorChooserHue state, colorChooserSaturation state, Between0And1 1.0)
       maybe (return ())
         (\rgb -> do
             i <- generateVImage (extractRgb rgb) (rectangleSize gaugeImageBounds)
             LowLevel.draw i (rectanglePosition gaugeImageBounds)
             LowLevel.destroy i)
         rgbM)
  when ((DamageAll `elem` damages) || (DamageExpose `elem` damages))
    (drawSlider s (Just mkGaugeSliderSpec) bounds)

gaugeImageRectangle :: GaugeSliderSpec -> Rectangle -> Rectangle
gaugeImageRectangle spec bounds =
  let (gX,gY,gW,gH) = fromRectangle (gaugeBoxBounds False spec bounds)
      imageSize = Size (Width (gW-2)) (Height (gH-2))
      imagePosition = (Position (X (gX+1)) (Y (gY+1)))
  in (Rectangle imagePosition imageSize)

extractRgb :: (Between0And1, Between0And1, Between0And1) -> RGB
extractRgb (Between0And1 _r, Between0And1 _g, Between0And1 _b) = (truncate (_r*255.0), truncate (_g*255.0), truncate (_b*255.0))

updateHsv :: IORef ColorChooserState -> Ref LowLevel.Slider -> Ref LowLevel.Box -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.Group -> Ref LowLevel.ColorChooser -> (Between0And6, Between0And1, Between0And1) -> IO Int
updateHsv stateRef slider hueBox rInput gInput bInput previewGroup colorChooser (Between0And6 _h, Between0And1 _s, Between0And1 _v) =
  let h =
        let __h = _h `mod'` 6.0
        in if (__h<0) then __h+6.0 else __h
      s =
        if (_s<0) then 0.0 else if (_s>1.0) then 1.0 else _s
      v =
        if (_v<0) then 0.0 else if (_v>1.0) then 1.0 else _v
  in do
   state <- readIORef stateRef
   let (Between0And6 currH) = colorChooserHue state
       (Between0And1 currS) = colorChooserSaturation state
       (Between0And1 currV) = colorChooserValue state
       changed :: Int
       changed = runST (do
           ref <- newSTRef 0
           when (h /= currH) (writeSTRef ref 1)
           when (s /= currS) (writeSTRef ref 1)
           when (v /= currV) (writeSTRef ref 1)
           readSTRef ref)
   when (h /= currH)
     (do
       LowLevel.setDamage hueBox [DamageScroll]
       LowLevel.setDamage slider [DamageExpose]
       LowLevel.setDamage previewGroup [DamageExpose]
       ds <- LowLevel.getDamage slider
       modifyIORef stateRef (\state -> state { colorChooserHue = Between0And6 h }))
   when (s /= currS)
     (do
       LowLevel.setDamage hueBox [DamageScroll]
       LowLevel.setDamage slider [DamageExpose]
       LowLevel.setDamage previewGroup [DamageExpose]
       modifyIORef stateRef (\state -> state { colorChooserSaturation = Between0And1 s }))
   when (v /= currV)
     (do
       LowLevel.setValue slider (1-v)
       LowLevel.setDamage previewGroup [DamageExpose]
       modifyIORef stateRef (\state -> state { colorChooserValue = Between0And1 v }))
   when (changed == 1)
     (do
       rgb <- LowLevel.hsv2Rgb (Between0And6 h, Between0And1 s, Between0And1 v)
       LowLevel.setChanged colorChooser
       case rgb of
         Just (r,g,b) -> modifyIORef stateRef (\s -> s { colorChooserR = r, colorChooserG = g, colorChooserB = b })
         Nothing -> return ()
       setValuators rInput gInput bInput colorChooser)
   setValuators rInput gInput bInput colorChooser
   return changed

updateRgb :: IORef ColorChooserState -> Ref LowLevel.Slider -> Ref LowLevel.Box -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.Group -> Ref LowLevel.ColorChooser -> (Between0And1, Between0And1, Between0And1) -> IO Int
updateRgb stateRef slider hueBox rValue gValue bValue previewGroup c rgb@(Between0And1 r, Between0And1 g, Between0And1 b) = do
  state <- readIORef stateRef
  let (Between0And1 currR) = colorChooserR state
      (Between0And1 currG) = colorChooserG state
      (Between0And1 currB) = colorChooserB state
      changed = runST (do
        ref <- newSTRef 0
        when (r /= currR) (writeSTRef ref 1)
        when (g /= currG) (writeSTRef ref 1)
        when (b /= currB) (writeSTRef ref 1)
        readSTRef ref)
  when (r /= currR)
    (do
       LowLevel.setDamage hueBox [DamageScroll]
       LowLevel.setDamage slider [DamageExpose]
       LowLevel.setDamage previewGroup [DamageExpose]
       modifyIORef stateRef (\state -> state { colorChooserR = Between0And1 r }))
  when (g /= currG)
    (do
       LowLevel.setDamage hueBox [DamageScroll]
       LowLevel.setDamage slider [DamageExpose]
       LowLevel.setDamage previewGroup [DamageExpose]
       modifyIORef stateRef (\state -> state { colorChooserG = Between0And1 g }))
  when (b /= currB)
    (do
       LowLevel.setDamage hueBox [DamageScroll]
       LowLevel.setDamage slider [DamageExpose]
       LowLevel.setDamage previewGroup [DamageExpose]
       modifyIORef stateRef (\state -> state { colorChooserB = Between0And1 b }))
  when (changed == 1)
    (do
      rgb <- LowLevel.rgb2Hsv rgb
      LowLevel.setChanged c
      case rgb of
        Just (h,s,v) -> modifyIORef stateRef (\state -> state { colorChooserHue = h,  colorChooserSaturation = s, colorChooserValue = v })
        Nothing -> return ())
  setValuators rValue gValue bValue c
  return changed

modeMenuCallback :: IORef ColorChooserState -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.ColorChooser -> Ref LowLevel.Choice -> IO ()
modeMenuCallback stateRef rValue gValue bValue chooser choice = do
  (AtIndex i) <- LowLevel.getValue choice
  case i of
    0 -> modifyIORef stateRef (\state -> state { colorChooserMode = RgbMode })
    1 -> modifyIORef stateRef (\state -> state { colorChooserMode = ByteMode })
    2 -> modifyIORef stateRef (\state -> state { colorChooserMode = HexMode })
    3 -> modifyIORef stateRef (\state -> state { colorChooserMode = HsvMode })
    _ -> throwIO (userError "modeMenuCallback: Only RGB, Byte, Hex and Hsv are valid modes.")
  setValuators rValue gValue bValue chooser

colorDialCallback :: Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.ColorChooser -> IO ()
colorDialCallback rValue gValue bValue c = do
  rV <- LowLevel.getValue rValue
  gV <- LowLevel.getValue gValue
  bV <- LowLevel.getValue bValue
  m <- LowLevel.getMode c
  case m of
    HsvMode -> LowLevel.setHsv c (Between0And6 rV, Between0And1 gV, Between0And1 bV)
    _ -> LowLevel.setRgb c (Between0And1 rV, Between0And1 gV, Between0And1 bV)
  LowLevel.doCallback c

sliderCallback :: IORef ColorChooserState -> Ref LowLevel.ColorChooser -> Ref LowLevel.Slider -> IO ()
sliderCallback stateRef c slider = do
  state <- readIORef stateRef
  v <- LowLevel.getValue slider
  LowLevel.setHsv c (colorChooserHue state, colorChooserSaturation state, Between0And1 (1.0-v))
  LowLevel.doCallback c

setValuators :: Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.FillDial -> Ref LowLevel.ColorChooser -> IO ()
setValuators rInput gInput bInput c = do
  let toThreeDecimals v = fromIntegral (truncate (v*1000)) / 1000
  m <- LowLevel.getMode c
  case m of
    RgbMode -> do
      rgbE <- LowLevel.getRgb c
      case rgbE of
        Left _ -> return ()
        Right (Between0And1 r, Between0And1 g, Between0And1 b) -> do
          mapM_ (\i -> do
                    LowLevel.range i 0 1
                    LowLevel.setStep i (1%1000)
                    LowLevel.precision i 3)
            [rInput,gInput,bInput]
          LowLevel.setValue rInput r
          LowLevel.setValue gInput g
          LowLevel.setValue bInput b
          LowLevel.setSelectionColor rInput redColor
          LowLevel.setSelectionColor gInput greenColor
          LowLevel.setSelectionColor bInput blueColor
          LowLevel.setLabel rInput (T.pack (show (toThreeDecimals r)))
          LowLevel.setLabel gInput (T.pack (show (toThreeDecimals g)))
          LowLevel.setLabel bInput (T.pack (show (toThreeDecimals b)))
          return ()
    HsvMode -> do
      hsvE <- LowLevel.getHsv c
      case hsvE of
        Left _ -> return ()
        Right (Between0And6 h, Between0And1 s, Between0And1 v) -> do
          LowLevel.range rInput 0 6
          LowLevel.setStep rInput (1%1000)
          mapM_ (\i -> do
                    LowLevel.range i 0 1
                    LowLevel.setStep i (1%1000))
            [gInput,bInput]
          LowLevel.setValue rInput h
          LowLevel.setValue gInput s
          LowLevel.setValue bInput v
          color <- commonFillColor
          LowLevel.setSelectionColor rInput color
          LowLevel.setSelectionColor gInput color
          LowLevel.setSelectionColor bInput color
          LowLevel.setLabel rInput (T.pack (show (toThreeDecimals h)))
          LowLevel.setLabel gInput (T.pack (show (toThreeDecimals s)))
          LowLevel.setLabel bInput (T.pack (show (toThreeDecimals v)))
          return ()
    HexMode -> do
      rgbE <- LowLevel.getRgb c
      case rgbE of
        Left _ -> return ()
        Right (Between0And1 r, Between0And1 g, Between0And1 b) -> do
          mapM_ (\i -> do
                    LowLevel.range i 0 1
                    LowLevel.setStep i 1
                    LowLevel.precision i 3)
            [rInput,gInput,bInput]
          LowLevel.setValue rInput r
          LowLevel.setValue gInput g
          LowLevel.setValue bInput b
          LowLevel.setSelectionColor rInput redColor
          LowLevel.setSelectionColor gInput greenColor
          LowLevel.setSelectionColor bInput blueColor
          LowLevel.setLabel rInput (T.pack ("0x" ++ (showHex (truncate (255*r+0.5)) "")))
          LowLevel.setLabel gInput (T.pack ("0x" ++ (showHex (truncate (255*g+0.5)) "")))
          LowLevel.setLabel bInput (T.pack ("0x" ++ (showHex (truncate (255*b+0.5)) "")))
          return ()
    ByteMode -> do
      rgbE <- LowLevel.getRgb c
      case rgbE of
        Left _ -> return ()
        Right (Between0And1 r, Between0And1 g, Between0And1 b) -> do
          mapM_ (\i -> do
                    LowLevel.range i 0 1
                    LowLevel.setStep i 1
                    LowLevel.precision i 3)
            [rInput,gInput,bInput]
          LowLevel.setValue rInput r
          LowLevel.setValue gInput g
          LowLevel.setValue bInput b
          LowLevel.setSelectionColor rInput redColor
          LowLevel.setSelectionColor gInput greenColor
          LowLevel.setSelectionColor bInput blueColor
          LowLevel.setLabel rInput (T.pack (show (truncate (255*r+0.5))))
          LowLevel.setLabel gInput (T.pack (show (truncate (255*g+0.5))))
          LowLevel.setLabel bInput (T.pack (show (truncate (255*b+0.5))))
          return ()
  LowLevel.getParent rInput >>= maybe (return ()) LowLevel.redrawLabel

fillBoxWithColor :: Ref LowLevel.Box -> Color -> IO ()
fillBoxWithColor b color = do
  bounds <- LowLevel.getRectangle b
  let slightlyDarker = Enumerations.colorAverage color blackColor 0.80
  let spec =
        BorderBoxSpec
          {
            borderBoxHoveringColor = slightlyDarker
          , borderBoxColor = slightlyDarker
          , borderBoxFocusedColor = slightlyDarker
          , borderBoxFillColor = color
          , borderBoxBounds = bounds
          }
  drawBorderBox b spec True

drawPreviewColor :: Ref LowLevel.ColorChooser -> Ref LowLevel.Box -> IO ()
drawPreviewColor c b = do
  rgbE <- LowLevel.getRgb c
  either
    (\_ -> return ())
    (\rgb -> do
        color <- rgbColorWithRgb (extractRgb rgb)
        fillBoxWithColor b color)
    rgbE

drawCurrentColor :: Color -> Ref LowLevel.Box -> IO ()
drawCurrentColor c b = fillBoxWithColor b c
{-

int fl_color_chooser(const char* name, double& r, double& g, double& b, int cmode) {
  int ret = 0;
  Fl_Window window(215,200,name);
  window.callback(cc_cancel_cb,&ret);
  Fl_Color_Chooser chooser(10, 10, 195, 115);
  ColorChip ok_color(10, 130, 95, 25);
  Fl_Return_Button ok_button(10, 165, 95, 25, fl_ok);
  ok_button.callback(cc_ok_cb,&ret);
  ColorChip cancel_color(110, 130, 95, 25);
  cancel_color.r = uchar(255*r+.5); ok_color.r = cancel_color.r;
  ok_color.g = cancel_color.g = uchar(255*g+.5);
  ok_color.b = cancel_color.b = uchar(255*b+.5);
  Fl_Button cancel_button(110, 165, 95, 25, fl_cancel);
  cancel_button.callback(cc_cancel_cb,&ret);
  window.resizable(chooser);
  chooser.rgb(r,g,b);
  chooser.callback(chooser_cb, &ok_color);
  if (cmode!=-1) chooser.mode(cmode);
  window.end();
  window.set_modal();
  window.hotspot(window);
  window.show();
  while (window.shown()) Fl::wait();
  if (ret) { // ok_button or Enter
    r = chooser.r();
    g = chooser.g();
    b = chooser.b();
  }
  return ret;
}
-}
--------------------------------------------------------------------------------
-- flcColorChooser :: T.Text ->                                               --
--                    ColorChooserRGB ->                                      --
--                    Maybe ColorChooserMode ->                               --
--                    IO (Maybe ColorChooserRGB)                              --
-- flcColorChooser name rgb modeM = do                                        --
--   w <- doubleWindowNew (Size (Width 400) (Height 200)) Nothing (Just name) --
--------------------------------------------------------------------------------

drawColorDials :: (?assets :: Assets) => Ref LowLevel.ColorChooser -> IORef ColorChooserState -> Rectangle -> Maybe Color -> IO (Ref LowLevel.FillDial,Ref LowLevel.FillDial,Ref LowLevel.FillDial,Ref LowLevel.Group)
drawColorDials c stateRef rect initialColor =
  let modeMenuHeight = 20
      colorDialHeight = 40
      modeMenuPadding = 3
      (x',y',w',h') = fromRectangle rect
      dialSize = Size (Width colorDialHeight) (Height colorDialHeight)
  in do
  modeMenu <- choiceNew (toRectangle (x',y',w',modeMenuHeight)) Nothing
  rgbI <- LowLevel.add modeMenu "RGB" Nothing (Nothing :: Maybe (Ref LowLevel.MenuItem -> IO ())) (MenuItemFlags [])
  LowLevel.add modeMenu "Byte" Nothing (Nothing :: Maybe (Ref LowLevel.MenuItem -> IO ())) (MenuItemFlags [])
  LowLevel.add modeMenu "Hex" Nothing (Nothing :: Maybe (Ref LowLevel.MenuItem -> IO ())) (MenuItemFlags [])
  LowLevel.add modeMenu "HSV" Nothing (Nothing :: Maybe (Ref LowLevel.MenuItem -> IO ())) (MenuItemFlags [])
  LowLevel.setValue modeMenu (LowLevel.MenuItemByIndex rgbI)
  let colorDialGroupY = y'+modeMenuHeight+modeMenuPadding
      colorDialGroupH = 3*colorDialHeight
  colorDialGroup <- groupNew (toRectangle (x',colorDialGroupY,w',colorDialGroupH)) Nothing
  LowLevel.begin colorDialGroup
  let rValuePosition = Position (X x') (Y (y'+modeMenuHeight+modeMenuPadding))
      gValuePosition = Position (X x') (Y (y'+modeMenuHeight+modeMenuPadding+colorDialHeight))
      bValuePosition = Position (X x') (Y (y'+modeMenuHeight+modeMenuPadding+(2*colorDialHeight)))
  rValue <- fillDialNew (Rectangle rValuePosition dialSize) Nothing
  gValue <- fillDialNew (Rectangle gValuePosition dialSize) Nothing
  bValue <- fillDialNew (Rectangle bValuePosition dialSize) Nothing
  mapM_ (\w -> do
           LowLevel.setAlign w (Alignments [AlignTypeRight])
           LowLevel.setCallback w (\_ -> colorDialCallback rValue gValue bValue c))
    [rValue,gValue,bValue]
  LowLevel.end colorDialGroup
  previewGroup <- groupNew (toRectangle (x',colorDialGroupY+colorDialGroupH,w',colorDialGroupY-y')) Nothing
  let previewBoxHeight = truncate ((fromIntegral (h'-colorDialGroupH-modeMenuHeight-modeMenuPadding))/2.0)
  LowLevel.begin previewGroup
  LowLevel.boxCustom (toRectangle (x',colorDialGroupY+colorDialGroupH+modeMenuPadding,w',previewBoxHeight-modeMenuPadding)) Nothing
    (Just (drawPreviewColor c))
    Nothing
  case initialColor of
    Just initialColor' -> do
      LowLevel.boxCustom (toRectangle (x',colorDialGroupY+colorDialGroupH+modeMenuPadding+previewBoxHeight,w',previewBoxHeight-modeMenuPadding)) Nothing
        (Just (drawCurrentColor initialColor'))
        Nothing
      return ()
    Nothing -> return ()
  LowLevel.end previewGroup
  LowLevel.setCallback modeMenu (modeMenuCallback stateRef rValue gValue bValue c)
  return (rValue, gValue, bValue,previewGroup)

colorChooserNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> Maybe Color -> IO (Ref LowLevel.ColorChooser)
colorChooserNew rect l' initialColor =
  let layout = makeColorChooserComponentLayout (rectangleSize rect) maxSliderWidth defaultGaugeHeight
      componentBounds = makeColorChooserComponentBounds layout rect defaultGaugeHeight
  in mdo
  let initialState =
        ColorChooserState
        {
          colorChooserR = Between0And1 1.0
        , colorChooserG = Between0And1 1.0
        , colorChooserB = Between0And1 1.0
        , colorChooserHue = Between0And6 0.0
        , colorChooserSaturation = Between0And1 0.0
        , colorChooserValue = Between0And1 1.0
        , colorChooserHueImage = hueImage
        , colorChooserMode = RgbMode
        }
  stateRef <- newIORef initialState
  c <- LowLevel.colorChooserCustom rect l' Nothing
         (Just
           (LowLevel.CustomColorChooserFuncs
             {
               LowLevel.getModeCustom = \_ -> fmap colorChooserMode (readIORef stateRef)
             , LowLevel.setModeCustom = \_ m -> modifyIORef stateRef (\state -> state { colorChooserMode = m })
             , LowLevel.hueCustom = \_ -> fmap colorChooserHue (readIORef stateRef)
             , LowLevel.saturationCustom = \_ -> fmap colorChooserSaturation (readIORef stateRef)
             , LowLevel.valueCustom = \_ -> fmap colorChooserValue (readIORef stateRef)
             , LowLevel.rCustom = \_ -> fmap colorChooserR (readIORef stateRef)
             , LowLevel.gCustom = \_ -> fmap colorChooserG (readIORef stateRef)
             , LowLevel.bCustom = \_ -> fmap colorChooserB (readIORef stateRef)
             , LowLevel.hsvCustom = updateHsv stateRef g b rValue gValue bValue previewGroup
             , LowLevel.rgbCustom = updateRgb stateRef g b rValue gValue bValue previewGroup
             }))
          Nothing
  cs <- LowLevel.getArray c
  mapM_ LowLevel.hide cs
  numChildren <- LowLevel.children c
  mapM_ (LowLevel.removeIndex c . AtIndex) [0 .. numChildren-1]
  let color = lightBackground
  LowLevel.setColor c color
  LowLevel.begin c
  hueImage <- generateImage (rectangleSize (colorChooserHueBoxBounds componentBounds))
  b <- LowLevel.boxCustom (colorChooserHueBoxBounds componentBounds) Nothing
         (Just (drawHueBox (hueBoxHandleRadius layout) defaultGaugeHeight stateRef c))
         (Just (LowLevel.defaultCustomWidgetFuncs {LowLevel.handleCustom = (Just (handleHueBox (hueBoxHandleRadius layout) defaultGaugeHeight c))}))
  g <- LowLevel.sliderCustom
         (colorChooserSliderBounds componentBounds)
         Nothing
         (Just (drawSaturationSlider stateRef))
         (Just (LowLevel.defaultCustomWidgetFuncs {LowLevel.handleCustom = Just handleHover}))
  sliderSetup g
  LowLevel.setCallback g (sliderCallback stateRef c)
  let (paneX, paneY, paneW, paneH) = fromRectangle (colorChooserPreviewPaneBounds componentBounds)
      (Rectangle (Position _ (Y hueBoxY)) (Size _ (Height hueBoxHeight))) = hueBoxBounds (colorChooserHueBoxBounds componentBounds)(hueBoxHandleRadius layout) defaultGaugeHeight
  rgbE <- LowLevel.getRgb c
  let previewPaneDialBounds = toRectangle (paneX,hueBoxY,paneW,hueBoxHeight)
  previewPaneDialGroup <- groupNew previewPaneDialBounds Nothing
  LowLevel.begin previewPaneDialGroup
  (rValue,gValue,bValue,previewGroup) <- drawColorDials c stateRef (toRectangle (paneX,hueBoxY,paneW,hueBoxHeight)) initialColor
  LowLevel.end previewPaneDialGroup
  LowLevel.setResizable previewPaneDialGroup (Nothing :: (Maybe (Ref LowLevel.Widget)))
  either
    (\_ -> return ())
    (\rgb@(Between0And1 r,Between0And1 g,Between0And1 b) -> do
        LowLevel.setValue rValue r
        LowLevel.setValue gValue g
        LowLevel.setValue bValue b
        setValuators rValue gValue bValue c
        return ())
    rgbE
  LowLevel.end c
  LowLevel.setColor c color
  LowLevel.setBox c BorderBox
  LowLevel.setLabelfont c commonFont
  LowLevel.setLabelsize c commonFontSize
  commonSelectionColor >>= LowLevel.setSelectionColor c
  return c

flcColorChooser :: (?assets :: Assets) => Maybe T.Text -> Maybe Rectangle -> Maybe Color -> Maybe ColorChooserMode -> IO (Maybe Color)
flcColorChooser name rectM initialColor mode =
  let buttonBarHeight = 50
      colorChooserPadding = 5
      windowBounds = maybe ((Rectangle (Position (X 215) (Y 200)) (Size (Width 400) (Height (200 + buttonBarHeight))))) id rectM
      colorChooserBounds =
        let (x',y',w',h') = fromRectangle windowBounds
        in toRectangle(colorChooserPadding,0,w'-(colorChooserPadding*2),h'-buttonBarHeight)
      buttonBarBounds =
        let (x',y',w',h') = fromRectangle windowBounds
        in toRectangle (colorChooserPadding,h'-buttonBarHeight,w'-(colorChooserPadding*2),buttonBarHeight)
      buttonWidth = 65
      buttonHeight = 30
      cancelButtonBounds =
        let (buttonGroupX, buttonGroupY, buttonGroupW, buttonGroupH) = fromRectangle buttonBarBounds
        in toRectangle (buttonGroupX+buttonGroupW-buttonWidth, buttonGroupY+(truncate ((fromIntegral (buttonGroupH-buttonHeight)) / 2)),buttonWidth,buttonHeight)
      okButtonBounds =
        let (buttonGroupX, buttonGroupY, buttonGroupW, buttonGroupH) = fromRectangle buttonBarBounds
        in toRectangle (buttonGroupX+buttonGroupW-buttonWidth-5-buttonWidth,buttonGroupY+(truncate ((fromIntegral (buttonGroupH-buttonHeight)) / 2)),buttonWidth,buttonHeight)
      showUntilHidden w = do
        isShown <- LowLevel.shown w
        if isShown then FL.wait >> showUntilHidden w
          else return ()
      buttonCallback :: IORef Bool -> Ref LowLevel.DoubleWindow -> Bool -> Ref LowLevel.Button -> IO ()
      buttonCallback selectedRef w v _ = do
        LowLevel.hide w
        writeIORef selectedRef v
  in do
    okSelectedRef <- newIORef False
    window <- doubleWindowNew (rectangleSize windowBounds) (Just (rectanglePosition windowBounds)) name
    LowLevel.begin window
    cc <- colorChooserNew colorChooserBounds Nothing initialColor
    buttonGroup <- groupNew buttonBarBounds Nothing
    LowLevel.begin buttonGroup
    okButton <- okButton okButtonBounds
    cancelButton <- cancelButton cancelButtonBounds
    LowLevel.setCallback okButton (buttonCallback okSelectedRef window True)
    LowLevel.setCallback cancelButton (buttonCallback okSelectedRef window False)
    LowLevel.end buttonGroup
    LowLevel.end window
    LowLevel.setModal window
    LowLevel.hotSpot window (LowLevel.ByWidget window) Nothing
    LowLevel.setResizable window (Just cc)
    LowLevel.showWidget window
    showUntilHidden window
    okSelected <- readIORef okSelectedRef
    if okSelected
      then do
      rgb <- LowLevel.getRgb cc
      case rgb of
        Left OutOfRange -> throwIO (userError "flcColorChooser: RGB value selected is out of range.")
        Right rgb' -> rgbColorWithRgb (extractRgb rgb') >>= return . Just
      else return initialColor
