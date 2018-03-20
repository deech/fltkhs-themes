{-# LANGUAGE ImplicitParams, OverloadedStrings, AllowAmbiguousTypes, GADTs, CPP, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Graphics.UI.FLTK.Theme.Simple.Common
  (
    Assets,
    BorderBoxSpec(..),
    OpenBorder(..),
    angleToCoordinate,
    cancelButtonImage,
    commonColor,
    commonFillColor,
    commonFont,
    commonFontSize,
    commonSelectionColor,
    degreesPerRadian,
    dejaVuSans,
    drawBorderBox,
    fromFltkAngle,
    handleHover,
    insideRectangle,
    isWidget,
    lightBackground,
    loadAssets,
    okButtonImage,
    percentOf,
    percentOfSize,
    returnButtonImage,
    roundedBoxPoints,
    withCustomBoxDraw,
    yanoneKaffesatz
  )
where
import Control.Exception
import Control.Monad
import Data.List
import Data.Maybe
import Graphics.UI.FLTK.LowLevel.Dispatch
import Graphics.UI.FLTK.LowLevel.FLTKHS hiding (colorAverage, isHorizontal, inactive, tabPositionsCustom, tabHeightCustom, tabDrawCustom, find, tabWhichCustom, tabRedrawTabs)
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.Font.Load
import Paths_fltkhs_themes
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Graphics.UI.FLTK.LowLevel.FL as FL

data OpenBorder = OpenBorderTop | OpenBorderBottom

data BorderBoxSpec =
  BorderBoxSpec
  {
    borderBoxBounds :: Rectangle,
    borderBoxFocusedColor :: Color,
    borderBoxHoveringColor :: Color,
    borderBoxColor :: Color,
    borderBoxFillColor :: Color
  } deriving Show


commonFont :: (?assets :: Assets) => Font
commonFont = dejaVuSans

commonFontSize :: FontSize
commonFontSize = FontSize 12

commonColor :: IO Color
commonColor = rgbColorWithRgb (0xF9, 0xF9, 0xF9)

commonFillColor :: IO Color
commonFillColor = rgbColorWithRgb (0x66, 0x94, 0xE3)

lightBackground :: Color
lightBackground = whiteColor

fromFltkAngle :: PreciseAngle -> PreciseAngle
fromFltkAngle (PreciseAngle a) =
  let unitCircleAngle = (270 - (truncate a)) `mod` 360
  in
    PreciseAngle
      (if (unitCircleAngle < 0)
       then 360.0 - (abs (fromIntegral unitCircleAngle))
       else (fromIntegral unitCircleAngle))

degreesPerRadian :: Double
degreesPerRadian = pi/180

angleToCoordinate :: PreciseAngle -> PrecisePosition
angleToCoordinate (PreciseAngle a) =
  case a of
     _ | a == 360 -> PrecisePosition (PreciseX (cos 0)) (PreciseY (sin 0))
       | otherwise ->
           let radians = a * degreesPerRadian
           in PrecisePosition (PreciseX (cos radians)) (PreciseY (sin radians))

percentOf :: Double -> Int -> Int
percentOf p a = truncate ((fromIntegral a) * p)

percentOfSize :: Double -> PreciseSize -> Double
percentOfSize percent (PreciseSize (PreciseWidth rW) (PreciseHeight rH)) = ((if (rW < rH) then rW else rH) * percent) / 100.0

commonSelectionColor :: IO Color
commonSelectionColor = fmap darker (rgbColorWithRgb (66, 104, 244))

isWidget :: (Parent a Widget) => Ref a -> IO (Maybe (Ref b)) -> IO Bool
isWidget this thatM = thatM >>= maybe (return False) (refPtrEquals this)

insideRectangle :: Position -> Rectangle -> Bool
insideRectangle (Position (X x) (Y y)) rect =
  let (rx,ry,rw,rh) = fromRectangle rect
  in x >= rx && x <= rx+rw && y >= ry && y <= ry+rh

roundedBoxPoints :: Rectangle -> Maybe Int -> Maybe OpenBorder -> [Position]
roundedBoxPoints (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) maybeRadius maybeOpen =
  let insideW = w-1
      insideH = h-1
  in
    map toPosition
      (case (maybeRadius,maybeOpen) of
         (Just radius, Nothing) ->
           [
             (x+radius,y)
           , (x+insideW-radius,y)
           , (x+insideW,y+radius)
           , (x+insideW,y+insideH-radius)
           , (x+insideW-radius,y+insideH)
           , (x+radius,y+insideH)
           , (x,y+insideH-radius)
           , (x,y+radius)
           , (x+radius,y)
           ]
         (Just radius, Just OpenBorderBottom) ->
           [
             (x,y+h)
           , (x,y+radius)
           , (x+radius,y)
           , (x+insideW-radius,y)
           , (x+insideW,y+radius)
           , (x+insideW,y+h)
           ]
         (Just radius, Just OpenBorderTop) ->
           [
             (x+insideW,y)
           , (x+insideW,y+insideH-radius)
           , (x+insideW-radius,y+insideH)
           , (x+radius,y+insideH)
           , (x,y+insideH-radius)
           , (x,y)
           ]
         (Nothing, Just OpenBorderBottom) ->
           [
             (x,y+insideH)
           , (x,y)
           , (x+insideW,y)
           , (x+insideW,y+insideH)
           ]
         (Nothing, Just OpenBorderTop) ->
           [
             (x,y)
           , (x,y+insideH)
           , (x+insideW,y+insideH)
           , (x+insideW,y)
           ]
         (Nothing, Nothing) ->
           [
             (x,y)
           , (x+insideW,y)
           , (x+insideW,y+insideH)
           , (x,y+insideH)
           , (x,y)
           ])

drawBorderBox :: (Parent a Widget) => Ref a -> BorderBoxSpec -> Bool -> IO ()
drawBorderBox w spec shouldFill = do
  oldColor <- flcColor
  focused <- isWidget w FL.focus
  hovering <- isWidget w FL.belowmouse
  when shouldFill (flcRectfWithColor (borderBoxBounds spec) (borderBoxFillColor spec))
  if focused
    then flcSetColor (borderBoxFocusedColor spec)
    else if hovering
         then flcSetColor (borderBoxHoveringColor spec)
         else flcSetColor (borderBoxColor spec)
  flcBeginLine
  mapM_ (flcVertex . toPrecisePosition) (roundedBoxPoints (borderBoxBounds spec) Nothing Nothing)
  flcEndLine
  flcSetColor oldColor

handleHover ::
  (
    Parent orig Widget,
    Match x ~ FindOp orig orig (Redraw ()),
    Op (Redraw ()) x orig (IO ()),
    Match y ~ FindOp orig orig (HandleSuper ()),
    Op (HandleSuper ()) y orig (Event -> IO (Either UnknownEvent ()))
  )
  => Ref orig -> Event -> IO (Either UnknownEvent ())
handleHover b e = do
  case e of
    Enter -> do
      () <- redraw b
      return (Right())
    Leave -> do
      () <- redraw b
      return (Right ())
    _ -> handleSuper b e

withCustomBoxDraw :: Boxtype -> BoxDrawF -> IO () -> IO ()
withCustomBoxDraw boxtype customBoxDrawF action = do
  fptr <- FL.getBoxtypePrim boxtype
  dx <- FL.boxDx boxtype
  dy <- FL.boxDy boxtype
  dw <- FL.boxDw boxtype
  dh <- FL.boxDh boxtype
  FL.setBoxtype boxtype (FL.FromSpec customBoxDrawF 0 0 0 0)
  action
  FL.setBoxtype boxtype (FL.FromFunPtr fptr (fromIntegral dx) (fromIntegral dy) (fromIntegral dw) (fromIntegral dh))

data Assets =
  Assets
  {
    _dejaVuSans :: Font
  , _yanoneKaffesatz :: Font
  , _returnButtonImage :: Ref PNGImage
  , _okButtonImage :: Ref PNGImage
  , _cancelButtonImage :: Ref PNGImage
  } deriving Show

dejaVuSans :: (?assets :: Assets) => Font
dejaVuSans = _dejaVuSans ?assets
yanoneKaffesatz :: (?assets :: Assets) => Font
yanoneKaffesatz = _yanoneKaffesatz ?assets
returnButtonImage :: (?assets :: Assets) => Ref PNGImage
returnButtonImage = _returnButtonImage ?assets
okButtonImage :: (?assets :: Assets) => Ref PNGImage
okButtonImage = _okButtonImage ?assets
cancelButtonImage :: (?assets :: Assets) => Ref PNGImage
cancelButtonImage = _cancelButtonImage ?assets

fonts :: [FilePath]
fonts =
  [
    "fonts/DejaVuSans.ttf"
  , "fonts/YanoneKaffeesatz-Regular.ttf"
  ]

fontNames :: [T.Text]
fontNames =
  [
    "DejaVu Sans"
  , "Yanone Kaffeesatz Regular"
  ]

imagePaths :: [FilePath]
imagePaths =
  [
    "images/dialog-ok.png"
  , "images/dialog-apply.png"
  , "images/dialog-cancel.png"
  ]

loadAssets :: IO Assets
loadAssets = do
  fontPaths <- mapM getDataFileName fonts
  loaded <- mapM loadFont fontPaths
  let errors = catMaybes (Data.List.map (\p -> case p of { Left err -> Just err; _ -> Nothing}) loaded)
  when (not (Data.List.null errors))
    (ioError (userError ("loadAssets (fata error): unable to load font assets for the Simple theme:\n" ++
                        (Data.List.concat (Data.List.intersperse "\n" errors)))))
  ips <- mapM getDataFileName imagePaths
  images <- mapM
              (\p -> do
                  bytes <- B.readFile p `catch`
                            (\(e :: SomeException) -> ioError
                                 (userError ("loadAssets (fatal error): image does not exist at path: " ++ p)))
                  iE <- pngImageNewWithData (T.pack "") bytes
                  case iE of
                    Left _ -> ioError (userError ("loadAssets (fatal error): unable to read image data into a PNGImage:\n" ++ p))
                    Right i -> return i)
              ips
  FL.setColor background2Color lightBackground
  numFaces <- FL.setFonts Nothing
  let fonts = Prelude.map Font [0 .. numFaces - 1 ]
  withFaces <- mapM
               (
                  \f -> do
                    (face,_) <- FL.getFontName f
                    return (face,f)
                )
                fonts
  let fontNumbers = catMaybes (Data.List.map (\fName -> Prelude.lookup fName withFaces) fontNames)
  FL.setScheme "gtk+"
  let assets =
       if (Data.List.length fontNumbers /= Data.List.length fontNames)
          then Assets helvetica helvetica (images !! 0) (images !! 1) (images !! 2)
          else Assets (fontNumbers !! 0) (fontNumbers !! 1) (images !! 0) (images !! 1) (images !! 2)
  tooltipSetFont (_dejaVuSans assets)
  tooltipSetSize commonFontSize
  return assets
