{-# LANGUAGE ImplicitParams, ScopedTypeVariables, OverloadedStrings #-}
module Graphics.UI.FLTK.Theme.Simple.Counter
  (
    counterComponentsBounds,
    counterNew,
    drawCounter,
    handleCounter
  )
where
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Simple.Button
import Graphics.UI.FLTK.Theme.Simple.Common
import Graphics.UI.FLTK.Theme.Simple.Input
import qualified Data.Text as T
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as LowLevel

data CounterButton = NormalLL | NormalL | NormalR | NormalRR | SimpleL | SimpleR deriving (Eq,Show)

counterComponentsBounds :: LowLevel.CounterType -> Rectangle -> [Rectangle]
counterComponentsBounds c rect =
  let (x',y',w',h') = fromRectangle rect
  in
    case c of
      LowLevel.SimpleCounterType ->
        let arrowButtonWidth = truncate ((fromIntegral w'*20) / 100)
            inputWidth = w' - (truncate ((fromIntegral w'*40) / 100))
        in
          [
            toRectangle (x',y',arrowButtonWidth,h')
          , toRectangle (x'+arrowButtonWidth+inputWidth,y',arrowButtonWidth,h')
          , toRectangle (x'+arrowButtonWidth,y',w'-(arrowButtonWidth*2),h')
          ]
      LowLevel.NormalCounterType ->
        let arrowButtonWidth = truncate ((fromIntegral w'*15) / 100)
        in
          [
            toRectangle (x',y',arrowButtonWidth,h')
          , toRectangle (x'+arrowButtonWidth,y',arrowButtonWidth,h')
          , toRectangle (x'+w'-arrowButtonWidth-arrowButtonWidth,y',arrowButtonWidth,h')
          , toRectangle (x'+w'-arrowButtonWidth,y',arrowButtonWidth,h')
          , toRectangle (x'+arrowButtonWidth+arrowButtonWidth,y',w'-(arrowButtonWidth*4),h')
          ]

calculatePressedButton :: Position -> Ref LowLevel.Counter -> IO (Either () (Maybe CounterButton))
calculatePressedButton pos c = do
  r <- LowLevel.getRectangle c
  ct <- LowLevel.getType_ c
  let cl = counterComponentsBounds ct r
      normalComponenets = [NormalLL,NormalL,NormalR,NormalRR]
      simpleComponents = [SimpleL,SimpleR]
  case cl of
     (dl:l:r:rr:_:[]) -> do
       let events = map (\r -> insideRectangle pos r) [dl,l,r,rr]
       return (maybe (Right Nothing) (\i -> (Right (Just (normalComponenets !! i)))) (Data.List.elemIndex True events))
     (l:r:_:[]) -> do
       let events = map (\r -> insideRectangle pos r) [l,r]
       return (maybe (Right Nothing) (\i -> (Right (Just (simpleComponents !! i)))) (Data.List.elemIndex True events))
     _ -> throwIO (userError "A Counter can only have 3 or 5 widget components.")

drawCounter :: IORef (Either () (Maybe CounterButton)) -> Ref LowLevel.Counter -> IO ()
drawCounter pressedRef c =
  let arrowBox color rect pressed logo = do
        selectionColor <- LowLevel.getSelectionColor c
        fontSize <- LowLevel.getLabelsize c
        spec <- makeFillSpec rect color selectionColor fontSize
        fillButton (spec { buttonRadius = 0 }) pressed
        LowLevel.flcBeginLine
        mapM_ (LowLevel.flcVertex . toPrecisePosition) (roundedBoxPoints rect Nothing Nothing)
        LowLevel.flcEndLine
        _ <- LowLevel.flcDrawSymbol (T.pack logo) rect selectionColor
        return ()
  in do
  pressed <- readIORef pressedRef
  color <- LowLevel.getColor c
  ct <- LowLevel.getType_ c
  r <- LowLevel.getRectangle c
  let cl = counterComponentsBounds ct r
      numButtons = length cl - 1
      valueBoxRect = last cl
  mapM_ (\(a,b,c)-> arrowBox color a b c)
    (zip3 (take (length cl - 1) cl)
      (case pressed of
         Left _                -> [False,False,False,False]
         Right Nothing         -> [False,False,False,False]
         Right (Just NormalLL) -> [True,False,False,False]
         Right (Just NormalL)  -> [False,True,False,False]
         Right (Just NormalR)  -> [False,False,True,False]
         Right (Just NormalRR) -> [False,False,False,True]
         Right (Just SimpleL)  -> [True,False]
         Right (Just SimpleR)  -> [False,True])
      (if (numButtons == 4)
        then ["@-4<<","@-4<","@-4>","@-4>>"]
        else ["@-4<","@-4>"]))
  str <- LowLevel.format c
  LowLevel.flcRectfWithColor valueBoxRect color
  inputBox False valueBoxRect color
  case str of
    Left _ -> LowLevel.flcDrawInBox "" valueBoxRect (Alignments [AlignTypeCenter]) Nothing Nothing
    Right s -> LowLevel.flcDrawInBox s valueBoxRect (Alignments [AlignTypeCenter]) Nothing Nothing

handleCounter :: IORef (Either () (Maybe CounterButton )) -> Ref LowLevel.Counter -> Event -> IO (Either UnknownEvent ())
handleCounter pressedRef c e = do
  pressed <- readIORef pressedRef
  let setPressed = do
        pos <- FL.eventPosition
        i <- calculatePressedButton pos c
        when (i /= pressed) (writeIORef pressedRef i)
  case e of
    Release -> do
      case pressed of
        Right  _ -> writeIORef pressedRef (Left ())
        Left _ -> return ()
    Push -> setPressed
    Drag -> setPressed
    _ -> return ()
  LowLevel.handleSuper c e

counterNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.Counter)
counterNew rect l = do
  i <- newIORef (Left ())
  c <- LowLevel.counterCustom rect l (Just (drawCounter i))
         (Just (LowLevel.defaultCustomWidgetFuncs
                 {
                   LowLevel.handleCustom = Just (handleCounter i)
                 }))
  LowLevel.setColor c lightBackground
  LowLevel.setBox c BorderBox
  LowLevel.setTextfont c commonFont
  LowLevel.setTextsize c commonFontSize
  color <- commonSelectionColor
  LowLevel.setSelectionColor c color
  LowLevel.setLabelfont c commonFont
  LowLevel.setLabelsize c commonFontSize
  return c
