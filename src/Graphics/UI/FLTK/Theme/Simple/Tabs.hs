{-# LANGUAGE ImplicitParams, OverloadedStrings, AllowAmbiguousTypes, GADTs, CPP, ExistentialQuantification, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
module Graphics.UI.FLTK.Theme.Simple.Tabs
  (
    TabSpec(..),
    drawTabs,
    tabClientAreaCustom,
    tabHandleCustom,
    tabHeightCustom,
    tabPositionsCustom,
    tabRedrawTabs,
    tabWhichCustom,
    tabsNew
  )
where
import Control.Monad
import Data.IORef
import Data.List
import Graphics.UI.FLTK.LowLevel.Dispatch (safeCast)
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.Theme.Simple.Common
import qualified Data.Text as T
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as LowLevel

data TabSpec =
  TabSpec
  {
    tabRadius :: Int,
    tabHorizontalPadding :: Int,
    tabVerticalPadding :: Int,
    tabInactiveBorderColor :: Color,
    tabBorderColor :: Color,
    tabFillColor :: Color,
    tabViewOffset :: IORef Int,
    tabPartiallyVisible :: Int
  }

drawTabs :: TabSpec -> Ref LowLevel.Tabs -> IO ()
drawTabs spec t = do
  bounds <- LowLevel.getRectangle t
  (selectedMaybe, _,widths) <- tabPositionsCustom spec t
  if (null widths) then return ()
    else do
      (Height tabHeight) <- tabHeightCustom spec t
      offset <- readIORef (tabViewOffset spec)
      let Rectangle (Position (X tabsX) (Y tabsY)) (Size (Width tabsWidth) (Height tabsHeight)) = bounds
          (AtIndex tabSelectedIndex) = maybe (AtIndex 0) id selectedMaybe
          offsetWidths = map (\(X x,Width w) -> (X (x+offset), Width w)) widths
          (X tabSelectedX, Width tabSelectedWidth) = offsetWidths !! tabSelectedIndex
      oldShortcut <- LowLevel.flcDrawShortcut
      oldColor <- LowLevel.flcColor
      LowLevel.flcSetDrawShortcut (Just NormalDrawShortcut)
      cs <- LowLevel.getArray t
      damages <- LowLevel.getDamage t
      when (elem DamageAll damages || elem DamageScroll damages)
        (let (X leftMostX,_) = head offsetWidths
             rightMostX = let (X rightX, Width w) = last offsetWidths in rightX + w
             indexedWidths = zip (map AtIndex [0 ..]) offsetWidths
             tabBorder yOffset openBorder = do
               LowLevel.flcPushClip (Rectangle (Position (X tabsX) yOffset) (Size (Width tabsWidth) (Height tabsHeight)))
               LowLevel.flcBeginLine
               LowLevel.flcVertex (toPrecisePosition (Position (X leftMostX) yOffset))
               LowLevel.flcVertex (toPrecisePosition (Position (X tabSelectedX) yOffset))
               LowLevel.flcEndLine
               LowLevel.flcBeginLine
               LowLevel.flcVertex (toPrecisePosition (Position (X (tabSelectedX + tabSelectedWidth)) yOffset))
               let tabsEnd = if (tabsX + tabsWidth < rightMostX) then tabsX + tabsWidth else rightMostX
               LowLevel.flcVertex (toPrecisePosition (Position (X tabsEnd) yOffset))
               LowLevel.flcEndLine
               LowLevel.flcPopClip
               let widgetBorder =
                     case openBorder of
                       OpenBorderTop ->
                         Rectangle
                           (Position (X tabsX) (Y (tabsY+tabHeight)))
                           (Size (Width tabsWidth) (Height (tabsHeight-(abs tabHeight))))
                       OpenBorderBottom ->
                         Rectangle
                           (Position (X tabsX) (Y tabsY))
                           (Size (Width tabsWidth) (Height (tabsHeight-(abs tabHeight))))
               LowLevel.flcBeginLine
               mapM_ (LowLevel.flcVertex . toPrecisePosition) (roundedBoxPoints widgetBorder Nothing (Just openBorder))
               LowLevel.flcEndLine
             drawTab openBorder yOffset (AtIndex i, (X tabX,Width tabWidth)) = do
               let tabPosition = Position (X tabX) yOffset
                   tabRectangle = Rectangle tabPosition (Size (Width tabWidth) (Height (abs tabHeight)))
               oldColor <- LowLevel.flcColor
               -- blank it out first
               drawBorderBox t
                  (BorderBoxSpec
                     {
                       borderBoxBounds = tabRectangle,
                       borderBoxFocusedColor = tabFillColor spec,
                       borderBoxHoveringColor = tabFillColor spec,
                       borderBoxColor = tabFillColor spec,
                       borderBoxFillColor = tabFillColor spec
                     })
                  True
               if (i == tabSelectedIndex) then
                 LowLevel.flcSetColor (tabBorderColor spec)
                 else LowLevel.flcSetColor (tabInactiveBorderColor spec)
               LowLevel.flcBeginLine
               mapM_ (LowLevel.flcVertex . toPrecisePosition) (roundedBoxPoints tabRectangle (Just (tabRadius spec)) (Just openBorder))
               LowLevel.flcEndLine
               let widget = cs !! i
               oldLabeltype <- LowLevel.getLabeltype widget
               case oldLabeltype of { NoLabelType -> LowLevel.setLabeltype widget NormalLabelType ResolveImageLabelDoNothing; _ -> return () }
               tabAlign <- LowLevel.getTabAlign t
               LowLevel.drawLabel widget (Just (tabRectangle, Alignments [tabAlign]))
               LowLevel.setLabeltype widget oldLabeltype ResolveImageLabelDoNothing
               LowLevel.flcSetColor oldColor
          in
          if (tabHeight < 0)
          then do
            oldColor <- LowLevel.flcColor
            let yOffset = tabsY + tabsHeight - (abs tabHeight)
            LowLevel.flcPushClip (toRectangle (tabsX,yOffset,tabsWidth,abs tabHeight))
            mapM_ (drawTab OpenBorderTop (Y yOffset)) indexedWidths
            LowLevel.flcPopClip
            LowLevel.flcSetColor (tabBorderColor spec)
            tabBorder (Y yOffset) OpenBorderBottom
            LowLevel.flcSetColor oldColor
          else do
            oldColor <- LowLevel.flcColor
            LowLevel.flcPushClip (toRectangle (tabsX,tabsY,tabsWidth,tabHeight))
            mapM_ (drawTab OpenBorderBottom (Y tabsY)) indexedWidths
            LowLevel.flcPopClip
            LowLevel.flcSetColor (tabBorderColor spec)
            tabBorder (Y (tabsY + tabHeight)) OpenBorderTop
            LowLevel.flcSetColor oldColor)
      if (elem DamageAll damages)
        then LowLevel.drawChild t (cs !! tabSelectedIndex)
        else LowLevel.updateChild t (cs !! tabSelectedIndex)
      LowLevel.flcSetColor oldColor
      LowLevel.flcSetDrawShortcut oldShortcut

tabRedrawTabs :: TabSpec -> Ref LowLevel.Tabs -> IO ()
tabRedrawTabs spec t = do
  bounds <- LowLevel.getRectangle t
  let (tabsX,tabsY,tabsWidth,tabsHeight) = fromRectangle bounds
  (Height tabHeight) <- LowLevel.tabHeight t
  LowLevel.setDamageInside t [DamageScroll]
    (if (tabHeight < 0)
      then toRectangle (tabsX,tabsY+tabsHeight-(abs tabHeight),tabsWidth,(abs tabHeight))
      else toRectangle (tabsX,tabsY,tabsWidth,tabHeight))

tabHandleCustom :: TabSpec -> Ref LowLevel.Tabs -> Event -> IO (Either UnknownEvent ())
tabHandleCustom spec t e = do
  tl <- LowLevel.getLabel t
  bounds <- LowLevel.getRectangle t
  let (tabsX,tabsY,tabsWidth,tabsHeight) = fromRectangle bounds
  offset <- readIORef (tabViewOffset spec)
  pos@(Position (X x') (Y y')) <- FL.eventPosition
  nextIndexAndWidget <- tabWhichCustom spec t pos
  (_,_,widths) <- tabPositionsCustom spec t
  let nextWidget = fmap (\(_,w) -> w) nextIndexAndWidget
      nextIndex = fmap (\(i,_) -> i) nextIndexAndWidget
      setSelectedWidget = LowLevel.setPush t nextWidget >> return (Right ())
      offsetWidths = map (\(X x, Width w) -> (X (x+offset), Width w)) widths
      adjustOffset newOffset = writeIORef (tabViewOffset spec) (offset-newOffset)
      partiallyShow i =
        let (_,Width w) = offsetWidths !! i in
        if (w >= (tabPartiallyVisible spec))
        then (tabPartiallyVisible spec)
        else w
      handleRelease = do
        tabBarRect <- tabBarRectangle spec t
        if (not (insideRectangle pos tabBarRect))
          then LowLevel.handleSuper t e
          else do
            lastSelected <- LowLevel.getPush t
            LowLevel.setPush t (Nothing :: (Maybe (Ref LowLevel.Widget)))
            focusedWidget <- FL.focus
            case lastSelected of
              Nothing -> return (Right ())
              Just w -> do
                vf <- FL.visibleFocus
                when vf
                  (do
                    imFocused <- refPtrEquals w t
                    if imFocused then tabRedrawTabs spec t
                    else FL.setFocus(t) >> tabRedrawTabs spec t)
                changed <- LowLevel.setValue t lastSelected
                case changed of
                  Left NoChange -> do
                    whens <- LowLevel.getWhen t
                    when (WhenNotChanged `elem` whens) (LowLevel.setChanged t >> LowLevel.doCallback t)
                  Right () -> LowLevel.setChanged t >> LowLevel.doCallback t
                deleted <- isNull w
                if (not deleted)
                  then LowLevel.tooltipSetCurrent (Just w) >> return (Right())
                  else return (Right ())
      handlePush =
        let adjustWhenNot yOutsideRange =
              if yOutsideRange then LowLevel.handleSuper (safeCast t :: Ref LowLevel.Group) e
              else
                case nextIndex of
                  Just (AtIndex i) ->
                    let (X x, Width w) = offsetWidths !! i
                        rightOverflow = (x+w) - (tabsX+tabsWidth)
                        leftOverflow = x-tabsX
                    in do
                    case i of
                      _ | i == 0 -> adjustOffset leftOverflow
                        | i == (length widths - 1) -> adjustOffset rightOverflow
                        | otherwise ->
                            if (rightOverflow >= 0)
                            then adjustOffset (rightOverflow + partiallyShow (i+1))
                            else if (leftOverflow < 0)
                                 then adjustOffset (leftOverflow - partiallyShow (i-1))
                                 else return ()
                    setSelectedWidget
                  _ -> setSelectedWidget
        in do
        (Height tabHeight) <- LowLevel.tabHeight t
        if (tabHeight < 0)
        then adjustWhenNot (y' < tabsY + tabsHeight - (abs tabHeight))
        else adjustWhenNot (y' > tabsY + tabHeight)

  case e of
    Push -> handlePush
    Drag -> handleRelease
    Release -> handleRelease
    Keydown -> do
      (selectedMaybe,_,widths) <- tabPositionsCustom spec t
      eventKey <- FL.eventKey
      case eventKey of
        SpecialKeyType Kb_Left ->
          case selectedMaybe of
            Just (AtIndex i) ->
              if (i == 0) then LowLevel.handleSuper t e
                else
                let (X x, Width w) = offsetWidths !! (i-1)
                    currOffset = x - tabsX
                in do
                 if (currOffset >= 0)
                 then return ()
                 else adjustOffset (currOffset - (if (i > 1) then partiallyShow (i-2) else 0))
                 LowLevel.handleSuper t e
            _ -> LowLevel.handleSuper t e
        SpecialKeyType Kb_Right ->
          case selectedMaybe of
            Just (AtIndex i) ->
              if (i == ((length widths) - 1)) then LowLevel.handleSuper t e
                else
                let (X x,Width w) = offsetWidths !! (i+1)
                    currOffset = (x+w) - (tabsX+tabsWidth)
                in do
                 when (currOffset >= 0) (adjustOffset (currOffset + (if (i < (length widths - 2)) then partiallyShow (i+2) else 0)))
                 LowLevel.handleSuper t e
            _ -> LowLevel.handleSuper t e
        _ -> LowLevel.handleSuper t e
    _ -> LowLevel.handleSuper t e

tabPositionsCustom :: TabSpec -> Ref LowLevel.Tabs -> IO (Maybe AtIndex, Int, [(X,Width)])
tabPositionsCustom spec t = do
  bounds <- LowLevel.getRectangle t
  cs <- LowLevel.getArray t
  let Rectangle (Position (X x) _) _ = bounds
  selected <- selectedTab (zip (map AtIndex [0 ..]) cs)
  widths <-
    mapM
      (\c -> do
          oldLabeltype <- LowLevel.getLabeltype c
          oldAlign <- LowLevel.getAlign c
          case oldLabeltype of { NoLabelType -> LowLevel.setLabeltype c NormalLabelType ResolveImageLabelDoNothing; _ -> return () }
          (Size (Width width') _) <- LowLevel.measureLabel c Nothing
          tabAlign <- LowLevel.getTabAlign t
          LowLevel.setAlign c (Alignments [tabAlign])
          LowLevel.setLabeltype c oldLabeltype ResolveImageLabelDoNothing
          LowLevel.setAlign c oldAlign
          return (width' + (tabHorizontalPadding spec) * 2)
      )
      cs
  let xOffsets = zip (map X (map ((+) x) (scanl (+) 0 widths))) (map Width widths)
  return (selected, (0 :: Int), xOffsets)
  where
   selectedTab :: [(AtIndex, Ref LowLevel.Widget)] -> IO (Maybe AtIndex)
   selectedTab [] = return Nothing
   selectedTab ((i,w):ws) = do
     v <- LowLevel.getVisible w
     if v then return (Just i) else selectedTab ws

tabHeightCustom :: TabSpec -> Ref LowLevel.Tabs -> IO Height
tabHeightCustom spec t = do
  cs <- LowLevel.getArray t
  (Y y) <- LowLevel.getY t
  (Height h) <- LowLevel.getH t
  tops <- mapM (\c -> do {(Y y) <- LowLevel.getY c; return (y+(tabVerticalPadding spec)*2);}) cs
  bottoms <- mapM (\c -> do {(Y y) <- LowLevel.getY c; (Height h) <- LowLevel.getH c; return (y+h-(tabVerticalPadding spec)*2);}) cs
  let topGap = minimum (map (\cy -> cy - y) tops)
      bottomGap = y+h - (maximum bottoms)
      height = if (bottomGap > topGap) then
                 if bottomGap <= 0 then 0 else (-bottomGap)
                 else if topGap <= 0 then 0 else topGap
  return (Height height)

tabClientAreaCustom :: TabSpec -> Ref LowLevel.Tabs -> LowLevel.TabsHeightOffset -> IO Rectangle
tabClientAreaCustom spec t tOffset = do
  cs <- LowLevel.getArray t
  case cs of
    (c:_) -> LowLevel.getRectangle c
    _ -> do
      lf <- LowLevel.getLabelfont t
      ls <- LowLevel.getLabelsize t
      (Height h) <- LowLevel.flcHeightOfFont lf ls
      let paddedHeight = h+(tabVerticalPadding spec * 2)
      (tabsX,tabsY,tabsW,tabsH) <- fmap fromRectangle (LowLevel.getRectangle t)
      let clientArea = case tOffset of
                         LowLevel.TabsAtTop Nothing -> (tabsX,tabsY+paddedHeight,tabsW,tabsH-paddedHeight)
                         LowLevel.TabsAtBottom Nothing -> (tabsX,tabsY,tabsW,tabsH-paddedHeight)
                         LowLevel.TabsAtTop (Just o) -> (tabsX,tabsY+o,tabsW,tabsH-o)
                         LowLevel.TabsAtBottom (Just o) -> (tabsX,tabsY,tabsW,tabsH-o)
      return (toRectangle clientArea)

tabBarRectangle :: TabSpec -> Ref LowLevel.Tabs -> IO Rectangle
tabBarRectangle spec t = do
  (tabsX,tabsY,tabsW,tabsH) <- fmap fromRectangle (LowLevel.getRectangle t)
  (Height tabHeight) <- tabHeightCustom spec t
  return
    (if (tabHeight < 0)
      then toRectangle (tabsX,tabsY+tabsH-(abs tabHeight),tabsW,abs tabHeight)
      else toRectangle (tabsX,tabsY,tabsW,tabHeight))

tabWhichCustom :: TabSpec -> Ref LowLevel.Tabs -> Position -> IO (Maybe (AtIndex, Ref LowLevel.Widget))
tabWhichCustom spec t pos = do
  tabBarRect <- tabBarRectangle spec t
  if (not (insideRectangle pos tabBarRect))
    then return Nothing
    else do
      let (_,tabBarY,_,tabBarHeight) = fromRectangle tabBarRect
      (_,_,widths) <- tabPositionsCustom spec t
      offset <- readIORef (tabViewOffset spec)
      let tab = find
                  (\(i,(x,w)) -> insideRectangle pos (Rectangle (Position x (Y tabBarY)) (Size w (Height tabBarHeight))))
                  (zip (map AtIndex [0 ..]) (map (\(X x, Width w) -> (X (x+offset), Width w)) widths))
      cs <- LowLevel.getArray t
      return (fmap (\(AtIndex i, _) -> (AtIndex i,cs !! i)) tab)

tabSpec :: (?assets :: Assets) => Rectangle -> IO TabSpec
tabSpec rectangle = do
  let (tabsX,tabsY,tabsWidth,tabsHeight) = fromRectangle rectangle
      buttonWidth = 15
      buttonHeight = 15
  borderColor <- commonSelectionColor
  color <- commonColor
  offset <- newIORef 0
  return
    (TabSpec
     {
       tabRadius = 1
     , tabHorizontalPadding = 10
     , tabVerticalPadding = 3
     , tabInactiveBorderColor = colorAverage color blackColor 0.97
     , tabBorderColor = borderColor
     , tabFillColor = lightBackground
     , tabViewOffset = offset
     , tabPartiallyVisible = 20
     })

tabsNew :: (?assets :: Assets) => Rectangle -> Maybe T.Text -> IO (Ref LowLevel.Tabs)
tabsNew rectangle label = do
  spec <- tabSpec rectangle
  t <- LowLevel.tabsCustom
         rectangle
         label
         (Just
           (LowLevel.CustomTabFuncs
             {
               LowLevel.tabDrawCustom = drawTabs spec
             , LowLevel.tabPositionsCustom = tabPositionsCustom spec
             , LowLevel.tabHeightCustom = tabHeightCustom spec
             , LowLevel.tabWhichCustom = tabWhichCustom spec
             , LowLevel.tabRedrawTabs = tabRedrawTabs spec
             , LowLevel.tabClientArea = tabClientAreaCustom spec
             }))
         (Just
            (LowLevel.defaultCustomWidgetFuncs
               {
                 LowLevel.handleCustom = Just (tabHandleCustom spec)
               }))
  p <- LowLevel.getParent t
  color <- maybe (return lightBackground) LowLevel.getColor p
  LowLevel.setColor t color
  LowLevel.setLabelfont t commonFont
  LowLevel.setLabelsize t commonFontSize
  return t
