------------------------------------------------------------------------------
-- |
-- Module      : Hacks
-- Description : Hacks
--
-- Copyright   : (c) Samuli Thomasson, 2022
-- License     : WTFPL
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
--
--
------------------------------------------------------------------------------
module Hacks (
  module Hacks
  ) where

-- * Comment area

{-
-- similar to DynamicProperty.dynamicPropertyChange, but acting on MapRequestEvents
docksEventHookExtra :: Event -> X All
docksEventHookExtra MapRequestEvent{ev_window = w} = do
    whenX (runQuery ManageDocks.checkDock w) $ do
      SizeHints{sh_win_gravity = wg} <- withDisplay $ \d -> io (getWMNormalHints d w)
      when (wg == Just staticGravity) $ moveWindowPerStrutPartial w
    return (All True)
docksEventHookExtra _ = return (All True)

moveWindowPerStrutPartial :: Window -> X ()
moveWindowPerStrutPartial w = do
    wda  <- getAtom "_NET_WM_DESKTOP"
    wsa  <- getAtom "_NET_WM_STRUT"
    wspa <- getAtom "_NET_WM_STRUT_PARTIAL"
    msp <- WinProp.getProp32 wspa w
    case msp of
      Just sp@[l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2] -> do
        rootw <- asks theRoot
        rwa <- withDisplay $ \d -> io (getWindowAttributes d rootw)
        mda <- WinProp.getProp32 wda w
        case mda of
          Just _ -> return ()
          Nothing -> do
            sr <- withWindowSet $ return . screenRect . W.screenDetail . W.current
            move' rwa sr (calcStruts rwa sr (map fi sp))
      _ -> return ()
  where

    calcStruts :: WindowAttributes -> Rectangle -> [Int32] -> [Int32]
    calcStruts rwa sr ps@(l:r:t:b:ly1:ly2:ry1:ry2:tx1:tx2:bx1:bx2:_)
      | l > 0 = [l + fi (rect_x sr),0,0,0, rect_y sr + ly1,rect_y sr + ly2, 0,0, 0,0, 0,0]
      | r > 0 = [0,fi (wa_width rwa) - rect_x sr - fi (rect_width sr) + r,0,0, 0,0, rect_y sr + ry1,rect_y sr + ry2, 0,0, 0,0]
      | t > 0 = [0,0,fi (rect_y sr) + t,0, 0,0, 0,0, rect_x sr + tx1,rect_x sr + max 0 (tx2 - 1), 0,0]
      | b > 0 = [0,0,0,b, 0,0, 0,0, 0,0, rect_x sr + bx1,rect_x sr + max 0 (bx2 - 1)]
      | otherwise = ps
    calcStruts rwa sr ps = ps

    move' :: WindowAttributes -> Rectangle -> [Int32] -> X ()
    move' rwa Rectangle{rect_x = sx, rect_y = sy, rect_width = sw} sp@[l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2] = withDisplay $ \d -> do
      wspa <- getAtom "_NET_WM_STRUT_PARTIAL"
      io (changeProperty32 d w wspa cARDINAL propModeReplace (map fi sp))
      when (l > 0) $ io $ moveWindow d w sx ly1
      when (r > 0) $ io $ moveWindow d w (fi (wa_width rwa) - r) ry1
      when (t > 0) $ io $ moveWindow d w tx1 sy
      when (b > 0) $ io $ moveWindow d w bx1 (fi (wa_height rwa) - b)

    move' rwa _ _ = return ()
-}
