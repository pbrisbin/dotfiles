{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
--
-- xmonad.hs, pbrisbin 2013
--
-------------------------------------------------------------------------------
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.Run

import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad
     $ withMyUrgencyHook
     $ defaultConfig
        { terminal   = "urxvtc"
        , manageHook = composeAll
            [ isFullscreen --> doFullFloat
            , manageHook defaultConfig
            ]
        }
        `additionalKeysP`
            [ ("M-p", spawn "x=$(yeganesh -x -- -i -fn Verdana-12) && exec $x")
            , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
            , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
            , ("<XF86AudioMute>",        spawn "amixer sset Master toggle")
            ]

withMyUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
withMyUrgencyHook = withUrgencyHookC LibNotifyUrgencyHook
                  $ urgencyConfig { suppressWhen = Focused }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" ["Terminal Bell", "Workspace: " ++ idx]
