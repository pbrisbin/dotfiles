{-# LANGUAGE FlexibleContexts #-}
-------------------------------------------------------------------------------
--
-- xmonad.hs, pbrisbin 2013
--
-------------------------------------------------------------------------------
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W

import qualified Data.Foldable as F

main :: IO ()
main = xmonad $ withUrgencyHook LibNotify $ defaultConfig
    { terminal = "urxvtc"
    , focusedBorderColor = "black"
    , logHook = mconcat
        [ logHook defaultConfig
        , ewmhDesktopsLogHook
        , fadeInactiveLogHook 0.7
        ]
    , manageHook = composeAll
        [ isFullscreen --> doFullFloat
        , namedScratchpadManageHook scratchpads
        , manageHook defaultConfig
        ]
    } `additionalKeysP`
        [ ("M-p", spawn "gmrun")
        , ("M-S-l", spawn "slock")
        , ("M-S-p", namedScratchpadAction scratchpads "scratch")
        , ("M-S-m", namedScratchpadAction scratchpads "mail")
        , ("<XF86AudioRaiseVolume>", spawn "nmixer up")
        , ("<XF86AudioLowerVolume>", spawn "nmixer down")
        , ("<XF86AudioMute>", spawn "nmixer toggle")
        ]

scratchpads :: [NamedScratchpad]
scratchpads =
    [ NS "mail" "urxvtc -e mutt" (title =? "mutt")
        (customFloating $ W.RationalRect 0.05 0.1 0.9 0.8)
    , NS "scratch" "urxvtc --title scratch" (title =? "scratch")
        (customFloating $ W.RationalRect 0.6 0.8 0.35 0.1)
    ]

data LibNotify = LibNotify deriving (Read, Show)

instance UrgencyHook LibNotify where
    urgencyHook LibNotify w = do
        (n, ws) <- (,)
            <$> getName w
            <*> gets windowset

        F.forM_ (W.findTag w ws) $ \idx -> safeSpawn "notify-send"
            ["-i" , "emblem-important" , show n , "workspace " ++ idx]
