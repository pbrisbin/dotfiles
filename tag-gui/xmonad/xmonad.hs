import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad $ defaultConfig
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
