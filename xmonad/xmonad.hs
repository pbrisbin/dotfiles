import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

main :: IO ()
main = xmonad $ def
    { terminal = "urxvtc"
    , logHook = mconcat
        [ logHook def
        , ewmhDesktopsLogHook
        ]
    , manageHook = mconcat
        [ manageHook def
        , className =? "Vncviewer" --> doFloat
        , className =? "zoom" --> doFloat
        , role =? "pop-up" --> doFloat
        ]
    }
    `additionalKeysP` [("M-S-p", spawn "passmenu --type")]

  where
    role = stringProperty "WM_WINDOW_ROLE"
