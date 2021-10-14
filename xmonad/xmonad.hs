import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.EZConfig

-- brittany-disable-next-binding

main :: IO ()
main = xmonad $ def
    { terminal = "alacritty"
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
