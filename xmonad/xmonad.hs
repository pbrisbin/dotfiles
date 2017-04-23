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
    }
    `additionalKeysP` [("M-S-p", spawn "passmenu --type")]
