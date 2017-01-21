import XMonad
import XMonad.Hooks.EwmhDesktops

main :: IO ()
main = xmonad $ def
    { terminal = "urxvtc"
    , logHook = mconcat
        [ logHook def
        , ewmhDesktopsLogHook
        ]
    }
