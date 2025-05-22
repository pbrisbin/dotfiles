import Graphics.X11.Xlib.Extras
import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig

main :: IO ()
main =
  xmonad
    $ docks
    $ disableEwmhManageDesktopViewport
    $ ewmh
    $ ewmhFullscreen
    $ def
      { terminal = "alacritty"
      , manageHook =
          mconcat
            [ manageHook def
            , className =? "Vncviewer" --> doFloat
            , className =? "zoom" --> doFloat
            , role =? "page-info" --> doFloat
            , role =? "pop-up" --> doFloat
            , isDialog --> doFloat
            ]
      , layoutHook = avoidStruts $ layoutHook def
      }
      `additionalKeysP` [ ("M-p", spawn "dmenu_run -fn 'Noto Sans-16'")
                        , ("M-S-o", spawn "passmenu-otp --type -fn 'Noto Sans-16'")
                        , ("M-S-p", spawn "passmenu --type -fn 'Noto Sans-16'")
                        ]
 where
  role = stringProperty "WM_WINDOW_ROLE"

isDialog :: Query Bool
isDialog =
  ask >>= \w ->
    liftX $
      (\a d -> a == Just d)
        <$> getAtomProp "_NET_WM_WINDOW_TYPE" w
        <*> getAtom "_NET_WM_WINDOW_TYPE_DIALOG"

getAtomProp :: String -> Window -> X (Maybe Atom)
getAtomProp name w = do
  a <- getAtom name
  mbr <- withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w
  case mbr of
    Just [r] -> pure $ Just $ fromIntegral r
    _ -> pure Nothing
