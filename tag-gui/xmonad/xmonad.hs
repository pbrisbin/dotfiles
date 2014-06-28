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
import XMonad.Util.NamedWindows
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import qualified XMonad.StackSet as W

main :: IO ()
main = do
    let conf = withMyUrgencyHook $ defaultConfig
            { terminal   = "urxvtc"
            , manageHook = composeAll
                [ isFullscreen    --> doFullFloat
                , title =? "mail" --> doCenterFloat
                , scratchpadManageHook (W.RationalRect 0.6 0.8 0.35 0.1)
                , manageHook defaultConfig
                ]
            }

    xmonad $ conf `additionalKeysP`
        [ ("M-p"                   , spawn "gmrun"             )
        , ("M-S-p"                 , scratchpadSpawnAction conf)
        , ("M-S-m"                 , spawn "mail"              )
        , ("<XF86AudioRaiseVolume>", spawn "nmixer up"         )
        , ("<XF86AudioLowerVolume>", spawn "nmixer down"       )
        , ("<XF86AudioMute>"       , spawn "nmixer toggle"     )
        ]

withMyUrgencyHook :: LayoutClass l Window => XConfig l -> XConfig l
withMyUrgencyHook = withUrgencyHookC LibNotifyUrgencyHook
                  $ urgencyConfig { suppressWhen = Focused }

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" ["-i", "emblem-important", show name, "workspace " ++ idx]
