import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Actions.Navigation2D
import XMonad.Actions.WindowGo (raiseMaybe)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main = do
    h <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal = "urxvt -e /usr/bin/zsh"
        , focusFollowsMouse = True
        , borderWidth = 1
        , normalBorderColor = "#444"
        , focusedBorderColor = "#f00"
        , workspaces         = [ show x | x <- [1..9] ]
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , logHook = myLogHook h
        , modMask = mod4Mask
        }

myKeys c = mkKeymap c $
    [ ("M-<Return>",        spawn $ XMonad.terminal c)

    -- modify layout
    , ("M-<Space>",         sendMessage NextLayout)
    , ("M-S-<Return>",      windows W.swapMaster)
    , ("M-b",               sendMessage ToggleStruts)

    -- process control
    , ("M-S-c",             kill)
    , ("M-S-q",             io (exitWith ExitSuccess))
    , ("M-r",               shellPrompt defaultXPConfig)

    -- resizing
    , ("M-C-h",             sendMessage Shrink)
    , ("M-C-l",             sendMessage Expand)

    -- swap windows
    , ("M-S-h",             windowSwap L False)
    , ("M-S-l",             windowSwap R False)
    , ("M-S-j",             windowSwap D False)
    , ("M-S-k",             windowSwap U False)

    -- move focus
    , ("M-h",               windowGo L False)
    , ("M-l",               windowGo R False)
    , ("M-j",               windowGo D False)
    , ("M-k",               windowGo U False)

    -- move screens/workspaces
    , ("M-<L>",             screenGo L False)
    , ("M-<R>",             screenGo R False)

    -- launch applications
    , ("M-S-f",             spawn "urxvt -e ranger")
    , ("M-S-m",               raiseMaybe (spawn "urxvt -name mutt -e mutt") (title =? "mutt")) 
    , ("M-S-n",             spawn "urxvt -e ncmpcpp")
    
    -- shortcuts & other keybindings
    , ("<XF86AudioMute>",   spawn "amixer -D pule set Master 1+ togglemut")
    , ("<XF86AudioLowerVolume>", spawn "amixer -q set Master 1%-")
    , ("<XF86AudioRaiseVolume>", spawn "amixer -q set Master 1%+")
    , ("<XF86AudioPlay>", spawn "mpc toggle")
    , ("M-<Home>",          spawn "i3lock -di /home/xc/images/lockscreen.png")
    ]
    ++
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
    , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]

myLogHook h = dynamicLogWithPP $ defaultPP
   {  ppOutput = hPutStrLn h
   , ppTitle = xmobarColor "white" "" . shorten 110
   , ppCurrent = xmobarColor "white" "black" . pad
   , ppHidden = pad
   , ppHiddenNoWindows = \w -> xmobarColor "#444" "" (" " ++ w ++ " ")
   , ppSep = xmobarColor "#555" "" " / "
   , ppWsSep = ""
   , ppLayout = \x -> case x of
        "Tall" -> "T"
        "Mirror Tall" -> "M"
        "Full" -> "F"
        _ -> "?"
   }

myLayoutHook = smartBorders $ avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 4/100

myManageHook = composeAll
               [ floatC "MPlayer"
               , floatC "Gimp"]
    where moveToC c w = className =? c --> doF (W.shift w)
          moveToT t w = title     =? t --> doF (W.shift w)
          floatC  c   = className =? c --> doFloat

