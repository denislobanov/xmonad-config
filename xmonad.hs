import System.Exit
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt (defaultXPConfig)
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Util.EZConfig
import XMonad.Util.Run
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

myKeys c = mkKeymap c $
    [ ("M-<Return>",        spawn $ XMonad.terminal c)
    , ("M-<Space>",         sendMessage NextLayout)
    , ("M-S-<Return>",      windows W.swapMaster)
    , ("M-S-c",             kill)
    , ("M-S-q",             io (exitWith ExitSuccess))
    , ("M-x",               shellPrompt defaultXPConfig)]
    ++
    [(m ++ k, windows $ f w)
        | (w, k) <- zip (XMonad.workspaces c) (map show [1..9])
    , (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    ]
 
myLogHook xmobar = dynamicLogWithPP $ defaultPP
   {  ppOutput = hPutStrLn xmobar
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

main = do xmobar <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { terminal = "urxvt -e /usr/bin/zsh"
        , focusFollowsMouse = True
        , borderWidth = 1
        , normalBorderColor = "#444"
        , focusedBorderColor = "#f00"
        , workspaces         = [ show x | x <- [1..9] ]
        , keys = myKeys
        , mouseBindings = myMouseBindings
        , logHook = myLogHook xmobar
        }

