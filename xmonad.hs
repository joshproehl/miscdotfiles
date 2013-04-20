import XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.Drawer -- TODO: Config
import XMonad.Layout.Spiral
import XMonad.Util.CustomKeys
import XMonad.Util.Run(spawnPipe)
import System.IO
import Data.List

-- Set up the hook for fadeWindows
myFadeHook = composeAll
  [ opaque                          -- Default to opaque windows
    , isFloating --> opacity 0.9
    , isUnfocused --> opacity 0.65    -- Unfocused is 65% of opaque
    , isDialog --> opaque             -- Dialog windows should go on top
  ]


-- Manage all of the various floation requirements
-- and send certain apps to certain places.
myManageHook = composeAll
  [ isFullscreen --> doFullFloat
    , className =? "Gimp" --> doFloat
    , className =? "Pidgin" --> doShift "9:comm"
    , isDialog --> doCenterFloat
  ]


myLayoutHook = avoidStruts $
  onWorkspace "9:comm" imLayout $
  standardLayouts
  where
    tall = Tall 1 0.02  0.5
    standardLayouts = tall ||| Mirror tall ||| Full ||| spiral (6/7)
    imLayout = withIM (1/5) (Role "buddy_list") Grid --(standardLayouts)


myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..7] ++ ["8:mail","9:comm"]

delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys XConfig {modMask = modm} = []

addKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addKeys conf@(XConfig {modMask = modm}) =
  [ ((modm, xK_a), spawn "xmenud") -- Mod-A to open app menu

  -- Cycle to workspaces
  , ((modm,               xK_Right),  nextWS)
  , ((modm,               xK_Left),   prevWS)
  , ((modm .|. shiftMask, xK_Right),  shiftToNext)
  , ((modm .|. shiftMask, xK_Left),   shiftToPrev)
  , ((modm,               xK_Down),   nextScreen)
  , ((modm,               xK_Up),     prevScreen)
  ]

main = do
-- Launch xmobar using the config specificed in it's rc file.
  xmproc <- spawnPipe "/usr/bin/xmobar /home/joshproehl/.xmobarrc"
  -- spawn xscreensaver -nosplash

  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
         $ defaultConfig
    { terminal    = "terminator"
    , modMask     = mod4Mask  -- Use the "windows" key as the mod key.
    , keys = customKeys delKeys addKeys
    , borderWidth = 1
    , normalBorderColor = "#444444"
    , focusedBorderColor = "#005577"
    , startupHook = setWMName "LG3D" --java hack
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    --, manageHook = insertPosition Master Newer <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = myLayoutHook
    , logHook = fadeWindowsLogHook myFadeHook <+> dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green"  "" . shorten 50
                , ppSep = " | "
                , ppLayout =
                    (\ x -> case x of
                         "Full"  -> xmobarColor "red" "" x
                         _       -> pad x
                    )
                }
    , handleEventHook = fadeWindowsEventHook
    , workspaces = myWorkspaces
    }
