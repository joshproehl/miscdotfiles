import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.Drawer -- TODO: Config
-- import XMonad.Layout.Spiral
import qualified XMonad.StackSet as W -- Prevent conflict of .workspaces
import XMonad.Util.CustomKeys
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import System.IO
import Data.List



-- TODO: Create workspace on the fly form Gimp

-- Set up the hook for fadeWindows
myFadeHook = composeAll
  [ opaque                          -- Default to opaque windows
    , isFloating --> opaque -- opacity 0.9
    , isUnfocused --> opacity 0.65    -- Unfocused is 65% of opaque
    , isDialog --> opaque             -- Dialog windows should go on top
  ]


-- Manage all of the various floation requirements
-- and send certain apps to certain places.
myManageHook = composeAll . concat $
  [ [ isFullscreen --> doFullFloat ]
  , [ isDialog --> doCenterFloat ]
  , [ className =? "Gimp" --> doFloat ]
  , [ className =? "Pidgin" --> doShift "1:comm" ]
  , [ className =? c --> doFloat | c <- myClassFloats ]
  , [ title =? t --> doFloat | t <- myTitleFloats ]
  ]
  where
    myClassFloats = [ "Gimp" ]
    myTitleFloats = [ "Downloads", "Add-ons", "Firefox Preferences" ]


myLayoutHook = avoidStruts $
  --onWorkspace "7" gimp $
  onWorkspace "1:comm" imLayout $
  standardLayouts
  where
    tall = Tall 1 0.02  0.5
    standardLayouts = ResizableTall 1 (3/100) (1/2) [] ||| Full ||| Mirror tall -- spiral (6/7) ||| Mirror tall
    imLayout = withIM (1/5) (Role "buddy_list") Grid --(standardLayouts)
    --gimp = withIM (0.11) (Role "gimp-toolbox") $
    --       reflectHoriz $
    --       withIM (0.15) (Role "gimp-dock") Full


-- Define our workspaces.
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:comm"] ++ (map show [2..13])

scratchpads =
  [ NS "terminal" "terminator -T popterm" (title =? "popterm") (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/2))
  , NS "htop" "urxvt -e htop" (title =? "htop") (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/2))
  , NS "alsamxier" "urxvt -e alsamixer" (title =? "alsamixer") (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/2))
  ] where role = stringProperty "WM_WINDOW_ROLE"

delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys XConfig {modMask = modm} = []

addKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addKeys conf@(XConfig {modMask = modm}) =
  [ ((modm, xK_a), spawn "xmenud") -- Mod-A to open app menu
  , ((modm,               xK_p),      spawn "~/.config/dmenu/dmenu-bind.sh")

  -- Cycle to workspaces
  , ((modm,               xK_Right),  nextWS)
  , ((modm,               xK_Left),   prevWS)
  , ((modm .|. shiftMask, xK_Right),  shiftToNext)
  , ((modm .|. shiftMask, xK_Left),   shiftToPrev)
  , ((modm,               xK_Down),   nextScreen)
  , ((modm,               xK_Up),     prevScreen)
  , ((modm,               xK_b),      toggleWS)

  -- ResizableTall adjsutments
  , ((modm,               xK_u), sendMessage MirrorShrink)
  , ((modm,               xK_k), sendMessage MirrorExpand)

  -- Warp pointer to focused window
  , ((modm,               xK_apostrophe), warpToWindow (1/2) (1/2))

  -- Bring up scratchpads
  , ((modm .|. shiftMask, xK_t), namedScratchpadAction scratchpads "terminal")
  , ((modm .|. shiftMask, xK_h), namedScratchpadAction scratchpads "htop")
  , ((modm .|. shiftMask, xK_m), namedScratchpadAction scratchpads "alsamxier")
  ]
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  ++
  [((m .|. modm, k), windows $ f i)
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_bracketleft, xK_bracketright, xK_BackSpace]
   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]



myStartupHook = do
    setWMName "LG3D" --java hack

main = do
-- Launch xmobar using the config specificed in it's rc file.
  xmproc <- spawnPipe "/usr/bin/xmobar /home/joshproehl/.xmobarrc"

  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
         $ defaultConfig
    { terminal    = "terminator"
    , modMask     = mod4Mask  -- Use the "windows" key as the mod key.
    , keys        = customKeys delKeys addKeys
    , borderWidth = 1
    , normalBorderColor = "#444444"
    , focusedBorderColor = "#005577"
    , startupHook = myStartupHook
    , manageHook  = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads <+> manageHook defaultConfig
    --, manageHook  = insertPosition Master Newer <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook  = myLayoutHook
    , logHook = fadeWindowsLogHook myFadeHook <+> dynamicLogWithPP xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle  = xmobarColor "green"  "" . shorten 50
                , ppSep    = " | "
                , ppLayout = (\ x -> case x of
                                         "Full"  -> xmobarColor "red" "" x
                                         _       -> pad x
                             )
                , ppSort   = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP)
                }
    , handleEventHook = fadeWindowsEventHook
    , workspaces      = myWorkspaces
    }
