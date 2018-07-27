import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.TagWindows
import XMonad.Actions.Warp
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog hiding (xmobar, xmobarPP, xmobarColor, sjanssenPP, byorgeyPP)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.FixedColumn
import XMonad.Layout.Grid
-- import XMonad.Layout.IM -- Not actually using right now, but might want to again.
import XMonad.Layout.LimitWindows
import XMonad.Layout.Magnifier
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
-- import XMonad.Layout.Reflect -- Don't need with GIMP specific layout disabled
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
-- import XMonad.Layout.Drawer -- Leaving to remember it exists in the future.
-- import XMonad.Layout.Spiral -- Leaving to remember it exists in the future.
import qualified XMonad.StackSet as W -- Prevent conflict of .workspaces
import XMonad.Util.CustomKeys
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Util.Themes
import System.IO
import Data.List


-- | Check if the current window has a specific tag
isTagged :: String -> Query Bool
isTagged t =  ask >>= \w -> liftX $ hasTag t w

-- | Check if the given window has one of the given titles
-- For example, a Chromium window opened to https://overwatchleague.com has the title "The Overwatch League - Chromium"
-- and a window opened to https://twitch.tv/overwatchleague has the title "OverwatchLeague",
-- so we can ensure that both of them never go transparent by using:
-- titleContains "OverwatchLeague" --> opaque
titleContains :: String -> Query Bool
titleContains t = ask >>= \w -> do
  wt <- title 
  return $ t `isSubsequenceOf` wt 

-- | Check if the given window's title begins with any of the strings in the given array.
-- Prefix is used because we often want a website title or something, but don't want to include the " - Chromium" suffix.
-- By adding "Inbox (" as an prefix to keep opaque you can make it so that any tab that's open that has a GMail Inbox view
-- will not be made transparent, but if you change your Gmail to be viewing anything other than the Inbox the entire window
-- will become opaque when not active.
hasTitlePrefixIn :: [String] -> Query Bool
hasTitlePrefixIn titles = ask >>= \w -> do
  windowTitle <- title
  return $ findMatchingPrefixIn titles windowTitle 
  where
    findMatchingPrefixIn [] _      = False
    findMatchingPrefixIn (p:ps) wt | p `isPrefixOf` wt  = True
                                   | otherwise          = findMatchingPrefixIn ps wt
  
-- |  Add "toggleTag" for XMonad.Actions.TagWindows
--toggleTag :: String -> Window -> X ()
--toggleTag s w = do
--  tags <- getTags w
--  tt w s tags
--  where
--    tt w s tags | liftX (hasTag s w) == True    = do delTag s w
--                | otherwise                     = do addTag s w




-- Set up the hook for fadeWindows
-- Note that these go left-to-right, so opaque is the default and other properties are applied in order. (Last prop wins)
-- (Which is contrary to the Xmonad.Hooks.FadeWindows hackage documentation as of 2018/04/10)
myFadeHook = composeAll
  [ opaque                                           -- By default all windows are opaque
  , isUnfocused                     --> opacity 0.65 -- How transparent are unfocused windows?
  , isFloating                      --> opacity 0.90 -- Floating windows should less transparent than what they're on top of
  , isDialog                        --> opaque       -- Dialog windows should go on top so they don't vanish, make them opaque
  , isTagged "_KEEP_OPAQUE"         --> opaque
  , titleContains "OverwatchLeague" --> opaque
  , hasTitlePrefixIn myOpaqueTitles --> opaque
  ]
  where
    myOpaqueTitles =
      [ "Inbox ("
      , "Duolingo"
      , "Learn language in context - Clozemaster"
      ]

-- Manage all of the various floation requirements
-- and send certain apps to certain places.
myManageHook = composeAll . concat $
  [ [ isFullscreen --> doFullFloat ]
  , [ isDialog --> doCenterFloat ]
  , [ className =? "Gimp" --> doFloat ]
  , [ className =? "Pidgin" --> doShift "1:comm" ]
  , [ className =? "Quasselclient" --> doShift "1:comm" ]
  , [ className =? c --> doFloat | c <- myClassFloats ]
  , [ title =? t --> doFloat | t <- myTitleFloats ]
  ]
  where
    myClassFloats = [ "Gimp", "Octave" ]
    myTitleFloats = [ "Downloads", "Add-ons", "Firefox Preferences" ]


-- Configure the Tabbed layout
-- TODO: Tune theme
myTabbedConfig = def { inactiveBorderColor = "#FF0000"
                      , activeTextColor = "#00FF00"
                      , fontName = "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*-*"
                      --, fontName = "-*-terminus-medium-*-normal-*-10-*-*-*-*-*-*-*"
                      --, fontName = "-*-iosevka thin-*-*-*-*-*-*-*-*-*-*-*-*"
                      }


myLayoutHook = avoidStruts $ standardLayouts
  --onWorkspace "7" gimp $
  --onWorkspace "1:comm" imLayout $
  --spacing 0 $ standardLayouts
  where
    standardLayouts = myTabbed ||| Grid ||| myResizableTall ||| Mirror myResizableTall -- ||| myZoomer -- spiral (6/7)
    myResizableTall = ResizableTall 1 (3/100) (1/2) []
    myZoomer = limitWindows 3 $ magnifiercz' 1.4 $ FixedColumn 1 20 80 10
    myTabbed = tabbed shrinkText (theme kavonForestTheme)
    --imLayout = withIM (1/5) (Role "buddy_list") Grid --(standardLayouts)
    --gimp = withIM (0.11) (Role "gimp-toolbox") $
    --       reflectHoriz $
    --       withIM (0.15) (Role "gimp-dock") Full


-- Define our workspaces.
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:comm"] ++ (map show [2..12]) ++ ["13:tasks"]

-- RationalRect params are: left,top,width,height
scratchpads =
  [ NS "terminal" "urxvt -e popterm"           (title =? "popterm")   (customFloating $ W.RationalRect 0 0.025 1 0.57)
  , NS "htop" "urxvt -e htop"                  (title =? "htop")      (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/2))
  , NS "alsamxier" "urxvt -e alsamixer"        (title =? "alsamixer") (customFloating $ W.RationalRect (0/1) (0/1) (1/1) (1/2))
  ] where role = stringProperty "WM_WINDOW_ROLE"


delKeys :: XConfig l -> [(KeyMask, KeySym)]
delKeys XConfig {modMask = modm} = []

addKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addKeys conf@(XConfig {modMask = modm}) =
  [ ((modm,               xK_a),      spawn "xmenud") -- Mod-A to open app menu
  , ((modm,               xK_p),      spawn "~/.miscdotfiles/scripts/app_launcher.sh")
  , ((modm,               xK_q),      spawn "killall dzen2; killall conky; killall trayer; xmonad --restart")

  , ((modm .|. shiftMask, xK_b),      sendMessage ToggleStruts)

  -- Tag/Untag window as "don't ever be transparent"
  -- Can this be merged into a single function that tags if untagged and untags if currently tagged?
  , ((modm,               xK_f),      withFocused (addTag "_KEEP_OPAQUE"))
  , ((modm .|. shiftMask, xK_f),      withFocused (delTag "_KEEP_OPAQUE"))

  -- Cycle to workspaces
  , ((modm,               xK_Right),  nextWS)
  , ((modm,               xK_Left),   prevWS)
  , ((0,                  0x1008ff26),prevWS)
  , ((0,                  0x1008ff27),nextWS)
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

  -- Fn-F2 (XF86ScreenSaver) locks machine
  ,((0,                   0x1008ff2d),spawn "xscreensaver-command --lock")

  -- Bring up scratchpads
  , ((modm .|. shiftMask, xK_t), namedScratchpadAction scratchpads "terminal")
  , ((modm .|. shiftMask, xK_h), namedScratchpadAction scratchpads "htop")
  , ((modm .|. shiftMask, xK_m), namedScratchpadAction scratchpads "alsamxier")
  ]
  -- mod-[1..{BACKSPACE}], Switch to workspace N
  -- mod-shift-[1..{BACKSPACE}], Move client to workspace N
  ++
  [((m .|. modm, k), windows $ f i)
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9, xK_0, xK_bracketleft, xK_bracketright, xK_BackSpace]
   , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ]



myStartupHook = do
  setWMName "LG3D" --java hack

myLogHook h = dynamicLogWithPP ( def
  {
      ppCurrent         = dzenColor color15 background
    , ppVisible         = dzenColor color7 background
    , ppHidden          = dzenColor color14 background
    , ppHiddenNoWindows = dzenColor background background
    , ppUrgent          = dzenColor "red" background . wrap ">" "<" . dzenStrip
    , ppWsSep           = " "
    , ppSep             = "  "
    , ppLayout          = wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor color2 background .
                          (\x -> case x of
                            -- TODO: Figure out why neither $HOME nor ~ work here and fix it.
                            "Full"                    ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_full.xbm)"
                            "Tabbed Simplest"         ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_full.xbm)"
                            "Tabbed"                  ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_full.xbm)"
                            "Spacing 5 ResizableTall" ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_tall.xbm)"
                            "ResizableTall"           ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_tall.xbm)"
                            "Mirror ResizableTall"    ->  "^i(/home/joshproehl/.xmonad/dzen2/layout_mirror_tall.xbm)"
                            "SimplestFloat"           ->  "^i(/home/joshproehl/.xmonad/dzen2/mouse_01.xbm)"
                            "Circle"                  ->  "^i(/home/joshproehl/.xmonad/dzen2/full.xbm)"
                            --_                         ->  x
                            _                         ->  "^i(/home/joshproehl/.xmonad/dzen2/grid.xbm)"
                          )
    , ppTitle           = wrap "^ca(1,xdotool key alt+shift+x)" "^ca()" . dzenColor color15 background . shorten 50 . pad
    , ppOrder           = \(ws:l:t:_) -> [ws,l, t]
    , ppOutput          = hPutStrLn h
--    , ppSort            = fmap (namedScratchpadFilterOutWorkspace .) (ppSort defaultPP)
  } )

myXmonadBar = "~/.xmonad/bar_left_xmonad '"++foreground++"' '"++background++"' "++myFont ++ " " ++ xmonadStatusWidth ++ " " ++ statusbarHeight ++ " " ++ trayerwidth
myStatusBar = "~/.xmonad/bar_right_status '"++foreground++"' '"++background++"' "++myFont ++ " " ++ xmonadStatusWidth ++ " " ++ statusbarHeight ++ " " ++ trayerwidth

main = do
  dzenLeftBar   <- spawnPipe myXmonadBar
  dzenRightBar  <- spawnPipe myStatusBar

  xmonad $ docks
         $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] }
         $ ewmh desktopConfig
    { terminal    = "terminator"
    , modMask     = mod4Mask  -- Use the "windows" key as the mod key.
    , keys        = customKeys delKeys addKeys
    , borderWidth = 1
    , normalBorderColor = "#444444"
    , focusedBorderColor = "#005577"
    , startupHook = myStartupHook
    , manageHook  = myManageHook <+> namedScratchpadManageHook scratchpads <+> manageHook def
    --, manageHook  = insertPosition Master Newer <+> manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook  = myLayoutHook
    , logHook     = myLogHook dzenLeftBar <+> fadeWindowsLogHook myFadeHook
    , handleEventHook = fadeWindowsEventHook
    --, handleEventHook = docksEventHook <+> fadeWindowsEventHook
    , workspaces      = myWorkspaces
    }



xmonadStatusWidth = "600"
statusbarHeight = "14"
trayerwidth = "90"

-- EROSION EDIT
myFont = "-*-terminus-medium-*-normal-*-10-*-*-*-*-*-*-*"
--myFont		= "-*-nu-*-*-*-*-*-*-*-*-*-*-*-*-*"
background= "#181512"
foreground= "#D6C3B6"
color0=  "#332d29"
color8=  "#817267"
color1=  "#8c644c"
color9=  "#9f7155"
color2=  "#746C48"
color10= "#9f7155"
color3=  "#bfba92"
color11= "#E0DAAC"
color4=  "#646a6d"
color12= "#777E82"
color5=  "#766782"
color13= "#897796"
color6=  "#4B5C5E"
color14= "#556D70"
color7=  "#504339"
color15= "#9a875f"
