-- xmonad config used by Malcolm MD
-- https://github.com/randomthought/xmonad-config

-------------------------------------------------
--- IMPORTS
-------------------------------------------------


    -- Base
import XMonad
import System.IO
import System.Exit
-- import System.Environment.Blank (getEnvDefault)
-- import System.Taffybar.Hooks.PagerHints (pagerHints)
-- import qualified Data.List as L

    -- Actions
import XMonad.Actions.Navigation2D
import XMonad.Actions.UpdatePointer
import XMonad.Actions.ConstrainedResize as Sqr
-- import XMonad.Actions.PerWindowKeys (bindFirst)

    -- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition
-- import XMonad.Hooks.SetWMName

    -- Layout
import XMonad.Layout.BinarySpacePartition as BSP
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import XMonad.Layout.GridVariants as GVR
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
-- import XMonad.Layout.ThreeColumns
-- import XMonad.Layout.VoidBorders
import XMonad.Layout.WindowNavigation
-- import XMonad.Layout.ZoomRow

    -- Utilities
import XMonad.Util.Run(spawnPipe)
-- import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS), namedScratchpadAction,
    namedScratchpadManageHook, customFloating,
  )
import XMonad.Util.Types as Direction

import Data.Ratio ((%))
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt
  ( XPConfig, XPPosition (CenteredAt), alwaysHighlight, bgColor, bgHLight, fgColor,
    fgHLight, font, height, position, promptBorderWidth,
  )



----------------------------mupdf--------------------------------------------
-- Terminimport XMonad.Hooks.EwmhDesktopsal
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
-- myTerminal = "gnome-terminal"
-- myTerminal      = "urxvtcd"
-- myTerminal = "alacritty"
myTerminal = "/usr/local/bin/wezterm"

-- The command to lock the screen or show the screensaver.
-- myScreensaver = "dm-tool switch-to-greeter"
myScreensaver = "xscreensaver-command -suspend"

-- The command to take a active window screenshot
myWindowScreenshot = "scrot -F '/home/brian/Pictures/Screenshots/Screenshot from %F %H-%M-%S.png' -u"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "scrot -F '/home/brian/Pictures/Screenshots/Screenshot from %F %H-%M-%S.png' -l style=solid,width=3,color=red -s"

-- The command to take a fullscreen screenshot.
myFullScreenshotDisplay0 = "scrot -F '/home/brian/Pictures/Screenshots/Screenshot from %F %H-%M-%S.png' -b -a 0,0,1920,1080"
myFullScreenshotDisplay1 = "scrot -F '/home/brian/Pictures/Screenshots/Screenshot from %F %H-%M-%S.png' -b -a 1920,0,1920,1080"
-- myFullScreenshot = myFullScreenshotDisplay1

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
myLauncher = "rofi -max-history-size 5 -sidebar-mode -parse-hosts -show-icons -show combi -combi-modes drun,window,ssh"

-- myNavigator     = "chromium"
-- myEditor        = "emacs -f server-start"


------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.
--
anonymousWorkspaces :: [Int]
anonymousWorkspaces = [5..9]
namedWorkspaces :: [[Char]]
namedWorkspaces = ["1: term","2: web","3: code","4: media"]
myWorkspaces :: [[Char]]
myWorkspaces =  namedWorkspaces ++ map show anonymousWorkspaces


------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [
      isDialog                        --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdesktop"         --> doIgnore
    , className =? "mpv"              --> doRectFloat (W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
    , className =? "Google-chrome"    --> doShift "2:web"
    , className =? "Firefox"          --> doShift "2:web"
    , className =? "Firefox" <&&> resource =? "Toolkit" --> doShift "2:web" >> doFloat
    , className =? "Xmessage"         --> doCenterFloat
    , className =? "Gxmessage"        --> doCenterFloat
    , className =? "Galculator"       --> doCenterFloat
    , className =? "Steam"            --> doCenterFloat
    , className =? "Gimp"             --> doCenterFloat
    , resource  =? "gpicview"         --> doCenterFloat
    , className =? "MPlayer"          --> doCenterFloat
    , className =? "Pavucontrol"      --> doCenterFloat
    , className =? "VirtualBox"       --> doShift "4:vm"
    , className =? "Xchat"            --> doShift "5:media"
    , className =? "stalonetray"      --> doIgnore
    , isFullscreen                    --> (doF W.focusDown <+> doFullFloat)
    -- , isFloating                      --> doSetBorderColor activeWarn
    , className =? "Mate-power-preferences"       --> doCenterFloat
    , className =? "Xfce4-power-manager-settings" --> doCenterFloat
    , namedScratchpadManageHook myScratchPads
    ]



------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

outerGaps    = 5
myGaps       = gaps [(Direction.U, outerGaps), (Direction.R, outerGaps), (Direction.L, outerGaps), (Direction.D, outerGaps)]
addSpace     = renamed [CutWordsLeft 2] . spacing gap

tabLayout    = renamed [Replace "Tabbed"]
               $ addTopBar
               $ myGaps
               $ tabbed shrinkText myTabTheme

bsp          = renamed [Replace "BSP"] -- renamed [CutWordsLeft 1]
               $ windowNavigation
               $ addTabs shrinkText myTabTheme
               $ subLayout [] Simplest
               $ myGaps
               $ addSpace (BSP.emptyBSP)

full         = renamed [Replace "Fullscreen"]
               $ noBorders (Full)

tiled        = renamed [Replace "Tiled"]
               $ Tall 1 (1/2) (3/100)

pdfLayout1   = renamed [Replace "Resizeable Tall"] $ ResizableTall 1 (3/100) (7/8) []
pdfLayout2   = renamed [Replace "Split Grid"] $ SplitGrid GVR.L 1 2 (7/8) (16/10) (3/100)
pdfLayouts   = pdfLayout1 ||| pdfLayout2

layouts      = avoidStruts (bsp ||| tabLayout ||| full ||| pdfLayouts ||| tiled ||| Mirror tiled)

myLayout     = smartBorders
               $ mkToggle (NOBORDERS ?? FULL ?? EOT)
               $ layouts

-- myLayout    = lessBorders Never $ layouts
-- myLayout    = smartBorders $ layouts

myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , layoutNavigation          = [("Full", centerNavigation)
    -- line/center same results   ,("Tabs", lineNavigation)
    --                            ,("Tabs", centerNavigation)
                                  ]
    , unmappedWindowRect        = [("Full", singleWindowRect)
    -- works but breaks tab deco  ,("Tabs", singleWindowRect)
    -- doesn't work but deco ok   ,("Tabs", fullScreenRect)
                                  ]
    }


------------------------------------------------------------------------
-- Colors and borders

base03  = "#002b36"
-- base02  = "#073642"
-- base01  = "#586e75"
-- base00  = "#657b83"
-- base0   = "#839496"
-- base1   = "#93a1a1"
-- base2   = "#eee8d5"
-- base3   = "#fdf6e3"
gold1   = "#68502e"
-- gold2   = "#b58900"
gold3   = "#d69131"
-- orange  = "#cb4b16"
red     = "#dc322f"
-- magenta = "#d33682"
-- violet  = "#6c71c4"
-- blue    = "#268bd2"
-- cyan    = "#2aa198"
-- green   = "#859900"

myNormalBorderColor     = "#000000" -- gold1
myFocusedBorderColor    = "#8489ff" -- gold3

active      = gold3
inactive    = gold1
-- floating    = violet
-- activeWarn  = red

-- Color of current window title in xmobar.
xmobarTitleColor = "#C678DD"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#51AFEF"

-- sizes
gap         = 5
topbar      = 5
border      = 1
-- prompt      = 20
-- status      = 20

-- Width of the window border in pixels.
myBorderWidth = border

-- myFont      = "-*-Zekton-medium-*-*-*-*-160-*-*-*-*-*-*"
-- myBigFont   = "-*-Zekton-medium-*-*-*-*-240-*-*-*-*-*-*"
myFont      = "xft:Zekton:size=9:bold:antialias=true"
-- myFont      = "xft:Hack:regular:size=12:antialias=true:hinting=true"
myBigFont   = "xft:Zekton:size=16:bold:antialias=true"
-- myWideFont  = "xft:Eurostar Black Extended:"
--             ++ "style=Regular:pixelsize=180:hinting=true"

-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
topBarTheme = def
    {
      fontName              = myFont
    , inactiveBorderColor   = inactive
    , inactiveColor         = inactive
    , inactiveTextColor     = inactive
    , activeBorderColor     = active
    , activeColor           = active
    , activeTextColor       = active
    , urgentBorderColor     = red
    , urgentTextColor       = gold3
    , decoHeight            = topbar
    }

addTopBar =  noFrillsDeco shrinkText topBarTheme

myTabTheme = def
    { fontName              = myFont
    , activeColor           = active
    , inactiveColor         = inactive
    , activeBorderColor     = active
    , inactiveBorderColor   = inactive
    , activeTextColor       = base03
    , inactiveTextColor     = gold3
    }

curLayout :: X String
curLayout = gets windowset >>= return . description . W.layout . W.workspace . W.current

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask
altMask = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Start a scratchpad terminal
  , ((controlMask .|. shiftMask, xK_space),
     -- namedScratchpadAction myScratchPads (NS.name scratchTerminal))
     namedScratchpadAction myScratchPads "scratchterm")

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask, xK_0),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take an active window screenshot using the command specified by myWindowScreenshot.
  , ((altMask, xK_Print),
     spawn myWindowScreenshot)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((shiftMask, xK_Print),
     spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myFullScreenshot.
  , ((0, xK_Print),
     sequence_ [
        spawn myFullScreenshotDisplay0
        , spawn myFullScreenshotDisplay1
     ])

  -- Toggle current focus window to fullscreen, noborders
  -- , ((modMask, xK_f), SendMEssage $ Toggle FULL)
  , ((modMask, xK_f), sendMessage $ JumpToLayout "Fullscreen")

  -- Calculator
  , ((modMask, xK_c), namedScratchpadAction myScratchPads "calc")

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "amixer -q set Master 5%+")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
--     sendMessage NextLayout >> (curLayout >>= \d->spawn$"xmessage "++d))
     sendMessage NextLayout >> (curLayout >>= \d->spawn$"echo "++d++"|dzen2 -ta c -p 1 -x 2750 -y 525 -w 250 -fg '#fdf6e3' -bg '#b58900' -h 50 -fn roboto-24 "))

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     confirmPrompt amberXPConfig "exit XMonad?" (io exitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     spawn "xmonad --recompile && xmonad --restart")
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


  ++
  -- Bindings for manage sub tabs in layouts please checkout the link below for reference
  -- https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-SubLayouts.html
  [
    -- Tab current focused window with the window to the left
    ((modMask .|. controlMask, xK_h), sendMessage $ pullGroup Direction.L)
    -- Tab current focused window with the window to the right
  , ((modMask .|. controlMask, xK_l), sendMessage $ pullGroup Direction.R)
    -- Tab current focused window with the window above
  , ((modMask .|. controlMask, xK_k), sendMessage $ pullGroup Direction.U)
    -- Tab current focused window with the window below
  , ((modMask .|. controlMask, xK_j), sendMessage $ pullGroup Direction.D)

  -- Tab all windows in the current workspace with current window as the focus
  , ((modMask .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
  -- Un-Group the current tabbed windows
  , ((modMask .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

  -- Toggle through tabs from the right
  , ((modMask, xK_Tab), onGroup W.focusDown')
  ]

  ++
  -- Some bindings for BinarySpacePartition
  -- https://github.com/benweitzman/BinarySpacePartition
  [
    ((modMask .|. controlMask,               xK_Right ), sendMessage $ ExpandTowards Direction.R)
  , ((modMask .|. controlMask .|. shiftMask, xK_Right ), sendMessage $ ShrinkFrom Direction.R)
  , ((modMask .|. controlMask,               xK_Left  ), sendMessage $ ExpandTowards Direction.L)
  , ((modMask .|. controlMask .|. shiftMask, xK_Left  ), sendMessage $ ShrinkFrom Direction.L)
  , ((modMask .|. controlMask,               xK_Down  ), sendMessage $ ExpandTowards Direction.D)
  , ((modMask .|. controlMask .|. shiftMask, xK_Down  ), sendMessage $ ShrinkFrom Direction.D)
  , ((modMask .|. controlMask,               xK_Up    ), sendMessage $ ExpandTowards Direction.U)
  , ((modMask .|. controlMask .|. shiftMask, xK_Up    ), sendMessage $ ShrinkFrom Direction.U)
  , ((modMask,                               xK_r     ), sendMessage BSP.Rotate)
  , ((modMask,                               xK_s     ), sendMessage BSP.Swap)
  -- , ((modMask,                               xK_n     ), sendMessage BSP.FocusParent)
  -- , ((modMask .|. controlMask,               xK_n     ), sendMessage BSP.SelectNode)
  -- , ((modMask .|. shiftMask,                 xK_n     ), sendMessage BSP.MoveNode)
  ]

------------------------------------------------------------------------
-- Prompt
xPConfig :: XPConfig
xPConfig =
  def
    { bgColor           = "#0010af",
      fgColor           = "#c8d8d8",
      bgHLight          = "#c8d8d8",
      fgHLight          = "#0010af",
      position          = CenteredAt 0.5 0.25,
      height            = 150,
      alwaysHighlight   = True,
      promptBorderWidth = 5,
      font              = "xft:monospace:size=12"
    }
amberXPConfig :: XPConfig
amberXPConfig =
  xPConfig { fgColor   = "#000000",
             bgColor   = "#ca8f2d",
             fgHLight  = "#eaaf4c",
             bgHLight  = "#ca8f2d",
             font      = myBigFont
           }

-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> Sqr.mouseResizeWindow w False))

    -- shift-mod-button3, Set the window to floating mode and resize by dragging, constrained aspect
    , ((modMask .|. shiftMask, button3),
       (\w -> focus w >> Sqr.mouseResizeWindow w False))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  -- setWMName "LG3D" -- Java hack for old apps < Java V6 (probably not needed now in 2023)
  spawn     "/bin/bash ~/.config/xmonad/startup.sh"
  setDefaultCursor xC_left_ptr

---------------------------------------------------
-- Returns [True] if the current window is floating
--isFloating :: Query Bool
--isFloating :: Bool
--isFloating = do
--  withFocused $ \windowId -> do
--    floats <- gets (W.floating . windowset)
--    r <- windowId `M.member` floats
--    return $ case r of
--        Just [_] -> True
--        _ -> False
--
----------------------------------------------------
--ifBrowser :: X () -> X () -> X ()
--ifBrowser thenX elseX =
--  withFocused $ \w -> do
--    c <- runQuery className w
--    if c == "Chromium-browser"
--      then thenX
--      else elseX
--------------------------------------------------
-- goal here is to set the floating windows boarder to different color so that
-- I know it was floating.  Too many times a window is floating and I don't know it
-- also could be nice to set a sudo window border to red color  ! :)
-- TODO - make this work
-- the functions isFloating and the managehook is not working
-- doSetBorderColor :: String -> ManageHook
-- doSetBorderColor color = do
-- doSetBorderColor color = doFocus
-- use setWindowBorderWithFallback :: Display -> Window -> String -> Pixel -> X ()

------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc.hs"
  -- xmproc <- spawnPipe "taffybar"
  xmonad $ docks
         $ withNavigation2DConfig myNav2DConf
         $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
                               [
                                  (mod4Mask,               windowGo  )
                                , (mod4Mask .|. shiftMask, windowSwap)
                               ]
                               False
         $ ewmh
         -- $ pagerHints -- uncomment to use taffybar
         $ defaults {
         logHook = dynamicLogWithPP xmobarPP {
                  ppCurrent = xmobarColor xmobarCurrentWorkspaceColor "" . wrap "[" "]"
                , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
                , ppSep = "   "
                , ppOutput = hPutStrLn xmproc
         } >> updatePointer (0.75, 0.75) (0.75, 0.75)
      }

------------------------------------------------------------------------
--- SCRATCHPADS
------------------------------------------------------------------------
myScratchPads = [ NS "calc" spawnCalc findCalc manageCalc ,
                  scratchTerminal
                ]
    where
    spawnCalc  = "galculator"
    findCalc   = resource =? "galculator"
    manageCalc = customFloating $ W.RationalRect l t w h
                 where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w

    scratchTerminal :: NamedScratchpad
    scratchTerminal =
      NS name command (appName =? name) (doCenteredFloat 0.5 0.2)
      where
      name :: String
      name = "scratchterm"
      command :: String
      command = "alacritty --config-file $HOME/.config/alacritty/alacritty-scratch.yml --class " ++ name

      doCenteredFloat :: Rational -> Rational -> ManageHook
      doCenteredFloat thewidth theheight =
        doRectFloat (W.RationalRect x y thewidth theheight)
        where
          x :: Rational
          x = (1 - thewidth) / 2

          y :: Rational
          y = (1 - theheight) / 2

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,

    -- handleEventHook    = E.fullscreenEventHook,
    handleEventHook    = fullscreenEventHook,
    manageHook         = insertPosition Below Newer <+> manageDocks <+> myManageHook,
    startupHook        = myStartupHook
}

-- TODO
-- use sendkeys and isBrowser to detect when focus is "chromium"
-- and send key combination Ctrl + L, + C, + W, + N, + V, and Enter
-- this will "move to url bar, copy url, close current window,
-- open new window, paste url"
-- effectively tearing off the current tab into it's own window
-- assign this to a key binding because there is no default keyboard
-- shortcut within chromium to do this directly.
