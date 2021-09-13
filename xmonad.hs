import XMonad
import Data.Monoid
import System.IO
import System.Exit
import Data.Maybe
import XMonad.Layout.Spacing
import XMonad.Config.Desktop
import XMonad.Util.SpawnOnce
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.WindowNavigation
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.LayoutModifier
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.UrgencyHook
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.SubLayouts
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.Magnifier
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import qualified XMonad.Layout.MultiToggle as MT (Toggle (..))
import qualified XMonad.Layout.NoBorders as BO
import qualified XMonad.Layout.Fullscreen as FU
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Util.ClickableWorkspaces (clickablePP)

----------------------------------------------

myTerminal      = "xterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["web","code","term","discord","pdf","market","vbox","misc"]

myNormalBorderColor  = "#808080"
myFocusedBorderColor = "#FFFFFF"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

	-- launch a terminal
	[ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

	-- launch dmenu
	, ((modm,               xK_p     ), spawn "dmenu_run")

	-- launch gmrun
	, ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

	-- close focused window
	, ((modm,               xK_q     ), kill)

	 -- Rotate through the available layout algorithms
	, ((modm,               xK_space ), sendMessage NextLayout)

	--  Reset the layouts on the current workspace to default
	--, ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

	-- Resize viewed windows to the correct size
	, ((modm,               xK_n     ), refresh)

	-- Move focus to the next window
	, ((modm,               xK_Tab   ), windows W.focusDown)

	, ((modm .|. shiftMask,  xK_j),   decWindowSpacing 4)        -- Decrease window spacing
	, ((modm .|. shiftMask,  xK_k),   incWindowSpacing 4)         -- Increase window spacing
	, ((modm .|. shiftMask,  xK_h),   decScreenSpacing 4)         -- Decrease screen spacing
	, ((modm .|. shiftMask,  xK_l),   incScreenSpacing 4)         -- Increse  screen spacing


	, ((modm,                xK_d), spawn "rofi -show run")

	-- Mute
	, ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")
	-- Volume Down
	, ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -10%")
	-- Volume Up
	, ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +10%")

	-- Shrink the master area
	, ((modm .|. mod1Mask,               xK_Left     ), sendMessage Shrink)

	-- Expand the master area
	, ((modm .|. mod1Mask,               xK_Right     ), sendMessage Expand)

	-- Shrink the slave area
	, ((modm .|. mod1Mask,               xK_Down     ), sendMessage MirrorShrink)

	-- Expand the slave area
	, ((modm .|. mod1Mask,               xK_Up        ), sendMessage MirrorExpand)

	-- Push window back into tiling
	, ((modm .|. shiftMask,              xK_space     ), withFocused $ windows . W.sink)

	-- Increment the number of windows in the master area
	, ((modm              , xK_comma ), sendMessage (IncMasterN 1))

	-- Deincrement the number of windows in the master area
	, ((modm              , xK_period), sendMessage (IncMasterN (-1)))

	-- Toggle bordes

	, ((modm              , xK_o), sendMessage (MT.Toggle NOBORDERS))

	, ((modm, xK_f), sendMessage $ MT.Toggle NBFULL)

	-- Toggle the status bar .
	
	, ((modm              , xK_b     ), spawn "dbus-send --session --dest=org.Xmobar.Control --type=method_call --print-reply '/org/Xmobar/Control' org.Xmobar.Control.SendSignal \"string:Toggle -1\"" >> (broadcastMessage $ ToggleStruts) >> refresh)

	-- Quit xmonad
	, ((modm .|. shiftMask, xK_x     ), io (exitWith ExitSuccess))

	-- Restart xmonad
	, ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
	]
	++

	[((m .|. modm, k), windows $ f i)
		| (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_8]
		, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList $

	-- mod-button1, Set the window to floating mode and move by dragging
	[ ((modm, button1), \w -> focus w >> mouseMoveWindow w
									  >> windows W.shiftMaster)

	-- mod-button2, Raise the window to the top of the stack
	, ((modm, button2), \w -> focus w >> windows W.shiftMaster)

	-- mod-button3, Set the window to floating mode and resize by dragging
	, ((modm, button3), \w -> focus w >> mouseResizeWindow w
									  >> windows W.shiftMaster)

	-- you may also bind events to the mouse scroll wheel (button4 and button5)
	]

------------------------------------------------------------------------
-- Layouts:

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayout = mkToggle (NBFULL ?? NOBORDERS ?? EOT)
  $ BO.smartBorders tiled ||| threeCol ||| grid ||| magnify ||| full
	where
	  tiled = renamed [Replace "tall"] $ avoidStruts $ mySpacing 10 $  ResizableTall nmaster delta ratio []
	  threeCol = renamed [Replace "threecol"] $ avoidStruts $ mySpacing 10 $ ResizableThreeCol nmaster delta ratio []
	  grid = renamed [Replace "grid"] $ avoidStruts $ mySpacing 10 $ GridRatio (4/3)
	  magnify = renamed [Replace "magnify"] $ avoidStruts $ mySpacing 10 $ magnifier $ ResizableTall nmaster delta ratio []
	  full = Full

	   -- The default number of windows in the master pane
	  nmaster = 1

	-- Default proportion of screen occupied by master pane
	  ratio   = 1/2

	-- Percent of screen to increment by when resizing panes
	  delta   = 3/100
------------------------------------------------------------------------
-- Window rules:

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.

myManageHook = composeAll
	[ className =? "confirm"         --> doFloat
	, className =? "file_progress"   --> doFloat
	, className =? "download"        --> doFloat
	, className =? "error"           --> doFloat
	, className =? "notification"    --> doFloat
	, className =? "Gimp"            --> doFloat
	, resource  =? "desktop_window"  --> doIgnore
	, className =? "MPlayer"         --> doFloat
	, resource  =? "kdesktop"        --> doIgnore
	, isDialog --> doFloat
	, isFullscreen --> doFullFloat
	, className =? "Gimp"            --> doFloat
	, (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
	, className =? "VirtualBox Manager" --> doShift ( myWorkspaces !! 6 )
	]

------------------------------------------------------------------------
-- Event handling

myEventHook = fullscreenEventHook
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
-- myLogHook =
------------------------------------------------------------------------

myNav2DConf = def
	{ defaultTiledNavigation    = centerNavigation
	, floatNavigation           = centerNavigation
	, layoutNavigation          = [("Full", centerNavigation)]
	, unmappedWindowRect        = [("Full", singleWindowRect)]
	-- works but breaks tab deco  ,("Tabs", singleWindowRect)
	-- doesn't work but deco ok   ,("Tabs", fullScreenRect)
	}

------------------------------------------------------------------------
-- Startup hook

myStartupHook :: X ()
myStartupHook = do
		spawnOnce "nitrogen --restore &"
		spawnOnce "picom"


willFloat::Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> do
  sh <- io $ getWMNormalHints d w
  let isFixedSize = isJust (sh_min_size sh) && sh_min_size sh == sh_max_size sh
  isTransient <- isJust <$> io (getTransientForHint d w)
  return (isFixedSize || isTransient)

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  xmproc <- spawnPipe "xmobar /home/dawn/.xmonad/xmobarrc"
  xmonad $ docks
		 $ withNavigation2DConfig myNav2DConf
		 $ additionalNav2DKeys (xK_Up, xK_Left, xK_Down, xK_Right)
							   [
								  (mod4Mask,               windowGo  )
								, (mod4Mask .|. shiftMask, windowSwap)
							   ]
							   False
		 $ ewmhFullscreen
		 $ ewmh def { terminal           = myTerminal
			, focusFollowsMouse  = myFocusFollowsMouse
			, borderWidth        = myBorderWidth
			, modMask            = myModMask
			, workspaces         = myWorkspaces
			, normalBorderColor  = myNormalBorderColor
			, focusedBorderColor = myFocusedBorderColor
			-- key bindings
			, keys               = myKeys
			, mouseBindings      = myMouseBindings
			-- hooks, layouts
			, layoutHook         = myLayout
			, manageHook         = manageDocks <+> myManageHook <+> (fmap not willFloat --> insertPosition End Newer)
			, handleEventHook    = myEventHook <+> docksEventHook
			, startupHook        = myStartupHook <+> setWMName "LG3D"
			, logHook = clickablePP xmobarPP {
				  ppOutput = hPutStrLn xmproc
				, ppHiddenNoWindows = xmobarColor "white" "" 
				, ppHidden = xmobarColor "#77CAFF" "" . wrap "[" "]"
				, ppCurrent = xmobarColor "#FC439E" "" . wrap "{" "}"
				, ppTitle = xmobarColor "#e8e4c9" "" . shorten 50 . wrap "> " ""
				, ppSep = " | "
		 } >>= dynamicLogWithPP
	  }