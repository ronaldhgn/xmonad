--
-- ~/.xmonad/xmonad.hs by Ronald Hgn
--
-- Comments                 {{{
--
-- Dependencies:
--      $ wget -O - http://pbrisbin.com:8080/bin/leave > ~/bin/leave
--      # pacman -S zenity wmctrl
--      # pacman -S stalonetray
--      # pacman -S xdotool
--
-- ALSO CHECK:
--      http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Thomas_ten_Cate%27s_xmonad.hs
--      http://paste.n-sch.de/xmonad/xmonad_hs.html
--
--
-- Navigation:
-- Mod+1..9         switch to workspace
--
-- Window Management:
--
-- Layout Management:
--
--
-- Other:
--
-- }}}

-- Imports                  {{{

import XMonad hiding ( (|||) )                           -- (00)

import Data.Monoid                                       -- (00)
import System.Exit                                       -- (00)
import Data.List (isPrefixOf,isInfixOf)
import qualified XMonad.StackSet as W                    -- (00)
import qualified Data.Map as M                           -- (00)
import Data.Maybe (fromMaybe)                            -- (  ) needed for layout icons


-- Utilities
import XMonad.Util.EZConfig as V                         -- (U1) "M-C-x" style keybindings
import XMonad.Util.Run                                   -- (U2) for spawnPipe and hPutStrLn
import XMonad.Util.NamedScratchpad                       -- (U3) named scratchpads that support several arbitrary applications at the same time


-- Action
import XMonad.Actions.WithAll                            -- (A1) perform action on all windows
import XMonad.Actions.CycleWS                            -- (A2) general workspace-switching
import XMonad.Actions.Promote                            -- (A3) moves focused window to master pane
import XMonad.Actions.RotSlaves                          -- (A4) rotate all slave windows
import qualified XMonad.Actions.FlexibleResize as FlexR  -- (A5) Resize floating windows from any corner
import qualified XMonad.Actions.ConstrainedResize as SQR -- (A6) allow constrained resize of floating windows with mouse
import XMonad.Actions.UpdatePointer                      -- (A7) perform action on all windows
import XMonad.Actions.CopyWindow                         -- (A8) duplicate a window on multiple workspaces


-- Layout
import XMonad.Layout.ResizableTile                       -- (L1) change width/height of window
import XMonad.Layout.ToggleLayouts                       -- (L2) a module to toggle between two layouts
import XMonad.Layout.NoBorders                           -- (L3) make given layout display without borders
import XMonad.Layout.Accordion                           -- (L4) accordion style layout
import XMonad.Layout.Named                               -- (L5) rename workspaces
import XMonad.Layout.LayoutCombinators


-- Hooks
import XMonad.Hooks.ManageDocks                          -- (H1) autom. manage dock type programs
import XMonad.Hooks.DynamicLog hiding (dzenStrip)        -- (H2) several drop-in loghooks to output status info
import XMonad.Hooks.EwmhDesktops                         -- (H3) allows the user to interact with xmonad by clicking on panels and window lists
import XMonad.Hooks.ManageHelpers


-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input

-- }}}

-- Global Settings          {{{
--
myModMask                = mod4Mask                          -- supermod key
monitorWidth             = 1280                              -- two bars span this width
leftBarWidth             = 700                               -- the right bar will span difference

myTerminal               = "terminal"
myBorderWidth            = 2
barHeight                = 16

myFont                   = "Terminus-8"                      -- xft enabled dzen req'd
--conkyFile              = "~/.dzen_conkyrc"                 -- populates the right status bar

myNormalBorderColor      = "#2e3436"                         -- dark grey
myFocusedBorderColor     = "#63b613"                         -- green

colorWhite               = "#ffffff"                         -- white
colorRed                 = "#ff0000"                         -- bright red
colorGrey0               = "#2e3436" --myNormalBorderColor               -- dark grey
colorGrey1               = "#606060"                         -- light grey (font color dzen bar)
colorGreen               = myFocusedBorderColor              -- green

-- define the bars
myLeftBar                = makeDzen 0 0 leftBarWidth barHeight "r"
--myRightBar             = "conky -c " ++ conkyFile ++ " | " 
--                         ++ makeDzen leftBarWidth 0 (monitorWidth - leftBarWidth) barHeight "r"


--myWorkspaces           = clickable . (map dzenEscape) $ nWorkspaces 9 ["web", "irc", "im"]
myWorkspaces             = clickable $ ["1","2","3","4","5","6","7","8","9"]
    where
        nWorkspaces n [] = map show [1 .. n]
        nWorkspaces n l  = init l ++ map show [length l .. n] ++ [last l]
        clickable l      = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                           | (i,ws) <- zip [1..] l
                           , let n = if i == 10 then 0 else i
                           ]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- }}}

-- Dzen Functions           {{{
makeDzen x y w h a = "dzen2 -p" ++
                     " -ta "    ++ a ++
                     " -x "     ++ show x ++
                     " -y "     ++ show y ++
--                   " -w "     ++ show w ++
                     " -h "     ++ show h ++
                     " -fn '"   ++ myFont ++ "'" ++
                     " -fg '"   ++ colorGrey1 ++ "'" ++
                     " -bg '"   ++ colorGrey0 ++ "' -e ''"

dzenStrip :: String -> String
dzenStrip = strip [] where
    strip keep x
      | null x || " " == x         = keep
      | null keep && ' ' == head x = strip keep (tail x)
      | "^"  `isPrefixOf` x        = strip keep (drop 1 . dropWhile (/= ')') $ x)
      | otherwise                  = let (good,x') = span (/= '^') x
                                     in strip (keep ++ good) x'
-- }}}

-- Named Scratchpads (U3)   {{{
--
scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
mySPFloat      = customFloating scratchpadSize

mySPs  =
    [ NS "term"  "xterm"                     (title =? "xterm")  mySPFloat
    , NS "top"   "xterm -e top"              (title =? "top")    mySPFloat
--  , NS "term"  "urxvt-custom -title term"  (title =? "term")   mySPFloat
--  , NS "ghci"  "urxvt-custom -e ghci"      (title =? "ghci")   mySPFloat
--  , NS "sync"  "urxvt-custom -e sy"        (title =? "sy")     mySPFloat
    ]
-- }}}

-- Shell Prompt Theme       {{{
--
mySP = defaultXPConfig
       { font              = myFont
       , bgColor           = colorGrey0
       , fgColor           = colorGreen
       , fgHLight          = "#f8f8f8"
       , bgHLight          = "steelblue3"
       , borderColor       = colorGreen
       , promptBorderWidth = 1
       , position          = Bottom
       , height            = barHeight
       , defaultText       = []
       }
--
myAutoSP       = mySP { autoComplete = Just 1000 }
myWaitSP       = mySP { autoComplete = Just 1000000 }
--
myLayoutPrompt = inputPromptWithCompl myAutoSP "Layout"
                 (mkComplFunFromList' myNamedLayouts) ?+ \l ->
                     sendMessage $ JumpToLayout $ drop 3 l
-- }}}

-- Key Bindings             {{{
--
---    -- launch gmrun
---    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
---
---    -- Resize viewed windows to the correct size
---    , ((modm,               xK_n     ), refresh)
---
---    -- Swap the focused window and the master window
---    , ((modm,               xK_Return), windows W.swapMaster)
---
---    -- Web brouwser
---    , ((modm              , xK_b     ), spawn "firefox")
---    ]

myKeys = \conf -> mkKeymap conf $
    [ ("M-S-<Return>"     , spawn $ XMonad.terminal conf        ) -- (00) launch a terminal
    , ("M-p"              , spawn myDmenu                       ) -- (00) launch dmenu

    -- scratchpads
    , ("M-s s"            , namedScratchpadAction mySPs "term"  ) -- (U3) start floating terminal
    , ("M-s t"            , namedScratchpadAction mySPs "top"   ) -- (U3) start top in floating terminal

    -- rotate workspaces
    , ("M-<R>"            , nextWS                              ) -- (A2) switch to next WS
    , ("M-<L>"            , prevWS                              ) -- (A2) switch to prev WS
    , ("M-C-<R>"          , moveTo Next HiddenNonEmptyWS        ) -- (A2) switch to next non-empty WS
    , ("M-C-<L>"          , moveTo Prev HiddenNonEmptyWS        ) -- (A2) switch to prev non-empty WS
    , ("M-S-C-<R>"        , shiftToNext                         ) -- (A2) send to next WS
    , ("M-S-C-<L>"        , shiftToPrev                         ) -- (A2) send to prev WS
    , ("M-S-<R>"          , shiftToNext >> nextWS               ) -- (A2) send and switch client to next WS
    , ("M-S-<L>"          , shiftToPrev >> prevWS               ) -- (A2) send and switch client to prev WS

    -- layout
    , ("M-\\"             , sendMessage NextLayout              ) -- (00) toggle layouts
    , ("M-S-\\"           , myLayoutPrompt                      ) -- layout prompt
    , ("M-C-\\"           , setLayout $ XMonad.layoutHook conf  ) -- (00) reset layout
    , ("M-f"              , sendMessage (Toggle "Full")         ) -- (L2,3) toggle Full
    , ("M-<Escape>"       , sendMessage ToggleStruts            ) -- (H1) toggle statusbar
    , ("M-t"              , withFocused $ windows . W.sink      ) -- (A1) sink focused window
    , ("M-S-t"            , sinkAll                             ) -- (A1) sink all windows

    -- layout modifiers
    , ("M-["              , sendMessage Shrink                  ) -- (L1) shrink master
    , ("M-]"              , sendMessage Expand                  ) -- (L1) expand master
    , ("M-S-["            , sendMessage MirrorShrink            ) -- (L1) shrink window in slave pane
    , ("M-S-]"            , sendMessage MirrorExpand            ) -- (L1) expand window in slave pane
    , ("M-C-["            , sendMessage (IncMasterN 1)          ) -- (00) increase master windows number
    , ("M-C-]"            , sendMessage (IncMasterN (-1))       ) -- (00) decrease master windows number

    -- window placements
    , ("M-<Tab>"          , windows W.focusDown                 ) -- (00) move focus down (clock-wise)
    , ("M-S-<Tab>"        , windows W.focusUp                   ) -- (00) move focus up (counter clock-wise)
    , ("M-C-<Tab>"        , rotSlavesUp                         ) -- (A4) rotate slaves
    , ("M-S-C-<Tab>"      , rotSlavesDown                       ) -- (A4) rotate slaves
    , ("M-<Page_Down>"    , windows W.focusDown                 ) -- (00) move focus down
    , ("M-<Page_Up>"      , windows W.focusUp                   ) -- (00) move focus up
    , ("M-S-<Page_Down>"  , windows W.swapDown                  ) -- swap focused window with next window
    , ("M-S-<Page_Up>"    , windows W.swapUp                    ) -- swap focused window with previous window
    , ("M-m"              , windows W.focusMaster               ) -- (00) focus master
    , ("M-S-m"            , promote                             ) -- (A3) promote to master
--  , ("M-z"              , focusUrgent                         ) -- focus urgent

    -- pin (copy) client
    , ("M-="              , windows copyToAll                   ) -- (A8) pin focused window to all workspaces
    , ("M-S-="            , killAllOtherCopies                  ) -- (A8) unpin focused window on all other WS
    , ("M-C-="            , kill1                               ) -- (A8) unpin focused window from current WS
                                                                  --      window is closed if it is only on current WS
    , ("M-C-S-="          , kill                                ) -- (00) close focused window, as well on the other WS

    -- client
    , ("M-S-c"            , kill                                ) -- (00) kill foced window
    , ("M-C-S-c"          , kill                                ) -- (00) kill foced window

    -- system
    , ("M-q"              , spawn myRestart                     ) -- (00) Restart xmonad
    , ("M-S-q"            , spawn "leave"                       ) -- (..) ~/bin/leave
    , ("M-C-q"            , io (exitWith ExitSuccess)           ) -- (00) exit, logout
    ]
    ++
    -- "M-[1..]"                                                  -- (00) Switch to workspace N
    -- "M-C-[1..]"                                                -- (A2) Switch to workspace N on other screen
    -- "M-C-S-[1..]"                                              -- (00) Send client to workspace N
    [ (m ++ "M-" ++ [k], f i)
           | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=\\"
           , (m, f) <- [ (""    , windows . W.view)
                       , ("C-"  , \ws -> nextScreen >> (windows . W.view $ ws))
                       , ("C-S-", windows . W.shift)
                       ]
    ]
    ++
    -- "M-S-[1..]"                                               -- Move client and switch to workspace N
    [ ("M-S-" ++ [k], (windows $ W.shift i) >> (windows $ W.view i))
           | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'])
    ]
    ++
    -- "M-{w,e,r}"                                               -- (00) Switch to physical/Xinerama screens 1, 2, or 3
    -- "M-S-{w,e,r}"                                             -- (00) Move client to screen 1, 2, or 3
    -- http://haskell.org/haskellwiki/Xmonad/Config_archive/doitan%27s_xmonad.hs
    [ ("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
           | (k, sc) <- zip ["w","e","r"] [1,0,2] -- was [0..] *** change to match screen order ***
           , (f, m)  <- [ (W.view, "")
                        , (W.shift, "S-")
                        ]
    ]

    where

      -- launches dmenu at the bottom of the screen
      myDmenu   = "exe=`dmenu_path | dmenu -b -i -sb \"#63b613\" -nb \"#303030\" -sf \"#303030\" -nf \"#63b613\" -fn \"terminus\"` && eval \"exec $exe\""

      -- kill all conky/dzen2 before executing default restart command
      myRestart = "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                  "for pid in `pgrep stalonetray`; do kill -9 $pid; done && " ++
                  "xmonad --recompile && xmonad --restart"


-- }}}

-- Mouse Bindings           {{{
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1)              , (\w -> focus w >> mouseMoveWindow w))             -- (00) M-button1, Set the window to floating mode and move by dragging
    , ((modMask, button2)              , (\w -> focus w >> windows W.swapMaster))          -- (00) M-button2, raise the window to the top of the stack
    , ((modMask, button3)              , (\w -> focus w >> FlexR.mouseResizeWindow w))     -- (A5) M-button3, resize floating window by dragging
    , ((modMask .|. shiftMask, button3), (\w -> focus w >> SQR.mouseResizeWindow w True )) -- (A6) M-S-button3, constrained resize floating window
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
--
-- }}}

-- Layouts                  {{{
--
myNamedLayouts   = ["1: Tall", "2: M.Tall", "3: Full", "4: M.Acc"]

-- http://www.n-sch.de/
myIconDir                             = "/home/ronald/.xmonad/icons"
myIcons layout
      | is "Tall"                     = Just "layout-tall-right.xbm"
      | is "M.Tall"                   = Just "layout-mirror-bottom.xbm"
      | is "Full"                     = Just "layout-full.xbm"
      | is "M.Acc"                    = Just "layout-accordion.xbm"
--    | is "IM Grid"                  = Just "layout-im.xbm"
--    | is "IM ResizableTall"         = Just "layout-im-tall.xbm"
--    | is "IM Mirror ResizableTall"  = Just "layout-im-mirror.xbm"
--    | is "IM Full"                  = Just "layout-im-full.xbm"
--    | is "IM ReflectX IM Full"      = Just "layout-gimp.xbm"
      | otherwise = Nothing
    where is = (`isInfixOf` layout)

myLayout = avoidStruts                                    -- (H1)
           $ toggleLayouts full                           -- (L2,3)
           $ basicLayout
    where
      basicLayout = tiled ||| mtiled ||| full ||| maccordion
      tiled       = named "Tall"   $ ResizableTall nmaster delta ratio []  -- (L5,1)
      mtiled      = named "M.Tall" $ Mirror tiled
      full        = named "Full"   $ noBorders Full
      maccordion  = named "M.Acc"  $ Mirror Accordion
      nmaster     = 1                                     -- number of windows in master pane
      ratio       = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio
      delta       = 3/100                                 -- Percent of screen


-- }}}

-- Window Rules             {{{

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
myManageHook = composeAll (

    -- Float apps
    [ className =? c <||> resource =? r <||> title =? t <||> isDialog --> doCenterFloat
    | c <- ["Switch2", "quantum-Quantum"]
    , r <- ["Extension", "Download"]
    , t <- ["Schriftart auswählen", "Choose a directory"]
    ]
    ++
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Zenity"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , resource  =? "stalonetray"    --> doIgnore
    , namedScratchpadManageHook mySPs               -- (U3)
    ]) <+> manageDocks                              -- (H1)

-- }}}

-- Event handling           {{{

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = ewmhDesktopsEventHook

-- }}}

-- Status Bars and Logging  {{{

myLogHook h = (dynamicLogWithPP $ defaultPP                  -- (H2,3)
   { ppCurrent            = dzenColor colorGrey0 colorGreen . pad
   , ppVisible            = dzenColor colorGrey0 colorGreen . pad
   , ppHidden             = dzenFG    colorGreen . pad
   , ppHiddenNoWindows    = dzenFG    colorGrey1 . pad
   , ppLayout             = dzenFG    colorGreen . wrap "^ca(1,xdotool key Super_L+backslash)" "^ca()" . pad . loadIcons
   , ppUrgent             = myWrap    colorRed   "{"  "}"  . pad . dzenStrip
   , ppTitle              = myWrap    colorGreen "[ " " ] " . shorten 40
   , ppWsSep              = ""
   , ppSep                = " ^fg(#a4a4a4)^r(2x2)^fg() "         -- separate with dots
   , ppOrder              = \(ws:l:t:xs) -> xs ++ [t,ws,l]       -- change order, place layout all the way to the right
                                                                 -- (U3) filters out workspace "NSP"
   , ppSort               = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)
   , ppOutput             = hPutStrLn h
   }) >> updatePointer (Relative 0.95 0.85)                      -- (A7)
   where
     loadIcons s          = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")"
     dzenFG c             = dzenColor c ""
     myWrap c l r         = wrap (dzenFG c l) (dzenFG c r)

-- }}}

-- Startup Hook             {{{

myStartupHook = return ()

-- }}}

-- Main                     {{{
--
main = do
--h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -e 'onstart=lower'"
  h <- spawnPipe myLeftBar
  --spawn "stalonetray -i 16 --geometry 5x1-0+0 --max-geometry 3x1 --icon-gravity E -bg '#2e3436' --sticky --skip-taskbar"
  xmonad $ defaultConfig
    { terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , layoutHook         = smartBorders (myLayout)
    , manageHook         = myManageHook
    , handleEventHook    = myEventHook
    , logHook            = ewmhDesktopsLogHook >> myLogHook h
    , startupHook        = myStartupHook
    }

-- }}}

-- vim:foldmethod=marker
