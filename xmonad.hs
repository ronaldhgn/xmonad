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

import XMonad -- hiding ( (|||) )                        -- (00)

import Data.Monoid                                       -- (00)
import System.Exit                                       -- (00)
import Data.List (isPrefixOf,isInfixOf)
import qualified XMonad.StackSet as W                    -- (00)
import qualified Data.Map as M                           -- (00)
import Data.Maybe (fromMaybe)                            -- (  ) needed for layout icons
import Data.Ratio ((%))

-- Utilities
import XMonad.Util.EZConfig as V                         -- (U1) "M-C-x" style keybindings
import XMonad.Util.Run                                   -- (U2) for spawnPipe and hPutStrLn
import XMonad.Util.NamedScratchpad                       -- (U3) named scratchpads that support several arbitrary applications at the same time
import XMonad.Util.NamedWindows (getName)                -- (U4)
import XMonad.Util.Scratchpad
--import XMonad.Util.Replace                               -- (U5) launch another window manager

-- Action
import XMonad.Actions.WithAll                            -- (A1) perform action on all windows
import XMonad.Actions.CycleWS                            -- (A2) general workspace-switching
import XMonad.Actions.Promote                            -- (A3) moves focused window to master pane
import XMonad.Actions.RotSlaves                          -- (A4) rotate all slave windows
import qualified XMonad.Actions.FlexibleResize as FlexR  -- (A5) Resize floating windows from any corner
import qualified XMonad.Actions.ConstrainedResize as SQR -- (A6) allow constrained resize of floating windows with mouse
import XMonad.Actions.UpdatePointer                      -- (A7) perform action on all windows
import XMonad.Actions.CopyWindow                         -- (A8) duplicate a window on multiple workspaces
import XMonad.Actions.WindowGo                           -- (A9) runOrRaise
import XMonad.Actions.SwapWorkspaces                     -- (A10) swap workspaces
import XMonad.Actions.GridSelect                         -- (A11) grid select
import XMonad.Actions.NoBorders                          -- (A12) toggle borders

import qualified XMonad.Actions.Submap as SM
import qualified XMonad.Actions.Search as S

-- Layout
import XMonad.Layout.ResizableTile                       -- (L1) change width/height of window
import XMonad.Layout.ToggleLayouts                       -- (L2) a module to toggle between two layouts
import XMonad.Layout.NoBorders                           -- (L3) make given layout display without borders
import XMonad.Layout.Accordion                           -- (L4) accordion style layout
import XMonad.Layout.Named                               -- (L5) rename workspaces
import XMonad.Layout.DragPane                            -- (L6) gimp layout
import XMonad.Layout.LayoutCombinators hiding ((|||))    -- (L7)
import XMonad.Layout.Minimize                            -- (L8) minimize window
import qualified XMonad.Layout.Magnifier as Mag          -- (L9) magnifier
import XMonad.Layout.Spacing                             -- (L10) add spacing between windows
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Gaps

-- Hooks
import XMonad.Hooks.ManageDocks                          -- (H1) autom. manage dock type programs
import XMonad.Hooks.DynamicLog hiding (dzenStrip)        -- (H2) several drop-in loghooks to output status info
import XMonad.Hooks.EwmhDesktops                         -- (H3) allows the user to interact with xmonad by clicking on panels and window lists
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.Place
-- import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.UrgencyHook                          -- (H4)
import XMonad.Hooks.RestoreMinimized                     -- (H5) restore minimize window
import XMonad.Hooks.SetWMName                            -- (H6) extension convince Java that xmonad is "LG3D"

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace

-- }}}
-- Global Settings          {{{
--
-- define the bars
myLeftBar                = makeDzen 0 0 leftBarWidth barHeight "l"
myRightBar               = "sleep 1; conky -c " ++ conkyFile ++ " | " 
                         ++ makeDzen leftBarWidth 0 (monitorWidth - leftBarWidth - 3*16) barHeight "r"

--myWorkspaces           = (map dzenEscape) $ nWorkspaces 0 ["1.www","2.sys","3","4","5","6","7","8.tex","9.art"]
myWorkspaces             = (map dzenEscape) $ nWorkspaces 0 ["1","2","3","4","5","6","7","8","9"]
    where
        nWorkspaces n [] = map show [1 .. n]
        nWorkspaces n l  = init l ++ map show [length l .. n] ++ [last l]

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myModMask                = mod4Mask                -- supermod key
monitorWidth             = 1280                    -- two bars span this width
leftBarWidth             = 470                     -- the right bar will span difference

myTerminal               = "urxvt"
myBorderWidth            = 2
barHeight                = 16

myFont                   = "Terminus-8"            -- xft enabled dzen req'd
conkyFile                = "~/.conkyrc"            -- populates the right status bar

myGreenColor             = "#9FBC00" --"#499401" --"#63b613"   -- green
myWhiteColor             = "#e0e0e0"               -- white
myRedColor               = "#ff5600"               -- (orange) red
myDarkGreyColor          = "#181818" -- "#262626"               -- dark grey
myLightGreyColor         = "#8f8f8f" --"#606060"   -- light grey (font color dzen bar)
myEmptyFGColor           = "#444444"
myEmptyBGColor           = ""

myNormalFGColor          = myGreenColor
myNormalBGColor          = myEmptyBGColor

myTitleFGColor           = myGreenColor
myTitleBGColor           = myEmptyBGColor

myFocusedFGColor         = myDarkGreyColor
myFocusedBGColor         = myGreenColor

myVisibleFGColor         = myDarkGreyColor
myVisibleBGColor         = myGreenColor

myUrgentFGColor          = myWhiteColor
myUrgentBGColor          = myRedColor

myHiddenFGColor          = myGreenColor
myHiddenBGColor          = myEmptyBGColor

myHiddenNWFGColor        = myLightGreyColor
myHiddenNWBGColor        = myEmptyBGColor

myNormalBorderColor      = myDarkGreyColor
myFocusedBorderColor     = myGreenColor

-- }}}
-- Dzen Functions           {{{
makeDzen x y w h a = "dzen2 -p -xs 1" ++
                     " -ta "    ++ a ++
                     " -x "     ++ show x ++
                     " -y "     ++ show y ++
                     " -w "     ++ show w ++
                     " -h "     ++ show h ++
                     " -fn '"   ++ myFont ++ "'" ++
                     " -fg '"   ++ myLightGreyColor ++ "'" ++
                     " -bg '"   ++ myDarkGreyColor ++ "' -e ''"

-- }}}
-- Named Scratchpads (U3)   {{{
--
scratchpadSize = W.RationalRect (1/4) (1/4) (1/2) (1/2)
mySPFloat      = customFloating scratchpadSize

mySPs  =
    [ NS "urxvt"   "urxvt -title 'SP : urxvt'"     (title     =? "SP : urxvt") mySPFloat
    , NS "top"     "urxvt -title top -e top"       (title     =? "top")        mySPFloat
    , NS "vol"     "urxvt -title vol -e alsamixer" (title     =? "vol")        mySPFloat
    , NS "ncmpcpp" "urxvt -e ncmpcpp"              (title     =? "ncmpcpp")    mySPFloat
    , NS "nzbget"  "urxvt -e nzbget -C"            (title     =? "nzbget")     mySPFloat
    , NS "zim"     "cryptzenity zim"               (className =? "Zim")        mySPFloat
--  , NS "term"    "urxvt-custom -title term"      (title     =? "term")       mySPFloat
--  , NS "ghci"    "urxvt-custom -e ghci"          (title     =? "ghci")       mySPFloat
--  , NS "sync"    "urxvt-custom -e sy"            (title     =? "sy")         mySPFloat
    ]
-- }}}
-- Shell Prompt Theme       {{{
--
mySP = defaultXPConfig
       { font              = myFont
       , bgColor           = myDarkGreyColor
       , fgColor           = myGreenColor
       , fgHLight          = "#f8f8f8"
       , bgHLight          = "steelblue3"
       , borderColor       = myGreenColor
       , promptBorderWidth = 0
       , position          = Bottom
       , height            = barHeight
       , defaultText       = []
       }
--
myAutoSP                   = mySP { autoComplete = Just 1000 }
myWaitSP                   = mySP { autoComplete = Just 1000000 }
--
myLayoutPrompt             = inputPromptWithCompl myAutoSP "Layout"
                             (mkComplFunFromList' myNamedLayouts) ?+ \l ->
                                 sendMessage $ JumpToLayout $ drop 3 l
-- }}}
-- Key Bindings             {{{
--
myKeys = \conf -> mkKeymap conf $
    [ ("M-S-<Return>"           , spawn $ XMonad.terminal conf                  ) -- (00) launch a terminal
    , ("M-p"                    , spawn myDmenu                                 ) -- (00) launch dmenu, i.e., open new program
    , ("M-S-p"                  , goToSelected defaultGSConfig                  ) -- (A11) grid select, choose open windows
    --, ("M-S-p"                  , spawnSelected defaultGSConfig ["xterm","mplayer","gvi"]) -- (A11) grid select default apps

    -- scratchpads
    , ("M-s s"                  , namedScratchpadAction mySPs "urxvt"           ) -- (U3) start floating terminal
    , ("M-s t"                  , namedScratchpadAction mySPs "top"             ) -- (U3) start top in floating terminal
    , ("M-s v"                  , namedScratchpadAction mySPs "vol"             ) -- (U3) start top in floating terminal
    , ("M-s m"                  , namedScratchpadAction mySPs "ncmpcpp"         ) -- (U3) start top in floating terminal
    , ("M-s n"                  , namedScratchpadAction mySPs "nzbget"          ) -- (U3) start top in floating terminal
    , ("M-s z"                  , namedScratchpadAction mySPs "zim"             ) -- (U3) start top in floating terminal

    -- run apps
    , ("M-x x"                  , spawn $ XMonad.terminal conf                  ) -- (A9) open gmrun replacement
    , ("M-x t"                  , spawn "bashrun"                               ) -- (A9) open gmrun replacement
    , ("M-x m"                  , spawn "firefox http://gmail.com"              )
    , ("M-x f"                  , runOrRaise "firefox" (className =? "Firefox") )
    , ("M-x g"                  , runOrRaise "gimp" (className =? "Gimp")       )

    , ("<XF86AudioPlay>"        , spawn "mpc toggle"                            ) -- play/pause mpd
    , ("<XF86AudioStop>"        , spawn "mpc stop"                              ) -- stop mpd
    , ("<XF86AudioPrev>"        , spawn "mpc prev"                              ) -- prev song
    , ("<XF86AudioNext>"        , spawn "mpc next"                              ) -- next song
    , ("<XF86AudioMute>"        , spawn "amixer set Master toggle ?> /dev/null" )
    , ("<XF86AudioLowerVolume>" , spawn "amixer set Master 2dB- ?> /dev/null"   )
    , ("<XF86AudioRaiseVolume>" , spawn "amixer set Master 2dB+ ?> /dev/null"   )

    , ("<XF86Launch3>"          , spawn "switchPenButton"                       ) -- switch stylus button, XF86Launch2 maps too button on screen
    , ("<XF86Launch4>"          , spawn "./bin/toggleCellwriter"                ) -- toggle cellwriter, XF86Launch3 maps too button on screen

    -- rotate workspaces
    , ("M-/"                    , toggleWSNoSP                                  ) -- (A2) toggle recently visited workspaces
    , ("M-<R>"                  , nextWS                                        ) -- (A2) switch to next WS
    , ("M-<L>"                  , prevWS                                        ) -- (A2) switch to prev WS
    , ("M-C-<R>"                , moveTo Next HiddenNonEmptyWS                  ) -- (A2) switch to next non-empty WS
    , ("M-C-<L>"                , moveTo Prev HiddenNonEmptyWS                  ) -- (A2) switch to prev non-empty WS
    , ("M-S-C-<R>"              , shiftToNext                                   ) -- (A2) send to next WS
    , ("M-S-C-<L>"              , shiftToPrev                                   ) -- (A2) send to prev WS
    , ("M-S-<R>"                , shiftToNext >> nextWS                         ) -- (A2) send and switch client to next WS
    , ("M-S-<L>"                , shiftToPrev >> prevWS                         ) -- (A2) send and switch client to prev WS
    , ("M-M1-<R>"               , swapTo Next                                   ) -- (A10) swap WS with next WS
    , ("M-M1-<L>"               , swapTo Prev                                   ) -- (A10) swap WS with prev WS

    -- layout
    , ("M-\\"                   , sendMessage NextLayout                        ) -- (00) toggle layouts
    , ("M-S-\\"                 , myLayoutPrompt                                ) -- layout prompt
    , ("M-C-\\"                 , setLayout $ XMonad.layoutHook conf            ) -- (00) reset layout
    --, ("M-M1-\\"                , restart "/home/ronald/bin/citoxmd" True       ) -- (U5) launch cinnamon WM
    , ("M-f"                    , sendMessage (Toggle "Full")                   ) -- (L2,3) toggle Full
    , ("M-<Escape>"             , sendMessage ToggleStruts                      ) -- (H1) toggle statusbar
    , ("M-t"                    , withFocused $ windows . W.sink                ) -- (A1) sink focused window
    , ("M-S-t"                  , sinkAll                                       ) -- (A1) sink all windows


    , ("M-S-f"                  , sendMessage $ ToggleGaps                      ) -- (A1) sink all windows
    , ("M-S-g"                  , sendMessage $ ToggleGap D                     ) -- (A1) sink all windows
    , ("M-S-b"                  , sendMessage $ IncGap 5 D                      ) -- (A1) sink all windows
    , ("M-S-v"                  , sendMessage $ DecGap 5 D                      ) -- (A1) sink all windows

    -- layout modifiers
    , ("M-["                    , sendMessage Shrink                            ) -- (L1) shrink master
    , ("M-]"                    , sendMessage Expand                            ) -- (L1) expand master
    , ("M-S-["                  , sendMessage MirrorShrink                      ) -- (L1) shrink window in slave pane
    , ("M-S-]"                  , sendMessage MirrorExpand                      ) -- (L1) expand window in slave pane
    , ("M-C-["                  , sendMessage (IncMasterN 1)                    ) -- (00) increase master windows number
    , ("M-C-]"                  , sendMessage (IncMasterN (-1))                 ) -- (00) decrease master windows number
    , ("M-o"                    , sendMessage Mag.Toggle                        ) -- (L9) magnifier toggle on/off
    , ("M-S-o"                  , sendMessage Mag.ToggleOn                      ) -- (L9) magnifier toggle on
    , ("M-C-o"                  , sendMessage Mag.ToggleOff                     ) -- (L9) magnifier toggle off
    , ("M-M1-["                 , sendMessage Mag.MagnifyLess                   ) -- (L9) magnifier decrease
    , ("M-M1-]"                 , sendMessage Mag.MagnifyMore                   ) -- (L9) magnifier increase

    -- window placements
    , ("M-<Tab>"                , windows W.focusDown                           ) -- (00) move focus down (clock-wise)
    , ("M-S-<Tab>"              , windows W.focusUp                             ) -- (00) move focus up (counter clock-wise)
    , ("M-C-<Tab>"              , rotSlavesUp                                   ) -- (A4) rotate slaves
    , ("M-S-C-<Tab>"            , rotSlavesDown                                 ) -- (A4) rotate slaves
    , ("M-<Page_Down>"          , windows W.focusDown                           ) -- (00) move focus down
    , ("M-<Page_Up>"            , windows W.focusUp                             ) -- (00) move focus up
    , ("M-S-<Page_Down>"        , windows W.swapDown                            ) -- swap focused window with next window
    , ("M-S-<Page_Up>"          , windows W.swapUp                              ) -- swap focused window with previous window
    , ("M-m"                    , windows W.focusMaster                         ) -- (00) focus master
    , ("M-S-m"                  , promote                                       ) -- (A3) promote to master
    , ("M-z"                    , focusUrgent                                   ) -- focus urgent
    , ("M-S-z"                  , clearUrgents                                  ) -- focus urgent
    , ("M-C-w"                  , swapPrevScreen                                ) -- swap current screen with previous screen
    , ("M-C-e"                  , swapNextScreen                                ) -- swap current screen with next screen

    -- pin (copy) client
    , ("M-="                    , windows copyToAll                             ) -- (A8) pin focused window to all workspaces
    , ("M-S-="                  , killAllOtherCopies                            ) -- (A8) unpin focused window on all other WS
    , ("M-C-="                  , kill1                                         ) -- (A8) unpin focused window from current WS
                                                                                  --      window is closed if it is only on current WS
    , ("M-C-S-="                , kill                                          ) -- (00) close focused window, as well on the other WS
    --, ("M-h"                    , withFocused (\f -> sendMessage (MinimizeWin f))) -- (L8) hide focused window
    , ("M-S-h"                  , sendMessage RestoreNextMinimizedWin           ) -- (H5) restore hidden window

    -- client
    , ("M-S-c"                  , kill                                          ) -- (00) kill foced window
    , ("M-C-S-c"                , kill                                          ) -- (00) kill foced window

    -- system
    , ("M-q"                    , spawn myRestart                               ) -- (00) Restart xmonad
    , ("M-S-q"                  , spawn "leave"                                 ) -- (..) ~/bin/leave
    , ("M-C-q"                  , io (exitWith ExitSuccess)                     ) -- (00) exit, logout
    ]
    ++
    -- "M-[1..]"                                                                  -- (00) Switch to workspace N
    -- "M-C-[1..]"                                                                -- (A2) Switch to workspace N on other screen
    -- "M-C-S-[1..]"                                                              -- (00) Send client to workspace N
    [ (m ++ "M-" ++ [k], f i)
           | (i, k) <- zip (XMonad.workspaces conf) "1234567890-=\\"
           , (m, f) <- [ (""    , windows . W.view)
                       , ("C-"  , \ws -> nextScreen >> (windows . W.view $ ws))
                       , ("C-S-", windows . W.shift)
                       ]
    ]
    ++
    -- "M-S-[1..]"                                                                -- Move client and switch to workspace N
    [ ("M-S-" ++ [k], (windows $ W.shift i) >> (windows $ W.view i))
           | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'])
    ]
    ++
    -- "M-{w,e,r}"                                                                -- (00) Switch to physical/Xinerama screens 1, 2, or 3
    -- "M-S-{w,e,r}"                                                              -- (00) Move client to screen 1, 2, or 3
    -- http://haskell.org/haskellwiki/Xmonad/Config_archive/doitan%27s_xmonad.hs
    [ ("M-" ++ m ++ k, screenWorkspace sc >>= flip whenJust (windows . f))
           | (k, sc) <- zip ["w","e","r"] [1,0,2] -- was [0..] *** change to match screen order ***
           , (f, m)  <- [ (W.view, "")
                        , (W.shift, "S-")
                        ]
    ]
    ++
    -- "M-A-[1..]"                                                               -- (A10) Swap current WS with workspace N
    [ ("M-M1-"  ++ [k], (windows $ swapWithCurrent i))
        | (i, k) <- zip (XMonad.workspaces conf) (['1' .. '9'])
    ]
    -- Search commands
    ++ [ ("M-a " ++ k, S.promptSearch mySP f) | (k,f) <- searchList ]
    ++ [ ("M-S-a " ++ k, S.selectSearch f) | (k,f) <- searchList ]

    where
      toggleWSNoSP = windows $ W.view =<< W.tag . head . scratchpadFilterOutWorkspace . W.hidden
      -- launches dmenu at the bottom of the screen
      -- myDmenu   = "exe=`dmenu_path | dmenu -b -i -sb \"#9FBC00\" -nb \"#262626\" -sf \"#262626\" -nf \"#9FBC00\" -fn \"terminus\"` && eval \"exec $exe\""
      myDmenu   = "exe=`dmenu_run -b -i -sb \"#9FBC00\" -nb \"#262626\" -sf \"#262626\" -nf \"#9FBC00\" -fn \"terminus\"` && eval \"exec $exe\""

      -- kill all conky/dzen2 before executing default restart command
      myRestart = "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
                  "for pid in `pgrep stalonetray`; do kill -9 $pid; done && " ++
                  "xmonad --recompile && xmonad --restart"

      searchList :: [ (String, S.SearchEngine) ]
      searchList = [ ("d", S.dictionary)
                   , ("g", S.google)
                   , ("i", S.images)
                   , ("j", S.imdb)
                   , ("l", S.lucky)
                   , ("m", S.maps)
                   , ("a", S.multi)
                   , ("t", S.thesaurus)
                   , ("y", S.youtube)
                   , ("w", S.wikipedia)
                   , ("1", S.searchEngine "archbbs" "http://bbs.archlinux.org/search.php?action=search&keywords=")
                   , ("2", S.searchEngine "archwiki" "http://wiki.archlinux.org/index.php/Special:Search?search=")
                   , ("3", S.searchEngine "arch" "http://www.archlinux.org/packages/?q=")
                   , ("4", S.searchEngine "AUR" "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=")
                   ]

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
myNamedLayouts   = ["1: Tall", "2: M.Tall", "3: Full", "4: M.Acc", "5: Grid", "6: Gimp"]

-- http://www.n-sch.de/
myIconDir                             = "/home/ronald/.xmonad/icons"
myIcons layout
      | is "Tall"                     = Just "layout-tall-right.xbm"
      | is "M.Tall"                   = Just "layout-mirror-bottom.xbm"
      | is "Full"                     = Just "layout-full.xbm"
      | is "M.Acc"                    = Just "layout-accordion.xbm"
--    | is "IM ResizableTall"         = Just "layout-im-tall.xbm"
--    | is "IM Mirror ResizableTall"  = Just "layout-im-mirror.xbm"
--    | is "IM Full"                  = Just "layout-im-full.xbm"
      | is "Gimp"                     = Just "layout-gimp.xbm"
      | otherwise = Nothing
    where is = (`isInfixOf` layout)

myLayout = avoidStruts                                    -- (H1)
           $ toggleLayouts full                           -- (L2,3)
           $ onWorkspace "9" gimpLayout
           $ Mag.maximizeVertical                         -- (L9)
           $ minimize basicLayout
    where
      --basicLayout = gaps [(D,110)] $ withIM (2/10) (Role "buddy_list") $ tiled ||| mtiled ||| full ||| grid ||| maccordion
      basicLayout = withIM (2/10) (Role "buddy_list") $ spacing 0 $ tiled ||| mtiled ||| full ||| grid ||| maccordion  -- (L10) spacing 0|4
      --gimpLayout  = withIM (2/10) (Role "buddy_list") $ Tall 1 (3 / 100) (1 / 2) ||| tabbedLayout ||| Full ||| myGimp
      gimpLayout  = withIM (2/10) (Role "buddy_list") $ Tall 1 (3 / 100) (1 / 2) ||| myGimp ||| Full

      myGimp      = named "Gimp"   $ tabbedLayout ****||* Full
      tabbedLayout = tabbedBottomAlways shrinkText defaultTheme

      tiled       = named "Tall"   $ ResizableTall nmaster delta ratio []  -- (L5,1)
      mtiled      = named "M.Tall" $ Mirror tiled
      full        = named "Full"   $ noBorders Full
      maccordion  = named "M.Acc"  $ Mirror Accordion
      grid        = named "Grid"   $ Grid

      nmaster     = 1                                     -- number of windows in master pane
      ratio       = toRational (2/(1 + sqrt 5 :: Double)) -- golden ratio
      delta       = 3/100                                 -- Percent of screen


-- }}}
-- Window Rules             {{{

myManageHook = ( composeAll . concat $
    [ [className =? c <&&> role /=? r --> doCenterFloat   | (c,r) <- myIM ]
    , [role      =? r                 --> doShift s       | (r,s) <- myShifts  ] -- move
    ]
    ++
    -- Float apps
    [ [className =? c <||> resource =? r <||> title =? t <||> name =? n <||> isDialog --> doCenterFloat
    | c <- myFloatsC , r <- myFloatsR , t <- myFloatsT , n <- myFloatsN]
    ]
    ++
    -- Ignore apps
    [ [(className =? c <||> resource =? r <||> title =? t <||> name =? n) --> doIgnore
    | c <- myIgnoresC , r <- myIgnoresR , t <- myIgnoresT , n <- myIgnoresN]
    ]
    ++
    [ [isDialog  --> placeHook (inBounds (underMouse (0,0))) <+> makeMaster <+> doFloat]
    , [namedScratchpadManageHook mySPs]                  -- (U3)
    ] )
    -- <+> positionStoreManageHook
    <+> manageDocks                                      -- (H1)
  where
     makeMaster = insertPosition Master Newer
     role       = stringProperty "WM_WINDOW_ROLE"
     name       = stringProperty "WM_NAME"

     -- [("ClassName","Role")]
     myIM       = [("Pidgin","buddy_list")]
     -- [("role","WS")]
     myShifts   = [("gimp-image-window","9"),("gimp-toolbox","9"),("gimp-dock","9")]

     -- doIgnore (cellwriter, see possible nice patch for xmonad)
     myIgnoresC = ["stalonetray"]
     myIgnoresR = ["stalonetray","desktop","desktop_window"]
     myIgnoresT = ["stalonetray"]
     myIgnoresN = ["stalonetray"]
     -- doCenterFloat
     myFloatsC  = ["MPlayer","Zenity","VirtualBox"]
     myFloatsR  = ["Extension", "Download"]
     myFloatsT  = ["Choose a directory","Custom Smiley Manager"]
     myFloatsN  = ["bashrun","Firefox Preferences"]
-- }}}
-- Event handling           {{{

myEventHook = do
    restoreMinimizedEventHook                                    -- (H5)
    ewmhDesktopsEventHook
--myEventHook e = do
--    positionStoreEventHook e

-- }}}
-- Status Bars and Logging  {{{

myLogHook h = do
  ewmhDesktopsLogHook
  (dynamicLogWithPP $ defaultPP                      -- (H2,3)
     { ppLayout             = dzenColor myNormalFGColor   myNormalBGColor . wrap "^ca(1,xdotool key Super_L+backslash)" "^ca()" . pad . loadIcons
     , ppTitle              = myWrap    myTitleFGColor    myTitleBGColor "[" "]" . dzenColor myWhiteColor myEmptyBGColor . pad . shorten 40

     , ppCurrent            = dzenColor myFocusedFGColor  myFocusedBGColor  . pad
     , ppVisible            = dzenColor myVisibleFGColor  myVisibleBGColor  . pad . clickable
     , ppHidden             = dzenColor myHiddenFGColor   myHiddenBGColor   . pad . clickable
     , ppHiddenNoWindows    = dzenColor myHiddenNWFGColor myHiddenNWBGColor . pad . clickable
     , ppUrgent             = dzenColor myUrgentFGColor   myUrgentBGColor   . pad . clickable . dzenStrip

     , ppWsSep              = ""
     , ppSep                = "^fg(#a4a4a4)^r(2x2)^fg() "          -- separate with dots
     , ppOrder              = \(ws:l:t:xs) -> [l,ws,t] ++ xs       -- change order, place layout all the way to the right
                                                                   -- (U3) filters out workspace "NSP"
     , ppSort               = fmap (namedScratchpadFilterOutWorkspace.) (ppSort defaultPP)
     , ppOutput             = hPutStrLn h
     }) >> updatePointer (Relative 0.95 0.85)                      -- (A7)
     where
       loadIcons s          = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")"
       myWrap f b l r       = wrap (dzenColor f b l) (dzenColor f b r)
       clickable s          = "^ca(1,xdotool key super+" ++ (take 1 s) ++ ")" ++ s ++ "^ca()"  -- function requires that one-number prefix


dzenStrip :: String -> String
dzenStrip = strip [] where
    strip keep x
      | null x || " " == x         = keep
      | null keep && ' ' == head x = strip keep (tail x)
      | "^"  `isPrefixOf` x        = strip keep (drop 1 . dropWhile (/= ')') $ x)
      | otherwise                  = let (good,x') = span (/= '^') x
                                     in strip (keep ++ good) x'

-- }}}
-- Startup Hook             {{{

--myStartupHook = return ()
myStartupHook = do
      setWMName "LG3D" -- (H6)
-- }}}
{-- LibNotifyUrgencyHook    {{{  not in use

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        whenJust (W.findTag w ws) (flash name)
      where flash name index =
                safeSpawn "notify-send" [show name ++ " requests your attention on workspace " ++ dzenStrip index]

--}-- }}}
-- MySpawnHook *HACK*       {{{
--
-- spawn an arbitrary command on urgent
data MySpawnHook = MySpawnHook String deriving (Read, Show)

instance UrgencyHook MySpawnHook where
    urgencyHook (MySpawnHook s) w = spawn $ s

myUrgencyHook = MySpawnHook "aplay /usr/share/sounds/purple/ding.wav"
--
-- }}}

-- Main                     {{{
--
main = do
  --h <- spawnPipe "dzen2 -ta r -fg '#a8a3f7' -bg '#3f3c6d' -e 'onstart=lower'"
  h <- spawnPipe myLeftBar
  spawn myRightBar
  --spawn "stalonetray -i 16 --geometry 0x1-0+0 --max-geometry 3x1 --icon-gravity NE -bg '#262626' --sticky --skip-taskbar"
  spawn "stalonetray -i 16 --geometry 3x1-0+0 --icon-gravity SE --grow-gravity NE -bg '#181818' --sticky --skip-taskbar --kludges=force_icons_size --window-strut top"
  --xmonad $ withUrgencyHook LibNotifyUrgencyHook defaultConfig
  --(U5)-- replace
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn",myFont, "-fg",myWhiteColor, "-bg",myRedColor, "-h","16", "-xs","1"] }
         $ withUrgencyHook myUrgencyHook $ defaultConfig
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
    --, logHook            = ewmhDesktopsLogHook >> myLogHook h >> setWMName "LG3D" -- (H6)
    , logHook            = ewmhDesktopsLogHook >> myLogHook h
    , startupHook        = myStartupHook
    }


-- }}}

-- vim:foldmethod=marker
