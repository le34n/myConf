{--  -------------------------------------------------------------------

 $\    $$\  $\       $$\                                            $\
  $ \ $$ /  $$ \    $$$ \                                           $ \
   $ $$ /   $ $ \  $$$$ |  /$$$$$$\   $$$$$$$\    $$$$$$$\    /$$$$$$ |
    $$ /    $  $ \$$ $$ |  $$  __ $\  $$  __ $\   $$\_   $\  $$  ___$ |
   $$  \    $ | $ $  $$ |  $$ |   $ | $$ |   $ |  $$$$$$$$ | $$ |   $ |
  $$ /$ \   $ |  $  /$$ |  $$ |   $ | $$ |   $ |  $$  ___$ | $$ |   $ |
 $$ /  $ \  $ |   \/ $$ |  \$$$$$$  | $$ |   $ |  $$ |   $ | \$$$$$$$ |
 \_/    \/  \_|      \__|   \______/  \__|   \_|  \__|   \_|  \_______|
                                                                      
--} ----------------------------------------------------------------  --
--                                                                    --
--                                                                    --
--                                                                    --
--                                                                    --
--  ----------------------------------------------------------------  --

import           XMonad
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.SpawnOn (spawnHere)
import           XMonad.Config.Desktop

import           XMonad.Hooks.DynamicBars
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import           XMonad.Hooks.ManageDocks         -- Автоматическое управление программами типа док (баром)
import           XMonad.Hooks.ManageHelpers       -- <doCenterFloat>
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook

-- import           XMonad.Layout.Circle
-- import           XMonad.Layout.Column
-- import           XMonad.Layout.Fullscreen 
-- import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
-- import           XMonad.Layout.HintedTile
-- import           XMonad.Layout.Magnifier
-- import           XMonad.Layout.Minimize
import           XMonad.Layout.Mosaic
import           XMonad.Layout.NoBorders (noBorders, smartBorders)
import           XMonad.Layout.Spacing
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.ThreeColumns

import qualified XMonad.StackSet as W

import qualified XMonad.Util.Cursor as Cur
-- import           XMonad.Util.Dzen
import qualified XMonad.Util.Dzen as Dzen
import           XMonad.Util.EZConfig (additionalKeys)
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.Loggers              -- ppExtras
import           XMonad.Util.Paste
import           XMonad.Util.Run (spawnPipe)      -- Для запуска внешний приложений/скриптов
import           XMonad.Util.Run (runProcessWithInput)
import           XMonad.Util.SpawnOnce            -- Для запуска внешний приложений/скриптов только один раз

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception as E

import           Data.List
import qualified Data.Map as M
import           Data.Monoid

import           Graphics.X11.ExtraTypes.XF86
import           Graphics.X11.Xinerama

import           System.Exit
import           System.IO
import           System.Posix.Unistd              -- Sharing a configuration across different hosts 

-- My import
-- import           Bindings

-- main = xmonad =<< xmobar defaults
-- main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
-- main = xmonad myConfig
-- main = xmonad $ ewmh myConfig
main = xmonad $ ewmh defaults

{-- main = xmonad desktopConfig
    { terminal    = "termite"
    , modMask     = mod4Mask
    }--}

-- {{{ ----------------------------------------------------------------
{---------------
-  Status bar  -
---------------}

unfocusedPP :: PP
unfocusedPP = focusedPP
    { ppCurrent         = xmobarColor colBlack     colWhite  . pad
    }

focusedPP :: PP
focusedPP = def
    { ppCurrent         = xmobarColor colTeal50  colTeal600   . pad        -- How to print the tag of the currently focused workspace (Current workspace (fore/back)ground colour)
    , ppVisible         = xmobarColor colTeal100  colPurple800 . pad        -- How to print tags of visible but not focused workspaces (xinerama only)
    , ppHidden          = xmobarColor colTeal300  colPurple800 . pad        -- How to print tags of hidden workspaces which contain windows
    -- , ppHiddenNoWindows = xmobarColor color9  color1 . pad      -- How to print tags of empty hidden workspaces
    -- , ppUrgent          = xmobarColor color9  color8 . pad      -- Format to be applied to tags of urgent workspaces
    , ppUrgent          = xmobarColor colWhite   colPurple900    . ('^':)
    -- , ppUrgent          = xmobarColor "#FF0000" "".wrap "!" "!"
    , ppSep             = "    "                                -- separator to use between different log sections (window name, layout, workspaces)
    , ppWsSep           = ""                                  -- separator to use between workspace tags
    , ppTitle           = xmobarColor colOrange50  colPurple900  . shorten 100   -- window title format
    -- , ppTitleSanitize   =                                     -- escape / sanitizes input to ppTitle
    , ppLayout          = xmobarColor colOrange50   colPurple900  . wrap ": " " :"
    -- , ppOrder           = \(ws:l:t:_)   -> [ws,l,t]                             -- how to order the different log sections. Example: ppOrder = \(ws:_:t:_) -> [ws,t], and so on.
    -- , ppSort            =                                     -- how to sort the workspaces. See XMonad.Util.WorkspaceCompare for some useful sorts.
    -- , ppExtras          = [ padL loadAvg, logCmd "fortune -n 40 -s" ] -- loggers for generating extra information such as time and date, system load, battery status, and so on. See XMonad.Util.Loggers for examples, or create your own!
    -- , ppOutput          =                                     -- applied to the entire formatted string in order to output it. Can be used to specify an alternative output method (e.g. write to a pipe instead of stdout), and/or to perform some last-minute formatting.
    }

barCreator :: DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid ++ " .config/xmobar/xmobar-only.hs"

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()
-- }}} ---------------------------------------------------------------
-- Tags/Workspaces {{{
{-------------------------
-  Workspaces & layouts  -
-------------------------}

-- Workspaces with single-character names that can be keyed in with no
-- modifiers.
-- simpleWorkspaces :: [XMonad.WorkspaceId]
-- simpleWorkspaces = [[w] | w <- "`1234567890-="]

-- Workspaces clickable via xmobar/xdotool
{-- xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces            :: [String]
myWorkspaces = clickable . (map xmobarEscape) $ ["1","2","3","4","5"] ++ map show [6..9]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,
                            let n = i ]--}

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces = clickable . (map xmobarEscape) $  ["1","2","3","4","5","6","7","8","9"] ++ (map snd myExtraWorkspaces) -- you can customize the names of the default workspaces by changing the list
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..9] l,
                            let n = i ]

myExtraWorkspaces = [(xK_0, "0")]


myAdditionalKeys =
    [ -- ... your other hotkeys ...
    ] ++ [
        ((myModMask, key), (windows $ W.greedyView ws))
        | (key, ws) <- myExtraWorkspaces
    ] ++ [
        ((myModMask .|. shiftMask, key), (windows $ W.shift ws))
        | (key, ws) <- myExtraWorkspaces
    ]

-- }}} -----------------------------------------------------------------
-- myConfig = desktopConfig { modMask            = myModMask
defaults = def { modMask            = myModMask
                         , terminal           = myTerminal
                         , workspaces         = myWorkspaces
                         -- , workspaces         = simpleWorkspaces
                         , borderWidth        = myBorderWidth
                         , normalBorderColor  = myNormalBorderColor
                         , focusedBorderColor = myFocusedBorderColor

                     ----- key bindings
                         , keys               = myKeys
                         , mouseBindings      = myMouseBindings

                     ----- hooks, layouts
                         , layoutHook         = myLayoutHook
                         , manageHook         = myManageHook
                         , handleEventHook    = myHandleEventHook
                         , logHook            = myLogHook
                         , startupHook        = myStartupHook
                         } `additionalKeys` myAdditionalKeys

-- Layouts: {{{
myLayoutHook = avoidStruts . smartBorders
    $ tiled ||| Mirror tiled ||| mosaic 2 [3,2] ||| Grid ||| noBorders Full
        where
            tiled   = Tall nmaster delta ratio
            nmaster = 1
            delta   = 1/100
            ratio   = 1/2
{-- myLayoutHook =  spacing 0
                $ gaps [(U,20)]
                $ tiled ||| Mirror tiled ||| Full
                  where
                    tiled = Tall nmaster delta ratio
                    nmaster = 1
                    delta = 3/100
                    ratio = 1/2
--}
{--
myLayoutHook = spacing 0
                $ avoidStruts
                -- $ toggleLayouts (noBorders Full)
                $ smartBorders
                -- $ minimize
                $ tiled ||| Mirror tiled ||| mosaic 2 [3,2] ||| noBorders Full
                     where
                     tiled   = Tall nmaster delta ratio
                     nmaster = 1
                     delta   = 1/100
                     ratio   = 1/4
--}
-- }}}

-- window rules {{{ ----------------------------------------------------
myManageHook = composeAll . concat $
    [ [isDialog        --> doCenterFloat                        ]
    , [isFullscreen    --> doFullFloat                          ]
    , [className  =? j --> doFloat       | j <- myFloats        ]
    , [className  =? c --> doCenterFloat | c <- myCenterFloats  ]
    , [title      =? t --> doFloat       | t <- myTitleFloats   ]
    , [resource   =? r --> doFloat       | r <- myResourceFloats]
    , [resource   =? i --> doIgnore      | i <- myIgnores       ]
    , [manageDocks]
    ]
    where

        myFloats         = ["Lxappearance", "Tor Browser"]
        myCenterFloats   = ["Galculator", "Gcolor2", "Xmessage"]
        myTitleFloats    = ["Громкость"]
        myResourceFloats = []
        myIgnores        = ["desktop_window", "kdesktop", "conky", "stalonetray"]
-- }}}

-- Event handling {{{ --------------------------------------------------

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- myHandleEventHook = mempty

-- myHandleEventHook = dynStatusBarEventHook barCreator barDestroyer

-- myHandleEventHook = do
--  dynStatusBarEventHook barCreator barDestroyer
--  docksEventHook

myHandleEventHook = handleEventHook def <+> fullscreenEventHook <+> docksEventHook <+> dynStatusBarEventHook barCreator barDestroyer

-- }}} -----------------------------------------------------------------
-- Status bars and logging {{{ 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
-- myLogHook = return ()
myLogHook = multiPP focusedPP unfocusedPP
-- }}} -----------------------------------------------------------------
-- Autostart {{{ 
myStartupHook = do
    setWMName "LG3D"
    spawnOnce "workrave"
    -- Cur.setDefaultCursor Cur.xC_crosshair
    dynStatusBarStartup barCreator barDestroyer
    -- spawnOnce ".config/dzen/standalone/dzen2/conky/main.sh"
    spawnOnce "sh .fehbg"
    spawnOnce "dunst"
    spawnOnce "parcellite"
    -- spawnOnce ".config/dzen/standalone/dzen2/conky/main.sh"
    spawnOnce "xfce4-power-manager"
    spawnOnce "xautolock -time 15 -locker blurlock"
    spawnOnce "compton -CG -e 1.0 --config .config/compton.conf -b"
    spawn "systemctl --user restart redshift"
    spawnOnce "stalonetray"
-- }}} -----------------------------------------------------------------

myModMask  =  mod4Mask
myTerminal = "alacritty"

-- {{{ Helpers ---------------------------------------------------------

alert = Dzen.dzenConfig centered . show . round
centered =  Dzen.onCurr (Dzen.center 150 66)
            Dzen.>=> Dzen.font "-xos4-terminus-medium-*-*--14-*-*-*-*-*-iso10646-*"
            Dzen.>=> Dzen.addArgs ["-fg", "#80c0ff"]
            Dzen.>=> Dzen.addArgs ["-bg", "#000040"]

myTerminus = "-xos4-terminus-medium-*-*--24-*-*-*-*-*-iso10646-*"

externalCommandInPopUp :: String -> [String] -> X ()
externalCommandInPopUp c p = do
    s <- runProcessWithInput c p ""
    Dzen.dzenConfig (   Dzen.onCurr(Dzen.center 800 30 )
                        Dzen.>=> Dzen.font myFont
                        Dzen.>=> Dzen.addArgs ["-fg", "#80c0ff"]
                        Dzen.>=> Dzen.addArgs ["-bg", "#000040"]
                    ) s
-- }}} -----------------------------------------------------------------

-- Theme windows
myNormalBorderColor  = "#4F4B58"
myFocusedBorderColor =   colPink600
myBorderWidth        =  1
myBarHeight          =  "20"

color1               = "#193441"    -- darkblue
color2               = "#00536B"    -- blue
color3               = "#00A388"    -- green
color4               = "#FF8A6B"    -- orange
color5               = "#F9AB5E"    -- yellow
color6               = "#ECF0F1"    -- white
color7               = "#783E57"    -- magenta
color8               = "#BFBFBF"    -- grey
color9               = "#FF0000"    -- red
color10              = "#888888"    -- darkgrey
colorTest            = "#BC4472"    -- Test
colorTest2           = "#4C9882"

-- {{{ GMC

colWhite = "#ffffff"
colBlack = "#000000"

colGrey50  = "#fafafa"
colGrey100 = "#f5f5f5"
colGrey200 = "#eeeeee"
colGrey300 = "#e0e0e0"
colGrey400 = "#bdbdbd"
colGrey500 = "#9e9e9e"
colGrey600 = "#757575"
colGrey700 = "#616161"
colGrey800 = "#424242"
colGrey900 = "#212121"

colRed50   = "#ffebee"
colRed100  = "#ffcdd2"
colRed200  = "#ef9a9a"
colRed300  = "#e57373"
colRed400  = "#ef5350"
colRed500  = "#f44336"
colRed600  = "#e53935"
colRed700  = "#d32f2f"
colRed800  = "#c62828"
colRed900  = "#b71c1c"
colRedA100 = "#ff8a80"
colRedA200 = "#ff5252"
colRedA400 = "#ff1744"
colRedA700 = "#d50000"

colPink50   = "#fce4ec"
colPink100  = "#f8bbd0"
colPink200  = "#f48fb1"
colPink300  = "#f06292"
colPink400  = "#ec407a"
colPink500  = "#e91e63"
colPink600  = "#d81b60"
colPink700  = "#c2185b"
colPink800  = "#ad1457"
colPink900  = "#880e4f"
colPinkA100 = "#ff80ab"
colPinkA200 = "#ff4081"
colPinkA400 = "#f50057"
colPinkA700 = "#c51162"

colIndigo50   = "#e8eaf6"
colIndigo100  = "#c5cae9"
colIndigo200  = "#9fa8da"
colIndigo300  = "#7986cb"
colIndigo400  = "#5c6bc0"
colIndigo500  = "#3f51b5"
colIndigo600  = "#3949ab"
colIndigo700  = "#303f9f"
colIndigo800  = "#283593"
colIndigo900  = "#1a237e"
colIndigoA100 = "#8c9eff"
colIndigoA200 = "#536dfe"
colIndigoA400 = "#3d5afe"
colIndigoA700 = "#304ffe"

colBlue50   = "#e3f2fd"
colBlue100  = "#bbdefb"
colBlue200  = "#90caf9"
colBlue300  = "#64b5f6"
colBlue400  = "#42a5f5"
colBlue500  = "#2196f3"
colBlue600  = "#1e88e5"
colBlue700  = "#1976d2"
colBlue800  = "#1565c0"
colBlue900  = "#0d47a1"
colBlueA100 = "#82b1ff"
colBlueA200 = "#448aff"
colBlueA400 = "#2979ff"
colBlueA700 = "#2962ff"

colYellow50   = "#fffde7"
colYellow100  = "#fff9c4"
colYellow200  = "#fff59d"
colYellow300  = "#fff176"
colYellow400  = "#ffee58"
colYellow500  = "#ffeb3b"
colYellow600  = "#fdd835"
colYellow700  = "#fbc02d"
colYellow800  = "#f9a825"
colYellow900  = "#f57f17"
colYellowA100 = "#ffff8d"
colYellowA200 = "#ffff00"
colYellowA400 = "#ffea00"
colYellowA700 = "#ffd600"

colTeal50   = "#e0f2f1"
colTeal100  = "#b2dfdb"
colTeal200  = "#80cbc4"
colTeal300  = "#4db6ac"
colTeal400  = "#26a69a"
colTeal500  = "#009688"
colTeal600  = "#00897b"
colTeal700  = "#00796b"
colTeal800  = "#00695c"
colTeal900  = "#004d40"
colTealA100 = "#a7ffeb"
colTealA200 = "#64ffda"
colTealA400 = "#1de9b6"
colTealA700 = "#00bfa5"

colGreen50   = "#e8f5e9"
colGreen100  = "#c8e6c9"
colGreen200  = "#a5d6a7"
colGreen300  = "#81c784"
colGreen400  = "#66bb6a"
colGreen500  = "#4caf50"
colGreen600  = "#43a047"
colGreen700  = "#388e3c"
colGreen800  = "#2e7d32"
colGreen900  = "#1b5e20"
colGreenA100 = "#b9f6ca"
colGreenA200 = "#69f0ae"
colGreenA400 = "#00e676"
colGreenA700 = "#00c853"
colGreenD40  = "#65A290"
colGreenD50  = "#4C9882"
colGreenD100 = "#008787"

colOrange50   = "#fff3e0"
colOrange100  = "#ffe0b2"
colOrange200  = "#ffcc80"
colOrange300  = "#ffb74d"
colOrange400  = "#ffa726"
colOrange500  = "#ff9800"
colOrange600  = "#fb8c00"
colOrange700  = "#f57c00"
colOrange800  = "#ef6c00"
colOrange900  = "#e65100"
colOrangeA100 = "#ffd180"
colOrangeA200 = "#ffab40"
colOrangeA400 = "#ff9100"
colOrangeA700 = "#ff6d00"

colDeepOrange50   = "#fbe9e7"
colDeepOrange100  = "#ffccbc"
colDeepOrange200  = "#ffab91"
colDeepOrange300  = "#ff8a65"
colDeepOrange400  = "#ff7043"
colDeepOrange500  = "#ff5722"
colDeepOrange600  = "#f4511e"
colDeepOrange700  = "#e64a19"
colDeepOrange800  = "#d84315"
colDeepOrange900  = "#bf360c"
colDeepOrangeA100 = "#ff9e80"
colDeepOrangeA200 = "#ff6e40"
colDeepOrangeA400 = "#ff3d00"
colDeepOrangeA700 = "#dd2c00"
-- }}} -------------------------
colTest              = "#5FD75F"
colPinkM50           = "#F4DAEE"
colPinkM100          = "#E3C5DC"
colPinkM200          = "#D3AACB"
colPinkM500          = "#AA759F"

colPurple200         = "#949CC9"
colPurple800         = "#292D3E"
colPurple900         = "#212432"


colBlueM50           = "#70A2CB"
colBlueM200          = "#516A95"

colBurgM50           = "#989DC0"
colBurgM100          = "#848DAF"
colBurgM200          = "#4E5579"
colBurgM500          = "#292D3E"
colBurgM900          = "#212432"

myFont               = "AnonymousPro-12:style=Regular"
myFgColor            = "#B7DDD2"
myBgColor            = "#193441"
myFocusedFGColor     = "#E5F8DF"
myFocusedBGColor     = "#65A290"

dmenuOptions =   " -fn " ++ quotify myFont
              ++ " -nf " ++ quotify myFgColor
              ++ " -nb " ++ quotify myBgColor
              ++ " -sf " ++ quotify myFocusedFGColor
              ++ " -sb " ++ quotify myFocusedBGColor
                where quotify = (\x -> "'" ++ x ++ "'")

-- xmobarOptions = "-f 'xft:Anonymous Pro:style=Regular:size=11.5' -B '#193441' -F grey -A '255' -i '.config/xmonad/icons/xpm' --screen "

--------------------------------------------------------------------------------------------
-- HARDCODED LOGGERS (you may need to amend them so that they work on your computer)      --
--------------------------------------------------------------------------------------------

-- Concat two Loggers
(++!) :: Logger -> Logger -> Logger
l1 ++! l2 = (liftA2 . liftA2) (++) l1 l2

-- Label
labelL :: String -> Logger
labelL = return . return

-- Init version for Logger
initL :: Logger -> Logger
initL = (fmap . fmap) initNotNull

-- Concat a list of loggers
concatL :: [Logger] -> Logger
concatL [] = return $ return ""
concatL (x:xs) = x ++! concatL xs

-- Concat a list of loggers with spaces between them
concatWithSpaceL :: [Logger] -> Logger
concatWithSpaceL [] = return $ return ""
concatWithSpaceL (x:xs) = x ++! (labelL " ") ++! concatWithSpaceL xs

initNotNull :: String -> String
initNotNull [] = "0\n"
initNotNull xs = init xs

tailNotNull :: [String] -> [String]
tailNotNull [] = ["0\n"]
tailNotNull xs = tail xs

-- Convert the content of a file into a Logger
fileToLogger :: (String -> String) -> String -> FilePath -> Logger
fileToLogger f e p = do
  let readWithE f1 e1 p1 = E.catch (do
      contents <- readFile p1
      return $ f1 (initNotNull contents) ) ((\_ -> return e1) :: E.SomeException -> IO String)
  str <- liftIO $ readWithE f e p
  return $ return str


-- Battery percent
batPercent :: Int -> String -> Logger
batPercent v c = fileToLogger format "N/A" "/sys/class/power_supply/BAT0/capacity" where
  format x = if ((read x::Int) <= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

-- Battery status
batStatus :: Logger
batStatus = fileToLogger (\x -> x) "AC Conection" "/sys/class/power_supply/BAT0/status"

-- Brightness percenn
brightPerc :: Int -> Logger
brightPerc p = fileToLogger format "0" "/sys/class/backlight/acpi_video0/actual_brightness" where
  format x = (show $ div ((read x::Int) * 100) p) ++ "%"

-- wifi signal
wifiSignal :: Logger
wifiSignal = fileToLogger format "N/A" "/proc/net/wireless" where
  format x = if (length $ lines x) >= 3 then (initNotNull ((words ((lines x) !! 2)) !! 2) ++ "%") else "Off"

-- CPU temperature
cpuTemp :: Int -> Int -> String -> Logger
cpuTemp n v c = initL $ concatWithSpaceL $ map (fileToLogger divc "0") pathtemps where
  pathtemps = map (++"/thermal_zone/temp") $ map ("/sys/bus/acpi/devices/LNXTHERM:0"++) $ take n $ map show [0..]
  divc x = crit $ div (read x::Int) 1000
  crit x = if (x >= v) then "^fg(" ++ c ++ ")" ++ show x ++ "°^fg()" else (show x ++ "°")

-- Memory usage
memUsage :: [(String -> String)] -> Logger
memUsage xs = initL $ concatWithSpaceL $ map funct xs where
  funct x = fileToLogger x "N/A" "/proc/meminfo"

--_memUsed x = (_memValues x !! 0) - ((_memValues x !! 2) + (_memValues x !! 3) + (_memValues x !! 1)) --old format
_memUsed x = (_memValues x !! 0) - (_memValues x !! 2)  --new format
_memPerc x = div (_memUsed x * 100) (_memValues x !! 0)
_memValues x = map (getValues x) $ take 4 [0..] where
  getValues x n = read (words (lines x !! n) !! 1)::Int

freeBMemUsage x = (show $ _memValues x !! 1) ++ "B"
freeMBMemUsage x = (show $ div (_memValues x !! 1) 1024) ++ "MB"
totBMemUsage = (++"B") . show . _memUsed
totMBMemUsage = (++"MB") . show . (`div` 1024) . _memUsed
percMemUsage = (++"%") . show . _memPerc

-- CPU Usage: this is an ugly hack that depends on "haskell-cpu-usage" app (See my github repo to get the app)
cpuUsage :: String -> Int -> String -> Logger
cpuUsage path v c = fileToLogger format "0" path where
  format x = if (null x) then "N/A" else initNotNull $ concat $ map (++" ") $ map crit $ tailNotNull $ words $ x
  crit x = if ((read x::Int) >= v) then "^fg(" ++ c ++ ")" ++ x ++ "%^fg()" else (x ++ "%")

-- Uptime
uptime :: Logger
uptime = fileToLogger format "0" "/proc/uptime" where
  u x  = read (takeWhile (/='.') x)::Integer
  h x  = div (u x) 3600
  hr x = mod (u x) 3600
  m x  = div (hr x) 60
  s x  = mod (hr x) 60
  format x = (show $ h x) ++ "h " ++ (show $ m x) ++ "m " ++ (show $ s x) ++ "s"

-- Gets screen count
screenCount :: X Int  
screenCount = withDisplay (io.fmap length.getScreenInfo)

-- Gets the current resolution given a display and a screen
getScreenRes :: String -> Int -> IO Res
getScreenRes d n = do
  dpy <- openDisplay d
  r <- liftIO $ getScreenInfo dpy
  closeDisplay dpy
  return $ Res
    { xRes = fromIntegral $ rect_width $ r !! n
    , yRes = fromIntegral $ rect_height $ r !! n
    }

-- Screen Resolution
data Res = Res
  { xRes :: Int
  , yRes :: Int
  }

-- Resolution logger
screenRes :: String -> Int -> Logger
screenRes d n = do
  res <- liftIO $ getScreenRes d n
  return $ return $ (show $ xRes res) ++ "x" ++ (show $ yRes res)


------------------------------------------------------------------------
-- Keyboard bindings
------------------------------------------------------------------------

altm               = mod1Mask

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
      [
      --Applications management
        ((0,                 0x1008ff13),  spawn "pamixer -i 5")             -- XF86AudioRaiseVolume
      , ((0,                 0x1008ff11),  spawn "pamixer -d 5")             -- XF86AudioLowerVolume
      , ((0,                 0x1008ff12),  spawn "pamixer -t")               -- XF86AudioMute
      , ((0,                 0x1008ff14),  spawn "mpc toggle")               -- XF86Play
      , ((0,                 0x1008ff15),  spawn "mpc stop")                 -- XF86AudioStop
      , ((0,                 0x1008ff16),  spawn "mpc prev")                 -- XF86AudioPrev
      , ((0,                 0x1008ff17),  spawn "mpc next")                 -- XF86AudioNext
--      , ((0,              0x1008ff30),  spawn "subl3")                     -- XF86Favorites
      , ((0,                 0x1008ff18),  spawnHere "firefox")                  -- XF86HomePage
      , ((0,                 0x1008ff19),  spawnHere "claws-mail")               -- XF86Mail
--      , ((0,                 0x1008ff33),  spawn "pcmanfm")                  -- XF86MyComputer
--      , ((0,                 0x1008ff5d),  spawn "pcmanfm")                  -- XF86Explorer
      , ((0,                 0x1008ff1d),  spawn "galculator")               -- XF86Calculator
--      , ((0,              0x1008ff1b),  nSA "HTOP")                        -- XF86Search
--      , ((0,              0x1008ff77),  nSA "NCMPCPP")                     -- XF86Save
--      , ((0,              0x1008ff46),  spawn "XMR")                       -- XF86Launch6
--      , ((0,              0x1008ff2f),  mySreenLock)                       -- XF86Sleep
--      , ((0,              0x1008ff56),  nSA "ExitMenu")                    -- XF86Close
--      , ((0,              0x1008ff73),  spawn "systemctl reboot")          -- XF86Reload
--      , ((0,                  0xff69),  spawn "systemctl poweroff")        -- Cancel
--      , ((0,                     0xff14), mySreenLock)                       -- Scroll_Lock
--      , ((0,                  0xff67),  spawn "gmrun")                     -- Menu
--      , ((0,                  0xffc9),  myQST)                             -- F12
      , ((0,                    0xff61), spawn "~/.local/bin/screenshot")    -- Print
      , ((0    .|. shiftMask,   0xff61), spawn "scrot -u -q 100 -e 'mv $f ~/Data/Pictures/Screenshots/ 2>/dev/null'") -- Shift+Print

--      , ((0,                  xK_Home  ), myAppMenu)                         -- Home
      , ((0,                  xK_Pause ), spawn "mpc toggle")
      , ((altm,                  0xff61), spawn "~/.local/bin/screenshot-area") -- Alt+Print

--      , ((modm,               xK_0     ), mySreenLock)

      , ((modm,               xK_Up    ), spawn "mpc play")
      , ((modm,               xK_Down  ), spawn "mpc stop")
      , ((modm,               xK_Left  ), spawn ("~/.local/bin/XMMPCprev"))
      , ((modm,               xK_Right ), spawn ("~/.local/bin/XMMPCnext"))
  
      -- TEst alert dzen
      , ((modm .|. shiftMask, xK_m), (externalCommandInPopUp "mpc" ["current"]))

      -- launch a terminal
      , ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

      -- launch dmenu
      , ((modm,               xK_p     ), spawn "dmenu_recency")

      -- launch gmrun
      , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

      -- close focused window
      , ((modm,               xK_x     ), kill)

      -- Rotate through the available layout algorithms
      , ((modm,               xK_space ), sendMessage NextLayout)

      --  Reset the layouts on the current workspace to default
      , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

      -- Resize viewed windows to the correct size
      , ((modm,               xK_n     ), refresh)

      -- Move focus to the next window
      , ((modm,               xK_Tab   ), windows W.focusDown)

      -- Move focus to the next window
      , ((modm,               xK_j     ), windows W.focusDown)

      -- Move focus to the previous window
      , ((modm,               xK_k     ), windows W.focusUp  )

      -- Move focus to the master window
      , ((modm,               xK_m     ), windows W.focusMaster  )

      -- Swap the focused window and the master window
      , ((modm,               xK_Return), windows W.swapMaster)

      -- Swap the focused window with the next window
      , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

      -- Swap the focused window with the previous window
      , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

      -- Shrink the master area
      , ((modm,               xK_h     ), sendMessage Shrink)

      -- Expand the master area
      , ((modm,               xK_l     ), sendMessage Expand)

      -- Push window back into tiling
      , ((modm,               xK_t     ), withFocused $ windows . W.sink)

      -- Increment the number of windows in the master area
      , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

      -- Deincrement the number of windows in the master area
      , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

      -- Toggle the status bar gap
      -- Use this binding with avoidStruts from Hooks.ManageDocks.
      -- See also the statusBar function from Hooks.DynamicLog.
      --
      --, ((modm              , xK_b     ), sendMessage ToggleStruts)

      -- Quit xmonad
      , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

      -- Restart xmonad
      , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

      -- Run xmessage with a summary of the default keybindings (useful for beginners)
--      , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
      ]
      ++

      --
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      --
      [((m .|. modm, k), windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
          , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
      ++

      --
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      --
      [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
