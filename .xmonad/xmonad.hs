import Graphics.X11.ExtraTypes.XF86
import System.Exit
import System.IO
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    xmproc <- spawnPipe "xmobar $HOME/.xmobarrc"
    xmonad $ defaultConfig
        { terminal = "xterm"
        , borderWidth = 0
        , modMask = mod4Mask
        , focusFollowsMouse = False
        , numlockMask = mod2Mask
        , keys = myKeys
        , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "grey" "" . shorten 100
            }
        -- , workspaces = myWorkspaces
        }

-- myWorkspaces = ["web", "mail", "im"] ++ map show [4..7] ++ ["windows", "_"]

secondMask = controlMask

myManageHook = composeAll
    [ className =? "Gimp"                                   --> doFloat
    , className =? "Vncviewer"                              --> doFloat
    , className =? "Gcalctool"                              --> doFloat
    , stringProperty "WM_WINDOW_ROLE" =? "buddy_list"       --> doFloat
    , stringProperty "WM_ICON_NAME" =? "Pidgin"             --> doFloat
    , stringProperty "WM_ICON_NAME" =? "Accounts"           --> doCenterFloat
    , stringProperty "WM_ICON_NAME" =? "Downloads"          --> doCenterFloat
    , stringProperty "WM_ICON_NAME" =? "Clear Private Data" --> doCenterFloat
    , className =? "Wfica"                                  --> doCenterFloat
    , className =? "Toplevel"                               --> doCenterFloat
    ]

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. secondMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_r     ), shellPrompt defaultXPConfig)
    , ((modMask .|. secondMask, xK_r     ), runOrRaisePrompt defaultXPConfig)
    , ((modMask,               xK_grave ), xmonadPrompt defaultXPConfig)
    , ((modMask,            xK_BackSpace), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. secondMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. secondMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_n     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_p     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_semicolon), windows W.focusMaster  ) -- %! Move focus to the master window
    , ((modMask,               xK_apostrophe), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. secondMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. secondMask, xK_n     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. secondMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window
    , ((modMask .|. secondMask, xK_p     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_bracketleft ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_bracketright), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_slash ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_equal ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_minus ), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    -- , ((modMask              , xK_backslash), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

    -- lookup
    , ((modMask,               xK_h     ), manPrompt defaultXPConfig)
    , ((modMask,               xK_u     ), spawn "gnome-dictionary")

    -- application launchers
    , ((modMask,               xK_x), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_l), spawn "xscreensaver-command -lock")
    , ((modMask .|. secondMask, xK_r), runOrRaisePrompt defaultXPConfig)
    , ((modMask,               xK_w), spawn "firefox")
    , ((modMask .|. secondMask, xK_w), runOrRaise "firefox" (className =? "Firefox"))
    , ((modMask,               xK_c), spawn "citrix")
    , ((modMask .|. secondMask, xK_c), runOrRaise "citrix" (className =? "Wfica"))
    , ((modMask,               xK_e), spawn "thunar")
    , ((modMask .|. secondMask, xK_e), runOrRaise "thunar" (className =? "Thunar"))
    , ((modMask,               xK_g), spawn "gimp")
    , ((modMask .|. secondMask, xK_g), runOrRaise "gimp" (className =? "Gimp"))

    -- multimedia
    , ((0,                     xK_Print ), spawn "import /tmp/screenshot.jpg && display /tmp/screenshot.jpg")
    , ((0,                     xF86XK_Calculater), spawn "gnome-calculator")

    -- cycling workspaces
   , ((modMask,                xK_Right ), nextWS)
   , ((modMask,                xK_Left  ), prevWS)
   , ((modMask,                xK_f      ), nextWS)
   , ((modMask,                xK_b      ), prevWS)
   , ((modMask .|. secondMask,  xK_Right ), shiftToNext >> nextWS)
   , ((modMask .|. secondMask,  xK_Left  ), shiftToPrev >> prevWS)
   , ((modMask .|. secondMask,  xK_b     ), shiftToNext >> nextWS)
   , ((modMask .|. secondMask,  xK_f     ), shiftToPrev >> prevWS)

    -- quit, or restart
    -- No manslaughter, please.
    -- , ((modMask .|. secondMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, secondMask)]]
    ++
    -- mod-{a,s,d} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{a,s,d} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, secondMask)]]
