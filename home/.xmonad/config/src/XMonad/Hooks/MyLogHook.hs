module XMonad.Hooks.MyLogHook
    ( StrFormatter
    , MyWsLogHookConfig(..)
    , myWsLogHook
    )
where

import XMonad
    ( Default(..)
    , LayoutClass(description)
    , Window
    , WindowSet
    , WindowSpace
    , X
    , gets
    , io
    , windowset
    )
import XMonad.Config.Workspaces (wsGetCurrentLevelIcons, wsGetWsInCurrentLevel)
import XMonad.Hooks.UrgencyHook (readUrgents)

import Data.List (find, intercalate)
import Data.Maybe (fromJust, isJust)

import qualified XMonad.StackSet as W

-- | The workspace name formatter
type StrFormatter = String -> String

-- | Configuration for the workspace tree log hook
data MyWsLogHookConfig = WsLogHookConfig
    { wsCurrentFmt      :: StrFormatter
    , wsVisibleFmt      :: StrFormatter
    , wsVisibleNoWinFmt :: StrFormatter
    , wsHiddenFmt       :: StrFormatter
    , wsHiddenNoWinFmt  :: StrFormatter
    , wsUrgentFmt       :: StrFormatter
    , wsListFmt         :: StrFormatter
    , wsListItemFmt     :: Int -> StrFormatter
    , wsIconFmt         :: StrFormatter
    , wsShowNumber      :: Bool
    , wsNumberFmt       :: StrFormatter
    , wsLogOutFile      :: String
    , wsLogSep          :: String
    , wsShowLevel       :: Bool
    , wsLevelFmt        :: StrFormatter
    , wsShowLayout      :: Bool
    , wsLayoutFmt       :: StrFormatter
    , wsShowWsName      :: Bool
    , wsWsNameFmt       :: StrFormatter
    }

instance Default MyWsLogHookConfig where
    def = WsLogHookConfig
        { wsCurrentFmt      = id
        , wsVisibleFmt      = id
        , wsVisibleNoWinFmt = id
        , wsHiddenFmt       = id
        , wsHiddenNoWinFmt  = id
        , wsUrgentFmt       = id
        , wsListFmt         = id
        , wsListItemFmt     = const id
        , wsIconFmt         = id
        , wsShowNumber      = False
        , wsNumberFmt       = id
        , wsLogOutFile      = "/tmp/.xmonad-ws-log"
        , wsLogSep          = " - "
        , wsShowLevel       = False
        , wsLevelFmt        = id
        , wsShowLayout      = False
        , wsLayoutFmt       = id
        , wsShowWsName      = False
        , wsWsNameFmt       = id
        }


-- | The status of the workspace for formatting
data WsStatus
    = WsCurrent
    | WsVisible
    | WsVisibleNoWin
    | WsHidden
    | WsHiddenNoWin
    | WsUrgent
    deriving (Eq, Show, Read)

-- | LogHook for logging the workspace list
myWsLogHook :: MyWsLogHookConfig -> X ()
myWsLogHook config =
    myWsLogMakeMsg config >>= io . myWsLogDoOutput (wsLogOutFile config)

-- | Append the string to the pipe file
myWsLogDoOutput :: String -> String -> IO ()
myWsLogDoOutput file = appendFile file . (++ "\n")

-- | Make the workspace level string
myWsLogLevel :: MyWsLogHookConfig -> X String
myWsLogLevel config =
    wsLevelFmt config . addRoot . intercalate "/" <$> wsGetCurrentLevelIcons
  where
    addRoot :: String -> String
    addRoot [] = "/"
    addRoot s  = s

-- | Make the layout string
myWsLogLayout :: MyWsLogHookConfig -> WindowSet -> X String
myWsLogLayout config =
    return
        . wsLayoutFmt config
        . description
        . W.layout
        . W.workspace
        . W.current

-- | Return the name of the current workspace
myWsWsName :: MyWsLogHookConfig -> WindowSet -> X String
myWsWsName config =
    return . wsWsNameFmt config . W.tag . W.workspace . W.current

-- | Make the log string
myWsLogMakeMsg :: MyWsLogHookConfig -> X String
myWsLogMakeMsg config = do
    ws      <- wsGetWsInCurrentLevel
    winset  <- gets windowset
    urgents <- readUrgents
    let wsList :: String
        wsList =
            wsListFmt config
                . unwords
                . myWsLogDoFormat config
                . parseWsStatus winset urgents
                $ ws
        wsStrings :: [Maybe (X String)]
        wsStrings =
            [ if wsShowLevel config then Just $ myWsLogLevel config else Nothing
            , Just . return $ wsList
            , if wsShowWsName config
                then Just $ myWsWsName config winset
                else Nothing
            , if wsShowLayout config
                then Just $ myWsLogLayout config winset
                else Nothing
            ]
    strings <- mapM fromJust . filter isJust $ wsStrings
    return $ intercalate (wsLogSep config) strings
  where
    parseWsStatus
        :: WindowSet -> [Window] -> [(String, String)] -> [(String, WsStatus)]
    parseWsStatus winset urgents =
        map $ \(n, i) -> (i, getWsStatus winset urgents n)
    getWsStatus :: WindowSet -> [Window] -> String -> WsStatus
    getWsStatus winset urgents ws = status
      where
        ws' :: WindowSpace
        ws' = fromJust . find ((== ws) . W.tag) $ W.workspaces winset
        visible :: [String]
        visible = map (W.tag . W.workspace) . W.visible $ winset
        status :: WsStatus
        status
            | any (\w -> (== Just ws) (W.findTag w winset)) urgents = WsUrgent
            | ws == W.currentTag winset = WsCurrent
            | ws `elem` visible && isJust (W.stack ws') = WsVisible
            | ws `elem` visible         = WsVisibleNoWin
            | isJust (W.stack ws')      = WsHidden
            | otherwise                 = WsHiddenNoWin

-- | Format the workspace names according to the status for each
myWsLogDoFormat :: MyWsLogHookConfig -> [(String, WsStatus)] -> [String]
myWsLogDoFormat config ws =
    let
        (names, statuses) = unzip ws
        names'            = getNamesToShow . map (wsIconFmt config) $ names
        formats           = map (($ config) . fmt) statuses
    in zipWith (wsListItemFmt config) [(0 :: Int) ..]
        . zipWith ($) formats
        $ names'
  where
    getNamesToShow :: [String] -> [String]
    getNamesToShow
        | wsShowNumber config
        = map (\(n, w) -> (++ w) . wsNumberFmt config $ show n ++ ":")
            . zip [(1 :: Int) ..]
        | otherwise
        = id
    fmt :: WsStatus -> MyWsLogHookConfig -> StrFormatter
    fmt WsCurrent      = wsCurrentFmt
    fmt WsVisible      = wsVisibleFmt
    fmt WsVisibleNoWin = wsVisibleNoWinFmt
    fmt WsHidden       = wsHiddenFmt
    fmt WsHiddenNoWin  = wsHiddenNoWinFmt
    fmt WsUrgent       = wsUrgentFmt

-- vim:ts=4:sw=4:sts=4:et:
