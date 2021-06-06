module XMonad.Config.Workspaces
    ( WSLevel(..)
    , Direction1D(..)
    , Direction2D(..)
    , myWorkspaces
    , workspaceTree
    , wsSep
    , wsLink
    , wsJoinNames
    , wsGetWsInCurrentLevel
    , wsGetCurrentLevel
    , wsGetCurrentLevelIcons
    , wsGetSide
    , wsGetParent
    , wsGetChild
    , wsGetNext
    , wsGetPrev
    , wsMove
    , wsMoveSide
    , wsMoveToParent
    , wsMoveToChild
    , wsMoveNext
    , wsMovePrev
    , wsShiftToTarget
    , wsShift
    , wsShiftSide
    , wsShiftToParent
    , wsShiftToChild
    , wsShiftToNext
    , wsShiftToPrev
    )
where

import XMonad
    ( X
    , ExtensionClass(..)
    , StateExtension(..)
    , Typeable
    , WorkspaceId
    , windows
    , withWindowSet
    )
import XMonad.Util.Types (Direction1D(..), Direction2D(..))

import Control.Monad (unless)
import Data.List (find, findIndex, intercalate)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Tree (Forest, Tree(..), flatten)

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

-- | Character used to separate names
wsSep :: Char
wsSep = '/'

-- | Character used to indicate linked workspaces
wsLink :: Char
wsLink = '~'

-- | Overall tree of workspaces
workspaceTree :: Forest WorkspaceItem
workspaceTree =
    [ Node (WSItem "web" "\xf269") []
    , Node
        (WSItem "dev" "\xe795")
        [ Node (WSLink ["web"])             []
        , Node (WSItem "rust" "\xe7a8")     []
        , Node (WSItem "haskell" "\xe777")  []
        , Node (WSItem "r" "\xf437")        []
        , Node (WSItem "markdown" "\xe73e") []
        ]
    , Node
        (WSItem "game" "\xf11b")
        [ Node (WSLink ["web"])           []
        , Node (WSItem "engine" "\xfbad") []
        , Node (WSItem "paint" "\xf1fc")  []
        , Node (WSItem "dev" "\xe795")    []
        ]
    , Node (WSItem "music" "\xfc58") []
    , Node
        (WSItem "study" "\xf8e9")
        [ Node (WSLink ["web"])                []
        , Node (WSLink ["study"])              []
        , Node (WSItem "placeholder" "\xf6d7") []
        ]
    , Node (WSItem "chat" "\xf868") []
    , Node (WSItem "etc" "\xf6d7")  []
    ]

-- | Either a workspace or a link to another workspace
data WorkspaceItem
    = WSLink [String]
    | WSItem { wsName :: String, wsIcon :: String }

-- | Extension state type to keep track of the ws hierarchy
newtype WSLevel =
    WSLevel [String]
    deriving (Typeable, Read, Show)

instance ExtensionClass WSLevel where
    initialValue  = WSLevel []
    extensionType = PersistentExtension

-- | Fetch the current level in the workspace tree
wsGetCurrentLevel :: X [String]
wsGetCurrentLevel = do
    WSLevel lvl <- XS.get
    return lvl

-- | Put the given value to the current level in the workspace tree
wsPutCurrentLevel :: [String] -> X ()
wsPutCurrentLevel newLvl = XS.put $ WSLevel newLvl

-- | Parsed workspace tree
data WSInfo
    = WSNameInfo { wsInfoPath :: [String], wsInfoIcon :: String }
    | WSLinkInfo { wsInfoPath :: [String], wsInfoTarget :: [String] }
    deriving (Typeable, Read, Show)

-- | Parsed workspace tree
wsInfo :: Forest WSInfo
wsInfo = map getInfoFromTree workspaceTree

-- | Virtual node with the workspace tree aggregated
wsInfoTop :: Tree WSInfo
wsInfoTop = Node (WSNameInfo [] "") wsInfo

-- | Workspace list generated from the workspace tree
myWorkspaces :: [WorkspaceId]
myWorkspaces = intoWorkspaceIds workspaceTree

-- | Join the name segments to get the full path
wsJoinNames :: [String] -> String
wsJoinNames = intercalate [wsSep]

-- | Separate the workspace name into name segments
wsSepName :: String -> [String]
wsSepName name = case break (== wsSep) name of
    (a , '/' : b) -> a : wsSepName b
    (a , ""     ) -> [a]
    (_a, _b     ) -> head [] -- impossible

-- | Parse the workspace tree into a more usable form
getInfoFromTree :: Tree WorkspaceItem -> Tree WSInfo
getInfoFromTree = infoFromTreeRecursive []
  where
    infoFromTreeRecursive :: [String] -> Tree WorkspaceItem -> Tree WSInfo
    infoFromTreeRecursive parents (Node (WSItem name icon) children) = Node
        (WSNameInfo fullName icon)
        (map (infoFromTreeRecursive fullName) children)
        where fullName = parents ++ [name]
    infoFromTreeRecursive parents (Node (WSLink linkTo) _) =
        Node (WSLinkInfo (parents ++ [last linkTo]) linkTo) []

-- | Convert the workspace tree into a list of workspace ids
intoWorkspaceIds :: Forest WorkspaceItem -> [WorkspaceId]
intoWorkspaceIds = concatMap (flatten . convert . getInfoFromTree) . filterOut
  where
    convert :: Tree WSInfo -> Tree String
    convert (Node (WSNameInfo segs _) children) =
        Node (intercalate [wsSep] segs) (map convert children)
    convert (Node (WSLinkInfo segs target) _) = Node
        (intercalate [wsSep] segs ++ [wsLink] ++ intercalate [wsSep] target)
        []
    filterOut :: Forest WorkspaceItem -> Forest WorkspaceItem
    filterOut = map filterOut' . filter filterPred
    filterOut' :: Tree WorkspaceItem -> Tree WorkspaceItem
    filterOut' (Node ws children) = Node ws $ filter filterPred children
    filterPred :: Tree WorkspaceItem -> Bool
    filterPred (Node (WSItem _ _) _) = True
    filterPred _                     = False

-- | Get the node with the given path from the given forest
wsGetNodeAt :: Forest WSInfo -> [String] -> Maybe (Tree WSInfo)
wsGetNodeAt _  []            = Just wsInfoTop
wsGetNodeAt ls [name       ] = find (\n -> name == (last . wsPathFromNode) n) ls
wsGetNodeAt ls (name : rest) = do
    Node _ ns <- wsGetNodeAt ls [name]
    wsGetNodeAt ns rest

-- | Fall back to the top level virtual node if not found
wsGetNodeAtFallback :: [String] -> Tree WSInfo
wsGetNodeAtFallback path = fromMaybe wsInfoTop $ wsGetNodeAt wsInfo path

-- | Get the path of the given node
wsPathFromNode :: Tree WSInfo -> [String]
wsPathFromNode (Node (WSNameInfo p _) _) = p
wsPathFromNode (Node (WSLinkInfo p _) _) = p

-- | Get the real path of the given node, resolving the link
wsResolvePathOfNode :: Tree WSInfo -> [String]
wsResolvePathOfNode (Node (WSNameInfo p _) _) = p
wsResolvePathOfNode (Node (WSLinkInfo _ l) _) =
    wsResolvePathOfNode . fromJust . wsGetNodeAt wsInfo $ l

-- | Get the icon of the given node, resolving the link
wsIconFromNode :: Tree WSInfo -> String
wsIconFromNode (Node (WSNameInfo _ i) _) = i
wsIconFromNode (Node (WSLinkInfo _ l) _) =
    wsIconFromNode . fromJust . wsGetNodeAt wsInfo $ l

-- | Get the children of the given node
wsGetChildrenOfNode :: Tree WSInfo -> Forest WSInfo
wsGetChildrenOfNode (Node _ children) = children

-- | Shift the focused window to the given workspace
wsShiftToTarget :: X WorkspaceId -> X ()
wsShiftToTarget target = do
    ws <- target
    unless (null ws) $ windows . W.shift $ ws

-- | Get the name of the parent workspace
wsGetParent :: X WorkspaceId
wsGetParent = wsJoinNames <$> wsGetCurrentLevel

-- | Focus to the parent workspace
wsMoveToParent :: X ()
wsMoveToParent = do
    ws <- wsGetParent
    unless (null ws) $ do
        wsPutCurrentLevel . init . wsSepName $ ws
        windows . W.view $ ws

-- | Shift the focused window to the parent workspace
wsShiftToParent :: X ()
wsShiftToParent = wsShiftToTarget wsGetParent

-- | Get the name of the first child workspace
wsGetChild :: X WorkspaceId
wsGetChild = do
    curr     <- withWindowSet $ return . last . wsSepName . W.currentTag
    curLevel <- wsGetCurrentLevel
    let (Node nodeInfo children) = wsGetNodeAtFallback $ curLevel ++ [curr]
    case nodeInfo of
        WSLinkInfo _ _ -> return ""
        WSNameInfo _ _ -> if null children
            then return ""
            else return . wsJoinNames . wsResolvePathOfNode . head $ children

-- | Focus on the first child workspace
wsMoveToChild :: X ()
wsMoveToChild = do
    ws <- wsGetChild
    unless (null ws) $ do
        wsPutCurrentLevel . wsSepName =<< withWindowSet (return . W.currentTag)
        windows . W.view $ ws

-- | Shift the focused window to the first child workspace
wsShiftToChild :: X ()
wsShiftToChild = wsShiftToTarget wsGetChild

-- | Get the name of the sibling workspace to the given direction
wsGetSide :: Direction1D -> X WorkspaceId
wsGetSide dir = do
    node <- wsGetNodeAt wsInfo <$> wsGetCurrentLevel
    if isJust node
        then do
            curName <- withWindowSet $ return . last . wsSepName . W.currentTag
            return . wsJoinNames . wsResolvePathOfNode $ wrapMove
                (wsGetChildrenOfNode . fromJust $ node)
                curName
                dir
        else return ""
  where
    wrapMove :: Forest WSInfo -> String -> Direction1D -> Tree WSInfo
    wrapMove list name dir' = list !! idx'
      where
        idx = fromJust . flip findIndex list $ \n ->
            (== name) (last . wsPathFromNode $ n)
        idx' = case dir' of
            Next -> (idx + 1) `mod` length list
            Prev -> (idx - 1) `mod` length list

-- | Get the name of the next sibling workspace
wsGetNext :: X WorkspaceId
wsGetNext = wsGetSide Next

-- | Get the name of the previous sibling workspace
wsGetPrev :: X WorkspaceId
wsGetPrev = wsGetSide Prev

-- | Focus on the sibling workspace to the given direction
wsMoveSide :: Direction1D -> X ()
wsMoveSide dir = do
    ws <- wsGetSide dir
    unless (null ws) $ windows (W.view ws)

-- | Focus on the next sibling workspace
wsMoveNext :: X ()
wsMoveNext = wsMoveSide Next

-- | Focus on the previous sibling workspace
wsMovePrev :: X ()
wsMovePrev = wsMoveSide Prev

-- | Shift the focused window to the sibling workspace to the given direction
wsShiftSide :: Direction1D -> X ()
wsShiftSide dir = wsShiftToTarget $ wsGetSide dir

-- | Shift the focused window to the next sibling workspace
wsShiftToNext :: X ()
wsShiftToNext = wsShiftSide Next

-- | Shift the focused window to the previous sibling workspace
wsShiftToPrev :: X ()
wsShiftToPrev = wsShiftSide Prev

-- | Move to the workspace to the given direction
wsMove :: Direction2D -> X ()
wsMove U = wsMoveToParent
wsMove D = wsMoveToChild
wsMove L = wsMovePrev
wsMove R = wsMoveNext

-- | Shift the focused window to the workspace to the given direction
wsShift :: Direction2D -> X ()
wsShift U = wsShiftToParent
wsShift D = wsShiftToChild
wsShift L = wsShiftToPrev
wsShift R = wsShiftToNext

-- | Return the list of workspaces in the current hierarchy with the icons
wsGetWsInCurrentLevel :: X [(String, String)]
wsGetWsInCurrentLevel = do
    path <- wsGetCurrentLevel
    let c :: Forest WSInfo
        c = wsGetChildrenOfNode . wsGetNodeAtFallback $ path
    return
        $   zip
        <$> map (wsJoinNames . wsResolvePathOfNode)
        <*> map wsIconFromNode
        $   c

wsGetCurrentLevelIcons :: X [String]
wsGetCurrentLevelIcons = do
    path <- wsGetCurrentLevel
    let segments = take <$> [1 .. length path] <*> [path]
        icons    = wsIconFromNode . fromJust . wsGetNodeAt wsInfo <$> segments
    return icons

-- vim:ts=4:sw=4:sts=4:et:
