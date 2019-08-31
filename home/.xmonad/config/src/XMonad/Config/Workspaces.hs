module XMonad.Config.Workspaces
    ( WSLevel(..)
    , Direction1D(..)
    , myWorkspaces
    , workspaceTree
    , wsSep
    , wsLink
    , wsJoinNames
    , wsFilterWsLevel
    , wsMoveHierarchy
    , wsMoveSide
    , wsMoveToParent
    , wsMoveToChild
    , wsMoveNext
    , wsMovePrev
    )
where

import XMonad
    ( X
    , ExtensionClass(..)
    , StateExtension(..)
    , Typeable
    , WindowSpace
    , WorkspaceId
    , windows
    , withWindowSet
    )
import XMonad.Util.Types (Direction1D(..))

import Data.List (find, intercalate)
import Data.Maybe (fromJust, fromMaybe)
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
    [ Node (WSItem "web") []
    , Node
        (WSItem "dev")
        [ Node (WSLink ["web"])   []
        , Node (WSItem "rust")    []
        , Node (WSItem "haskell") []
        ]
    , Node (WSItem "music") []
    , Node (WSItem "chat")  []
    , Node (WSItem "etc")   []
    ]

-- | Either a workspace or a link to another workspace
data WorkspaceItem
    = WSItem String
    | WSLink [String]

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

-- | Parsed workspace tree
data WSInfo
    = WSNameInfo [String]
    | WSLinkInfo [String] [String]
    deriving (Typeable, Read, Show)

-- | Parsed workspace tree
wsInfo :: Forest WSInfo
wsInfo = map getInfoFromTree workspaceTree

-- | Virtual node with the workspace tree aggregated
wsInfoTop :: Tree WSInfo
wsInfoTop = Node (WSNameInfo []) wsInfo

-- | Workspace list generated from the workspace tree
myWorkspaces :: [WorkspaceId]
myWorkspaces = intoWorkspaceIds workspaceTree

-- | Join the name segments to get the full path
wsJoinNames :: [String] -> String
wsJoinNames = intercalate [wsSep]

wsSepName :: String -> [String]
wsSepName name = case break (== wsSep) name of
    (a, ',' : b) -> a : wsSepName b
    (a, ""     ) -> [a]
    (a, b      ) -> head [] -- impossible

-- | Parse the workspace tree into a more usable form
getInfoFromTree :: Tree WorkspaceItem -> Tree WSInfo
getInfoFromTree = infoFromTreeRecursive []
  where
    infoFromTreeRecursive :: [String] -> Tree WorkspaceItem -> Tree WSInfo
    infoFromTreeRecursive parents (Node (WSItem name) children) = Node
        (WSNameInfo fullName)
        (map (infoFromTreeRecursive fullName) children)
        where fullName = parents ++ [name]
    infoFromTreeRecursive parents (Node (WSLink linkTo) _) =
        Node (WSLinkInfo (parents ++ [last linkTo]) linkTo) []

-- | Convert the workspace tree into a list of workspace ids
intoWorkspaceIds :: Forest WorkspaceItem -> [WorkspaceId]
intoWorkspaceIds = concatMap (flatten . convert . getInfoFromTree) . filterOut
  where
    convert :: Tree WSInfo -> Tree String
    convert (Node (WSNameInfo segs) children) =
        Node (intercalate [wsSep] segs) (map convert children)
    convert (Node (WSLinkInfo segs target) _) = Node
        (intercalate [wsSep] segs ++ [wsLink] ++ intercalate [wsSep] target)
        []
    filterOut :: Forest WorkspaceItem -> Forest WorkspaceItem
    filterOut = map filterOut' . filter filterPred
    filterOut' :: Tree WorkspaceItem -> Tree WorkspaceItem
    filterOut' (Node ws children) = Node ws $ filter filterPred children
    filterPred :: Tree WorkspaceItem -> Bool
    filterPred (Node (WSItem _) _) = True
    filterPred _                   = False

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
wsPathFromNode (Node (WSNameInfo p  ) _) = p
wsPathFromNode (Node (WSLinkInfo p _) _) = p

-- | Get the real path of the given node, resolving the link
wsResolvePathOfNode :: Tree WSInfo -> [String]
wsResolvePathOfNode (Node (WSNameInfo p) _) = p
wsResolvePathOfNode (Node (WSLinkInfo _ l) _) =
    wsResolvePathOfNode . fromJust . wsGetNodeAt wsInfo $ l

-- | Get the children of the given node
wsGetChildren :: Tree WSInfo -> Forest WSInfo
wsGetChildren (Node _ children) = children

wsMoveHierarchy :: Direction1D -> X ()
wsMoveHierarchy Next = wsMoveToChild
wsMoveHierarchy Prev = wsMoveToParent

wsMoveToParent :: X ()
wsMoveToParent = wsGetCurrentLevel >>= move
  where
    move :: [String] -> X ()
    move [] = return ()
    move n  = do
        XS.put . WSLevel . init $ n
        windows . W.view . wsJoinNames $ n

wsMoveToChild :: X ()
wsMoveToChild =
    withWindowSet (return . wsGetNodeAt wsInfo . wsSepName . W.currentTag)
        >>= maybe (return ()) curNodeToAction
  where
    curNodeToAction :: Tree WSInfo -> X ()
    curNodeToAction (Node _ []      ) = return ()
    curNodeToAction (Node _ children) = do
        target     <- return . head $ children
        targetPath <- return . wsJoinNames . wsResolvePathOfNode $ target
        XS.put . WSLevel . init . wsPathFromNode $ target
        windows . W.view $ targetPath

wsMoveSide :: Direction1D -> X ()
wsMoveSide dir =
    --curLevel <- wsGetCurrentLevel
    --curPath <- withWindowSet $ return . last . wsSepName . W.currentTag
    return ()

wsMoveNext :: X ()
wsMoveNext = wsMoveSide Next

wsMovePrev :: X ()
wsMovePrev = wsMoveSide Prev

wsFilterWsLevel :: X ([WindowSpace] -> [WindowSpace])
wsFilterWsLevel = do
    path <- wsGetCurrentLevel
    return $ \l -> filter (filterPred $ wsGetNodeAtFallback path) l
  where
    filterPred :: Tree WSInfo -> WindowSpace -> Bool
    filterPred (Node _ nodes) ws =
        W.tag ws `elem` map (wsJoinNames . wsPathFromNode) nodes

-- vim:ts=4:sw=4:sts=4:et:
