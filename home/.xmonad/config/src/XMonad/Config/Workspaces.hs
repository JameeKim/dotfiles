module XMonad.Config.Workspaces
    ( WSTree(..)
    , WSLevel(..)
    , myWorkspaces
    , workspaceTree
    , wsSep
    , wsLink
    , wsJoinNames
    )
where

import XMonad

import Data.List (find, intercalate)
import Data.Tree (Forest, Tree(..), flatten)

import qualified XMonad.Util.ExtensibleState as XS

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

-- | Parsed workspace tree
data WSInfo
    = WSNameInfo [String]
    | WSLinkInfo [String] [String]
    deriving (Typeable, Read, Show)

-- | Extension state type to store workspace tree structure
newtype WSTree =
    WSTree (Forest WSInfo)
    deriving (Typeable, Read, Show)

instance ExtensionClass WSTree where
    initialValue  = WSTree $ map getInfoFromTree workspaceTree
    extensionType = PersistentExtension

-- | Character used to separate names
wsSep :: Char
wsSep = '/'

-- | Character used to indicate linked workspaces
wsLink :: Char
wsLink = '~'

-- | Overall tree of workspaces
workspaceTree :: Forest WorkspaceItem
workspaceTree
    = [ Node (WSItem "web") []
      , Node
          (WSItem "dev")
          [ Node (WSLink ["web"])   []
          , Node (WSItem "rust")    []
          , Node (WSItem "haskell") []
          ]
      , Node (WSItem "chat") []
      , Node (WSItem "etc")  []
      ]

-- | Workspace list generated from the workspace tree
myWorkspaces :: [WorkspaceId]
myWorkspaces = intoWorkspaceIds workspaceTree

-- | Join the name segments to get the full path
wsJoinNames :: [String] -> String
wsJoinNames = intercalate [wsSep]

-- | Parse the workspace tree info a more usable form
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
    filterOut = map filterOut' . filter pred
    filterOut' :: Tree WorkspaceItem -> Tree WorkspaceItem
    filterOut' (Node ws children) = Node ws $ filter pred children
    pred :: Tree WorkspaceItem -> Bool
    pred (Node (WSItem _) _) = True
    pred _                   = False

-- | Get the node with the given path from the given forest
wsGetNodeAt :: Forest WSInfo -> [String] -> Maybe (Tree WSInfo)
wsGetNodeAt _  []     = Nothing
wsGetNodeAt ls [name] = find (\n -> name == (last . getPathOfNode) n) ls
  where
    getPathOfNode :: Tree WSInfo -> [String]
    getPathOfNode (Node (WSNameInfo p  ) _) = p
    getPathOfNode (Node (WSLinkInfo p _) _) = p
wsGetNodeAt ls (name : rest) = do
    n <- wsGetNodeAt ls [name]
    wsGetNodeAt ls rest

data Something a b = This a | That b deriving (Eq, Show)

-- vim:ts=4:sw=4:sts=4:et:
