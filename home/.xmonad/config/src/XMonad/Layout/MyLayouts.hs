{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module XMonad.Layout.MyLayouts
    ( myLayouts
    )
where

import XMonad
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.PerWorkspace (PerWorkspace, onWorkspaces)
import XMonad.Layout.Spacing (Border(..), Spacing, spacingRaw)
import XMonad.Layout.ToggleLayouts (ToggleLayouts, toggleLayouts)

-- | The final layout
myLayouts
    :: Show a
    => ModifiedLayout
           Spacing
           ( PerWorkspace
                 (ToggleLayouts (Choose Tall (Mirror Tall)) Full)
                 (PerWorkspace Full (ToggleLayouts Full Tall))
           )
           a
myLayouts = applySpacing $ onWorkspaces ["web"] layoutWeb $ onWorkspaces
    ["chat", "game", "game/engine", "game/paint"]
    Full
    layoutFullAndTall
    -- FIXME applying layouts to children workspaces not working

-- | "web" workspace layout
layoutWeb :: Show a => ToggleLayouts (Choose Tall (Mirror Tall)) Full a
layoutWeb = flip toggleLayouts Full $ layoutTall ||| layoutMirrorTall

-- | Tall layout with toggling full layout
layoutFullAndTall :: Show a => ToggleLayouts Full Tall a
layoutFullAndTall = toggleLayouts Full layoutTall

-- | Two columns with the master pane on the left
layoutTall :: Tall a
layoutTall = Tall 1 (3 / 100) (1 / 2)

-- | Two rows with the master pane on the top
layoutMirrorTall :: Mirror Tall a
layoutMirrorTall = Mirror layoutTall

-- | Spacing options
applySpacing :: l a -> ModifiedLayout Spacing l a
applySpacing = spacingRaw
    onlyWhenMany
    screenBorder
    enableScreenBorder
    windowBorder
    enableWindowBorder
  where
    onlyWhenMany       = True
    enableScreenBorder = False
    enableWindowBorder = True
    screenBorder       = Border 0 0 0 0
    windowBorder       = Border 10 10 10 10

-- vim:ts=4:sw=4:sts=4:et:
