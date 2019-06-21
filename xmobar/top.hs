import Xmobar

config = defaultConfig
    { position = TopW L 100
    , font = "xft:UbuntuMono Nerd Font Mono:size=16"
    , additionalFonts =
        [ "xft:UbuntuMono Nerd Font Mono:size=24"
        , "xft:UbuntuMono Nerd Font Mono:size=12"
        ]
    , textOffsets = [24, 12]
    , borderColor = "black"
    , border = BottomB
    , bgColor = "black"
    , fgColor = "grey"
    , lowerOnStart = False
    , persistent = True
    , overrideRedirect = False
    , iconRoot = "/home/jamee/dotfiles/xmobar/icons"
    , sepChar = "%"
    , alignSep = "}{"
    -- , template = "%UnsafeStdinReader%}{%time% | %battery% | <icon=%pad%/><action=`rofi -width 30 -theme gruvbox-dark-soft -e \ ﬦ\ Brought\ to\ you\ by\ Haskell`><fc=purple><fn=1></fn></fc></action> "
    , template = "%UnsafeStdinReader%}"
              ++ "{%time% | %battery% | <icon=%pad%/>"
              ++ "<action=`rofi -width 30 -theme gruvbox-dark-soft -e \\ \xfb26\\ Brought\\ to\\ you\\ by\\ Haskell`><fc=purple><fn=1>\xe61f</fn></fc></action> "
    , commands =
        [ Run Date "%Y-%m-%d %a %H:%M:%S" "time" 10
        , Run
            Battery
            [ "-t", "<acstatus> <leftipat> <left>%"
            , "-h", "red"
            , "-H", "50"
            , "-n", "orange"
            , "-L", "30"
            , "-l", "red"
            , "--"
            , "--off-icon-pattern", "<icon=battery_%%.xpm/>"
            , "--on-icon-pattern", "<icon=battery_%%.xpm/>"
            , "--idle-icon-pattern", "<icon=battery_%%.xpm/>"
            -- , "-O", "ﮣ"
            , "-O", "\xfba3"
            -- , "-i", "ﮣ"
            , "-i", "\xfba3"
            -- , "-o", "<fn=1>ﮤ</fn>"
            , "-o", "<fn=1>\xfba4</fn>"
            ]
            10
        , Run ComX "xmobar_icon_pad" [] "" "pad" 10
        , Run UnsafeStdinReader
        ]
    }

-- vim:ft=haskell:ts=4:shiftwidth=4:softtabstop=4:expandtab:
