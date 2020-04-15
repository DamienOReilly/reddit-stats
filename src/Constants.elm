module Constants exposing
    ( backgroundColors
    , borderColors
    , color1
    , color10
    , color11
    , color2
    , color3
    , color4
    , color5
    , color6
    , color7
    , color8
    , color9
    , colora1
    , colora10
    , colora11
    , colora2
    , colora3
    , colora4
    , colora5
    , colora6
    , colora7
    , colora8
    , colora9
    , githubRepoUrl
    , httpTimeout
    , lastNumberOfMonthsForLineGraph
    , pushShiftServer
    , redditUserUrl
    , topSubreddits
    , version
    )

import Color exposing (Color, rgb, rgba)


pushShiftServer =
    "api.pushshift.io"


redditUserUrl =
    "https://www.reddit.com/user/"


githubRepoUrl =
    "https://github.com/DamienOReilly/reddit-stats"


topSubreddits =
    10


lastNumberOfMonthsForLineGraph =
    24


{-| timeout in millis for the http calls
-}
httpTimeout =
    120 * 1000


version =
    1


color1 =
    rgb 0.65 0.807 0.89


color2 =
    rgb 0.121 0.47 0.705


color3 =
    rgb 0.698 0.874 0.541


color4 =
    rgb 0.2 0.627 0.172


color5 =
    rgb 0.984 0.603 0.6


color6 =
    rgb 0.89 0.101 0.109


color7 =
    rgb 0.992 0.749 0.435


color8 =
    rgb 1 0.498 0


color9 =
    rgb 0.792 0.698 0.839


color10 =
    rgb 0.415 0.239 0.603


color11 =
    rgb 0.694 0.349 0.156



-- alpha colors


colora1 =
    rgba 0.65 0.807 0.89 0.2


colora2 =
    rgba 0.121 0.47 0.705 0.2


colora3 =
    rgba 0.698 0.874 0.541 0.2


colora4 =
    rgba 0.2 0.627 0.172 0.2


colora5 =
    rgba 0.984 0.603 0.6 0.2


colora6 =
    rgba 0.89 0.101 0.109 0.2


colora7 =
    rgba 0.992 0.749 0.435 0.2


colora8 =
    rgba 1 0.498 0 0.2


colora9 =
    rgba 0.792 0.698 0.839 0.2


colora10 =
    rgba 0.415 0.239 0.603 0.2


colora11 =
    rgba 0.694 0.349 0.156 0.2


borderColors : List Color
borderColors =
    [ color1
    , color2
    , color3
    , color4
    , color5
    , color6
    , color7
    , color8
    , color9
    , color10
    , color11
    ]


backgroundColors : List Color
backgroundColors =
    [ colora1
    , colora2
    , colora3
    , colora4
    , colora5
    , colora6
    , colora7
    , colora8
    , colora9
    , colora10
    , colora11
    ]
