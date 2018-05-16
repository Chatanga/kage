module Graphics.Color (
    maroon,
    darkRed,
    brown,
    firebrick,
    crimson,
    red,
    tomato,
    coral,
    indianRed,
    lightCoral,
    darkSalmon,
    salmon,
    lightSalmon,
    orangeRed,
    darkOrange,
    orange,
    gold,
    darkGoldenRod,
    goldenRod,
    paleGoldenRod,
    darkKhaki,
    khaki,
    olive,
    yellow,
    yellowGreen,
    darkOliveGreen,
    oliveDrab,
    lawnGreen,
    chartReuse,
    greenYellow,
    darkGreen,
    green,
    forestGreen,
    lime,
    limeGreen,
    lightGreen,
    paleGreen,
    darkSeaGreen,
    mediumSpringGreen,
    springGreen,
    seaGreen,
    mediumAquaMarine,
    mediumSeaGreen,
    lightSeaGreen,
    darkSlateGray,
    teal,
    darkCyan,
    aqua,
    cyan,
    lightCyan,
    darkTurquoise,
    turquoise,
    mediumTurquoise,
    paleTurquoise,
    aquaMarine,
    powderBlue,
    cadetBlue,
    steelBlue,
    cornFlowerBlue,
    deepSkyBlue,
    dodgerBlue,
    lightBlue,
    skyBlue,
    lightSkyBlue,
    midnightBlue,
    navy,
    darkBlue,
    mediumBlue,
    blue,
    royalBlue,
    blueViolet,
    indigo,
    darkSlateBlue,
    slateBlue,
    mediumSlateBlue,
    mediumPurple,
    darkMagenta,
    darkViolet,
    darkOrchid,
    mediumOrchid,
    purple,
    thistle,
    plum,
    violet,
    magenta, fuchsia,
    orchid,
    mediumVioletRed,
    paleVioletRed,
    deepPink,
    hotPink,
    lightPink,
    pink,
    antiqueWhite,
    beige,
    bisque,
    blanchedAlmond,
    wheat,
    cornSilk,
    lemonChiffon,
    lightGoldenRodYellow,
    lightYellow,
    saddleBrown,
    sienna,
    chocolate,
    peru,
    sandyBrown,
    burlyWood,
    tan,
    rosyBrown,
    moccasin,
    navajoWhite,
    peachPuff,
    mistyRose,
    lavenderBlush,
    linen,
    oldLace,
    papayaWhip,
    seaShell,
    mintCream,
    slateGray,
    lightSlateGray,
    lightSteelBlue,
    lavender,
    floralWhite,
    aliceBlue,
    ghostWhite,
    honeydew,
    ivory,
    azure,
    snow,
    black,
    dimGray, dimGrey,
    gray, grey,
    darkGray, darkGrey,
    silver,
    lightGray, lightGrey,
    gainsboro,
    whiteSmoke,
    white
) where

import Prelude hiding (tan)

import Graphics.Rendering.OpenGL

maroon = mkColor 128 0 0
darkRed = mkColor 139 0 0
brown = mkColor 165 42 42
firebrick = mkColor 178 34 34
crimson = mkColor 220 20 60
red = mkColor 255 0 0
tomato = mkColor 255 99 71
coral = mkColor 255 127 80
indianRed = mkColor 205 92 92
lightCoral = mkColor 240 128 128
darkSalmon = mkColor 233 150 122
salmon = mkColor 250 128 114
lightSalmon = mkColor 255 160 122
orangeRed = mkColor 255 69 0
darkOrange = mkColor 255 140 0
orange = mkColor 255 165 0
gold = mkColor 255 215 0
darkGoldenRod = mkColor 184 134 11
goldenRod = mkColor 218 165 32
paleGoldenRod = mkColor 238 232 170
darkKhaki = mkColor 189 183 107
khaki = mkColor 240 230 140
olive = mkColor 128 128 0
yellow = mkColor 255 255 0
yellowGreen = mkColor 154 205 50
darkOliveGreen = mkColor 85 107 47
oliveDrab = mkColor 107 142 35
lawnGreen = mkColor 124 252 0
chartReuse = mkColor 127 255 0
greenYellow = mkColor 173 255 47
darkGreen = mkColor 0 100 0
green = mkColor 0 128 0
forestGreen = mkColor 34 139 34
lime = mkColor 0 255 0
limeGreen = mkColor 50 205 50
lightGreen = mkColor 144 238 144
paleGreen = mkColor 152 251 152
darkSeaGreen = mkColor 143 188 143
mediumSpringGreen = mkColor 0 250 154
springGreen = mkColor 0 255 127
seaGreen = mkColor 46 139 87
mediumAquaMarine = mkColor 102 205 170
mediumSeaGreen = mkColor 60 179 113
lightSeaGreen = mkColor 32 178 170
darkSlateGray = mkColor 47 79 79
teal = mkColor 0 128 128
darkCyan = mkColor 0 139 139
aqua = mkColor 0 255 255
cyan = mkColor 0 255 255
lightCyan = mkColor 224 255 255
darkTurquoise = mkColor 0 206 209
turquoise = mkColor 64 224 208
mediumTurquoise = mkColor 72 209 204
paleTurquoise = mkColor 175 238 238
aquaMarine = mkColor 127 255 212
powderBlue = mkColor 176 224 230
cadetBlue = mkColor 95 158 160
steelBlue = mkColor 70 130 180
cornFlowerBlue = mkColor 100 149 237
deepSkyBlue = mkColor 0 191 255
dodgerBlue = mkColor 30 144 255
lightBlue = mkColor 173 216 230
skyBlue = mkColor 135 206 235
lightSkyBlue = mkColor 135 206 250
midnightBlue = mkColor 25 25 112
navy = mkColor 0 0 128
darkBlue = mkColor 0 0 139
mediumBlue = mkColor 0 0 205
blue = mkColor 0 0 255
royalBlue = mkColor 65 105 225
blueViolet = mkColor 138 43 226
indigo = mkColor 75 0 130
darkSlateBlue = mkColor 72 61 139
slateBlue = mkColor 106 90 205
mediumSlateBlue = mkColor 123 104 238
mediumPurple = mkColor 147 112 219
darkMagenta = mkColor 139 0 139
darkViolet = mkColor 148 0 211
darkOrchid = mkColor 153 50 204
mediumOrchid = mkColor 186 85 211
purple = mkColor 128 0 128
thistle = mkColor 216 191 216
plum = mkColor 221 160 221
violet = mkColor 238 130 238
magenta = mkColor 255 0 255
fuchsia = magenta
orchid = mkColor 218 112 214
mediumVioletRed = mkColor 199 21 133
paleVioletRed = mkColor 219 112 147
deepPink = mkColor 255 20 147
hotPink = mkColor 255 105 180
lightPink = mkColor 255 182 193
pink = mkColor 255 192 203
antiqueWhite = mkColor 250 235 215
beige = mkColor 245 245 220
bisque = mkColor 255 228 196
blanchedAlmond = mkColor 255 235 205
wheat = mkColor 245 222 179
cornSilk = mkColor 255 248 220
lemonChiffon = mkColor 255 250 205
lightGoldenRodYellow = mkColor 250 250 210
lightYellow = mkColor 255 255 224
saddleBrown = mkColor 139 69 19
sienna = mkColor 160 82 45
chocolate = mkColor 210 105 30
peru = mkColor 205 133 63
sandyBrown = mkColor 244 164 96
burlyWood = mkColor 222 184 135
tan = mkColor 210 180 140
rosyBrown = mkColor 188 143 143
moccasin = mkColor 255 228 181
navajoWhite = mkColor 255 222 173
peachPuff = mkColor 255 218 185
mistyRose = mkColor 255 228 225
lavenderBlush = mkColor 255 240 245
linen = mkColor 250 240 230
oldLace = mkColor 253 245 230
papayaWhip = mkColor 255 239 213
seaShell = mkColor 255 245 238
mintCream = mkColor 245 255 250
slateGray = mkColor 112 128 144
lightSlateGray = mkColor 119 136 153
lightSteelBlue = mkColor 176 196 222
lavender = mkColor 230 230 250
floralWhite = mkColor 255 250 240
aliceBlue = mkColor 240 248 255
ghostWhite = mkColor 248 248 255
honeydew = mkColor 240 255 240
ivory = mkColor 255 255 240
azure = mkColor 240 255 255
snow = mkColor 255 250 250
black = mkColor 0 0 0
dimGray = mkColor 105 105 105
dimGrey = dimGray
gray = mkColor 128 128 128
grey = gray
darkGray = mkColor 169 169 169
darkGrey = darkGray
silver = mkColor 192 192 192
lightGray = mkColor 211 211 211
lightGrey = lightGray
gainsboro = mkColor 220 220 220
whiteSmoke = mkColor 245 245 245
white = mkColor 255 255 255

mkColor :: Int -> Int -> Int -> Color4 GLfloat
mkColor r g b = Color4 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0) 1
