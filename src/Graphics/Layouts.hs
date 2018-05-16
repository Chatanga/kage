module Graphics.Layouts (
    fixedLayout,
    borderLayout,
    AnchorConstraint(..),
    anchorLayout,
    adaptativeLayout,
) where

import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

import Graphics.Rendering.OpenGL

import Graphics.View

----------------------------------------------------------------------------------------------------

setBounds position size tree = layoutSetSize (viewLayout (rootLabel tree')) size tree' where
    tree' = setPosition position tree

fixedLayout (Size w h) = Layout
    (const (Just w, Just h))
    setSize

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃           top            ┃
    ┠─────┬──────────────┬─────┨
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃left │    center    │right┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┃     │              │     ┃
    ┠─────┴──────────────┴─────┨
    ┃          bottom          ┃
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-}

data BordelLayouPosition = Center | Top | Left | Bottom | Right

borderLayout :: [BordelLayouPosition] -> Layout a
borderLayout positions = undefined -- TODO

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃               ▲          ┃
    ┃               ┆ top      ┃
    ┃               ▼          ┃
    ┃           ┌───────┐      ┃
    ┃    left   │       │ right┃
    ┃◀---------▶│       │◀----▶┃
    ┃           │       │      ┃
    ┃           └───────┘      ┃
    ┃               ▲          ┃
    ┃               ┆ bottom   ┃
    ┃               ▼          ┃
    ┗━━━━━━━━━━━━━━━━━━━━━━━━━━┛
-}

data AnchorConstraint = AnchorConstraint
    { topDistance :: Maybe GLsizei
    , rightDistance :: Maybe GLsizei
    , bottomDistance :: Maybe GLsizei
    , leftDistance :: Maybe GLsizei
    } deriving Show

anchorLayout :: [AnchorConstraint] -> Layout a
anchorLayout constraints = Layout getter setter where
    getter t = (Nothing, Nothing)
    setter s t = setSize s t{ subForest = zipWith updateChild constraints (subForest t) } where
        (Size w h) = s
        updateChild constraint child = setBounds (Position xChild yChild) (Size wChild hChild) child where
            (mWidth, mHeigh) = layoutGetNaturalSize (viewLayout (rootLabel child)) child
            (xChild, wChild) = case (leftDistance constraint, mWidth, rightDistance constraint) of
                (Nothing, _, Nothing) -> let iw = fromMaybe w mWidth in ((w - iw) `div` 2, iw) -- centrage
                (Just l, _, Nothing) -> (l, w') where w' = fromMaybe (w - l) mWidth
                (Nothing, _, Just r) -> (w - w' - r, w') where w' = fromMaybe (w - r) mWidth
                (Just l, _, Just r) -> (l, w - l - r) -- La taille naturelle du composant est ignorée.
            (yChild, hChild) = case (topDistance constraint, mHeigh, bottomDistance constraint) of
                (Nothing, _, Nothing) -> let ih = fromMaybe h mHeigh in ((h - ih) `div` 2, ih) -- centrage
                (Just t, _, Nothing) -> (t, h') where h' = fromMaybe (h - t) mHeigh
                (Nothing, _, Just b) -> (h - h' - b, h') where h' = fromMaybe (h - b) mHeigh
                (Just t, _, Just b) -> (t, h - t - b) -- La taille naturelle du composant est ignorée.

{-
    ┏━━━━━━━━━━━━━━━━━━━━━━━━━━┓
    ┃                          ┃
    ┃           9x27           ┃
    ┃         (master)         ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┃                          ┃
    ┠────────┬────────┬────────┨
    ┃  3x9   │  3x9   │  3x9   ┃
    ┃(slave) │(slave) │(slave) ┃
    ┗━━━━━━━━┷━━━━━━━━┷━━━━━━━━┛
-}

adaptativeLayout :: Layout a
adaptativeLayout = Layout getter setter where
    getter t = case subForest t of
        [] -> (Nothing, Nothing)
        (master : slaves) ->
            let n = fromIntegral (length slaves)
                (mw, mh) = layoutGetNaturalSize (viewLayout (rootLabel master)) master
            in  if n > 0
                then (mw, (\h -> h + h `div` n) <$> mh)
                else (mw, mh)
    setter s t = setSize s t' where
        t' = case subForest t of
            [] -> t
            (master : slaves) ->
                let n = fromIntegral (length slaves)
                in  if n > 0
                    then
                        let
                            (Size w h) = s
                            wMaster = w
                            hMaster = (n * h) `div` (n + 1)
                            wSlave = w `div` n
                            hSlave = h - hMaster
                            master' = setBounds (Position 0 0) (Size wMaster hMaster) master
                            updateSlave index = setBounds (Position (wSlave * index) hMaster) (Size wSlave hSlave)
                        in
                        t{ subForest = master' : zipWith updateSlave [0..] slaves }
                    else
                        t{ subForest = [setBounds (Position 0 0) s master] }
