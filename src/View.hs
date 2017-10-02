module View (
    Event(..),
    ViewHandleEvent,
    Layout(..),
    fixedLayout,
    defaultLayout,
    borderLayout,
    AnchorConstraint(..),
    anchorLayout,
    adaptativeLayout,
    Bounds,
    View(..),
    createView,
    sendEvent,
    UI(..),
    createUI,
    layout,
    processEvent,
    --
    testProcessEvent
) where

import Control.Arrow ((***))
import Control.Monad
import Data.IORef
import Data.List
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as LP

import Debug

----------------------------------------------------------------------------------------------------

data UI a = UI
    { uiRoot :: Tree (View a)
    , uiCursorPos :: Maybe (Double, Double)
    , uiMouseDragged :: Bool
    , uiClosing :: Bool
    } deriving Show

createUI :: Tree (View a) -> UI a
createUI root = UI root Nothing False False

type Bounds = (GL.Position, GL.Size)

data View a = View
    { viewLocalBounds :: Bounds
    , viewContent :: a
    , viewHasFocus :: Bool
    , viewHasCursor :: Bool
    , viewEventQueue :: [Event]
    , viewHandleEvent :: ViewHandleEvent a
    , viewLayout :: Layout a
    }

createView :: a -> View a
createView ref = View
    (GL.Position 0 0, GL.Size 0 0)
    ref
    False
    False
    []
    (\_ _ -> return Nothing)
    defaultLayout

instance Show (View a) where
    show view = "View" ++
        "{" ++ "localBounds=" ++ show (viewLocalBounds view) ++
        "," ++ "hasFocus=" ++ show (viewHasFocus view) ++
        "," ++ "hasCursor=" ++ show (viewHasCursor view) ++
        "," ++ "eventQueue=" ++ show (viewEventQueue view) ++
        "}"

----------------------------------------------------------------------------------------------------

data Layout a = Layout
    { layoutGetNaturalSize :: Tree (View a) -> (Maybe GL.GLsizei, Maybe GL.GLsizei)
    , layoutSetSize :: GL.Size -> Tree (View a) -> Tree (View a)
    }

layout :: GL.Size -> UI a -> UI a
layout size ui = ui{ uiRoot = r' } where
    r = uiRoot ui
    l = viewLayout (rootLabel r)
    r' = layoutSetSize l size r

defaultLayout = Layout
    (const (Nothing, Nothing))
    setSize

fixedLayout (GL.Size w h) = Layout
    (const (Just w, Just h))
    setSize

setPosition position tree = tree { rootLabel = view' } where
    view = rootLabel tree
    (_, size) = viewLocalBounds view
    view' = view{ viewLocalBounds = (position, size) }

setSize size tree = tree { rootLabel = view' } where
    view = rootLabel tree
    (position, _) = viewLocalBounds view
    view' = view{ viewLocalBounds = (position, size) }

setBounds position size tree = layoutSetSize (viewLayout (rootLabel tree')) size tree' where
    tree' = setPosition position tree

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
    { topDistance :: Maybe GL.GLsizei
    , rightDistance :: Maybe GL.GLsizei
    , bottomDistance :: Maybe GL.GLsizei
    , leftDistance :: Maybe GL.GLsizei
    } deriving Show

anchorLayout :: [AnchorConstraint] -> Layout a
anchorLayout constraints = Layout getter setter where
    getter t = (Nothing, Nothing)
    setter s t = setSize s t{ subForest = zipWith updateChild constraints (subForest t) } where
        (GL.Size w h) = s
        updateChild constraint child = setBounds (GL.Position xChild yChild) (GL.Size wChild hChild) child where
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
                            (GL.Size w h) = s
                            wMaster = w
                            hMaster = (n * h) `div` (n + 1)
                            wSlave = w `div` n
                            hSlave = h - hMaster
                            master' = setBounds (GL.Position 0 0) (GL.Size wMaster hMaster) master
                            updateSlave index = setBounds (GL.Position (wSlave * index) hMaster) (GL.Size wSlave hSlave)
                        in
                        t{ subForest = master' : zipWith updateSlave [0..] slaves }
                    else
                        t{ subForest = [setBounds (GL.Position 0 0) s master] }

----------------------------------------------------------------------------------------------------

data Event
    = EventMouseButton  !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
    | EventCursorPos    !Double !Double
    | EventKey          !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
    | EventChar         !Char
    | EventCursorExited
    | EventCursorEntered
    | EventClose
    deriving Show

-- Just => consume event / Nothing => bubble up
type ViewHandleEvent a = Event -> TreeLoc (View a) -> IO (Maybe (TreeLoc (View a)))

defaultViewHandleEvent :: Show a => ViewHandleEvent a
defaultViewHandleEvent event loc = do
    putStrLn $ "Handle event " ++ show event ++ " in " ++ show (viewContent (getLabel loc))
    return (Just loc)

pushEvent :: Event -> View a -> View a
pushEvent event view = view{ viewEventQueue = event : viewEventQueue view }

popEvent :: View a -> Maybe (View a, Event)
popEvent view = case viewEventQueue view of
    [] -> Nothing
    events -> Just (view{ viewEventQueue = init events }, last events)

requestExclusiveFocus :: TreeLoc (View a) -> TreeLoc (View a)
requestExclusiveFocus = undefined -- TODO

sendEvent :: Event -> TreeLoc (View a) -> TreeLoc (View a)
sendEvent event = modifyLabel (pushEvent event)

testProcessEvent :: IO ()
testProcessEvent = do
    let
        ui = createUI $
            Node (createDumpView "1" False (0, 0, 800, 600))
            [   Node (createDumpView "1.1" True (0, 0, 800, 480))
                [   Node (createDumpView "1.1.1" False (600, 50, 150, 150)) []
                ]
            ,   Node (createDumpView "1.2" False (0, 480, 200, 120)) []
            ,   Node (createDumpView "1.3" False (200, 480, 200, 120)) []
            ,   Node (createDumpView "1.4" False (400, 480, 200, 120)) []
            ,   Node (createDumpView "1.5" False (600, 480, 200, 120)) []
            ]

        createDumpView :: String -> Bool -> (GL.GLint, GL.GLint, GL.GLsizei, GL.GLsizei) -> View String
        createDumpView content hasFocus (x, y, w, h) = View
            (GL.Position x y, GL.Size w h)
            content
            hasFocus
            False
            []
            defaultViewHandleEvent
            defaultLayout

    print ui

    let events =
            [ EventCursorPos 10 500
            , EventCursorPos 20 500
            , EventCursorPos 10 10
            , EventCursorPos 100 100
            ]

    let
        inject :: UI a -> Event -> IO (UI a)
        inject ui event = do
            ui' <- processEvent ui event
            print ui'
            return ui'

    foldM_ inject ui events

processEvent :: UI a -> Event -> IO (UI a)

processEvent ui@(UI _ _ _ True) event = return ui

processEvent ui EventClose = return ui{ uiClosing = True }

processEvent ui (EventCursorPos xPos yPos) = do
    let cursorPos = (xPos, yPos)
        -- Register an EventCursorExited in each view where it is located (0-1 in principle).
        r = fromTree $ flip fmap (uiRoot ui) $ \view ->
            if viewHasCursor view
                then pushEvent EventCursorExited (view{ viewHasCursor = False })
                else view
    -- Pick the deepest view with the cursor in and, from it up to the root, update the tree
    -- using the first handler which accepts the event. Note that an Exit+Enter combination is
    -- coalesced into no event at all.
    l2 <- case selectDeepestAt cursorPos r of
        Nothing -> return r -- Coalesced into nothing, like I’ve said.
        Just loc -> bubbleUpCursorEntry loc
    -- Unpop all the remaining exit events, updating the tree each time.
    l3 <- updateUntilStable (\event loc -> viewHandleEvent (getLabel loc) event loc) (root l2)
    -- Then process a final event for the actual cursor move to the view where it is now
    -- located.
    l4 <- case selectDeepestAt cursorPos (root l3) of
        Nothing -> return l3
        Just loc -> bubbleUp (\l -> viewHandleEvent (getLabel l) (EventCursorPos xPos yPos) l) loc
    return (UI (toTree (root l4)) (Just cursorPos) (uiMouseDragged ui) False)

processEvent ui event@EventMouseButton{} =
    case selectOn viewHasCursor (fromTree (uiRoot ui)) of
        Nothing -> return ui
        Just loc -> do
            l <- bubbleUp (\l -> viewHandleEvent (getLabel l) event l) loc
            return $ ui{ uiRoot = toTree (root l) }

processEvent ui event@(EventKey k _ ks _) = do
    when (ks == GLFW.KeyState'Released && k == GLFW.Key'Q) $
        print (uiRoot ui)
    case selectOn viewHasFocus (fromTree (uiRoot ui)) of
        Nothing -> return ui
        Just loc -> do
            l <- bubbleUp (\l -> viewHandleEvent (getLabel l) event l) loc
            return $ ui{ uiRoot = toTree (root l) }

processEvent ui event = error $ "Unhandled event type: " ++ show event

bubbleUpCursorEntry :: TreeLoc (View a) -> IO (TreeLoc (View a))
bubbleUpCursorEntry loc = let view = getLabel loc in
    case popEvent view of
        -- si EventMouseExited alors simplement le dépiler et arrêter,
        Just (view', EventCursorExited) -> return (setLabel view'{ viewHasCursor = True } loc)
        Just (_, event) -> error $ "Unexpected event type: " ++ show event
        -- sinon
        Nothing -> do
            -- tenter la transformation avec EventMouseEntered :
            result <- viewHandleEvent view EventCursorEntered loc
            case result of
                -- si accepté alors arrêter après prise en compte,
                Just loc' ->
                    let view' = (getLabel loc'){ viewHasCursor = True }
                    in  return (setLabel view' loc')
                -- sinon remonter
                Nothing -> case parent loc of
                    Nothing -> return loc
                    Just p -> bubbleUpCursorEntry p

bubbleUp
    :: (TreeLoc (View a) -> IO (Maybe (TreeLoc (View a))))
    -> TreeLoc (View a)
    -> IO (TreeLoc (View a))
bubbleUp update loc = do
    result <- update loc
    case result of
        Just loc' -> return loc'
        Nothing -> case parent loc of
            Nothing -> return loc
            Just p -> bubbleUp update p

-- TODO Parcourir tout l’arbre pour trouver l’événement le plus ancien,
-- pas simplement le premier.
updateUntilStable
    :: ViewHandleEvent a
    -> TreeLoc (View a)
    -> IO (TreeLoc (View a))
updateUntilStable update loc = do
    let hasEvent = not . null . viewEventQueue . getLabel
    case listToMaybe (filter hasEvent (walkDeepFirst False loc)) of
        Nothing -> return loc
        Just loc' ->
            let view = getLabel loc'
                Just (view', event) = popEvent view
            -- TODO exit on error if too many iterations
            in  bubbleUp (update event) (setLabel view' loc') >>= updateUntilStable update

walkDeepFirst :: Bool -> TreeLoc (View a) -> [TreeLoc (View a)]
walkDeepFirst preFixed loc = x ++ maybe [] (walkDeepFirst preFixed) (right loc) where
    x = case firstChild loc of
        Nothing -> [loc]
        Just start ->
            let children = walkDeepFirst preFixed start in
            if preFixed
                then loc : children
                else children ++ [loc]

selectOn :: (View a -> Bool) -> TreeLoc (View a) -> Maybe (TreeLoc (View a))
selectOn p loc = result where
    result = if p (getLabel loc)
        then Just loc
        else
            case firstChild loc of
                Just start -> walk start
                Nothing -> Nothing
    -- walk :: TreeLoc (View a) -> Maybe (TreeLoc (View a))
    walk childLoc = case selectOn p childLoc of
        Nothing -> right childLoc >>= walk
        Just childLoc' -> Just childLoc'

selectDeepestAt :: (Double, Double) -> TreeLoc (View a) -> Maybe (TreeLoc (View a))
selectDeepestAt cursorPos loc = result where
    bounds = viewLocalBounds (getLabel loc)
    result = if inside cursorPos bounds
        then Just $
            case firstChild loc of
                Just start -> fromMaybe loc (walk start)
                Nothing -> loc
        else Nothing
    walk :: TreeLoc (View a) -> Maybe (TreeLoc (View a))
    walk childLoc = case selectDeepestAt cursorPos childLoc of
        Nothing -> right childLoc >>= walk
        Just childLoc' -> Just childLoc'

inside :: (Double, Double) -> Bounds -> Bool
inside (xPos, yPos) (GL.Position x y, GL.Size w h) =
    fromIntegral x <= xPos && xPos < fromIntegral (x + w) &&
    fromIntegral y <= yPos && yPos < fromIntegral (y + h)
