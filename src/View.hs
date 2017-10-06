module View (
    Event(..),
    EventAction(..),
    ViewHandleEvent,
    Layout(..),
    defaultLayout,
    setPosition,
    setSize,
    Bounds,
    View(..),
    createView,
    UI(..),
    createUI,
    layout,
    processEvent,
) where

import Control.Monad
import Data.Maybe
import Data.Tree
import Data.Tree.Zipper

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

----------------------------------------------------------------------------------------------------

data UI a = UI
    { uiRoot :: Tree (View a)
    , uiCursorPos :: Maybe (Double, Double)
    , uiMouseDrag :: Maybe (Double, Double) -- latest drag position
    , uiTerminated :: Bool
    } deriving Show

createUI :: Tree (View a) -> UI a
createUI root = UI root Nothing Nothing False

type Bounds = (GL.Position, GL.Size)

data View a = View
    { viewLocalBounds :: Bounds
    , viewContent :: a
    , viewHasFocus :: Bool
    , viewHasCursor :: Bool
    , viewIsDragOrigin :: Bool
    , viewEventQueue :: [Event]
    , viewHandleEvent :: ViewHandleEvent a
    , viewLayout :: Layout a
    }

createView :: a -> View a
createView content = View
    (GL.Position 0 0, GL.Size 0 0)
    content
    False
    False
    False
    []
    (\_ _ -> return BubbleUp)
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

setPosition position tree = tree { rootLabel = view' } where
    view = rootLabel tree
    (_, size) = viewLocalBounds view
    view' = view{ viewLocalBounds = (position, size) }

setSize size tree = tree { rootLabel = view' } where
    view = rootLabel tree
    (position, _) = viewLocalBounds view
    view' = view{ viewLocalBounds = (position, size) }

----------------------------------------------------------------------------------------------------

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
        createDumpView content hasFocus (x, y, w, h) = (createView content)
            {   viewLocalBounds = (GL.Position x y, GL.Size w h)
            ,   viewHasCursor = hasFocus
            ,   viewHandleEvent = defaultViewHandleEvent
            ,   viewLayout = defaultLayout
            }

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

----------------------------------------------------------------------------------------------------

data Event
    = EventMouseButton  !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
    | EventCursorPos    !Double !Double
    | EventDrag         !Double !Double -- Delta between last drag position and current position.
    | EventKey          !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
    | EventChar         !Char
    | EventCursorExited
    | EventCursorEntered
    deriving Show

data EventAction a = BubbleUp | Terminate | Discard | Consume (TreeLoc (View a))

type ViewHandleEvent a = Event -> TreeLoc (View a) -> IO (EventAction a)

defaultViewHandleEvent :: Show a => ViewHandleEvent a
defaultViewHandleEvent event loc = do
    putStrLn $ "Handle event " ++ show event ++ " in " ++ show (viewContent (getLabel loc))
    return BubbleUp

pushEvent :: Event -> View a -> View a
pushEvent event view = view{ viewEventQueue = event : viewEventQueue view }

popEvent :: View a -> Maybe (View a, Event)
popEvent view = case viewEventQueue view of
    [] -> Nothing
    events -> Just (view{ viewEventQueue = init events }, last events)

processEvent :: UI a -> Event -> IO (UI a)

processEvent ui@(UI _ _ _ True) event = return ui

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
    ml3 <- updateUntilStable (\event loc -> viewHandleEvent (getLabel loc) event loc) (root l2)
    -- Then process a final event for the actual cursor move to the view where it is now
    -- located.
    ml4 <- case ml3 of
        Just l3 ->
            case selectDeepestAt cursorPos (root l3) of
                Nothing -> return (Just l3)
                Just loc -> bubbleUp (\l -> viewHandleEvent (getLabel l) (EventCursorPos xPos yPos) l) loc
        Nothing -> return Nothing
    let drag = uiMouseDrag ui
    (ml5, drag') <- case ml4 of
        Just l4 ->
            case selectOn viewIsDragOrigin (root l4) of
                Nothing -> return (Just l4, drag)
                Just loc -> do
                    let (xPosOrigin, yPosOrigin) = fromMaybe (xPos, yPos) drag
                        (dx, dy) = (xPos - xPosOrigin, yPos - yPosOrigin)
                    ml5 <- bubbleUp (\l -> viewHandleEvent (getLabel l) (EventDrag dx dy) l) loc
                    return (ml5, Just (xPos, yPos))
        Nothing -> return (Nothing, drag)
    case ml5 of
        Just l5 -> return (UI (toTree (root l5)) (Just cursorPos) drag' False)
        Nothing -> return (ui{ uiTerminated = True })

processEvent ui event@(EventMouseButton b bs _) = do
    ui' <- case selectOn viewHasCursor (fromTree (uiRoot ui)) of
        Nothing -> return ui
        Just loc -> do
            ml <- bubbleUp (\l -> viewHandleEvent (getLabel l) event l) loc
            case ml of
                Just l -> return $ ui{ uiRoot = toTree (root l) }
                Nothing -> return (ui{ uiTerminated = True })
    let resetDrag v = v{ viewIsDragOrigin = False }
        ui'' = case b of
            -- Consider the other buttons too.
            GLFW.MouseButton'1 -> if bs == GLFW.MouseButtonState'Released
                then ui'{ uiRoot = resetDrag <$> uiRoot ui', uiMouseDrag = Nothing }
                else case (uiMouseDrag ui', uiCursorPos ui', uiMouseDrag ui') of
                    (Nothing, Just p, Nothing) -> case selectDeepestAt p (fromTree (uiRoot ui')) of
                        Just loc ->
                            let loc' = modifyLabel (\v -> v{ viewIsDragOrigin = True }) loc
                            in  ui'{ uiRoot = toTree (root loc'), uiMouseDrag = uiCursorPos ui' }
                        _ -> ui'
                    _ -> ui'
            _ -> ui'
    return ui''

processEvent ui event@(EventKey k _ ks _) = do
    when (ks == GLFW.KeyState'Released && k == GLFW.Key'Q) $
        print (uiRoot ui)
    case selectOn viewHasFocus (fromTree (uiRoot ui)) of
        Nothing -> return ui
        Just loc -> do
            ml <- bubbleUp (\l -> viewHandleEvent (getLabel l) event l) loc
            case ml of
                Just l -> return $ ui{ uiRoot = toTree (root l) }
                Nothing -> return (ui{ uiTerminated = True })

processEvent ui event = error $ "Unhandled event type: " ++ show event

bubbleUpCursorEntry :: TreeLoc (View a) -> IO (TreeLoc (View a))
bubbleUpCursorEntry loc = let view = getLabel loc in
    case popEvent view of
        -- EventMouseExited + EventMouseEntry pairs are coalesced into nothing.
        Just (view', EventCursorExited) -> return (setLabel view'{ viewHasCursor = True } loc)
        Just (_, event) -> error $ "Unexpected event type: " ++ show event
        Nothing -> do
            result <- viewHandleEvent view EventCursorEntered loc
            case result of
                Consume loc' ->
                    let view' = (getLabel loc'){ viewHasCursor = True }
                    in  return (setLabel view' loc')
                Discard ->
                    let view' = (getLabel loc){ viewHasCursor = True }
                    in  return (setLabel view' loc)
                BubbleUp -> case parent loc of
                    Nothing -> return loc
                    Just p -> bubbleUpCursorEntry p

bubbleUp
    :: (TreeLoc (View a) -> IO (EventAction a))
    -> TreeLoc (View a)
    -> IO (Maybe (TreeLoc (View a)))
bubbleUp update loc = do
    result <- update loc
    case result of
        Consume loc' -> return (Just loc')
        Discard -> return (Just loc)
        BubbleUp -> case parent loc of
            Nothing -> return (Just loc)
            Just p -> bubbleUp update p
        Terminate -> return Nothing

-- TODO Iterate the whole tree to pick the oldest event, not simply the first found.
updateUntilStable
    :: ViewHandleEvent a
    -> TreeLoc (View a)
    -> IO (Maybe (TreeLoc (View a)))
updateUntilStable update loc = do
    let hasEvent = not . null . viewEventQueue . getLabel
    case listToMaybe (filter hasEvent (walkDeepFirst False loc)) of
        Nothing -> return (Just loc)
        Just loc' -> do
            let view = getLabel loc'
                Just (view', event) = popEvent view
            -- TODO exit on error if too many iterations
            ml <- bubbleUp (update event) (setLabel view' loc')
            case ml of
                Just l -> updateUntilStable update l
                Nothing -> return Nothing

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
