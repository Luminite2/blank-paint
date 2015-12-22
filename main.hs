{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import           Graphics.Blank

{-
 - Types
 -}
data State = State
        { down :: Maybe (Double, Double)
        , up :: Maybe (Double, Double)
        , draw :: Draw
        , thickness :: Double
        , color :: Text.Text
        , buttons :: [Button]
        , iconUpdate :: Canvas ()
        }

data Button = Button
        { upLeft :: (Double, Double)
        , loRight :: (Double, Double)
        , mut :: Button -> State -> State
        }

data Draw = Line | Circle | Rectangle | Erase
    deriving (Eq, Enum)

{-
 - Constants
 -}
cCOLORS = ["red", "green", "blue", "black"]
cDRAWS = [toEnum 0 :: Draw ..]
cWIDTHS = [5, 10, 15, 20]
cBUTTONSIZE = 50
cBUTTONS = 
            [ Button (0, 0) (cBUTTONSIZE, cBUTTONSIZE) (mutDraw cDRAWS)
            , Button (0, cBUTTONSIZE) (cBUTTONSIZE, 2*cBUTTONSIZE) (mutColor cCOLORS)
            , Button (0, 2*cBUTTONSIZE) (cBUTTONSIZE,  3*cBUTTONSIZE) (mutThickness cWIDTHS)
            ]

{-
 - Utilities
 -}
nextElem :: Eq a => [a] -> a -> a
nextElem xs x = (dropWhile (/= x) (cycle xs)) !! 1

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

{-
 - Button Functions
 -}

--Constructors for "mutator" field in Button type, which determines
--the next state to go to when the associated button is clicked
--and records the draw actions needed to update the icon
mutDraw :: [Draw] -> Button -> State -> State
mutDraw draws b state = let d = nextElem draws (draw state) in
                            state {draw = d, iconUpdate = updateIconDraw b d}

mutColor :: [Text.Text] -> Button -> State -> State
mutColor colors b state = let c = nextElem colors (color state) in
                            state {color = c, iconUpdate = updateIconColor b c}

mutThickness :: [Double] -> Button -> State -> State
mutThickness widths b state = let w = nextElem widths (thickness state) in
                            state {thickness = w, iconUpdate = updateIconThickness b w}

--Checks whether a click was within any button's area
--if so, returns the state that has been mutated
--by that button's mutator. Also, clears
--the click
processButtons :: (Double, Double) -> State -> State
processButtons (x,y) state = 
                            let res = (filter (\ m -> case m of Nothing -> False ; Just a -> True) (map (\ button -> 
                                    let (ulx, uly) = upLeft button
                                        (lrx, lry) = loRight button
                                    in
                                        if x <= lrx && x >= ulx && y <= lry && y >= uly then
                                             (Just (((mut button) button state) {down = Nothing}))
                                        else
                                            Nothing)
                                    (buttons state)))
                            in case res of
                                        [] -> state
                                        ss -> case head ss of
                                                Nothing -> state
                                                Just s -> s
--Draws the outlines for the buttons
drawButtons :: [Button] -> Canvas ()
drawButtons bs = sequence_ (map (\ b -> do 
                            let (ulx, uly) = upLeft b
                            let (lrx, lry) = loRight b
                            lineWidth 1
                            strokeStyle "black"
                            strokeRect(ulx, uly, (lrx - ulx), (lry - uly))
                            ) bs)
--Updates the icon for figure type
updateIconDraw :: Button -> Draw -> Canvas ()
updateIconDraw b d = do
                    let (ulx, uly) = upLeft b
                    let (lrx, lry) = loRight b
                    clearRect(ulx+1, uly+1, (lrx - ulx)-2, (lry - uly)-2)
                    case d of
                        Circle -> drawCircle 1 "black" (ulx, (lry + uly)/2) (lrx, (lry + uly)/2)
                        Line -> drawLine 1 "black" (ulx, (lry + uly)/2) (lrx, (lry + uly)/2)
                        Rectangle -> drawRectangle 1 "black" ((lrx + 4 * ulx)/5, (lry + 3 * uly)/4) ((4 * lrx + ulx)/5, (3 * lry + uly)/4)
                        Erase -> do
                            beginPath()
                            lineWidth 1
                            strokeStyle "pink"
                            rect((lrx + 2 * ulx)/3, (lry + 2 * uly)/3, (lrx - ulx)/3, (lry - uly)/3)
                            fillStyle "pink" >> fill() >> stroke()

--Updates the icon for color
updateIconColor :: Button -> Text.Text -> Canvas ()
updateIconColor b c = do
                    let (ulx, uly) = upLeft b
                    let (lrx, lry) = loRight b
                    clearRect(ulx+1, uly+1, (lrx - ulx)-2, (lry - uly)-2)
                    drawLine 1 c (ulx, (lry + uly)/2) (lrx, (lry + uly)/2)

--Updates the icon for thickness
updateIconThickness :: Button -> Double -> Canvas ()
updateIconThickness b w = do
                    let (ulx, uly) = upLeft b
                    let (lrx, lry) = loRight b
                    clearRect(ulx+1, uly+1, (lrx - ulx)-2, (lry - uly)-2)
                    drawLine w "black" (ulx, (lry + uly)/2) (lrx, (lry + uly)/2)

--Updates each button's icon 
--(useful for when the user draws into the ribbon area)
updateAllIcons :: State -> Canvas()
updateAllIcons state = do
                    updateIconDraw (cBUTTONS !! 0) (draw state)
                    updateIconColor (cBUTTONS !! 1) (color state)
                    updateIconThickness (cBUTTONS !! 2) (thickness state)

{-
 - Figure type draw logic
 -}
drawLine :: Double -> Text.Text -> (Double, Double) -> (Double, Double) -> Canvas ()
drawLine w c (dx, dy) (ux, uy) = do
    beginPath()
    lineWidth w
    strokeStyle c
    moveTo(dx, dy)
    lineTo(ux, uy)
    closePath()
    stroke()

drawCircle :: Double -> Text.Text -> (Double, Double) -> (Double, Double) -> Canvas ()
drawCircle w c (dx, dy) (ux, uy) = do
    let x' = ux - dx
    let y' = uy - dy
    let diameter = sqrt (x'*x' + y'*y')
    lineWidth w
    strokeStyle c
    beginPath()
    arc((dx + ux)/2, (dy + uy)/2, diameter/2, 0, 2 * pi, False)
    closePath()
    stroke()

drawRectangle :: Double -> Text.Text -> (Double, Double) -> (Double, Double) -> Canvas ()
drawRectangle w c (dx, dy) (ux, uy) = do
    lineWidth w
    strokeStyle c
    strokeRect(dx, dy, (ux - dx), (uy - dy))

drawErase :: (Double, Double) -> (Double, Double) -> Canvas ()
drawErase (dx, dy) (ux, uy) = do
    clearRect(dx, dy, (ux - dx), (uy - dy)) 
    
--Selects one of the above functions based on state
--note that the functions are only partiall applied
drawFunc :: State -> ((Double, Double) -> (Double, Double) -> Canvas ())
drawFunc state = case (draw state) of
                Circle -> drawCircle (thickness state) (color state)
                Line -> drawLine (thickness state) (color state)
                Rectangle -> drawRectangle (thickness state) (color state)
                Erase -> drawErase

{-
 - Main control loop functions
 -}
main :: IO ()
main = blankCanvas 3000 { events = ["mouseup", "mousedown"] } $ \ context -> initContext context

initContext :: DeviceContext -> IO()
initContext context = do
        let buttonSize = 50
        send context $ do
            (drawButtons cBUTTONS)
            updateIconDraw (head cBUTTONS) (head cDRAWS)
            updateIconColor (cBUTTONS !! 1) (head cCOLORS)
            updateIconThickness (cBUTTONS !! 2) (head cWIDTHS)

        control context (State Nothing Nothing (head cDRAWS) (head cWIDTHS) (head cCOLORS) cBUTTONS (return ()))


loop :: DeviceContext -> State -> IO ()
loop context state = do
        let procState = case (down state) of
                    (Just (dx, dy)) -> processButtons (dx, dy) state
                    Nothing -> state
        send context $ do
                case (down state, up state) of
                        (Just (dx, dy), Just (ux, uy)) -> do
                            (drawFunc state) (dx, dy) (ux, uy)
                            drawButtons (buttons state)
                            updateAllIcons state
                        (Just (dx, dy), Nothing) -> iconUpdate procState
                        _ -> return ()

        let state' = case (down state, up state) of
                (Nothing, Nothing) -> state
                (Just (x,y), Nothing) -> procState
                (_, Just (x,y)) -> state {down = Nothing, up = Nothing}
        control context state'

control :: DeviceContext -> State -> IO ()
control context state = do
    event <- wait context
    let new_down = case (eType event,ePageXY event) of
                     ("mousedown",Just (x,y)) -> Just (x,y)
                     _ -> down state
    let new_up = case (eType event,ePageXY event) of
                     ("mouseup",Just (x,y)) -> Just (x,y)
                     _ -> up state
                
    let state' = state {down = new_down, up = new_up}
    
    loop context state'
