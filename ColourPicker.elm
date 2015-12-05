import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Signal exposing (..)
import Color exposing (..)
import String exposing (slice)
import Text exposing (..)
import Mouse

------------------------------------------------------------------------------------------
{--MODEL--}

type alias State = {h:Float, s:Float, l:Float, sx:Float, sy:Float, cx:Float, cy:Float}

model : State
model = { h = 0, s = 0, l = 0.5, sx = 150, sy = 0, cx = 0, cy = 0 }

------------------------------------------------------------------------------------------
{--UPDATE--}

changeColor x' y' m = let x = toFloat x' - 300
                          y =  ((toFloat y') - 300) * (-1)
                       in if | radius (x,y) <= 100 -> {m | h <- hue x y, s <- sat x y, cx <- x, cy <- y}
                             | (x >= 140 && x <= 160) && (y<=100 && y>=(-100)) -> {m | l <- light x y, sy <- y}
                             | otherwise -> m

update : Update -> State -> State
update lastPos m = 
    case lastPos of
        Mouse (x,y) -> changeColor x y m
        Click clickdown -> m

radius (x,y) = sqrt ((( x)^2) + (( y)^2))
hue x y = atan2 ( y) ( x)
sat x y = radius (x,y)/100
light x y = (( y)+100)/200  

------------------------------------------------------------------------------------------
{--DISPLAY--}

display : State -> Element
display mod = let {-- VARIABLES NEEDED FOR DISPLAY FUNCTION --}

                  -- format values that are to be displayed
                  hue' = toString <| round <| if | mod.h < 0 -> ((mod.h+(2*pi))*180/pi) 
                                                 | otherwise -> (mod.h*180/pi) 
                  sat' =  (slice 0 4 (toString mod.s))
                  satPercent = (toString (round (mod.s*100))) ++ "%"
                  light' =  (slice 0 4 (toString mod.l))
                  lightPercent = (toString (round (mod.l*100))) ++ "%"

                  -- title + subtitle
                  mainTitle = group [ text (fromString "HSL") |> scale 2 |> move (-50,250)
                                    , text (fromString "PICKING COLOURS BY HUE, SATURATION, AND LIGHTNESS") |> move (-50,220) |>  scale 0.90]

                  -- additional text 
                  hslLegend = group 
                    [ (text (fromString "Saturation = % of radius")) |> move (0,-110)
                    , (text (fromString "Hue = angle (in degrees)")) |> move (0,-125)
                    , text (fromString "Lightness (%)") |> move (150,-110)
                    ]
                  instruction = text <| fromString <| "Copy and paste the following code:"

                  -- additional images
                  highlight_background = toForm <| image 360 96 "http://i.imgur.com/AVp3no0.png"
                  adjust_image = toForm <| image 101 143 "http://i.imgur.com/o0uqK9O.png"
                  select_image = toForm <| image 121 110 "http://i.imgur.com/l8u28ip.png"
                  finalColour_image = toForm <| image 127 147 "http://i.imgur.com/5Xb35Ru.png"
                  highlight_image = toForm <| image 300 28 "http://i.imgur.com/2T4VwBg.png"
                  copy_marker1 = toForm <| image 98 32 "http://i.imgur.com/6N2E8q2.png"
                  copy_marker2 = toForm <| image 119 33 "http://i.imgur.com/v43ffae.png"

                  -- generate colour wheel and lightness bar             
--                  lst = List.concatMap (\ s -> List.map (\ h -> (h,s)) [0..360]) [0..100]
--                  shape (h,s) = move ((x h s ),(y h s)) <| filled (hsl (degrees h) (s/100) 0.5) <| circle 1
                  colourWheel_image = toForm (image 204 204 "http://i.imgur.com/AyFNQiG.png")
                  lst2 = [0..100]
                  shape2 l = move(150,-100+(2*l)) <| filled (hsl (mod.h) mod.s (l/100)) <| rect 20 2
                  x h s = s * (cos (degrees h))
                  y h s = s * (sin (degrees h))

                  -- Slider and Circle
                  sliderShape = outlined (solid black) (rect 30 10)
                  selectorCircle = group 
                      [ outlined (solid (hsl (mod.h+pi) mod.s 0.75)) (circle 5) |> move (mod.cx,mod.cy)
                      , outlined (solid (hsl (mod.h+pi) mod.s 0.75)) <| segment (0,0) (mod.cx,mod.cy)
                      ]

                  -- 3 different squares to view colours    
                  colourDisplayH =  filled (hsl (mod.h) 1 (0.5)) <| rect 50 30
                  colourDisplayHS =  rect 50 30 |> filled (hsl (mod.h) (mod.s) 0.5)
                  colourDisplayHSL = square 50 |> filled (hsl (mod.h) (mod.s) (mod.l))
                                    
                  -- display hue, sat, light values                                    
                  hueInfo = text (fromString ("HUE: " ++ hue')) |>  scale 0.9
                  hueCode = toForm <| container 300 20 middle (centered <| Text.height 12 <| monospace <| fromString <| ("(hsl (degrees "  ++ hue' ++ ")" ++ " 1 " ++ "0.5)"))                            
                  satInfo = text (fromString ("SAT: " ++ satPercent)) |>  scale 0.9
                  hueSatCode = toForm <| container 300 20 middle (centered <| Text.height 12 <| monospace <| fromString <| ("(hsl (degrees "  ++ hue' ++ ")" ++ " " ++ sat' ++ " " ++ "0.5)"))
                  lightInfo = text (fromString ("LIGHT: " ++ lightPercent)) |>  scale 0.9
                  hslCode = toForm <| container 300 20 middle (centered <| Text.height 12 <|monospace <| fromString <| ("(hsl (degrees "  ++ hue' ++ ")" ++ " " ++ sat' ++ " " ++ light' ++ ")"))

                  -- final hsl function
                  codeToCopy = toForm <| container 500 30 middle  (centered <| monospace <| Text.height 18 <| fromString <| ("(hsl (degrees "  ++ hue' ++ ")" ++ " " ++ sat' ++ " " ++ light' ++ ")"))                 

              in collage 600 600
                    
                    -- colour wheel + lightness
                    ([ colourWheel_image
                     , select_image |> move (100,-148)]
                    -- ((List.map shape lst)
                    ++ (List.map shape2 lst2)

                    -- Hue
                    ++ [ colourDisplayH |> move (-250,85)
                       , hueInfo |> move (-195,85)
                       
                    -- Hue + Sat
                       , colourDisplayHS |> move (-250,20)
                       , hueInfo |> move (-195,27.5)
                       , satInfo |> move (-195,12.5)
                                           
                    -- Hue + Sat + Light
                       , finalColour_image |> move (-235,-145)   
                       , colourDisplayHSL |> move (-250,-65)         
                       , hueInfo |> move (-195,-50)
                       , satInfo |> move (-195,-65)                    
                       , lightInfo |> move (-195,-80)
                       , sliderShape |> move (mod.sx,mod.sy)
                       , adjust_image |> move (220,60)
                       
                    -- Text
                       , highlight_background |> move (-50,235)
                       , mainTitle
                       , highlight_image |> move (-50,160)
                       , instruction |> move (-50,180)
                       , hslLegend 
                       , selectorCircle
                       , codeToCopy |> move (-50,160)
                       , copy_marker1 |> move (-250,160)
                       , copy_marker2 |> move (160,160)

                    -- Copyable code
                       , hueCode |> move (-205,60)
                       , hueSatCode |> move (-205,-5)
                       , hslCode |> move (-205,-100)

                       ]
                    )
      
------------------------------------------------------------------------------------------
{--MAIN--}

type Update = Mouse (Int,Int) | Click Bool

lastPos : Signal Update
lastPos = Signal.sampleOn (Signal.map Click Mouse.isDown) (Signal.map Mouse Mouse.position)

main : Signal Element
main = Signal.map display (Signal.foldp update model lastPos)
