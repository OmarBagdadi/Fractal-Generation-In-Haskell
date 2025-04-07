module ColourOp
(
convertHSVtoRGB
) where

import Data.Fixed

type RGBColour = (Integer,Integer,Integer)      -- RGBColour three tuple value used to store RGB colours between 0-255
type HSVColour = (Float,Float,Float)            -- HSVColour three tuple value used to store Hue Saturation Brightness colours between 0.0-1.0

-- Function that checks where the h' value is and returns the corresponding order for (R1,G1,B1)
checkHue :: (Float, Float, Float) -> (Float, Float, Float)
checkHue (hue, x, c)
    | hue >= 0 && hue < 60 = (c, x, 0)
    | hue >= 60 && hue < 120 = (x, c, 0)
    | hue >= 120 && hue < 180 = (0, c, x)
    | hue >= 180 && hue < 240 = (0, x, c)
    | hue >= 240 && hue < 300 = (x, 0, c)
    | otherwise = (c, 0, x)

-- Conversion is based off the equation shown in this wikipedia page https://en.wikipedia.org/wiki/HSL_and_HSV#HSV_to_RGB
convertHSVtoRGB :: HSVColour -> RGBColour
convertHSVtoRGB (h, s, v) =
  let
    -- Adding an arbitrary constant(0.455) to the hue to change its colour along the spectrum you can mess around with this const to see the fractal in different colours
    hue = (h + 0.455) * 360
    c = v * s
    x = c * (1 - abs ((hue / 60) `mod'` 2 - 1))
    m = v - c
    (r1, g1, b1) = checkHue (hue,x,c)
  in
    ( round ((r1 + m) * 255)
    , round ((g1 + m) * 255)
    , round ((b1 + m) * 255)
    )