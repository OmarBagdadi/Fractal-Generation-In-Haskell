module Mandelbrot(printMandelbrotFractal
            ,getMandelbrotRows
            ,getMandelbrotCols
            ) where

import ColourOp
import Data.List (intercalate)

type RGBColour = (Integer,Integer,Integer)
type HSVColour = (Float,Float,Float)
-- 
zoomX = (-2.00, 0.47)
zoomY = (-1.12, 1.12)
rows = 1000
cols = 1000
maxIterations = 100                                                         -- Stores the number of iterations done to check if the complex number is bounded or not
ca = 0
cb = 0

getMandelbrotRows :: Integer
getMandelbrotRows = rows

getMandelbrotCols :: Integer
getMandelbrotCols = cols

-- Function that takes in a value outputs a value that is the translation of the value from one range to another range
translate :: Integer -> Integer -> Integer -> Float -> Float -> Float
translate value fromMin fromMax toMin toMax = fromIntegral (value - fromMin) * (toMax - toMin) / fromIntegral (fromMax - fromMin) + toMin

{- Function that checks if the complex number is bounded in the specific version of the mandelbrot set or not 
if its bounded to the mandelbrot set it recurses for the maxIterations if its bounded by infinity it exits the function-}
checkBounding :: Float -> Float -> Integer -> Integer
checkBounding a b n
    | n == maxIterations = n
    | abs (a*a + b*b) >= 16 = n                                                                  -- Check if bounded by infinity
    | otherwise = 
        let newA = a * a - b * b
            newB = 2 * a * b
            ca = a
            cb = b
        in checkBounding (newA - ca) (newB - cb) (n+1)

-- Function that calculates the RGB value of a specific pixel in order to form and colour the mandelbrot set
calculatePoint :: Integer -> Integer -> RGBColour
calculatePoint x y =
    let a = translate x 0 rows (fst zoomX) (snd zoomX)
        b = translate y 0 cols (fst zoomY) (snd zoomY)
        n = checkBounding a b 0  
        hue = fromIntegral n / fromIntegral maxIterations                                        -- Uses the escape time colouring method
        brightness = if n < maxIterations then 1.0 else 0.0
    in if n == maxIterations then (0,0,0) else convertHSVtoRGB (hue, 1.0, brightness) 

-- Function that creates the 2D array of pixels and applies the calculatePoint function on each point
createPixels :: Integer -> Integer -> [[RGBColour]]
createPixels width height = [[calculatePoint y x | y <- [0..rows-1]] | x <- [0..cols-1]]

{- The printPixels function maps the printRow function to all [RGBColour] rows in the 2D array of [[RGBColour]] and 
 unlines adds a new line character to every row resulting in a string that is the RGB values of all the pixels
 spaced -}
printPixels :: [[RGBColour]] -> String
printPixels xs = unlines (map printRow xs)
  where
    {- printRow maps the printTuple function to every RGBColour tuple in the row and 
    intercalates a " " between each of the strings returned from the printTuple function -}
    printRow :: [RGBColour] -> String
    printRow row = intercalate " " (map printTuple row)
    
    -- printTuple uses the intercalate function to add a " " between the r, g and b values of the tuple
    printTuple :: RGBColour -> String
    printTuple (r, g, b) = intercalate " " [show r, show g, show b]

-- printMandelbrotFractal Function formats the output string according to the bitmap pixel format
printMandelbrotFractal :: Integer -> Integer -> String
printMandelbrotFractal rows cols =
    let format = "P3"
        dimensions = show rows ++ " " ++ show cols ++ " 255"
        pixels = printPixels (createPixels rows cols)
    in format ++ "\n" ++ dimensions ++ "\n" ++ pixels