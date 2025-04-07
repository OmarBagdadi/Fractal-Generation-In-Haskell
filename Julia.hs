module Julia(printJuliaFractal
            ,getJuliaRows
            ,getJuliaCols
            ) where

import ColourOp
import Data.List (intercalate)

type RGBColour = (Integer,Integer,Integer)
type HSVColour = (Float,Float,Float)

zoomDistance = -1.5                                                         -- Stores the zoom distance to the fractal how far or close
juliaConst = [(-0.8, 0.156),(0,-0.8),(-0.4,0.6),(-0.835 ,-0.2321)]          -- Stores the different consts for the variations of the julia set
rows = 2000
cols = 2000
maxIterations = 100                                                         -- Stores the number of iterations done to check if the complex number is bounded or not

getJuliaRows :: Integer
getJuliaRows = rows

getJuliaCols :: Integer
getJuliaCols = cols

-- Function that takes in a value outputs a value that is the translation of the value from one range to another range
translate :: Integer -> Integer -> Integer -> Float -> Float -> Float
translate value fromMin fromMax toMin toMax = fromIntegral (value - fromMin) * (toMax - toMin) / fromIntegral (fromMax - fromMin) + toMin

{- Function that checks if the complex number is bounded in the specific version of the julia set or not 
if its bounded to the julia set it recurses for the maxIterations if its bounded by infinity it exits the function-}
checkBounding :: Float -> Float -> Integer -> Int -> Integer
checkBounding a b n var
    | n >= maxIterations = n
    | abs (a + b) > 4 = n                                                                               -- Check if bounded by infinity
    | otherwise = 
        let newA = a * a - b * b                                                                        -- Computes the next Zn which checks if its bounded
            newB = 2 * a * b
        in checkBounding (newA + fst (juliaConst!!var)) (newB + snd (juliaConst!!var)) (n+1) var

-- Function that calculates the RGB value of a specific pixel in order to form and colour the julia set 
calculatePoint :: Integer -> Integer -> Int -> RGBColour
calculatePoint x y var = 
    let a = translate y 0 cols zoomDistance (abs zoomDistance)
        b = translate x 0 rows zoomDistance (abs zoomDistance)
        n = checkBounding a b 0 var 
        hue = fromIntegral n / fromIntegral maxIterations                                               -- Uses the escape time colouring method
        brightness = if n < maxIterations then 1.0 else 0.0
    in if n == maxIterations then (0,0,0) else convertHSVtoRGB (hue, 1.0, brightness)

-- Function that creates the 2D array of pixels and applies the calculatePoint function on each point
createPixels :: Integer -> Integer -> Int -> [[RGBColour]]
createPixels width height var = [[calculatePoint x y var | y <- [0..rows-1]] | x <- [0..cols-1]]

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

-- printJulia Function formats the output string according to the bitmap pixel format
printJuliaFractal :: Integer -> Integer -> Int -> String
printJuliaFractal rows cols var =
    let format = "P3"
        dimensions = show rows ++ " " ++ show cols ++ " 255"
        pixels = printPixels (createPixels rows cols (var-1))
    in format ++ "\n" ++ dimensions ++ "\n" ++ pixels