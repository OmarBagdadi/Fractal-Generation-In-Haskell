import Julia
import Mandelbrot
import GHC.Base (IO)
import System.IO (hFlush, stdout)

-- All the questions and error messages
fractalQuestion = "Which fractal would you like to see\n1) Mandelbrot Set\n2) Julia Set"
variationQuestion = "Which variation of the julia set would you like to see\n1) Variation 1 (c = -0.8 + 0.156i)\n2) Variation 2 (c = -0.8i)\n3) Variation 3 (c = -0.4 + 0.6i)\n4) Variation 4 (c = -0.835 - 0.2321i)" 
inputError = "is an invalid value input!\nPlease select one of the options provided"
noinputError = "Please enter a value"
outputMessage = "Please wait while the fractal image is being generated\nETA around 1 min"

-- Main function
main :: IO ()
main = do
    question <- putStrLn fractalQuestion    -- Ask the initial Question
    input <- getLine                        -- Get the first response
    if input /= "" then checkInput input else putStrLn noinputError     -- process input

-- Function that checks the input and matches it with one of the options
checkInput :: String -> IO ()
checkInput a 
    | read a == 1 = do 
        let mandelbrotFile = "mandelbrotSet.pbm"
            contents = (printMandelbrotFractal getMandelbrotRows getMandelbrotCols)
        putStrLn outputMessage
        writeFile mandelbrotFile contents
    | read a == 2 = do
        question <- putStrLn variationQuestion
        input <- getLine
        if input /= "" then checkVariation input else putStrLn noinputError
    | otherwise = do 
        putStrLn ( a ++ " " ++ inputError)
        pauseCommand

-- Function that checks which variation of the julia set is picked and calls the printJuliaFractal with the variation number passed
checkVariation :: String -> IO ()
checkVariation a
    | read a == 1 = do
        let juliaFile = "JuliaSetVar1.pbm"
            contents = (printJuliaFractal getJuliaRows getJuliaCols (read a))
        putStrLn outputMessage
        writeFile juliaFile contents
    | read a == 2 = do
        let juliaFile = "JuliaSetVar2.pbm"
            contents = (printJuliaFractal getJuliaRows getJuliaCols (read a))
        putStrLn outputMessage
        writeFile juliaFile contents
    | read a == 3 = do
        let juliaFile = "JuliaSetVar3.pbm"
            contents = (printJuliaFractal getJuliaRows getJuliaCols (read a))
        putStrLn outputMessage
        writeFile juliaFile contents
    | read a == 4 = do
        let juliaFile = "JuliaSetVar4.pbm"
            contents = (printJuliaFractal getJuliaRows getJuliaCols (read a))
        putStrLn outputMessage
        writeFile juliaFile contents
    | otherwise = do 
        putStrLn ( a ++ " " ++ inputError)
        pauseCommand

-- Pauses the command line to see the error message
pauseCommand :: IO ()
pauseCommand = do
    putStrLn "Press any key to continue..."
    x <- getLine  -- Read a line of input (waiting for a key to be pressed)
    putStrLn x
    