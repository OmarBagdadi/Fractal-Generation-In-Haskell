@echo off

del /S bin\*.hi
del /S bin\*.o

ghc -outputdir bin -o docs/Main Main.hs

pause