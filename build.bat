@echo off

del /S bin\*.hi
del /S bin\*.o
del /S images\*.pbm

ghc -outputdir bin -o Main Main.hs

pause