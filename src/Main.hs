module Main where

import Controller
import Model
import View

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.IO

import Data.Time.Clock

main :: IO ()
main = do random <- getStdGen --can add custom pause message from file
          fileHandle <- openFile "scores.txt" ReadWriteMode
          x <- hGetContents' fileHandle
          tijd <- getCurrentTime
          hPutStr fileHandle (show tijd)
          hClose fileHandle
          playIO (InWindow "Asteroid" (400,400) (0, 0)) gameColor 30 (initialState random x) view inputHandler frameHandler   
    where 
        gameColor = light black


hGetContents' :: Handle -> IO String
hGetContents' h = do
          eof <- hIsEOF h
          if eof
            then
              return []
            else do
              c <- hGetChar h
              fmap (c:) $ hGetContents' h
