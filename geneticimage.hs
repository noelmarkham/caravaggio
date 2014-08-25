{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module GeneticImage where

import Diagrams.Prelude 
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.List
import Data.Colour
import Data.Colour.SRGB
import Data.Foldable (fold)
import Data.Bits.Bitwise (fromListBE)
import Data.Word
import Control.Monad
import Control.Monad.State.Lazy

import Genetics

class GeneticImage a where
  generateImage :: Chromasome -> a
  persistImage :: FilePath -> Double -> Double -> a -> IO ()
  imageValue :: a -> a -> IO Double
  fromDisk :: FilePath -> IO (Either String a)



instance GeneticImage (Diagram Cairo R2) where
  generateImage chromasome = fst $ runState fullDiagram chromasome
  persistImage path x y = renderCairo path (Dims x y) 
  imageValue = cmp
  fromDisk path = do
              dImageEither <- loadImageExt path
              return $ fmap image dImageEither
              

triangleCount :: Int
triangleCount = 200

bitCount :: Int
bitCount = (((7 * 32) + (3 * 8)) * triangleCount) + (3 * 32)

drawTriangle :: Angle -> 
                Angle -> 
                Double -> 
                Double -> 
                Colour Double -> 
                R2 -> 
                Angle -> 
                Diagram Cairo R2
drawTriangle a1 a2 l1 l2 c t ro = polygon (with & polyType .~ PolySides [a1, a2][l1, l2]) # 
                                  rotate ro #
                                  translate t # 
                                  opacity 0.5 # 
                                  fc c # 
                                  lw none

integerParameter :: State [Bool] Int
integerParameter = do
                 bools <- get
                 let (forInteger, rest) = splitAt 32 bools
                 put rest
                 return $ fromListBE forInteger

word8Parameter :: State [Bool] Word8
word8Parameter = do
                bools <- get
                let (forWord, rest) = splitAt 8 bools
                put rest
                return $ fromListBE forWord

triangleFromBools :: State [Bool] (Diagram Cairo R2)
triangleFromBools = triangleFromParams
       <$> fmap fromIntegral integerParameter
       <*> fmap fromIntegral integerParameter
       <*> fmap fromIntegral integerParameter
       <*> fmap fromIntegral integerParameter
       <*> word8Parameter
       <*> word8Parameter
       <*> word8Parameter
       <*> fmap fromIntegral integerParameter
       <*> fmap fromIntegral integerParameter
       <*> fmap fromIntegral integerParameter
     where triangleFromParams a1 a2 l1 l2 r g b tx ty ro = drawTriangle (a1 @@ deg) (a2 @@ deg) l1 l2 (sRGB24 r g b) (r2 (tx, ty)) (ro @@ deg)

fullDiagram :: State [Bool] (Diagram Cairo R2)
fullDiagram = do
               redC <- fmap fromIntegral integerParameter
               greenC <- fmap fromIntegral integerParameter
               blueC <- fmap fromIntegral integerParameter
               triangles <- replicateM triangleCount triangleFromBools
               return $ fold triangles # bg (sRGB24 redC greenC blueC)

cmp :: Diagram Cairo R2 -> Diagram Cairo R2 -> IO Double
cmp base img = do
           baseAlphaColours <- renderToList 100 100 base
           imgAlphaColours <- renderToList 100 100 img
           let difference = imgDiff baseAlphaColours imgAlphaColours
           putStrLn $ "diff: " ++ show difference
           return difference

imgDiff :: [[AlphaColour Double]] -> [[AlphaColour Double]] -> Double
imgDiff img1 img2 = abs $ sum $ zipWith diffPix (concat img1) (concat img2)

diffPix :: AlphaColour Double -> AlphaColour Double -> Double
diffPix a1 a2 = fullDiff
            where normNaN v = if (isNaN v) then 0 else v
                  redF a = normNaN $ channelRed $ toSRGB (a `over` black)
                  greenF a = normNaN $ channelGreen $ toSRGB (a `over` black)
                  blueF a = normNaN $ channelBlue $ toSRGB (a `over` black)
                  diffRed = redF a1 - redF a2
                  diffGreen = greenF a1 - greenF a2
                  diffBlue = blueF a1 - blueF a2
                  fullDiff = (diffRed * diffRed) - (diffGreen * diffGreen) - (diffBlue * diffBlue)
                  err = show $ "Colour: " ++ show a1 ++ " red a1: " ++ show (redF a1) ++ " red a2: " ++ show (redF a2) ++ " diffRed: " ++ show diffRed ++ " diffGreen: " ++ show diffGreen ++ " diffBlue: " ++ show diffBlue
