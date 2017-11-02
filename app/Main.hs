{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import ParseArgs
import ParseFile
import Data.Array.Accelerate.Math.Wigner
import Data.Array.Accelerate.Math.Hilbert
import Data.Array.Accelerate.Math.WindowFunc
import Data.Attoparsec.Text
import System.Directory
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as ALI
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native as ALN
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX as ALP
#endif
import Data.Array.Accelerate.Array.Sugar as S 
import Data.Monoid
import System.IO
import GHC.Float
import Data.List
import qualified Data.Text.IO as TI
import qualified Data.Double.Conversion.Text as DT 
import qualified Data.Text as T
import qualified Data.Vector as V


main :: IO ()
main = do
  args <- opts
  case args of 
       (OptsWV _ _ _) -> makeWVall args
       (OptsPWV _ _ _ _ _) -> makePWVall args
  
writeString :: Handle -> [Double] -> Int -> Int -> IO ()
writeString _ [] _ _  = return ()
writeString file (x:xs) w n = do 
  TI.hPutStr file $ DT.toPrecision 5 x <> " "
  if (n `mod` w) == 0 
  then do 
    TI.hPutStr file "\n"
    writeString file xs w (n + 1)
  else writeString file xs w (n + 1)

-- | make Pseudo Wigner-Ville transform to all files in a directory. 

makePWVall :: Opts -> IO ()
makePWVall (OptsPWV path dev wlen wfun nosAVGflag) = do
  if (even wlen)
  then error "Window length maust be odd !!"
  else do  
    files <- listDirectory path
    setCurrentDirectory path 
    let fFiles = filter (isSuffixOf ".txt") files
    let window = makeWindow wfun (A.unit $ A.constant wlen) :: A.Acc (A.Array A.DIM1 Double)
        sAVGflag = A.constant $ not nosAVGflag
    mapM_ (makePWV dev window sAVGflag) fFiles

-- | Make Pseudo Wigner-Ville transform to all columns in the given file

makePWV :: 
  CalcDev -- ^ Calculation device - CPU or GPU
  -> A.Acc (A.Array A.DIM1 Double) -- ^ Smoothing window in time-domain. Length must be odd.
  -> A.Exp Bool -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> FilePath -- ^ Name of file with data.
  -> IO ()
makePWV dev window sAVGflag file = do 
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file 
  let parseRes = parseOnly parseFile text 
  case parseRes of 
    (Left errStr) -> error $ errStr 
    (Right dataF) -> mapM_ (startPWV dev window file sAVGflag) dataF 

-- | Make Pseudo Wigner-Ville transform to the given column. 
-- At first it applies hilbert transform to make analytic signal. After that it applies Pseudo-Wigner transftorm
-- and save data to file with 

startPWV :: CalcDev -> A.Acc (A.Array A.DIM1 Double) -> FilePath -> A.Exp Bool -> (T.Text,[Double]) -> IO ()
startPWV dev window oldName sAVGflag (file,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack file ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      processed = case dev of
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 ((pWignerVille window) . hilbert . supAVG sAVGflag) pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 ((pWignerVille window) . hilbert . supAVG sAVGflag) pData
#endif
                    CPU -> ALI.run1 ((pWignerVille window) . hilbert . supAVG sAVGflag) pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $ processed
  file <- openFile newFName WriteMode
  writeString file pList leng 1
  hClose file
  putStrLn "Done !"

makeWVall :: Opts -> IO ()
makeWVall (OptsWV path dev nosAVGflag) = do 
  files <- listDirectory path
  setCurrentDirectory path 
  let fFiles = filter (isSuffixOf ".txt") files
      sAVGflag = A.constant $ not nosAVGflag
  mapM_ (makeWV dev sAVGflag) fFiles
       

makeWV :: CalcDev -> A.Exp Bool -> FilePath -> IO ()
makeWV dev sAVGflag file = do 
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of 
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startWV dev file sAVGflag) dataF
  
startWV :: CalcDev -> FilePath -> A.Exp Bool -> (T.Text,[Double]) -> IO ()
startWV dev oldName sAVGflag (file,dataF) = do 
  let newFName = oldName ++ "-" ++ T.unpack file ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      processed = case dev of 
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 (wignerVille . hilbert . supAVG sAVGflag) pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 (wignerVille . hilbert . supAVG sAVGflag) pData
#endif
                    CPU -> ALI.run1 (wignerVille . hilbert . supAVG sAVGflag) pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $  processed
  file <- openFile newFName WriteMode
  writeString file pList leng 1
  hClose file
  putStrLn "Done !"

supAVG :: A.Exp Bool -> A.Acc (Array DIM1 Double) -> A.Acc (Array DIM1 Double)
supAVG flag arr = 
  A.acond flag (A.map (\x -> x - avg) arr) arr
  where leng = A.length arr
        avg = (A.the $ A.sum arr)/(A.fromIntegral leng)