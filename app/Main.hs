{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import ParseArgs
import ParseFile
import Data.Array.Accelerate.Math.Qtfd
import Data.Array.Accelerate.Math.Hilbert
import Data.Array.Accelerate.Math.WindowFunc
import Data.Attoparsec.Text
import Control.Exception
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
  makeQTFDAll args

-- | Transform data to text(using double conversion. It is much faster) and write it to file. 

writeData :: 
  Handle            -- ^ output file 
  -> [Double]       -- ^ Data to write
  -> Int            -- ^ number of element   
  -> Int            -- ^ elements per row
  -> IO ()         
writeData _ [] _ _  = return ()
writeData file (x:xs) w n = do 
  TI.hPutStr file $ DT.toPrecision 5 x <> " "
  if (n `mod` w) == 0 
  then do 
    TI.hPutStr file "\n"
    writeData file xs w (n + 1)
  else writeData file xs w (n + 1)

-- | make Pseudo Wigner-Ville transform to all files in a directory. 

makeQTFDAll :: Opts -> IO ()
makeQTFDAll opts = do 
  let path = getPath opts
      nosAVGflag = subAVGflag opts
  files <- listDirectory path
  setCurrentDirectory path 
  let fFiles = filter (isSuffixOf ".txt") files
      sAVGflag = A.constant $ not nosAVGflag
  case opts of 
    (OptsPWV _ dev wlen wfun _) -> 
      do 
        if (even wlen)
        then error "Window length maust be odd !"
        else 
          do 
            let window = makeWindow wfun (A.unit $ A.constant wlen) :: A.Acc (A.Array A.DIM1 Double)
            mapM_ (makePWV dev window sAVGflag) fFiles
    (OptsWV path dev nosAVGflag) -> mapM_ (makeWV dev sAVGflag) fFiles
    (OptsCW path dev sigma nosAVGflag) -> mapM_ (makeCW dev sAVGflag sigma) fFiles
    (OptsCWW path dev wlen wfun sigma nosAVGflag) -> 
      do 
        if (even wlen)
        then error "Window length maust be odd !"
        else 
          do 
            let window = makeWindow wfun (A.unit $ A.constant wlen) :: A.Acc (A.Array A.DIM1 Double)
            mapM_ (makeCWW dev window sAVGflag sigma) fFiles
  return ()

makeCW :: CalcDev        -- ^ Calculation device - CPU or GPU 
  -> A.Exp Bool  -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> Double       -- ^ sigma
  -> FilePath    -- ^ Name of file with data.
  -> IO ()
makeCW dev sAVGflag sigma file = do
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of 
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startCW dev file sAVGflag sigma) dataF

makeCWW :: CalcDev                   -- ^ Calculation device - CPU or GPU
  -> A.Acc (A.Array A.DIM1 Double)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> A.Exp Bool                      -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> Double                          -- ^ Sigma parameter 
  -> FilePath                        -- ^ Name of file with data.
  -> IO ()
makeCWW dev window sAVGflag sigma file = do 
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file 
  let parseRes = parseOnly parseFile text 
  case parseRes of 
    (Left errStr) -> error $ errStr 
    (Right dataF) -> mapM_ (startCWW dev window file sAVGflag sigma) dataF 

-- | Make Pseudo Wigner-Ville transform to all columns in the given file

makePWV :: 
  CalcDev                            -- ^ Calculation device - CPU or GPU
  -> A.Acc (A.Array A.DIM1 Double)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> A.Exp Bool                      -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> FilePath                        -- ^ Name of file with data.
  -> IO ()
makePWV dev window sAVGflag file = do 
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file 
  let parseRes = parseOnly parseFile text 
  case parseRes of 
    (Left errStr) -> error $ errStr 
    (Right dataF) -> mapM_ (startPWV dev window file sAVGflag) dataF 

-- | Make Pseudo Wigner-Ville transform to the given column. 
-- At first it makes subtraction of average value (if supAVGflag) and applies hilbert transform to make analytic signal. After that it applies Pseudo-Wigner transftorm
-- and save data to file with


startPWV :: 
  CalcDev                          -- ^ Calculation device - CPU or GPU 
  -> A.Acc (A.Array A.DIM1 Double) -- ^ Smoothing window in time-domain. Length must be odd.
  -> FilePath                      -- ^ Name of file with data.
  -> A.Exp Bool                    -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> (T.Text,[Double])             -- ^ Name of column and parsed data from column
  -> IO ()
startPWV dev window oldName sAVGflag (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      appPWV = (pWignerVille window) . hilbert . supAVG sAVGflag
      processed = case dev of
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 appPWV pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 appPWV pData
#endif
                    CPU -> ALI.run1 appPWV pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $! processed
  file <- openFile newFName WriteMode
  onException (writeData file pList leng 1) (removeFile newFName)
  hClose file
  putStrLn "Done !"

-- | Make Wigner-Ville transform to all columns in the given file       

makeWV :: 
  CalcDev        -- ^ Calculation device - CPU or GPU 
  -> A.Exp Bool  -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> FilePath    -- ^ Name of file with data.
  -> IO ()
makeWV dev sAVGflag file = do 
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of 
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startWV dev file sAVGflag) dataF

-- | Make Wigner-Ville transform to the given column. 
-- At first it makes subtraction of average value (if supAVGflag) and applies hilbert transform to make analytic signal. After that it applies Pseudo-Wigner transftorm
-- and save data to file with
  
startWV :: 
  CalcDev                 -- ^ Calculation device - CPU or GPU 
  -> FilePath             -- ^ Name of file with data.
  -> A.Exp Bool           -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> (T.Text,[Double])    -- ^ Name of column and parsed data from column
  -> IO ()
startWV dev oldName sAVGflag (column_name,dataF) = do 
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      appWV = wignerVille . hilbert . supAVG sAVGflag
      processed = case dev of 
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 appWV pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 appWV pData
#endif
                    CPU -> ALI.run1 appWV pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $  processed
  file <- openFile newFName WriteMode
  onException (writeData file pList leng 1) (removeFile newFName)
  hClose file
  putStrLn "Done !"

startCW :: 
  CalcDev                 -- ^ Calculation device - CPU or GPU 
  -> FilePath             -- ^ Name of file with data.
  -> A.Exp Bool           -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Double               -- ^ Sigma
  -> (T.Text,[Double])    -- ^ Name of column and parsed data from column
  -> IO ()
startCW dev oldName sAVGflag sigma (column_name,dataF) = do 
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      appCW = ((flip choiWilliams) (A.constant sigma)) . hilbert . supAVG sAVGflag
      processed = case dev of 
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 appCW pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 appCW pData
#endif
                    CPU -> ALI.run1 appCW pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $  processed
  file <- openFile newFName WriteMode
  onException (writeData file pList leng 1) (removeFile newFName)
  hClose file
  putStrLn "Done !"

startCWW :: 
  CalcDev                          -- ^ Calculation device - CPU or GPU 
  -> A.Acc (A.Array A.DIM1 Double) -- ^ Smoothing window in time-domain. Length must be odd.
  -> FilePath                      -- ^ Name of file with data.
  -> A.Exp Bool                    -- ^ Apply subtraction of the mean value from all elements in a column. 
  -> Double                        -- ^ Sigma
  -> (T.Text,[Double])             -- ^ Name of column and parsed data from column
  -> IO ()
startCWW dev window oldName sAVGflag sigma (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      appCWW = (choiWilliams_w window (A.constant sigma)) . hilbert . supAVG sAVGflag
      processed = case dev of
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 appCWW pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 appCWW pData
#endif
                    CPU -> ALI.run1 appCWW pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $! processed
  file <- openFile newFName WriteMode
  onException (writeData file pList leng 1) (removeFile newFName)
  hClose file
  putStrLn "Done !"

-- | Subtraction of average value from all elements of column

supAVG :: A.Exp Bool -> A.Acc (Array DIM1 Double) -> A.Acc (Array DIM1 Double)
supAVG flag arr = 
  A.acond flag (A.map (\x -> x - avg) arr) arr
  where leng = A.length arr
        avg = (A.the $ A.sum arr)/(A.fromIntegral leng)