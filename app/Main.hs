{-# LANGUAGE CPP, OverloadedStrings, TypeOperators #-}

module Main where

import Control.Exception
import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Interpreter as ALI
import Data.Array.Accelerate.Math.Hilbert
import Data.Array.Accelerate.Math.Qtfd
import Data.Array.Accelerate.Math.WindowFunc
import Data.Attoparsec.Text
import ParseArgs
import ParseFile
import System.Directory
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native as ALN
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
import qualified Data.Array.Accelerate.LLVM.PTX as ALP
#endif
import Data.Array.Accelerate.Array.Sugar as S
import qualified Data.ByteString.Builder as BB
import Data.Complex
import qualified Data.Double.Conversion.Convertable as DC
import Data.List
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import qualified Data.Vector as V
import GHC.Float
import Math.Gamma
import System.IO


main :: IO ()
main = do
  args <- opts
  makeQTFDAll args

-- | Transform data to text(using Float conversion. It is much faster) and write it to file.

writeData ::
  Handle            -- ^ output file
  -> [Float]       -- ^ Data to write
  -> Int            -- ^ number of element
  -> Int            -- ^ elements per row
  -> IO ()
writeData _ [] _ _  = return ()
writeData file (x:xs) w n = do
  TI.hPutStr file $ DC.toPrecision 5 x <> " "
  if (n `mod` w) == 0
  then do
    TI.hPutStr file "\n"
    writeData file xs w (n + 1)
  else writeData file xs w (n + 1)

makeString ::
     Handle
  -> [Float]       -- ^ Data to write
  -> Int            -- ^ number of element
  -> Int            -- ^ elements per row
  -> BB.Builder
  -> IO ()
makeString _ [] _ _ _  = return ()
makeString file (x:xs) w n acc =
  if (n `mod` w) == 0
  then BB.hPutBuilder file (acc <> DC.toPrecision 5 x <> " " <> "\n") >> makeString file xs w (n+1) ""
  else makeString file xs w (n+1) (acc <> DC.toPrecision 5 x <> " ")

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
            let window = makeWindow wfun (A.unit $ A.constant wlen) :: A.Acc (A.Array A.DIM1 Float)
            mapM_ (makePWV dev window sAVGflag) fFiles
    (OptsWV path dev nosAVGflag) -> mapM_ (makeWV dev sAVGflag) fFiles
    (OptsCW path dev sigma uWindow uWindowFunc nWindow nWindowFunc normalise nosAVGflag) ->
      do
        if (even uWindow || even nWindow)
        then error "Window length maust be odd !"
        else
          let uWindowArray = makeWindow uWindowFunc (A.unit $ A.constant uWindow) :: A.Acc (A.Array A.DIM1 Float)
              nWindowArray = makeWindow nWindowFunc (A.unit $ A.constant nWindow) :: A.Acc (A.Array A.DIM1 Float)
              bothRectangular = uWindowFunc == Rect && nWindowFunc == Rect
          in mapM_ (makeCW dev sAVGflag sigma uWindowArray nWindowArray normalise bothRectangular) fFiles
    (OptsBJ path dev alpha uWindow uWindowFunc nWindow nWindowFunc nosAVGflag) ->
      do
        if (even uWindow || even nWindow)
        then error "Window length maust be odd !"
        else
          let uWindowArray = makeWindow uWindowFunc (A.unit $ A.constant uWindow) :: A.Acc (A.Array A.DIM1 Float)
              nWindowArray = makeWindow nWindowFunc (A.unit $ A.constant nWindow) :: A.Acc (A.Array A.DIM1 Float)
              bothRectangular = uWindowFunc == Rect && nWindowFunc == Rect
          in mapM_ (makeBJ dev sAVGflag alpha uWindowArray nWindowArray bothRectangular) fFiles
    (OptsEMBD path dev alpha beta uWindow uWindowFunc nWindow nWindowFunc normalise nosAVGflag) ->
      do
        if (even uWindow || even nWindow)
        then error "Window length maust be odd !"
        else let gammas = makeGammaArray beta uWindow
                 uWindowArray = makeWindow uWindowFunc (A.unit $ A.constant uWindow) :: A.Acc (A.Array A.DIM1 Float)
                 nWindowArray = makeWindow nWindowFunc (A.unit $ A.constant nWindow) :: A.Acc (A.Array A.DIM1 Float)
                 bothRectangular = uWindowFunc == Rect && nWindowFunc == Rect
             in mapM_ (makeEMBD dev sAVGflag alpha gammas uWindowArray nWindowArray bothRectangular normalise) fFiles
  return ()

makeCW :: CalcDev                   -- ^ Calculation device - CPU or GPU
  -> A.Exp Bool                     -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float                          -- ^ sigma
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if the core array should be normalised
  -> Bool                           -- ^ if Both windows are rectangular
  -> FilePath                       -- ^ Name of file with data.
  -> IO ()
makeCW dev sAVGflag sigma uWindowArray nWindowArray normalise bothRect file = do
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startCW dev file sAVGflag sigma uWindowArray nWindowArray normalise bothRect) dataF

makeBJ :: CalcDev                   -- ^ Calculation device - CPU or GPU
  -> A.Exp Bool                     -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float                          -- ^ sigma
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if Both windows are rectangular
  -> FilePath                       -- ^ Name of file with data.
  -> IO ()
makeBJ dev sAVGflag alpha uWindowArray nWindowArray bothRect file = do
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startBJ dev file sAVGflag alpha uWindowArray nWindowArray bothRect) dataF

-- | Make Pseudo Wigner-Ville transform to all columns in the given file

makePWV ::
  CalcDev                            -- ^ Calculation device - CPU or GPU
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
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

--makeEMBD dev alpha gammas uWindowArray nWindowArray bothRectangular normalise 

makeEMBD :: CalcDev                   -- ^ Calculation device - CPU or GPU
  -> A.Exp Bool                     -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float                          -- ^ alpha
  -> [Float]                        -- ^ gammas 
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if the core array should be normalised
  -> Bool                           -- ^ if Both windows are rectangular
  -> FilePath                       -- ^ Name of file with data.
  -> IO ()
makeEMBD dev sAVGflag alpha gammas uWindowArray nWindowArray normalise bothRect file = do
  putStrLn $ "processing " ++ file ++ "..."
  text <- TI.readFile file
  let parseRes = parseOnly parseFile text
  case parseRes of
    (Left errStr) -> error $ errStr
    (Right dataF) -> mapM_ (startEMBD dev file sAVGflag alpha gammas uWindowArray nWindowArray normalise bothRect) dataF

-- | Make Pseudo Wigner-Ville transform to the given column.
-- At first it makes subtraction of average value (if supAVGflag) and applies hilbert transform to make analytic signal. After that it applies Pseudo-Wigner transftorm
-- and save data to file with


startPWV ::
  CalcDev                          -- ^ Calculation device - CPU or GPU
  -> A.Acc (A.Array A.DIM1 Float) -- ^ Smoothing window in time-domain. Length must be odd.
  -> FilePath                      -- ^ Name of file with data.
  -> A.Exp Bool                    -- ^ Apply subtraction of the mean value from all elements in a column.
  -> (T.Text,[Float])             -- ^ Name of column and parsed data from column
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
  -> (T.Text,[Float])    -- ^ Name of column and parsed data from column
  -> IO ()
startWV dev oldName sAVGflag (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      pData = A.fromList (A.Z A.:. leng) dataF
      appWV = wignerVilleNew . hilbert . supAVG sAVGflag
      appWVGPU = wignerVille . hilbert . supAVG sAVGflag
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
  hSetBinaryMode file True >> hSetBuffering file LineBuffering
  -- let resultBsBuilder = makeString pList leng 1 ""
  onException (makeString file pList leng 1 "") (removeFile newFName) >> hFlush file
  hClose file
  putStrLn "Done !"

startCW ::
  CalcDev                 -- ^ Calculation device - CPU or GPU
  -> FilePath             -- ^ Name of file with data.
  -> A.Exp Bool           -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float               -- ^ Sigma
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if the core array should be normalised
  -> Bool                           -- ^ if Both windows are rectangular
  -> (T.Text,[Float])    -- ^ Name of column and parsed data from column
  -> IO ()
startCW dev oldName sAVGflag sigma uWindowArray nWindowArray normalise bothRect (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      uWindow = A.length uWindowArray
      nWindow = A.length nWindowArray
      mWindowArrays = if bothRect then Nothing else Just (uWindowArray,nWindowArray)
      pData = A.fromList (A.Z A.:. leng) dataF
      appCW = (choiWilliams (A.constant sigma) mWindowArrays uWindow nWindow normalise) . hilbert . supAVG sAVGflag
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

startBJ ::
  CalcDev                 -- ^ Calculation device - CPU or GPU
  -> FilePath             -- ^ Name of file with data.
  -> A.Exp Bool           -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float               -- ^ Sigma
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if Both windows are rectangular
  -> (T.Text,[Float])    -- ^ Name of column and parsed data from column
  -> IO ()
startBJ dev oldName sAVGflag alpha uWindowArray nWindowArray bothRect (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      uWindow = A.length uWindowArray
      nWindow = A.length nWindowArray
      mWindowArrays = if bothRect then Nothing else Just (uWindowArray,nWindowArray)
      pData = A.fromList (A.Z A.:. leng) dataF
      appCW = (bornJordan (A.constant alpha) mWindowArrays uWindow nWindow) . hilbert . supAVG sAVGflag
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

startEMBD ::
  CalcDev                 -- ^ Calculation device - CPU or GPU
  -> FilePath             -- ^ Name of file with data.
  -> A.Exp Bool           -- ^ Apply subtraction of the mean value from all elements in a column.
  -> Float                -- ^ Alpha 
  -> [Float]              -- ^ Gammas 
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in frequensy-domain. Length must be odd.
  -> A.Acc (A.Array A.DIM1 Float)   -- ^ Smoothing window in time-domain. Length must be odd.
  -> Bool                           -- ^ if the core array should be normalised
  -> Bool                           -- ^ if Both windows are rectangular
  -> (T.Text,[Float])    -- ^ Name of column and parsed data from column
  -> IO ()
startEMBD dev oldName sAVGflag alpha gammas uWindowArray nWindowArray normalise bothRect (column_name,dataF) = do
  let newFName = oldName ++ "-" ++ T.unpack column_name ++ ".txt"
  putStr $ "   Creating file " ++ newFName ++ " ..."
  let leng = length dataF
      uWindow = A.length uWindowArray
      nWindow = A.length nWindowArray
      gammasArr = A.use $ A.fromList (A.Z A.:. (length gammas)) gammas 
      mWindowArrays = if bothRect then Nothing else Just (uWindowArray,nWindowArray)
      pData = A.fromList (A.Z A.:. leng) dataF
      appEMBD = (eModifiedB (A.constant alpha) gammasArr mWindowArrays uWindow nWindow normalise) . hilbert . supAVG sAVGflag
      processed = case dev of
#ifdef ACCELERATE_LLVM_NATIVE_BACKEND
                    CPU -> ALN.run1 appEMBD pData
#endif
#ifdef ACCELERATE_LLVM_PTX_BACKEND
                    GPU -> ALP.run1 appEMBD pData
#endif
                    CPU -> ALI.run1 appEMBD pData
                    GPU -> error "Compiled without GPU support"
      pList = S.toList $  processed
  file <- openFile newFName WriteMode
  onException (writeData file pList leng 1) (removeFile newFName)
  hClose file
  putStrLn "Done !"


-- | Subtraction of average value from all elements of column

supAVG :: A.Exp Bool -> A.Acc (Array DIM1 Float) -> A.Acc (Array DIM1 Float)
supAVG flag arr =
  A.acond flag (A.map (\x -> x - avg) arr) arr
  where leng = A.length arr
        avg = (A.the $ A.sum arr)/(A.fromIntegral leng)


makeGammaArray :: Float -> Int -> [Float]
makeGammaArray beta wLength =
  let dwLength = fromIntegral wLength :: Float
      v = [(-0.5), ((-0.5) + 1.0/dwLength)..0.5]
      s = map (\x -> beta :+ pi*x) v
      f = map (\x -> abs $ ((magnitude $ gamma x) ^ 2)/((gamma beta) ^ 2)) s
      n = floor $ (fromIntegral $ length f)/2.0
      frst = Data.List.take (n-1) f
      secnd  = Data.List.drop n f
  in secnd ++ frst
