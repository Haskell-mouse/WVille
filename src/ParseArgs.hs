{-# LANGUAGE OverloadedStrings #-}

-- | Module with parsers for command line arguments and cmd help.

module ParseArgs(QTFD_type(..), opts, Opts(..), CalcDev(..)) where

import Data.Array.Accelerate.Math.WindowFunc
import Data.Text as T
import Options.Applicative as O
import Data.Semigroup ((<>))
default (T.Text)

data QTFD_type = WV | PWV | SPWV | CW | CWW | BJ | BJW 
type NoSubAVG = Bool 
data Opts = OptsWV {getPath::FilePath, getDev :: CalcDev, subAVGflag :: NoSubAVG} | 
            OptsPWV {getPath::FilePath, getDev::CalcDev,getWLength::Int, getWFunc::WindowFunc, subAVGflag::NoSubAVG} | 
            OptsSPWV {getPath::FilePath, getDev::CalcDev, getWFLength::Int, getWFFunc::WindowFunc, getWTLength::Int, getWTFunc::WindowFunc, subAVGflag::NoSubAVG } | 
            OptsBJ {getPath::FilePath, getDev::CalcDev,subAVGflag :: NoSubAVG} | 
            OptsBJW {getPath::FilePath, getDev::CalcDev, getWLength::Int, getWFunc::WindowFunc, subAVGflag :: NoSubAVG} |
            OptsCW {getPath::FilePath, getDev::CalcDev, getQ::Double,subAVGflag :: NoSubAVG} | 
            OptsCWW {getPath::FilePath, getDev::CalcDev, getWLength::Int, getWFunc::WindowFunc, getQ::Double, subAVGflag :: NoSubAVG}
data CalcDev = CPU | GPU

-- | This function uses execParser function to do all work with catching cmd arguments
-- and parse it with parser that has been created by combine of simple parsers. 

opts :: IO Opts
opts = execParser $ info (optU <**> helper) (header "Make QTFD transform to all columns in all files in a given path")

-- | Parse function for cmd args. It just makes commands and combine it with helper. 

optU :: Parser Opts
optU = subparser (command "WV" $ info (optWV <**> helper) (progDesc "Make Wigner-Ville transform")) 
       <|> subparser (command "PWV" $ info (optPWV <**> helper) (progDesc "Make Pseudo Wigner-Ville transform")) 
       <|> subparser (command "CW" $ info (optCW <**> helper) (progDesc "Make Choi-Williams transform"))
       <|> subparser (command "CWW" $ info (optCWW <**> helper) (progDesc "Make windowed Choi-Willaims transform"))
       <|> subparser (command "BJ" $ info (optBJ <**> helper) (progDesc "Make Born-Jordan transform"))
       <|> subparser (command "BJW" $ info (optBJW <**> helper) (progDesc "Make windowed Born-Jordan transform"))

optWV :: O.Parser Opts
optWV = (OptsWV
  <$> dataPath
  <*> gpu_cpu
  <*> no_subtract_avg)

optPWV :: O.Parser Opts  
optPWV = (OptsPWV 
  <$> dataPath
  <*> gpu_cpu
  <*> twindow
  <*> winfunc
  <*> no_subtract_avg)

optCW :: O.Parser Opts 
optCW = OptsCW
 <$> dataPath
 <*> gpu_cpu
 <*> sigma
 <*> no_subtract_avg

optCWW :: O.Parser Opts  
optCWW = OptsCWW 
  <$> dataPath
  <*> gpu_cpu
  <*> twindow
  <*> winfunc
  <*> sigma
  <*> no_subtract_avg

optBJ :: O.Parser Opts 
optBJ = OptsBJ
 <$> dataPath
 <*> gpu_cpu
 <*> no_subtract_avg

optBJW :: O.Parser Opts  
optBJW = OptsBJW 
  <$> dataPath
  <*> gpu_cpu
  <*> twindow
  <*> winfunc
  <*> no_subtract_avg

dataPath :: O.Parser FilePath
dataPath = strOption
 (   long "data"
  <> short 'D'
  <> metavar "PATH"
  <> help "Path with files for Wigner-Ville distribution."
 )


gpu_cpu :: O.Parser CalcDev
gpu_cpu = flag CPU GPU
  (   long "gpu"  
   <> short 'G'
   <> help "Set GPU as main calculation device. Program should be compiled with llvm-PTX backend support."
  ) 

twindow :: O.Parser Int
twindow = option auto
  (   long "twindow"
   <> short 'T'
   <> metavar "length"
   <> showDefault
   <> value 1025
   <> help "Length of frequency smoothing window in time domain."
  )

winfunc :: O.Parser WindowFunc
winfunc = option auto 
  (   long "winfunc"
   <> short 'F'
   <> metavar "WinFunc"
   <> showDefault
   <> value Rect
   <> help "Select window function. Currently supporrted : Rect, Sin, Lanczos, Hanning, Hamming, Bartlett "
  )

no_subtract_avg :: O.Parser NoSubAVG
no_subtract_avg = switch
  (   long "no-subtract-average"
   <> short 'N'
   <> help "If enabled, then dont subtract average value from all elemets of column before Hilbert transform."
  )

sigma :: O.Parser Double
sigma = option auto
  (   long "sigma"
   <> short 'S'
   <> showDefault
   <> value 1
   <> help "Sigma constant for Choi-Williams distribution"
  )
