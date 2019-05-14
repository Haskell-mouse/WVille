{-# LANGUAGE OverloadedStrings #-}

-- | Module with parsers for command line arguments and cmd help.

module ParseArgs(QTFD_type(..), opts, Opts(..), CalcDev(..)) where

import Data.Array.Accelerate.Math.WindowFunc
import Data.Semigroup ((<>))
import Data.Text as T
import Options.Applicative as O
default (T.Text)

data QTFD_type = WV | PWV | SPWV | CW | BJ | EMBD
type NoSubAVG = Bool
data Opts = OptsWV {getPath::FilePath, getDev :: CalcDev, subAVGflag :: NoSubAVG} |
            OptsPWV {getPath::FilePath, getDev::CalcDev,getWLength::Int, getWFunc::WindowFunc, subAVGflag::NoSubAVG} |
            OptsSPWV {getPath::FilePath, getDev::CalcDev, getWFLength::Int, getWFFunc::WindowFunc, getWTLength::Int, getWTFunc::WindowFunc, subAVGflag::NoSubAVG } |
            OptsBJ {getPath::FilePath, getDev::CalcDev, getAlpha::Float, getUWindow :: Int,
                     getUWinfFunc :: WindowFunc, getNWindow :: Int,getNWinFunc :: WindowFunc, subAVGflag :: NoSubAVG} |
            OptsCW {getPath::FilePath, getDev::CalcDev, getSigma::Float, getUWindow :: Int,
                     getUWinfFunc :: WindowFunc, getNWindow :: Int,getNWinFunc :: WindowFunc,getNormalised :: Bool, subAVGflag :: NoSubAVG} |
            OptsEMBD {getPath::FilePath, getDev::CalcDev, getAlpha::Float, getBeta :: Float, getUWindow :: Int, getNWindow :: Int, subAVGflag :: NoSubAVG}
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
       <|> subparser (command "BJ" $ info (optBJ <**> helper) (progDesc "Make Born-Jordan transform"))
       <|> subparser (command "EMBD" $ info (optEMBD <**> helper) (progDesc "Make Extended Modified Beta transform"))

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
 <*> uWindow
 <*> uWinfunc
 <*> nWindow
 <*> nWinfunc
 <*> normalise
 <*> no_subtract_avg

optEMBD :: O.Parser Opts
optEMBD = OptsEMBD
 <$> dataPath
 <*> gpu_cpu
 <*> alpha
 <*> beta
 <*> uWindow
 <*> nWindow
 <*> no_subtract_avg

optBJ :: O.Parser Opts
optBJ = OptsBJ
 <$> dataPath
 <*> gpu_cpu
 <*> alphaBJ
 <*> uWindow
 <*> uWinfunc
 <*> nWindow
 <*> nWinfunc
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

uWinfunc :: O.Parser WindowFunc
uWinfunc = option auto
  (   long "uWinfunc"
   <> short 'U'
   <> metavar "WinFunc"
   <> showDefault
   <> value Rect
   <> help "Select window function for time. Currently supporrted : Rect, Sin, Lanczos, Hanning, Hamming, Bartlett "
  )

nWinfunc :: O.Parser WindowFunc
nWinfunc = option auto
  (   long "nWinfunc"
   <> metavar "WinFunc"
   <> showDefault
   <> value Rect
   <> help "Select window function for frequency. Currently supporrted : Rect, Sin, Lanczos, Hanning, Hamming, Bartlett "
  )

normalise :: O.Parser Bool
normalise = option auto
  (   long "normalise"
   <> metavar "Normalise"
   <> showDefault
   <> value True
   <> help "If the kernel array should be normalised over mu not"
  )

no_subtract_avg :: O.Parser NoSubAVG
no_subtract_avg = switch
  (   long "no-subtract-average"
   <> short 'N'
   <> help "If enabled, then dont subtract average value from all elemets of column before Hilbert transform."
  )

sigma :: O.Parser Float
sigma = option auto
  (   long "sigma"
   <> short 'S'
   <> showDefault
   <> value 1.0
   <> help "Sigma constant for Choi-Williams distribution"
  )

alpha :: O.Parser Float
alpha = option auto
  (   long "alpha"
   <> short 'A'
   <> showDefault
   <> value 1.0
   <> help "Alpha constant for EMBD distribution"
  )

alphaBJ :: O.Parser Float
alphaBJ = option auto
  (   long "alpha"
   <> short 'A'
   <> showDefault
   <> value 1.0
   <> help "Alpha constant for BJ distribution"
  )

beta :: O.Parser Float
beta = option auto
  (   long "beta"
   <> short 'B'
   <> showDefault
   <> value 1.0
   <> help "Beta constant for EMBD distribution"
  )

uWindow :: O.Parser Int
uWindow = option auto
  (  long "uWindow"
  <> showDefault
  <> value 11
  <> help "window length along lag"
  )

nWindow :: O.Parser Int
nWindow = option auto
  (  long "nWindow"
  <> showDefault
  <> value 11
  <> help "window length along time"
  )
