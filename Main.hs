{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

-- intentionally left dirty

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.Char (toLower)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as L
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Word

import System.Directory

import Data.SVD
import Data.Bits.Pretty
import Text.XML.HXT.Core hiding (when, trace)

import Debug.Trace

type Address = Int

data Value = Hex Int | Dec Int
  -- msb lsb
  | BitRange Int Int | Reg AccessType Address | WhoKnows ByteString
  deriving (Eq, Show)

data Out = Comment ByteString | IfDef ByteString | Define ByteString Value | Junk
  deriving (Eq, Show)

parseFile = lineParser `sepBy` endOfLine

lineParser :: Parser Out
lineParser = defineParser <|> commentParser

commentParser = (string "// " *> (Comment <$> takeWhile (/='\n')))
ifdefs = ((string "#idfef") <|> (string "#ifndef")) *> (IfDef <$> takeWhile (/='\n'))

defineParser :: Parser Out
defineParser = do
  skipSpace
  string "#define"
  space
  key <- takeWhile1 (not . isSpace)
  skipSpace
  val <- parseVal
  takeWhile (==' ')
  return $ Define key val

parseVal :: Parser Value
parseVal = parseHWReg
  <|> (string "0x" *> (Hex <$> hexadecimal))
  <|> (BitRange <$> (decimal <* char ':') <*> decimal)
  <|> (Dec <$> decimal)
  <|> (WhoKnows <$> takeWhile (/='\n'))

parseHWReg = do
  string "HW_REGISTER_"
  mode <- (string "RW" *> pure ReadWrite) <|> (string "RO" *> pure ReadOnly)
  char '('
  space
  string "0x"
  addr <- hexadecimal
  space
  char ')'
  return $ Reg mode addr
  --val <- takeWhile1 (not . isSpace)

hexVal :: Parser Int
hexVal = string "0x" *> hexadecimal

parseDefineEnding valParser with = do
  skipSpace
  string "#define"
  space
  name <- takeWhile1 (/=' ')
  guard (with `B.isSuffixOf` ("_" <> name))
  skipSpace
  base <- valParser
  takeWhile (==' ')
  endOfLine
  return (fromJust $ B.stripSuffix ("_" <> with) $ name, base)

hexDefineEnds with = parseDefineEnding (hexVal <|> hexadecimal) with
decDefineEnds with = parseDefineEnding decimal with

parsePeriph f = do
  many (commentParser <|> ifdefs <|> (pure Junk <$> endOfLine))
  mpass <- optional $ hexDefineEnds "PASSWORD"
  (name, base) <- hexDefineEnds "BASE"
  mapb_id <- optional $ hexDefineEnds "APB_ID"

  regs <- many1 (parseReg name base)
  return $ defaultPeripheral {
      periphName = B.unpack name
    , periphDescription = "Derived from " ++ f
        ++ (maybe "" (\(_, pass) -> (" password: " ++ showHex32 pass)) mpass)
        ++ (maybe "" (\(_, apb_id) -> (" apb_id: " ++ showHex32 apb_id)) mapb_id)
    , periphBaseAddress = base
    , periphRegisters = regs
    }

parseReg p base = do
  string $ "#define " <> p <> "_"
  name <- takeWhile (/=' ')
  skipSpace
  (Reg mode addr) <- parseHWReg
  _mask <- optional $ hexDefineEnds "MASK"
  width <- optional $ decDefineEnds "WIDTH"
  reset <- optional $ hexDefineEnds "RESET"

  fields <- many $ parseField (p <> "_" <> name)

  let reg = widthMay width
              $ defaultRegister {
                regName = B.unpack name
              , regAddressOffset = addr - base
              , regAccess = mode
              , regFields = fields
              , regResetValue = snd <$> reset
              }
      regReserved = reg { regFields = reverse $ L.sortBy (comparing fieldBitOffset) $ procFields reg}
      regOk = continuityCheck regReserved

  {-- DEBUG buggy register fields
  unless regOk $ do
    trace (show ("Buggy register", p, name)) $ return ()
    flip mapM_ (regFields regReserved) $ \f -> trace (shortField f) $ return ()
  --}

  return $ regReserved { regDescription = if regOk then "" else "Buggy fields detected" }

  where widthMay Nothing r = r { regSize = 32 }
        widthMay (Just (_, w)) r = r { regSize = w }

parseField pr =do
  b <- parseFieldBits pr
  many (do
    Define key val <- (defineParser <* endOfLine)
    guard ((not $ "BITS" `B.isSuffixOf` key) && (pr `B.isPrefixOf` key))
    --trace (show (pr, key, val)) $ return ()
    )
  return b
parseFieldBits pr = do
  skipSpace
  string $ "#define " <> pr <> "_"

  name <- takeWhile (/=' ')
  guard ("BITS" `B.isSuffixOf` name)
  skipSpace
  msb <- decimal
  char ':'
  lsb <- decimal
  endOfLine
  return $ defaultField {
      fieldName = B.unpack $ fromJust $ B.stripSuffix ("_BITS") name
    --, fieldDescription = B.unpack val
    , fieldBitOffset = lsb
    , fieldBitWidth = (msb - lsb + 1)
    }

rpi = defaultDevice {
    deviceName = "RasbperryPi"
  , deviceVersion = "1"
  , deviceDescription = "broadcom/bcm2708_chip/*.h based SVD"
  , deviceWidth = 32
  }

main = do
  ps <- map snd <$> goall

  let d = rpi { devicePeripherals = ps }

  runX (root [] [toSVD d] >>> writeDocument [withIndent yes] "bcm2708.svd")
  -- check we roundtrip
  res <- parseSVDPeripherals "bcm2708.svd"
  print res
  return ()

-- each file is periph, at start it has
-- // comment
-- #define ${PERIPH}_BASE
-- -//- _PASSWORD
-- #define ?? some weirdnesss
--
-- MODE=RW|RO
-- #define ${PERIPH}_${REG} HW_REGISTER_${MODE}
--

toSVD Device{..} = mkelem "device" [] [
    el "name" deviceName
  , el "version" deviceVersion
  , el "description" deviceDescription
  , mkelem "peripherals" [] (map mkPeriph devicePeripherals)
  ]
  where
    el name val = mkelem name [] [ txt val ]
    mkPeriph Peripheral{..} = mkelem "peripheral" [] [
        el "name" periphName
      , el "description" periphDescription
      , el "baseAddress" (showHex32 periphBaseAddress)
      , mkelem "registers" [] (map mkReg periphRegisters)
      ]

    mkReg Register{..} = mkelem "register" [] $ [
        el "name" regName
      , el "addressOffset" (showHex8 regAddressOffset)
      , el "access" (showAccessType regAccess)
      , el "size" (showHex8 regSize)
      ]
      ++ (maybe [] (\x -> [ el "resetValue" (showHex8 x) ]) regResetValue)
      ++ [mkelem "fields" [] (map mkField regFields)]

    mkField Field{..} = mkelem "field" [] $ [
        el "name" fieldName
      -- , el "description" fieldDescription
      , el "bitOffset" (show fieldBitOffset)
      , el "bitWidth" (show fieldBitWidth)
      ]

bcmDir = "rpi-open-firmware/common/broadcom/bcm2708_chip"

-- excluded
-- hardware.h <-- interrupts, DMA sources
-- register_map.h <-- toplevel?
ignored = [
    "arm_control.h"
  , "aux_io.h"
  , "cpr_clkman_a0.h"
  , "flow_config.tcl"
  , "fpga_peripheral.h"
  , "hardware.h"
  , "register_map.h"
  , "register_map_macros.h"
  , "README.txt"
  , "cryptohw.h"
  , "rnghw.h"
  , "sv_chip_regmap.h"
  , "v3d.h"
  , "vcodec.h"
  ]

goall = do
  cs <- filter (\x -> (not $ "." `L.isPrefixOf` x) && (not $ elem x ignored) && (not $ "israel" `L.isPrefixOf` x)) <$> getDirectoryContents bcmDir
  r <- forM (L.sort cs) $ \f -> do
    let pth = (bcmDir++) . ("/"++) $ f

    test <- doesFileExist pth
    case test of
      False -> return $ Left (f, "no file")
      True -> do
        x <- B.readFile pth
        case parseOnly (parsePeriph f) x of
          Left er -> return $ Left (f, er)
          Right p -> return $ Right (f, p)

  let
    oks = rights r
    fails = lefts r
    tot = length r
    loks = length oks
    lfails = length fails

  print (loks, lfails)
  putStrLn $ "Success rate " ++ (show $ (100 * (fromIntegral :: Int -> Double) loks) / (fromIntegral tot))
  print fails
  --print oks
  when (loks < 96) $ error $ "parser sux, only parsed " ++ show loks
  return oks
