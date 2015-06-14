{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.Word (Word8)
{-import System.Environment (getArgs)-}

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Helpers --------------------------------------------

xorByteStringsWithFilter :: (Word8 -> Bool) -> ByteString -> ByteString -> ByteString
xorByteStringsWithFilter predicate bs1 bs2 = BS.pack $ filter (predicate) $ BS.zipWith (xor) bs1 bs2

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origFileName modFileName = do
  bsOrig <- BS.readFile origFileName
  bsMod <- BS.readFile modFileName
  return $ xorByteStringsWithFilter (/=0) bsOrig bsMod

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encrypted <- BS.readFile (path ++ ".enc")
  BS.writeFile path $ xorByteStringsWithFilter (const True) encrypted (BS.cycle key)

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = BS.readFile path >>= return . decode

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transPath = do
  mvs <- parseFile victimsPath
  mts <- parseFile transPath
  return $ bad <$> mvs <*> mts
  where bad vs ts = filter ((`elem` vs) . tid) ts

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = foldl (build) Map.empty ts
  where build m (Transaction { from = giver
                             , to = receiver
                             , amount = amt }) = upsert (+) giver (amt*(-1)) $ upsert (+) receiver amt m

upsert :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
upsert updater k a m = case Map.lookup k m of
  Just a' -> Map.adjust (updater a') k m
  Nothing -> Map.insert k a m

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldlWithKey (go) ("Nobody", 0)
  where go p@(worstPerson, biggestAmt) person amt = if (amt > biggestAmt) then (person, amt) else p

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  key <- getSecret "dog-original.jpg" "dog.jpg"
  decryptWithKey key "victims.json"
  ts <- getBadTs "victims.json" "transactions.json"
  case ts of
    Just xs -> print $ length xs
    Nothing -> print (0 :: Int)

{-main :: IO ()-}
{-main = do-}
  {-args <- getArgs-}
  {-crim <--}
    {-case args of-}
      {-dog1:dog2:trans:vict:ids:out:_ ->-}
          {-doEverything dog1 dog2 trans vict ids out-}
      {-_ -> doEverything "dog-original.jpg"-}
                        {-"dog.jpg"-}
                        {-"transactions.json"-}
                        {-"victims.json"-}
                        {-"new-ids.json"-}
                        {-"new-transactions.json"-}
  {-putStrLn crim-}

