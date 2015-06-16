{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Hw05 where

import Control.Applicative ((<*>), (<$>))
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import Data.Bits (xor)
import Data.Word (Word8)
import System.Environment (getArgs)

import qualified Data.List as L
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
                             , amount = amt }) = upsert (+) giver (amt * (-1)) $ upsert (+) receiver amt m

upsert :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
upsert updater k a m = case Map.lookup k m of
  Just _ -> Map.adjust (updater a) k m
  Nothing -> Map.insert k a m

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . Map.foldlWithKey (go) ("Nobody", 0)
  where go p@(_, biggestAmt) person amt = if (amt > biggestAmt) then (person, amt) else p

-- Exercise 7 -----------------------------------------

setId :: TId -> Transaction -> Transaction
setId tid t = Transaction (from t) (to t) (amount t) tid

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ids = zipWith (setId) ids (personAmtsToTrans $ Map.toList m)

personAmtsToTrans :: [(String, Integer)] -> [Transaction]
personAmtsToTrans [] = []
personAmtsToTrans ps = ts ++ personAmtsToTrans remainder
  where (remainder, ts) = personAmtsToTransWithRem ps

sortPeopleByAmt :: Bool -> [(String, Integer)] -> [(String, Integer)]
sortPeopleByAmt asc ps = L.sortBy (\x y -> (if asc then compare else flip compare) (snd x) (snd y)) ps

-- payers: those who ended up with positive balance
-- payees: those who ended up with negative balance

payers :: [(String, Integer)] -> [(String, Integer)]
payers = sortPeopleByAmt False . filter ((>0) . snd)

payees :: [(String, Integer)] -> [(String, Integer)]
payees = sortPeopleByAmt True . filter ((<0) . snd)

zip2Tup2WithPad :: (a, b) -> [(a, b)] -> [(a, b)] -> [((a, b), (a, b))]
zip2Tup2WithPad pad (x:xs) (y:ys) = (x, y)  :(zip2Tup2WithPad pad xs ys)
zip2Tup2WithPad pad []     (x:xs) = (pad, x):(zip2Tup2WithPad pad [] xs)
zip2Tup2WithPad pad (x:xs) []     = (x, pad):(zip2Tup2WithPad pad xs [])
zip2Tup2WithPad _ _ _             = []

personAmtsToTransWithRem :: [(String, Integer)] -> ([(String, Integer)], [Transaction])
personAmtsToTransWithRem ps = foldl (transact) ([], []) $ zip2Tup2WithPad ("ignore", 0) (payees ps) (payers ps)
  where transact (ps', ts) (("ignore", 0), n)             = (n:ps', ts)
        transact (ps', ts) (p, ("ignore", 0))             = (p:ps', ts)
        transact (ps', ts) ((pye, pyeAmt), (pyr, pyrAmt)) = case compare (abs pyeAmt) pyrAmt of
                                                             GT -> ((pye, pyrAmt+pyeAmt):ps', (Transaction pyr pye pyrAmt "replace"):ts)       -- was owed more than had to give
                                                             LT -> ((pyr, pyrAmt+pyeAmt):ps', (Transaction pyr pye (abs pyeAmt) "replace"):ts) -- had to give more than was owed
                                                             EQ -> (ps',                      (Transaction pyr pye pyrAmt "replace"):ts)       -- cancel each other out!

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp x = BS.writeFile fp (encode x)

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
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
