{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser
import Data.Bits (xor)
import Data.Functor
import Data.List
import Data.Ord
import Data.Function (on)

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFilePath modifiedFilePath = do
                                              originalFile <- BS.readFile originalFilePath
                                              modifiedFile <- BS.readFile modifiedFilePath
                                              return $ BS.pack . filter (/= 0) $ BS.zipWith xor originalFile modifiedFile

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filePathToWrite = do
                                      cipherText <- BS.readFile $ filePathToWrite ++ ".enc" 
                                      BS.writeFile filePathToWrite $ BS.pack . BS.zipWith xor cipherText $ repeated key

repeated :: ByteString -> ByteString
repeated byteString = byteString `BS.append` repeated byteString

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filePath = Parser.decode <$> BS.readFile filePath 

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsFilePath transactionsFilePath = do
                                  maybeVictimIds <- parseFile victimsFilePath
                                  maybeTransactions <- parseFile transactionsFilePath
                                  return $ let 
                                              victimIds = extractObject maybeVictimIds victimsFilePath
                                              transactions = extractObject maybeTransactions transactionsFilePath
                                           in 
                                              Just $ filter (\transaction -> not (tid transaction `elem` victimIds)) transactions
                                  
extractObject :: Maybe a -> FilePath -> a
extractObject Nothing filePath = error ("failed to parse file " ++ filePath)
extractObject (Just a) _ = a

-- Exercise 5 -----------------------------------------
-- TODO: write tests
getFlow :: [Transaction] -> Map String Integer
getFlow transactions = foldr addTransactionToMap Map.empty transactions
                        where addTransactionToMap transaction acc =
                                    Map.insertWith combineValues toPerson dollars $ Map.insertWith combineValues fromPerson (-dollars) acc
                                    where
                                      combineValues oldValue newValue = oldValue + newValue
                                      fromPerson = from transaction
                                      toPerson = to transaction
                                      dollars = amount transaction

-- Exercise 6 -----------------------------------------
-- TODO: write tests
getCriminal :: Map String Integer -> String
getCriminal = fst . maximumBy (comparing snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mp transactionIds = let descendingPayers = sortByValuesDescending payers
                               ascendingPayees = sortByValuesAscending payees
                           in
                                reconcileDifference descendingPayers ascendingPayees transactionIds
                           where
                                (payers, payees) = Map.partition (> 0) mp
                                  
reconcileDifference :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
reconcileDifference _ _ [] = error "Not enough transaction ids"
reconcileDifference [] _ _ = []
reconcileDifference _ [] _ = []
reconcileDifference ((payer, amountToBePaid):restPayers) ((payee, amountToBeReceived):restPayees) (transactionId:remainingTransactionIds) = 
                                        Transaction { from = payer, to = payee, amount = transferAmount, tid = transactionId } :
                                        reconcileDifference newPayers newPayees remainingTransactionIds
                                        where transferAmount = min amountToBePaid (-amountToBeReceived)
                                              newPayerAmount = amountToBePaid - transferAmount
                                              newPayeeAmount = amountToBeReceived + transferAmount
                                              newPayers = (if newPayerAmount == 0 then restPayers else ((payer, newPayerAmount):restPayers))
                                              newPayees = (if newPayeeAmount == 0 then restPayees else ((payee, newPayeeAmount):restPayees))

sortByValuesDescending :: Map String Integer -> [(String, Integer)]
sortByValuesDescending = sortByDescending (compare `on` snd) . Map.toList
sortByDescending cmp = sortBy (flip cmp) 

sortByValuesAscending :: Map String Integer -> [(String, Integer)]
sortByValuesAscending = sortBy (compare `on` snd) . Map.toList

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON filePathToWrite = BS.writeFile filePathToWrite <$> Parser.encode

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

