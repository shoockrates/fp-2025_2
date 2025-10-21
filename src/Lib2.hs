{-# LANGUAGE LambdaCase #-}
module Lib2(
    parseCommand
  , ToCliCommand(..)
  , process
  ) where

import qualified Lib1
import Data.List (isPrefixOf)
import Data.Char (isDigit)

-- Basic types
type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- Helpers
keyword :: String -> Parser String
keyword prefix input =
  if prefix `isPrefixOf` input
  then Right (prefix, drop (length prefix) input)
  else Left (prefix ++ " expected, got: " ++ input)

ws :: Parser String
ws input = Right (takeWhile (== ' ') input, dropWhile (== ' ') input)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right r -> Right r
  Left _  -> p2 input

-- pmap was unused; removed to silence warnings

-- basic token parsers
parseString :: Parser String
parseString ('"':input) =
  let (str, rest) = break (== '"') input
  in case rest of
       ('"':rest') -> Right (str, rest')
       _ -> Left "Unterminated string"
parseString _ = Left "Expected quoted string"

parseInt :: Parser Integer
parseInt input =
  let (digits, rest) = span isDigit input
  in if null digits then Left "Expected integer" else Right (read digits, rest)

parseDouble :: Parser Double
parseDouble input =
  let (num, rest) = span (\r -> isDigit r || r == '.') input
  in if null num then Left "Expected number" else Right (read num, rest)

-- Parse RoundStatus
parseRoundStatus :: Parser Lib1.RoundStatus
parseRoundStatus input =
  case dropWhile (== ' ') input of
    ('A':'c':'t':'i':'v':'e':rest) -> Right (Lib1.Active, rest)
    ('F':'i':'n':'i':'s':'h':'e':'d':rest) -> Right (Lib1.Finished, rest)
    ('C':'a':'n':'c':'e':'l':'l':'e':'d':rest) -> Right (Lib1.Cancelled, rest)
    _ -> Left "Expected round status (Active|Finished|Cancelled)"

-- Parse GameType
parseGameType :: Parser Lib1.GameType
parseGameType input =
  case dropWhile (== ' ') input of
    ('B':'l':'a':'c':'k':'j':'a':'c':'k':rest) -> Right (Lib1.Blackjack, rest)
    ('R':'o':'u':'l':'e':'t':'t':'e':rest) -> Right (Lib1.Roulette, rest)
    ('P':'o':'k':'e':'r':rest) -> Right (Lib1.Poker, rest)
    ('B':'a':'c':'c':'a':'r':'a':'t':rest) -> Right (Lib1.Baccarat, rest)
    ('S':'l':'o':'t':'s':rest) -> Right (Lib1.Slots, rest)
    _ -> Left "Unknown game type"

-- Parse BetType
parseBetType :: Parser Lib1.BetType
parseBetType input =
  case dropWhile (== ' ') input of
    ('S':'t':'r':'a':'i':'g':'h':'t':rest) -> Right (Lib1.Straight, rest)
    ('S':'p':'l':'i':'t':rest) -> Right (Lib1.Split, rest)
    ('C':'o':'r':'n':'e':'r':rest) -> Right (Lib1.Corner, rest)
    ('R':'e':'d':rest) -> Right (Lib1.Red, rest)
    ('B':'l':'a':'c':'k':rest) -> Right (Lib1.Black, rest)
    ('O':'d':'d':rest) -> Right (Lib1.Odd, rest)
    ('E':'v':'e':'n':rest) -> Right (Lib1.Even, rest)
    ('P':'a':'s':'s':rest) -> Right (Lib1.Pass, rest)
    ('D':'o':'n':'t':'P':'a':'s':'s':rest) -> Right (Lib1.DontPass, rest)
    _ -> Left "Unknown bet type"

-- Parse BetOutcome
parseBetOutcome :: Parser Lib1.BetOutcome
parseBetOutcome input =
  case dropWhile (== ' ') input of
    ('W':'i':'n':rest) -> case parseDouble (dropWhile (== ' ') rest) of
      Right (v, rest') -> Right (Lib1.Win v, rest')
      Left _ -> Left "Expected number after Win"
    ('L':'o':'s':'e':rest) -> Right (Lib1.Lose, rest)
    ('P':'u':'s':'h':rest) -> Right (Lib1.Push, rest)
    _ -> Left "Expected bet outcome (Win|Lose|Push)"

-- Procedural parsers for commands
parseDump :: Parser Lib1.Command
parseDump input = case keyword "Dump" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (_, rest2) <- keyword "Examples" rest1
    Right (Lib1.Dump Lib1.Examples, rest2)
  Left e -> Left e

parseAddPlayer :: Parser Lib1.Command
parseAddPlayer input = case keyword "AddPlayer" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (pid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (name, rest4) <- parseString rest3
    (_, rest5) <- ws rest4
    (bal, rest6) <- parseDouble rest5
    Right (Lib1.AddPlayer pid name bal, rest6)
  Left _ -> Left "Not AddPlayer"

parseAddGame :: Parser Lib1.Command
parseAddGame input = case keyword "AddGame" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (gid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (gname, rest4) <- parseString rest3
    (_, rest5) <- ws rest4
    (gtype, rest6) <- parseGameType rest5
    Right (Lib1.AddGame gid gname gtype, rest6)
  Left _ -> Left "Not AddGame"

parseAddDealer :: Parser Lib1.Command
parseAddDealer input = case keyword "AddDealer" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (did, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (dname, rest4) <- parseString rest3
    (_, rest5) <- ws rest4
    (tref, rest6) <- parseInt rest5
    Right (Lib1.AddDealer did dname tref, rest6)
  Left _ -> Left "Not AddDealer"

parseAddTable :: Parser Lib1.Command
parseAddTable input = case keyword "AddTable" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (tid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (tname, rest4) <- parseString rest3
    (_, rest5) <- ws rest4
    (gref, rest6) <- parseInt rest5
    (_, rest7) <- ws rest6
    (minb, rest8) <- parseDouble rest7
    (_, rest9) <- ws rest8
    (maxb, rest10) <- parseDouble rest9
    -- optional dealerRef
    let restAfterMax = dropWhile (== ' ') rest10
    case parseInt restAfterMax of
      Right (did, rest11) -> Right (Lib1.AddTable tid tname gref minb maxb (Just did), rest11)
      Left _ -> Right (Lib1.AddTable tid tname gref minb maxb Nothing, rest10)
  Left _ -> Left "Not AddTable"

parseAddRound :: Parser Lib1.Command
parseAddRound input = case keyword "AddRound" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (rid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (tref, rest4) <- parseInt rest3
    -- optional parentRoundId
    let afterTref = dropWhile (== ' ') rest4
    case parseInt afterTref of
      Right (prid, rest5) -> do
        (_, rest6) <- ws rest5
        (status, rest7) <- parseRoundStatus rest6
        Right (Lib1.AddRound rid tref (Just prid) (Just status), rest7)
      Left _ -> do
        (_, rest5) <- ws rest4
        (status, rest6) <- parseRoundStatus rest5
        Right (Lib1.AddRound rid tref Nothing (Just status), rest6)
  Left _ -> Left "Not AddRound"

parsePlaceBet :: Parser Lib1.Command
parsePlaceBet input = case keyword "PlaceBet" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (bid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (pref, rest4) <- parseInt rest3
    (_, rest5) <- ws rest4
    (tref, rest6) <- parseInt rest5
    (_, rest7) <- ws rest6
    (amt, rest8) <- parseDouble rest7
    (_, rest9) <- ws rest8
    (btype, rest10) <- parseBetType rest9
    -- accept either: [parent <id>] round <id>
    let afterBType = dropWhile (== ' ') rest10
    if "parent" `isPrefixOf` afterBType then
      case keyword "parent" afterBType of
        Right (_, rest11) -> do
          (_, rest12) <- ws rest11
          (pbid, rest13) <- parseInt rest12
          (_, rest14) <- ws rest13
          case keyword "round" rest14 of
            Right (_, rest15) -> do
              (_, rest16) <- ws rest15
              (rref, rest17) <- parseInt rest16
              Right (Lib1.PlaceBet bid pref tref amt btype (Just pbid) rref, rest17)
            Left _ -> Left "Expected 'round' keyword"
        Left _ -> Left "Invalid parent"
    else if "round" `isPrefixOf` afterBType then
      case keyword "round" afterBType of
        Right (_, rest11) -> do
          (_, rest12) <- ws rest11
          (rref, rest13) <- parseInt rest12
          Right (Lib1.PlaceBet bid pref tref amt btype Nothing rref, rest13)
        Left _ -> Left "Invalid round"
    else
      -- backward-compat: accept bare integer as roundRef
      case parseInt afterBType of
        Right (rref, rest11) -> Right (Lib1.PlaceBet bid pref tref amt btype Nothing rref, rest11)
        Left _ -> Left "Expected round or parent info"
  Left _ -> Left "Not PlaceBet"

parseResolveBet :: Parser Lib1.Command
parseResolveBet input = case keyword "ResolveBet" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (bre, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (outcome, rest4) <- parseBetOutcome rest3
    Right (Lib1.ResolveBet bre outcome, rest4)
  Left _ -> Left "Not ResolveBet"

parseDeposit :: Parser Lib1.Command
parseDeposit input = case keyword "Deposit" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (pid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (amt, rest4) <- parseDouble rest3
    Right (Lib1.Deposit pid amt, rest4)
  Left _ -> Left "Not Deposit"

parseWithdraw :: Parser Lib1.Command
parseWithdraw input = case keyword "Withdraw" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (pid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (amt, rest4) <- parseDouble rest3
    Right (Lib1.Withdraw pid amt, rest4)
  Left _ -> Left "Not Withdraw"

parseSetLimit :: Parser Lib1.Command
parseSetLimit input = case keyword "SetLimit" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (pid, rest2) <- parseInt rest1
    (_, rest3) <- ws rest2
    (ltype, rest4) <- case dropWhile (== ' ') rest3 of
      ('D':'a':'i':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.DailyLimit, rest)
      ('W':'e':'e':'k':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.WeeklyLimit, rest)
      ('M':'o':'n':'t':'h':'l':'y':'L':'i':'m':'i':'t':rest) -> Right (Lib1.MonthlyLimit, rest)
      _ -> Left "Expected limit type"
    (_, rest5) <- ws rest4
    (amt, rest6) <- parseDouble rest5
    Right (Lib1.SetLimit pid ltype amt, rest6)
  Left _ -> Left "Not SetLimit"

parseShow :: Parser Lib1.Command
parseShow input = case keyword "Show" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    case dropWhile (== ' ') rest1 of
      ('P':'l':'a':'y':'e':'r':'s':rest) -> Right (Lib1.ShowPlayers, rest)
      ('G':'a':'m':'e':'s':rest) -> Right (Lib1.ShowGames, rest)
      ('T':'a':'b':'l':'e':'s':rest) -> Right (Lib1.ShowTables, rest)
      ('B':'e':'t':'s':rest) -> Right (Lib1.ShowBets, rest)
      ('R':'o':'u':'n':'d':'s':rest) -> Right (Lib1.ShowRounds, rest)
      _ -> Left "Unknown show target"
  Left _ -> Left "Not Show"

parseRemovePlayer :: Parser Lib1.Command
parseRemovePlayer input = case keyword "RemovePlayer" input of
  Right (_, rest0) -> do
    (_, rest1) <- ws rest0
    (pid, rest2) <- parseInt rest1
    Right (Lib1.RemovePlayer pid, rest2)
  Left _ -> Left "Not RemovePlayer"

-- top-level parser
parseCommand :: Parser Lib1.Command
parseCommand = foldr1 orElse
  [ parseDump
  , parseAddPlayer
  , parseAddGame
  , parseAddDealer
  , parseAddTable
  , parseAddRound
  , parsePlaceBet
  , parseResolveBet
  , parseDeposit
  , parseWithdraw
  , parseSetLimit
  , parseShow
  , parseRemovePlayer
  ]

-- process and ToCliCommand
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand = \case
    Lib1.Dump d -> "Dump " ++ show d
    Lib1.AddPlayer pid name bal ->
      "AddPlayer " ++ show pid ++ " \"" ++ name ++ "\" " ++ show bal
    Lib1.AddGame gid name gt ->
      "AddGame " ++ show gid ++ " \"" ++ name ++ "\" " ++ show gt
    Lib1.AddTable tid tname gref minb maxb mdr ->
      "AddTable " ++ show tid ++ " \"" ++ tname ++ "\" " ++ show gref ++ " " ++ show minb ++ " " ++ show maxb ++ maybe "" ( (" " ++) . show) mdr
    Lib1.AddRound rid tref pr st ->
      let parentPart = maybe "" ( (" " ++) . show) pr
          statusPart = maybe "" ( (" " ++) . show) st
      in "AddRound " ++ show rid ++ " " ++ show tref ++ parentPart ++ statusPart
    Lib1.PlaceBet bid pref tref amt btype pbr rref ->
      let parentPart = maybe "" ( (" parent " ++) . show) pbr
      in "PlaceBet " ++ show bid ++ " " ++ show pref ++ " " ++ show tref ++ " " ++ show amt ++ " " ++ show btype ++ parentPart ++ " round " ++ show rref
    Lib1.ResolveBet bre outcome ->
      "ResolveBet " ++ show bre ++ " " ++ show outcome
    Lib1.AddDealer did name tref ->
      "AddDealer " ++ show did ++ " \"" ++ name ++ "\" " ++ show tref
    Lib1.Deposit pid amt ->
      "Deposit " ++ show pid ++ " " ++ show amt
    Lib1.Withdraw pid amt ->
      "Withdraw " ++ show pid ++ " " ++ show amt
    Lib1.SetLimit pid ltype amt ->
      "SetLimit " ++ show pid ++ " " ++ show ltype ++ " " ++ show amt
    Lib1.ShowPlayers -> "Show Players"
    Lib1.ShowGames -> "Show Games"
    Lib1.ShowTables -> "Show Tables"
    Lib1.ShowBets -> "Show Bets"
    Lib1.ShowRounds -> "Show Rounds"
    Lib1.RemovePlayer pid -> "RemovePlayer " ++ show pid

-- Provide Eq instances for relevant Lib1 types (manual implementations)
-- Eq instances moved to `Lib1` via deriving Eq; removed orphan manual instances

