{-# OPTIONS_GHC -Wno-partial-fields #-}

module Lib1
  ( keywords,
    examples,
    Command (..),
    Dumpable (..),
    GameType (..),
    BetType (..),
    BetOutcome (..),
    RoundStatus (..),
    LimitType (..),
  )
where

data Dumpable = Examples
  deriving (Show, Read, Eq)

keywords :: [String]
keywords =
  [ "add",
    "player",
    "game",
    "table",
    "bet",
    "place",
    "resolve",
    "win",
    "lose",
    "show",
    "players",
    "games",
    "tables",
    "bets",
    "remove",
    "dump",
    "examples",
    "parent",
    "round",
    "deposit",
    "withdraw",
    "set",
    "limit",
    "dealer",
    "balance",
    "amount",
    "type",
    "status"
  ]

data Command
  = AddPlayer
      { playerId :: Integer,
        playerName :: String,
        initialBalance :: Double
      }
  | AddGame
      { gameId :: Integer,
        gameName :: String,
        gameType :: GameType
      }
  | AddTable
      { tableId :: Integer,
        tableName :: String,
        gameRef :: Integer,
        minBet :: Double,
        maxBet :: Double,
        dealerRef :: Maybe Integer
      }
  | PlaceBet
      { betId :: Integer,
        playerRef :: Integer,
        tableRef :: Integer,
        amount :: Double,
        betType :: BetType,
        parentBetId :: Maybe Integer,
        roundRef :: Integer
      }
  | AddRound
      { roundId :: Integer,
        tableRef :: Integer,
        parentRoundId :: Maybe Integer,
        status :: Maybe RoundStatus
      }
  | ResolveBet
      { betRef :: Integer,
        outcome :: BetOutcome
      }
  | AddDealer
      { dealerId :: Integer,
        dealerName :: String,
        tableRef :: Integer
      }
  | Deposit
      { playerRef :: Integer,
        amount :: Double
      }
  | Withdraw
      { playerRef :: Integer,
        amount :: Double
      }
  | SetLimit
      { playerRef :: Integer,
        limitType :: LimitType,
        amount :: Double
      }
  | ShowPlayers
  | ShowGames
  | ShowTables
  | ShowBets
  | ShowRounds
  | RemovePlayer
      { playerId :: Integer
      }
  | Dump Dumpable
  deriving (Show, Read, Eq)

data GameType = Blackjack | Roulette | Poker | Baccarat | Slots
  deriving (Show, Read, Eq)

data BetType = Straight | Split | Corner | Red | Black | Odd | Even | Pass | DontPass
  deriving (Show, Read, Eq)

data BetOutcome = Win Double | Lose | Push
  deriving (Show, Read, Eq)

data RoundStatus = Active | Finished | Cancelled
  deriving (Show, Read, Eq)

data LimitType = DailyLimit | WeeklyLimit | MonthlyLimit
  deriving (Show, Read, Eq)

examples :: [Command]
examples =
  [ AddPlayer 1 "John Smith" 1000.0,
    AddGame 1 "European Roulette" Roulette,
    AddDealer 1 "Maria Garcia" 1,
    AddTable 1 "High Roller Roulette" 1 100.0 5000.0 (Just 1),
    AddRound 1 1 Nothing (Just Active),
    PlaceBet 1 1 1 500.0 Red Nothing 1,
    AddRound 2 1 (Just 1) (Just Active),
    PlaceBet 2 1 1 100.0 Odd (Just 1) 2,
    PlaceBet 3 1 1 200.0 Straight (Just 1) 1,
    ResolveBet 1 (Win 1000.0),
    ResolveBet 2 (Win 200.0),
    ResolveBet 3 Lose,
    Deposit 1 2000.0,
    SetLimit 1 DailyLimit 10000.0,
    Withdraw 1 500.0,
    RemovePlayer 1,
    Dump Examples
  ]
