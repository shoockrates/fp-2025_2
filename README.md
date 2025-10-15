# Casino Management DSL

## Domain Description

This Domain Specific Language (DSL) is designed for managing casino operations. The main entities in this domain are:

- **Players**: Casino patrons who place bets and manage their account balances and limits
- **Games**: Different types of casino games (Roulette, Blackjack, Poker, etc.)
- **Tables**: Physical or virtual gaming tables where games are played
- **Bets**: Wagers placed by players on various outcomes
- **Rounds**: Game sessions that can contain multiple bets and sub-rounds
- **Dealers**: Casino staff who operate the games

## How to Identify Recursion in This System

### 🔍 **Visual Recursion Indicators**

**Look for these patterns that signal recursive structures:**

1. **Self-Referencing Fields**: Data types that contain references to themselves
2. **Parent-Child Relationships**: Fields like `parent`, `parentBetId`, `parentRoundId`
3. **Tree-Like Nesting**: Commands that reference other commands of the same type
4. **Optional Parent References**: `Maybe Integer` fields indicating optional parent relationships

### 🌳 **Three Types of Recursion**

1. **Bet Recursion**: Bets can contain sub-bets (parent-child betting)
2. **Round Recursion**: Rounds can contain sub-rounds (nested game sessions)
3. **Data Structure Recursion**: Player database uses recursive tree structure

## Key Features

### Player Management
- **Hierarchical Storage**: Players are stored in a BST ordered by player ID for O(log n) lookup
- **Name-based Search**: Support for both exact and partial name matching (case-insensitive)
- **Balance Management**: Track player balances with deposit/withdrawal operations
- **Limit Setting**: Support for daily, weekly, and monthly limits

### Data Structures
- **PlayerDatabase**: Tree-like structure using BST for efficient player management
- **GameState**: Central state container holding players, games, tables, and bets
- **Player Limits**: Each player can have multiple limit types with associated amounts

## BNF Grammar


```bnf
<command> ::= "AddPlayer" <player>                         
            | "AddGame" <game>                           
            | "AddTable" <table>                         
            | "PlaceBet" <player> <gameRef> <amount>     
            | "AddRound" <gameRef> <round>               
            | "ResolveBet" <player> <gameRef> <roundRef> 
            | "AddDealer" <dealer>                       
            | "Deposit" <player> <amount>                
            | "Withdraw" <player> <amount>               
            | "SetLimit" <player> <amount>               
            | "ShowCommand" <command>                    
            | "RemovePlayer" <player>                    
            | "FindPlayer" <name>                        
            | "Sequence" <command> <command>             
            | "Dump" <dumpable>                          

<player> ::= "Player" <playerId> <name> <balance>        

<dealer> ::= "Dealer" <name> <experience>                

<game> ::= "Game" <gameId> <name> <tables>               
<gameRef> ::= <gameId>                                    

<table> ::= "Table" <tableId> <players>                   
<tableRef> ::= <tableId>                                  

<round> ::= "Round" <roundId> <status>                    
<roundRef> ::= <roundId>                                  

<players> ::= "[" <playerList> "]"                        
<playerList> ::= <player> | <player> "," <playerList>     

<name> ::= <string>                                       
<playerId> ::= <integer>                                  
<gameId> ::= <integer>                                    
<tableId> ::= <integer>                                   
<roundId> ::= <integer>                                   

<balance> ::= <integer>                                   
<experience> ::= <integer>                                
<amount> ::= <integer>

<tables> ::= "[" <tableList> "]"

<tableList> ::= <table> | <table> "," <tableList>


<integer> ::= <digit> | <digit> <integer>

<digit> ::= [0-9]   

<status> ::= "Open" | "Closed" | "Resolved"               

<string> ::= <char> | <char> <string>

<char> ::= [a-z]

<dumpable> ::= "Examples"                                 

```

### 🔍 **How to Spot Recursion in BNF:**

1. **Self-referencing rules**: Look for rules where the left-hand side appears on the right-hand side
2. **Cross-referencing rules**: Rules that reference other rules of the same "type" 
3. **Optional parent patterns**: Rules with optional references to IDs of the same entity type

**Examples in this grammar:**
- `<parent_bet_ref>` can contain `<bet_id>` which refers back to another `<place_bet>` command
- `<parent_round_ref>` can contain `<round_id>` which refers back to another `<add_round>` command  
- `<player_database>` directly contains two more `<player_database>` instances

## Command Examples

### 1. Basic Player and Game Setup
```
add player 1 "John Smith" 1000.0
add player 2 "Jane Smith" 1500.0
add player 3 "John Doe" 800.0
add player 4 "Maria Garcia" 2000.0
add game 1 "European Roulette" Roulette
add dealer 1 "Maria Garcia" table 1
add table 1 "High Roller Roulette" 1 100.0 5000.0 dealer 1
```

### 2. Complex Betting with Recursion *(🔄 Recursive Pattern)*
```
add round 1 table 1 status Active
place bet 1 player 1 table 1 amount 500.0 type Red round 1                    ← MAIN BET
add round 2 table 1 parent 1 status Active                                     ← SUB-ROUND (parent 1)
place bet 2 player 1 table 1 amount 100.0 type Odd parent 1 round 2          ← CHILD BET (parent 1)
place bet 3 player 1 table 1 amount 200.0 type Straight parent 1 round 1     ← CHILD BET (parent 1)
```

**🌳 This creates a betting hierarchy:**
```
Main Bet 1 ($500 Red)
├── Child Bet 2 ($100 Odd) [in sub-round 2]
└── Child Bet 3 ($200 Straight) [in main round 1]
```

**🔍 Recursion indicators:** Notice the `parent 1` fields - both bets reference bet 1 as their parent!

### 3. Bet Resolution and Account Management
```
resolve bet 1 win 1000.0
resolve bet 2 win 200.0
resolve bet 3 lose
deposit player 1 amount 2000.0
set limit player 1 DailyLimit 10000.0
withdraw player 1 amount 500.0
```

### 4. Player Search and Administrative Operations
```
find player name "John Smith"
find player name "Smith"
show players
show games
show tables
show bets
show rounds
remove player 1
dump examples
```

## Player Database Implementation

The player database uses a Binary Search Tree (BST) structure for efficient operations:

### Core Functions
- `addPlayerToDatabase`: Adds a player while maintaining BST ordering by player ID
- `removePlayerFromDatabase`: Removes a player and rebalances the tree
- `findPlayersByName`: Finds players by exact name match
- `findPlayersByNamePartial`: Finds players by partial name match (case-insensitive)
- `findPlayerById`: Efficiently locates a player by ID using BST properties

### Search Capabilities
The system supports two types of name-based searches:
1. **Exact Match**: `findPlayersByName "John Smith"` returns players with exactly that name
2. **Partial Match**: `findPlayersByNamePartial "Smith"` returns all players whose names contain "Smith" (case-insensitive)

### Example Usage
```haskell
-- Build database from commands
let playerCommands = [AddPlayer 1 "John Smith" 1000.0, AddPlayer 2 "Jane Smith" 1500.0]
let db = buildPlayerDatabase playerCommands

-- Search operations
let johns = findPlayersByName "John Smith" db
let smiths = findPlayersByNamePartial "Smith" db
```

## 🔄 Recursive Elements Demonstration

### **1. Bet Recursion Pattern** 
**🔍 How to spot it:** Look for `parent <integer>` in bet commands

```
place bet 1 player 1 table 1 amount 500.0 type Red round 1                    ← PARENT BET
place bet 2 player 1 table 1 amount 100.0 type Odd parent 1 round 2          ← CHILD of bet 1
place bet 3 player 1 table 1 amount 200.0 type Straight parent 1 round 1     ← CHILD of bet 1
```

**Visual Tree Structure:**
```
Bet 1 (Red, $500)
├── Bet 2 (Odd, $100) 
└── Bet 3 (Straight, $200)
```

### **2. Round Recursion Pattern**
**🔍 How to spot it:** Look for `parent <integer>` in round commands

```
add round 1 table 1 status Active                          ← PARENT ROUND
add round 2 table 1 parent 1 status Active                 ← CHILD of round 1
add round 3 table 1 parent 2 status Active                 ← GRANDCHILD (child of round 2)
```

**Visual Tree Structure:**
```
Round 1 (Main Game)
└── Round 2 (Bonus Round)
    └── Round 3 (Sub-bonus Round)
```

### **3. Data Structure Recursion**
**🔍 How to spot it:** The `PlayerDatabase` type definition shows self-reference

```haskell
data PlayerDatabase
  = EmptyDB
  | PlayerNode Player PlayerDatabase PlayerDatabase  ← PlayerDatabase contains PlayerDatabase!
```

**Visual Tree Structure:**
```
        Player 3
       /        \
  Player 1    Player 5
      \         /    \
   Player 2  Player 4  Player 6
```

### **🎯 Recursion Recognition Checklist**

When reading commands, ask:
- ✅ **Does it have a `parent` field?** → Likely recursive
- ✅ **Can this thing contain other things of the same type?** → Recursive structure  
- ✅ **Does the data type reference itself?** → Definitely recursive
- ✅ **Can you draw it as a tree?** → Recursive hierarchy

### **Real-World Casino Scenario**
```
Main Roulette Bet: $500 on Red (Round 1)
├── Side Bet: $100 on Odd numbers (Round 2, child of Round 1)
├── Insurance Bet: $50 on specific number (Round 1, sibling bet)
└── Bonus Round Bet: $25 on color (Round 3, triggered by main bet)
```

This creates a **recursive betting tree** where the main bet spawns related sub-bets, each potentially spawning their own sub-bets, creating unlimited nesting depth.

## Data Types Overview

### Core Types
- `Player`: Contains ID, name, balance, and limits
- `PlayerDatabase`: BST structure for player storage
- `GameState`: Central state containing all game entities
- `Command`: Sum type representing all possible operations
- `LimitType`: Daily, Weekly, or Monthly limits for responsible gaming

### Example Game State Initialization
```haskell
-- Initialize empty game state
let initialState = initGameState

-- Add players to build the database
let updatedState = initialState { players = buildPlayerDatabase playerCommands }
```
