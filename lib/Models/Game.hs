{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Game where

import Prelude hiding (Word)
import GHC.Generics
import Data.Aeson
import Data.Map.Strict (Map)

type CodenameKey = Int
type Word = String
type Team = Int
data Owner = Players Team | Neutral | Assassin | Unknown deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
data Role = Spy Team | Spymaster Team deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
data Codename = Codename 
    { selectedBy :: Maybe Team  -- Nothing if not selected
    , ownedBy    :: Owner
    , word       :: Word 
    } deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
data Move = Tip Word Int | Select Int Role | Concede Team deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
data Change = NextPlayer Role | TeamWins Team | TurnOver Int Codename deriving (Show, Eq, Read, Generic)
data IllegalChange = AlreadySelected Codename | NotYourTurn Team | UnknownCodename CodenameKey | SpymasterCannotSelect 
    deriving (Show, Eq, Read, Generic, ToJSON, FromJSON) 
data Game = Game
    { turnOf         :: Team
    --, codenamesLeft  :: Map Team Int
    , codenames      :: Map CodenameKey Codename
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)
