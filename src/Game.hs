{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Game where

import Prelude hiding (Word)
import GHC.Generics
import Data.Aeson
import Data.Map.Strict (Map)

type CodenameKey = Int
type Word = String
type Team = Int
data Owner = Players Team | Neutral | Assassin | Unknown deriving (Show, Eq, Read, Generic)
data Role = Spy Team | Spymaster Team deriving (Show, Eq, Read, Generic)
data Codename = Codename 
    { selectedBy :: Maybe Team  -- Nothing if not selected
    , ownedBy    :: Owner
    , word       :: Word 
    } deriving (Show, Eq, Read, Generic)
data Move = Tip Word Int | Select Int Role | Concede Team deriving (Show, Eq, Read, Generic)
data Change = NextPlayer Role | TeamWins Team | TurnOver Int Codename deriving (Show, Eq, Read, Generic)
data IllegalChange = AlreadySelected Codename | NotYourTurn Team | UnknownCodename CodenameKey | SpymasterCannotSelect 
    deriving (Show, Eq, Read, Generic) 
data Game = Game
    { turnOf         :: Team
    , codenamesLeft  :: Map Team Int
    , codenames      :: Map CodenameKey Codename
    } deriving (Show, Eq, Generic)

instance ToJSON Owner
instance FromJSON Owner
instance ToJSON Role
instance FromJSON Role
instance ToJSON Change
instance FromJSON Change
instance ToJSON Move
instance FromJSON Move
instance ToJSON IllegalChange
instance FromJSON IllegalChange
instance ToJSON Codename
instance FromJSON Codename
instance ToJSON Game
instance FromJSON Game
