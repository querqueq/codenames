{-# LANGUAGE RecordWildCards #-}

module Game where

import Prelude hiding (Word)
import Data.Either
import Data.Matrix
import Control.Monad.Random hiding (fromList)
import Control.Monad.State
import System.Random.Shuffle

type Word = String
data Team = Red | Blue | Yellow | Grey deriving (Show, Eq)
data Codename = Codename 
    { selectedBy :: Maybe Team
    , ownedBy    :: Team
    , word       :: Word 
    } deriving (Show, Eq)
data Move = Select Word deriving (Show, Eq)
data Change = NextTeam Team | TeamWins Team | Selected Codename deriving (Show, Eq)
data IllegalChange = AlreadySelected Codename | NotYourTurn Team | UnknownCodename Word deriving (Show, Eq) 
data Game = Game
    { turnOf     :: Team
    , redLeft    :: Int
    , blueLeft   :: Int
    , codenames  :: [Codename]
    } deriving (Show, Eq)

playerTeams :: [Team]
playerTeams = [Red, Blue]

allTeams :: [Team]
allTeams = [Red, Blue, Yellow, Grey]

game :: (MonadRandom m) => Team -> [Word] -> m Game
game starter words = do
        teamDistribution' <- shuffleM teamDistribution
        return $ Game starter (teamSize Red) (teamSize Blue) $ zipWith (Codename Nothing) teamDistribution' words
    where baseTeamSize = div (length words) 3 
          teamSize Grey = 1
          teamSize Yellow = baseTeamSize
          teamSize team = if team == starter then baseTeamSize + 1 else baseTeamSize
          teamDistribution = concat $ map (\t -> replicate (teamSize t) t) allTeams

checkWin :: Game -> Maybe Team
checkWin = checkWin' . foldl checkCodename (0,0,Nothing) . codenames
 where checkCodename (r,b,g) (Codename Nothing Red    _)   = (r+1,b  ,g)
       checkCodename (r,b,g) (Codename Nothing Blue   _)   = (r  ,b+1,g)
       checkCodename (r,b,g) (Codename t       Grey   _)   = (r  ,b  ,t)
       checkCodename (r,b,g) (Codename _       _      _)   = (r  ,b  ,g)
       checkWin' (0,_,_) = Just Red
       checkWin' (_,0,_) = Just Blue
       checkWin' (_,_,Just Red)  = Just Blue
       checkWin' (_,_,Just Blue) = Just Red
       checkWin' (_,_,_) = Nothing

nextTeam :: Team -> Team
nextTeam Red = Blue
nextTeam Blue = Red
        
move :: Game -> Team -> Move -> Either IllegalChange (Game, [Change])
move g@(Game {..}) t mv
    | turnOf /= t = Left $ NotYourTurn turnOf
    | otherwise = move' mv
    where move' (Select w) = let
                select l@(Left _) _ = l
                select (Right (Game {..}, cs)) cn@(Codename {..})
                    | word /= w = Right (Game turnOf redLeft blueLeft (cn : codenames), cs)
                    | selectedBy /= Nothing = Left $ AlreadySelected cn
                    | otherwise = Right (Game turnOf' redLeft' blueLeft' (cn' : codenames), cs')
                    where cn' = Codename (Just t) ownedBy word
                          turnOf' = if selectedBy == Just ownedBy then turnOf else nextTeam turnOf
                          redLeft' = if ownedBy == Red then redLeft - 1 else redLeft
                          blueLeft' = if ownedBy == Blue then blueLeft - 1 else blueLeft
                          cs' = [Selected cn']
                            ++ if ownedBy == Grey then [TeamWins turnOf'] else []
                            ++ if blueLeft' == 0 then [TeamWins Blue] else []
                            ++ if redLeft' == 0 then [TeamWins Red] else []
                            ++ [NextTeam turnOf']
                            ++ cs
            in foldl select (Right (Game turnOf redLeft blueLeft [], [])) codenames
    {--
          move' (Select w) = let
                select l@(Left _) = l
                select (Right cn@(Codename {..}))
                    | selectedBy /= Nothing = Left $ AlreadySelected cn
                    | otherwise = Right $ Selected $ Codename (Just t) ownedBy word
                safeHead [] = Left $ UnknownCodename w
                safeHead [x:_] = x
            in select $ safeHead $ filter (\Codename {..} -> word == w) codenames
          finish = fmap ((,) g)
            let select r cn@(Codename {..})
                    | word /= w = r
                    | selectedBy /= Nothing = Left $ AlreadySelected cn
                    | otherwise = fmap ((:) (Selected $ Codename (Just t) ownedBy word)) r
            in foldl select (Right []) codenames
            --}

-- Fix points
-- Check winner
-- Set next team

testGame :: (MonadRandom m) => m Game
testGame = game Red testWords

testWords :: [Word]
testWords = 
    [ "TURKEY"
    , "FILE"
    , "PAPER"
    , "MAIL"
    , "SPIKE"
    , "PIT"
    , "SWING"
    , "WEB"
    , "COLD"
    , "SNOW"
    , "UNDERTAKER"
    , "SOUND"
    , "NOTE"
    , "CHARGE"
    , "GLASS"
    , "LION"
    , "OCTOPUS"
    , "CZECH"
    , "KETCHUP"
    , "SPACE"
    , "POINT"
    , "CAPITAL"
    , "ATLANTIS"
    , "GLOVE"
    , "BUTTON"
    ]
