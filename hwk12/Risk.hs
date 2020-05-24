{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }


oneDie :: Rand StdGen [DieValue]
oneDie = die >>= \d -> return [d]

threeDice :: Rand StdGen [DieValue]
threeDice = die >>= \d1 -> die >>= \d2 -> die >>= \d3 -> return [d1, d2, d3]

twoDice :: Rand StdGen [DieValue]
twoDice = die >>= \d1 -> die >>= \d2 -> return [d1, d2]

diceFunc = [oneDie, twoDice, threeDice]

battle :: Battlefield -> Rand StdGen Battlefield
battle b
        | noDefenderDice b == 2 = doubleBattle b
        | otherwise = singleBattle b
           

noAttackerDice b = if attackers b > 3 then 3 else attackers b
noDefenderDice b = if defenders b > 2 then 2 else defenders b

singleBattle :: Battlefield -> Rand StdGen Battlefield
singleBattle b = diceFunc !! (1 + noAttackerDice b) >>= 
                 \attackerDice -> oneDie >>=
                 \defenderDie -> if maximum attackerDice > maximum defenderDie then
                        return b { defenders = defenders b - 1 } else
                        return b { attackers = attackers b - 1 }

doubleBattle :: Battlefield -> Rand StdGen Battlefield
doubleBattle = undefined
