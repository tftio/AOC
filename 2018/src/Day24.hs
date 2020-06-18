{-# LANGUAGE OverloadedStrings #-}
module Day24 where

import Data.List
import Data.Function (on)
import Data.List.Split
import qualified Data.Map

type Initiative   = Int
type Count        = Int
type EffectivePower = Int
type Damage       = Int
type HitPoints    = Int
type GroupId      = Int

data DamageType   = Radiation | Bludgeoning | Fire | Slashing | Cold deriving (Show, Eq)
data Defense      = Weak DamageType | Immune DamageType              deriving (Show, Eq)
data Attack       = Attack DamageType Damage                         deriving (Show, Eq)
data Team         = Antibody | Infection                             deriving (Show, Eq)

data GroupStatus  = Active | Dead deriving (Show, Eq)

data Group = Group { groupId    :: GroupId,
                     team       :: Team,
                     count      :: Count,
                     initiative :: Initiative,
                     attack     :: Attack,
                     hitPoints  :: HitPoints,
                     status     :: GroupStatus,
                     defenses   :: [Defense] } deriving (Show)

instance Eq Group where
  (==) a b = groupId a == groupId b

type BattleState = Data.Map.Map Int Group

testDataStr = "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n"

parse s =
  antibodies ++ infections
  where
    antibodies = map (\(i, o) -> makeGroup i Antibody  o) (zip [0..] a)
    infections = map (\(i, o) -> makeGroup i Infection o) (zip [length a..] b)
    [a,b] = map (map parseLine . lines) (splitOn "\n\n" s)
    
    makeGroup groupId team (count, hp, attackType, damage, defenses, initiative) =
      Group { groupId = groupId,
              team = team,
              count = count,
              status = Active,
              initiative = initiative,
              defenses = defenses,
              hitPoints = hp,
              attack = Attack attackType damage }
    parseLine :: String -> (Count, HitPoints, DamageType, Damage, [Defense], Initiative)
    parseLine l = (ct, hp, attackType, read pts :: Int, concat ds, read i :: Int)
      where
        [u,d,a] = splitter "()" l
        [ct,hp] = map (\i -> read i :: Int) (splitter "abcdefghijklmnopqrstuvwxyz " u)
        [_,_,_,_,_,pts,damage,_,_,_,i] = splitter " " a
        attackType = stringToDamageType damage
        ds = map (buildDefenses . filter (/= "to") . splitter ", ") (splitter ";" d)
  
        buildDefenses (strength:types) =
          let
            s = case strength of "weak" -> Weak
                                 "immune" -> Immune
          in
            map (s  . stringToDamageType) types
    
        splitter s = split (dropInitBlank . dropFinalBlank . condense . dropDelims $ oneOf s)

        stringToDamageType d = case d of
                                 "fire"        -> Fire
                                 "radiation"   -> Radiation
                                 "bludgeoning" -> Bludgeoning
                                 "cold"        -> Cold
                                 "slashing"    -> Slashing
                                 _             -> error ("cannot parse " ++ d)

effectivePower g =
  count g * damage (attack g)
  where
    damage (Attack _ d) = d

attackDamage (Attack _ d) = d
attackType   (Attack t _) = t

targetSelection groups =
  map (\(a, b) -> (groupId a, groupId b)) (aux [] orderedMatches)
  where
    orderedMatches = map (\a -> (a, orderOpponents a groups)) (sortGroupsForTargetSelection (filter (\g -> status g == Active) groups))
    aux acc [] = acc
    aux acc ((a,ts):gs) = aux (acc ++ [(a, head ts')]) gs
      where
        ts' = ts \\ seen
        seen = map snd acc
        
    orderOpponents g ds =
      (map snd . sortBy s)
      [ (tupler g d, d) | d <- ds, canAttack g d ]
      where
        s (a, _) (b, _) = compare b a
        tupler g d = (groupDealsDamage g d, effectivePower d, initiative d)


    sortGroupsForTargetSelection = sortBy s
      where s g g' =
              let
                p = effectivePower g
                p' = effectivePower g'
                i = initiative g
                i' = initiative g'
              in
                case compare p' p of EQ -> compare i' i
                                     c  -> c
    canAttack a d = team a /= team d && a /= d && status d == Active
    
groupDealsDamage attacker target =
  effectivePower attacker * multiplier
  where
    t = attackType . attack $ attacker
    multiplier | Weak t `elem` defenses target   = 2
               | Immune t `elem` defenses target = 0
               | otherwise                       = 1

executeAttacks :: BattleState -> [(GroupId, GroupId)] -> BattleState
executeAttacks = error "foo"

dealDamage :: Damage -> Group -> Group
dealDamage damage g = case count g - (damage `div` hitPoints g) of
  c | c > 0 -> g { count = c }
  _         -> g { count = 0, status = Dead }

    
      
