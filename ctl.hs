-- simple model checker for computation tree logic (CTL)

import qualified Data.Set as Set
import qualified Data.Map as Map

type State = String
type Prop = String
data Model = Model { states :: Set.Set State
                   , transitions :: Set.Set (State, State)
                   , labels :: Map.Map Prop (Set.Set State) }

succs model state = Set.map snd (Set.filter ((==) state . fst) (transitions model))

predsA model set = Set.filter (flip Set.isSubsetOf set . succs model) (states model)
predsE model set = Set.filter (not . Set.null . Set.intersection set . succs model) (states model)

data Formula = Atom Prop
  | F | T | Neg Formula | And Formula Formula | Or Formula Formula
  | EX Formula | AX Formula
  | EF Formula | AF Formula
  | EG Formula | AG Formula
  | EU Formula Formula | AU Formula Formula
  | ER Formula Formula | AR Formula Formula

fix f x = let y = f x in
  if y == x then x else fix f y

sat model formula = case formula of
  Atom prop   -> labels model Map.! prop

  F           -> Set.empty
  T           -> states model

  Neg p       -> Set.difference (states model) (sat model p)
  Or p q      -> Set.union (sat model p) (sat model q)
  And p q     -> Set.intersection (sat model p) (sat model q)

  EX p        -> predsE model (sat model p)
  AX p        -> predsA model (sat model p)

  EF p        -> fix (Set.union (sat model p) . predsE model) Set.empty
  AF p        -> fix (Set.union (sat model p) . predsA model) Set.empty

  EG p        -> fix (Set.intersection (sat model p) . predsE model) (states model)
  AG p        -> fix (Set.intersection (sat model p) . predsA model) (states model)

  EU p q      -> fix (Set.union (sat model q) . Set.intersection (sat model p) . predsE model) Set.empty
  AU p q      -> fix (Set.union (sat model q) . Set.intersection (sat model p) . predsA model) Set.empty
  
  ER p q      -> fix (Set.intersection (sat model q) . Set.union (sat model p) . predsE model) (states model)
  AR p q      -> fix (Set.intersection (sat model q) . Set.union (sat model p) . predsA model) (states model)

model = Model
  (Set.fromList ["S1", "S2", "S3", "S4"])
  (Set.fromList [("S2","S3"), ("S1","S3"), ("S2","S3"), ("S2","S4"), ("S1","S1"), ("S1","S4")])
  (Map.fromList [ ("P1", Set.fromList ["S3", "S4"])
                , ("P2", Set.fromList ["S3", "S2"])
                , ("P3", Set.fromList ["S1", "S2"])])

main = putStrLn $ show $ sat model (ER (Atom "P1") (AG (Atom "P2")))
