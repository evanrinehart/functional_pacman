module Level where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative

import Linear
import Signal

data Level = Level
  {
    edges :: [Edge],
    nodes :: (Node, [Node]), -- empty level impossible
    layout :: Map NodeId R2,
    start :: L
  }

data Node = Node NodeId [Edge] | Jail NodeId [Edge]
data Edge = Edge EdgeId Node Node Length deriving (Show)
data L = LE Edge R | LN Node deriving (Show)
data Path = Path L [Node]

type Length = R
type Speed = R
type NodeId = Int
type EdgeId = Int
type Layout = Map NodeId R2

path0 :: L -> Path
path0 l = Path l []

levelMotion :: Path -> Speed -> Signal L
levelMotion (Path l nodes) speed = pure l

trivialLevel :: Level
trivialLevel = Level es ns lo st where
  es = []
  n0 = Node 0 []
  ns = (n0, [])
  lo = M.fromList [(0,(0,0))]
  st = LN n0

nodeId :: Node -> NodeId
nodeId (Node nid _) = nid
nodeId (Jail nid _) = nid

edgesOf :: Node -> [Edge]
edgesOf (Node _ es) = es
edgesOf (Jail _ es) = es

pathL :: Path -> L
pathL (Path l _) = l

instance Show Level where
  show lvl = "Level "++show (edges lvl)++" "++show (nodes lvl)

instance Show Node where
  show (Node nid _) = "N"++(show nid)
  show (Jail nid _) = "J"++(show nid)

-- a set of all such points forms a metric space
crossesL :: Path -> L -> Maybe (T,L)
crossesL path l = Nothing

--pathsCross :: G -> Path -> Path -> Maybe (T,L)
--ghostPath :: G -> L -> NodeId -> Path
--pacPath :: G -> Layout -> Pacman -> Path
someL :: Level -> L
someL lvl = LN l where (l, _) = nodes lvl

