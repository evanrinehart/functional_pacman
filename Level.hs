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

{- coords
we use R2 as coordinates in three ways

[0,16] x [0,18]
first the level defines a mapping from nodes to R2
this R2 should be "game space" where 0 is the left side of the screen
and maze columns increase to the right by 1. similarly for rows, 0 at the
top and each row increases y coord by 1 as you go down. these coordinates
are fed to the renderer. my reference maze has 17 columns and 19 rows.

[0,640] x [0,480]
the renderer must map game coordinates to a rectangular space, making sure
to wrap the negative and greater-than-max x values according to the cylindrical
topology of the game. this coordinate system should put the left side of the
max at 0 x, and increase by 1 for each pixel, more closely resembling what we want.
this should push the maze over to the center of screen.

[-1,1] x [-1,1]
a final transformation is done by the renderer to account for aspect ratio
and mapping from the pixel based coordinates to one where the entire window
is mapped to the region [-1,-1] x [1,1] according to graphics drawing combinators.

-}

-- a point in the level
-- a set of all such points forms a metric space
crossesL :: Path -> L -> Maybe (T,L)
crossesL path l = Nothing

--pathsCross :: G -> Path -> Path -> Maybe (T,L)
--ghostPath :: G -> L -> NodeId -> Path
--pacPath :: G -> Layout -> Pacman -> Path
someL :: Level -> L
someL lvl = LN l where (l, _) = nodes lvl

