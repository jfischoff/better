module BettingGraph where
    
import Data.Graph.Inductive

data MyNode = User String
             | Computer
             
data MyEdge = 