{-# LANGUAGE OverloadedStrings #-}

module Data.Graph.Immutable.Dot
  ( toDotGraph
  , toLabeledDotGraph
  ) where

import Data.Graph.Types
import Dot.Types
import Control.Applicative
import Data.Text (Text)
import qualified Data.Graph.Immutable as Graph
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

-- | This is a fairly general way to build a dot file from a graph.
toDotGraph :: Directionality -> (v -> [Attribute]) -> (v -> v -> e -> [Attribute]) -> Graph g e v -> DotGraph
toDotGraph directionality vertexAttrs edgeAttrs g =
  DotGraph strictness directionality Nothing $ []
    ++ vertexDeclarations
    ++ edgeDeclarations
  where
  vertexDeclarations = getConst $ flip Graph.traverseVertices_ g $ \vertex v ->
    Const [StatementNode $ NodeStatement (vertexToNodeId vertex) (vertexAttrs v)]
  edgeDeclarations = getConst $ flip Graph.traverseEdges_ g $ \vertexFrom vertexTo vFrom vTo e ->
    Const [StatementEdge $ EdgeStatement
             (ListTwo
               (EdgeNode (vertexToNodeId vertexFrom))
               (EdgeNode (vertexToNodeId vertexTo))
               []
             )
             (edgeAttrs vFrom vTo e)
          ]
  strictness = case directionality of
    Undirected -> NonStrict
    Directed -> Strict

-- | This is a more convenient variant of 'toDotGraph' that just labels
-- all of the nodes and edges. It does not color or style anything.
toLabeledDotGraph :: Directionality -> (v -> Text) -> (e -> Text) -> Graph g e v -> DotGraph
toLabeledDotGraph dir vertexLabel edgeLabel =
  toDotGraph dir (textToLabel . vertexLabel) (const $ const $ textToLabel . edgeLabel)

textToLabel :: Text -> [Attribute]
textToLabel name = [Attribute "label" (Id name)]

vertexToNodeId :: Vertex g -> NodeId
vertexToNodeId = id
  . flip NodeId Nothing . Id
  . LText.toStrict . Builder.toLazyText
  . mappend "a" . Builder.decimal . Graph.vertexInt

