{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Monoid
import Data.String (IsString(fromString))
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy as LText

data Strictness = Strict | NonStrict
  deriving (Show,Read)

data Directionality = Directed | Undirected
  deriving (Show,Read)

data CardinalDirection
  = North
  | East
  | South
  | West
  | Northeast
  | Northwest
  | Southeast
  | Southwest
  deriving (Show,Read)

data Element = Graph | Node | Edge
  deriving (Show,Read)

data EdgeElement = EdgeSubgraph Subgraph | EdgeNode NodeId
  deriving (Show,Read)

instance IsString EdgeElement where
  fromString str = EdgeNode (fromString str)

newtype Id = Id Text
  deriving (Show,Read,IsString)

data NodeId = NodeId Id (Maybe Port)
  deriving (Show,Read)

instance IsString NodeId where
  fromString str = NodeId (fromString str) Nothing

-- | Stole this from semigroups. Remove it once GHC 8.0 gains
--   widespread adoption.
data NonEmpty a = a :| [a]

data ListTwo a = ListTwo
  { listTwoFirst :: a
  , listTwoSecond :: a
  , listTwoOther :: [a]
  } deriving (Show,Read)

data Port = Port
  { portId :: Id
  , portCompass :: Maybe CardinalDirection
  } deriving (Show,Read)

data FullGraph = FullGraph Strictness Directionality (Maybe Id) [Statement]
  deriving (Show,Read)

data Statement
  = StatementAttribute AttributeStatement
  | StatementNode NodeStatement
  | StatementEdge EdgeStatement
  | StatementSubgraph Subgraph
  | StatementEquality Id Id
  deriving (Show,Read)

data AttributeStatement = AttributeStatement Element [Attribute]
  deriving (Show,Read)

data Attribute = Attribute Id Id
  deriving (Show,Read)

data NodeStatement = NodeStatement NodeId [Attribute]
  deriving (Show,Read)

data EdgeStatement = EdgeStatement Directionality (ListTwo EdgeElement) [Attribute]
  deriving (Show,Read)

data Subgraph = Subgraph
  { subgraphId :: Maybe Id
  , subgraphStatements :: [Statement]
  } deriving (Show,Read)

levelSpaces :: Int
levelSpaces = 2

indentationBuilder :: Builder
indentationBuilder = "  "

encode :: FullGraph -> Text
encode = LText.toStrict . encodeLazy

encodeLazy :: FullGraph -> LText.Text
encodeLazy = Builder.toLazyText . encodeBuilder

encodeBuilder :: FullGraph -> Builder
encodeBuilder (FullGraph strictness directionality mid statements) = mempty
  <> encodeStrictness strictness
  <> encodeGraphDirectionality directionality
  <> encodeMaybeId mid
  <> "{\n"
  <> foldr (\statement builder -> encodeStatement indentationBuilder statement <> builder) mempty statements
  <> "}"

encodeId :: Id -> Builder
encodeId (Id theId) = Builder.fromText theId

encodeNodeId :: NodeId -> Builder
encodeNodeId (NodeId theId mport) =
  encodeId theId <> maybe mempty encodePort mport

encodePort :: Port -> Builder
encodePort (Port theId mcompass) =
  ":" <> encodeId theId <> maybe mempty encodeCompass mcompass

encodeCompass :: CardinalDirection -> Builder
encodeCompass x = case x of
  North -> "n"
  East -> "e"
  South -> "s"
  West -> "w"
  Northeast -> "ne"
  Northwest -> "nw"
  Southeast -> "se"
  Southwest -> "sw"

encodeMaybeId :: Maybe Id -> Builder
encodeMaybeId x = case x of
  Just theId -> encodeId theId <> " "
  Nothing -> mempty

encodeStrictness :: Strictness -> Builder
encodeStrictness x = case x of
  Strict -> "strict "
  NonStrict -> mempty

encodeElement :: Element -> Builder
encodeElement x = case x of
  Graph -> "graph "
  Node -> "node "
  Edge -> "edge "

encodeSubgraph :: Subgraph -> Builder
encodeSubgraph = error "encodeSubgraph: have not written this function yet"

encodeStatement :: Builder -> Statement -> Builder
encodeStatement indentation x = case x of
  StatementAttribute (AttributeStatement element attrs) -> indentation
    <> encodeElement element
    <> encodeAttributes attrs
  StatementNode (NodeStatement theNodeId attrs) -> indentation
    <> encodeNodeId theNodeId
    <> encodeAttributes attrs
  StatementSubgraph subgraph -> encodeSubgraph subgraph
  StatementEdge (EdgeStatement directionality elements attrs) -> indentation
    <> encodeEdgeElements directionality elements
    <> encodeAttributes attrs
  StatementEquality a b -> indentation
    <> encodeId a <> " = " <> encodeId b <> "\n"
  where nextIndentation = indentationBuilder <> indentation

encodeEdgeOp :: Directionality -> Builder
encodeEdgeOp x = case x of
  Undirected -> " -- "
  Directed -> " -> "

encodeEdgeElements :: Directionality -> ListTwo EdgeElement -> Builder
encodeEdgeElements edgeOp (ListTwo a b xs) =
  encodeEdgeElement a <> edgeOpBuilder <> encodeEdgeElement b
  <> foldr (\e builder -> edgeOpBuilder <> encodeEdgeElement e <> builder) mempty xs
  where edgeOpBuilder = encodeEdgeOp edgeOp

encodeEdgeElement :: EdgeElement -> Builder
encodeEdgeElement x = case x of
  EdgeSubgraph subgraph -> encodeSubgraph subgraph
  EdgeNode theNodeId -> encodeNodeId theNodeId

encodeAttributes :: [Attribute] -> Builder
encodeAttributes (x : xs) = " ["
  <> foldr (\attr builder -> encodeAttribute attr <> "," <> builder) (encodeAttribute x) xs
  <> "];\n"
encodeAttributes [] = " [];\n"

encodeAttribute :: Attribute -> Builder
encodeAttribute (Attribute attrId valId) = encodeId attrId <> "=" <> encodeId valId

encodeGraphDirectionality :: Directionality -> Builder
encodeGraphDirectionality x = case x of
  Directed -> "digraph "
  Undirected -> "graph "

example :: FullGraph
example = FullGraph Strict Directed (Just "foobar")
  [ StatementNode $ NodeStatement "a1"
    [ Attribute "color" "blue"
    , Attribute "shape" "box"
    ]
  , StatementNode $ NodeStatement "a2" []
  , StatementEdge $ EdgeStatement Directed (ListTwo "a1" "a2" ["a3"])
    [ Attribute "color" "red"
    ]
  ]

