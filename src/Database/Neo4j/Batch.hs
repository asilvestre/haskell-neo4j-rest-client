{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Database.Neo4j.Batch (
    Batch, runBatch,
    createNode, getNode, deleteNode
    )where

import Database.Neo4j.Batch.Node
import Database.Neo4j.Batch.Types
