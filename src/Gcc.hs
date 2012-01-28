{-# LANGUAGE UnicodeSyntax #-}

module Gcc where

data Stage = Preprocess | Compile | Assemble | Link | Run
  deriving (Show, Eq)
