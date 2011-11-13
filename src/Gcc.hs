module Gcc where

data Stage = Compile | Assemble | Link | Run
  deriving (Show, Eq)
