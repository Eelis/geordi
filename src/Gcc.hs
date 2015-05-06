{-# LANGUAGE UnicodeSyntax #-}

module Gcc where

import Data.Maybe (isJust)
import Text.Regex (mkRegex, matchRegex)

data Stage = Preprocess | Analyze | Compile | Assemble | Link | Run
  deriving (Show, Eq, Enum, Ord)

isMainMissingDiagnostic :: String → Bool
isMainMissingDiagnostic =
  isJust . matchRegex (mkRegex "undefined reference to [^[:alnum:]]+main[^[:alnum:]]+$")
