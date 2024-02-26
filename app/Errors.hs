module Errors where

data Error = FileError {msg :: String, line :: Integer} | ParseError {msg :: String}