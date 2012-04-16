module Abfc.ResolvedArg where

data ResolvedArg =
        IntLiteral Int |
        CharLiteral Char |
        StringLiteral String |
        AddressLiteral Int
    deriving (Eq, Show)
