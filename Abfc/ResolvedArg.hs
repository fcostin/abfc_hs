module Abfc.ResolvedArg where

data ResolvedArg =
        IntLiteral Int |
        StringLiteral String |
        AddressLiteral Int
    deriving (Eq, Show)
