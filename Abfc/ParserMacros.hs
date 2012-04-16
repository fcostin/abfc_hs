module Abfc.ParserMacros where

-- [data types for parsed macro programs]
data KeywordIdentifier = Keyword String
    deriving (Eq, Show)

data Identifier = Ident String
    deriving (Eq, Show)

data StringLiteral = StringLit String |
        ArchStringLit String
    deriving (Eq, Show)

data Argument = IdentArg Identifier |
        StringConstantArg StringLiteral |
        IntConstantArg Int |
        StackAddressArg Int |
        CharConstantArg Char
    deriving (Eq, Show)

data Statement = CallKeyword KeywordIdentifier [Argument] |
        IfBlock Identifier [Statement] |
        WhileBlock Identifier [Statement]
    deriving (Eq, Show)

data Macro = MacroDef [Identifier] [Statement]
    deriving (Eq, Show)
