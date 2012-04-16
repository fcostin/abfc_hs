module Abfc.Macros where

data LIdentifier = 
        Ident String |
        HiddenIdent String
    deriving (Eq, Show, Ord)

data LAddress =
        EnvGet LIdentifier |
        OuterEnvGet LIdentifier |
        AllocateLocal |
        StackAddressConstant Int
    deriving (Eq, Show)

data LConstant =
        IntConstant Int |
        CharConstant Char |
        StringConstant String |
        ArchStringConstant String
    deriving (Eq, Show)

data LArgument =
        IdentArg LIdentifier |
        Address LAddress |
        Constant LConstant
    deriving (Eq, Show)

data LStatement =
        EnvBegin |
        EnvEnd |
        EnvDeclare LIdentifier |
        EnvSet LIdentifier LAddress |
        EnvSetConstant LIdentifier LConstant |
        FreeLocalOnEnvExit LIdentifier |
        WhileBlock LIdentifier [LStatement] |
        IfBlock LIdentifier [LStatement] |
        UserMacroCall String [LArgument] |
        BuiltInCall String [LArgument]
    deriving (Eq, Show)

data LMacro = MacroDef String [LIdentifier] [LStatement]
    deriving (Eq, Show)

