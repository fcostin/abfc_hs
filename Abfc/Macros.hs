module Abfc.Macros where

data LIdentifier = 
        Ident String |
        HiddenIdent String
    deriving (Eq, Show)

data LValue =
        EnvGet LIdentifier |
        OuterEnvGet LIdentifier |
        AllocateLocal
    deriving (Eq, Show)

data LArgument =
        IdentArg LIdentifier |
        Value LValue |
        IntConstant Int |
        CharConstant Char |
        StringConstant String |
        ArchStringConstant String |
        StackAddressConstant Int
    deriving (Eq, Show)

data LStatement =
        EnvBegin |
        EnvEnd |
        EnvDeclare LIdentifier |
        EnvSet LIdentifier LValue |
        FreeLocalOnEnvExit LValue |
        BeginLoop LIdentifier |
        EndLoop LIdentifier |
        WhileBlock LIdentifier [LStatement] |
        IfBlock LIdentifier [LStatement] |
        UserMacroCall String [LArgument] |
        BuiltInCall String [LArgument]
    deriving (Eq, Show)

data LMacro = MacroDef String [LIdentifier] [LStatement]
    deriving (Eq, Show)

