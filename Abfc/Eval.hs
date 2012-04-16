module Abfc.Eval where

import qualified Abfc.Env as Env
import qualified Abfc.Allocator as Alloc
import qualified Abfc.Machine as Bf
import qualified Abfc.MachineCodegen as Codegen
import Data.Maybe (catMaybes)
import Abfc.Macros

evaluate :: [LStatement] -> [String]
evaluate statements = eval_statements statements Env.root Bf.initial Alloc.initial []

eval_statements :: [LStatement] -> Env.Env -> Bf.Machine -> Alloc.Allocator -> [String] -> [String]
eval_statements statements env machine alloc code =
    case statements of

        (EnvBegin):xs -> let
                env' = (Env.begin env)
            in
                eval_statements xs env' machine alloc code

        (EnvEnd):xs -> let
                variables = Env.get_deallocation_list env
                addresses = catMaybes $ map (\k -> Env.get_address k env) variables
                alloc' = deallocate addresses alloc
                env' = Env.end env
            in case Env.end env of
                Just env' -> eval_statements xs env' machine alloc' code

        (EnvDeclare _):xs -> eval_statements xs env machine alloc code -- EnvDeclare is ignored

        (EnvSet k AllocateLocal):xs -> let
                addr = Alloc.next_free_cell alloc
                alloc' = Alloc.mark_cell_used addr alloc
                env' = Env.set_address k addr env
            in
                eval_statements xs env' machine alloc' code

        (EnvSet k (OuterEnvGet k')):xs ->
                case (Env.get_address k' env) of
                    Just addr -> let
                            x' = (EnvSet k addr)
                        in
                            eval_statements (x':xs) env machine alloc code

        (EnvSet k (StackAddressConstant addr)):xs -> let
                env' = Env.set_address k (StackAddressConstant addr) env
            in
                eval_statements xs env' machine alloc code

        (EnvSetConstant k c):xs -> let
                env' = Env.set_constant k c env
            in
                eval_statements xs env' machine alloc code

        (FreeLocalOnEnvExit k):xs -> let
                env' = Env.add_to_deallocation_list k env
            in
                eval_statements xs env' machine alloc code

        (BuiltInCall x args):xs -> let
                args' = map (\arg -> lookup_arg arg env) args
                (ops, machine', alloc') = call_built_in x args' machine alloc
                code' = (ops:code)
            in
                eval_statements xs env machine' alloc' code'

        (x:xs) -> error ("Error: cannot evaluate statement: " ++ show x)

        [] -> reverse code




deallocate :: [LAddress] -> Alloc.Allocator -> Alloc.Allocator
deallocate ((StackAddressConstant c):xs) alloc = let
        alloc' = Alloc.free_cell (StackAddressConstant c) alloc
    in deallocate xs alloc'
deallocate [] alloc = alloc

data ResolvedArg =
        IntLiteral Int |
        CharLiteral Char |
        StringLiteral String |
        AddressLiteral Int
    deriving (Eq, Show)

lookup_arg :: LArgument -> Env.Env -> ResolvedArg
lookup_arg arg env =
    case arg of
        IdentArg k ->
            case (Env.get_constant k env) of
                Just x -> lookup_arg (Constant x) env
                Nothing ->
                    case (Env.get_address k env) of
                        Just (StackAddressConstant x) -> AddressLiteral x
        Address (StackAddressConstant x) -> AddressLiteral x
        Constant (IntConstant x) -> IntLiteral x
        Constant (CharConstant x) -> CharLiteral x
        Constant (StringConstant x) -> StringLiteral x
        Constant (ArchStringConstant x) -> StringLiteral ("arch" ++ x) -- XXX TODO
        _ -> error ("Error: cannot evaluate argument: " ++ show arg)

call_built_in :: String -> [ResolvedArg] -> Bf.Machine -> Alloc.Allocator -> (String, Bf.Machine, Alloc.Allocator)

call_built_in name args machine alloc = (("call " ++ name ++ " with args " ++ show args), machine, alloc)
