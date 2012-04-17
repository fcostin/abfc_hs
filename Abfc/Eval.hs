module Abfc.Eval where

import qualified Abfc.Env as Env
import qualified Abfc.Allocator as Alloc
import qualified Abfc.Machine as Bf
import qualified Data.Char
import Data.Maybe (catMaybes)
import Data.Either (lefts, rights)
import Abfc.Macros
import Abfc.ResolvedArg
import Abfc.BuiltInDispatch (call_built_in)


type DebugInfo = String
type Code = (String, DebugInfo)

evaluate :: [LStatement] -> [Code]
evaluate statements = eval_statements statements Env.root Bf.initial Alloc.initial []

eval_statements :: [LStatement] -> Env.Env -> Bf.Machine -> Alloc.Allocator -> [Code] -> [Code]
eval_statements statements env machine alloc code =
    case statements of

        (EnvBegin):xs -> let
                env' = (Env.begin env)
            in
                eval_statements xs env' machine alloc code

        (EnvEnd):xs -> let
                variables = Env.get_deallocation_list env
                addresses = lefts $ catMaybes $ map (\k -> Env.get_value k env) variables
                alloc' = deallocate addresses alloc
                Just env' = Env.end env
            in
                eval_statements xs env' machine alloc' code

        (EnvSet k AllocateLocal):xs -> let
                addr = Alloc.next_free_cell alloc
                alloc' = Alloc.mark_cell_used addr alloc
                env' = Env.set_address k addr env
            in
                eval_statements xs env' machine alloc' code

        (EnvSet k (OuterEnvGet k')):xs -> let
                Just (Left address) = Env.outer_get_value k' env
                x' = (EnvSet k address)
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
                
                code' = ((ops, show (x, args, args')):code)
            in
                eval_statements xs env machine' alloc' code'

        (x:xs) -> error ("Error: cannot evaluate statement: " ++ show x)

        [] -> reverse code




deallocate :: [LAddress] -> Alloc.Allocator -> Alloc.Allocator
deallocate ((StackAddressConstant c):xs) alloc = let
        alloc' = Alloc.free_cell (StackAddressConstant c) alloc
    in deallocate xs alloc'
deallocate [] alloc = alloc

lookup_arg :: LArgument -> Env.Env -> ResolvedArg
lookup_arg arg env =
    case arg of
        IdentArg k ->
            case (Env.get_value k env) of
                Just (Left address) -> lookup_arg (Address address) env
                Just (Right constant) -> lookup_arg (Constant constant) env
                Nothing -> error ("Env: cannot resolve arg lookup: " ++ (show arg))
        Address (StackAddressConstant x) -> AddressLiteral x
        Constant (IntConstant x) -> IntLiteral x
        Constant (CharConstant x) -> IntLiteral (Data.Char.ord x)
        Constant (StringConstant x) -> StringLiteral x
        Constant (ArchStringConstant x) -> StringLiteral ("arch" ++ x) -- XXX TODO
        _ -> error ("Error: cannot evaluate argument: " ++ show arg)

