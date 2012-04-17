module Abfc.Env (
    Env,
    root,
    end,
    begin,
    get_value,
    outer_get_value,
    set_address,
    set_constant,
    add_to_deallocation_list,
    get_deallocation_list) where

import Abfc.Macros

import qualified Data.Map as Map

type Bindings a = Map.Map LIdentifier a
type Value = Either LAddress LConstant

data Env =
    ChildEnv Env (Bindings Value) [LIdentifier] |
    RootEnv

root :: Env
root = RootEnv

end :: Env -> Maybe Env
end (ChildEnv p _ _) = Just p
end RootEnv = Nothing

begin :: Env -> Env
begin RootEnv = ChildEnv RootEnv Map.empty []
begin (ChildEnv p m d) = ChildEnv (ChildEnv p m d) Map.empty []

get_value :: LIdentifier -> Env -> Maybe Value
get_value k (ChildEnv p m _) =
    case Map.lookup k m of
        Just x -> Just x
        Nothing -> get_value k p
get_value k RootEnv = Nothing

outer_get_value :: LIdentifier -> Env -> Maybe Value
outer_get_value k (ChildEnv p _ _) = get_value k p
outer_get_value k RootEnv = Nothing

set_address :: LIdentifier -> LAddress -> Env -> Env
set_address k v (ChildEnv p m d) = ChildEnv p (Map.insert k (Left v) m) d

set_constant :: LIdentifier -> LConstant -> Env -> Env
set_constant k v (ChildEnv p m d) = ChildEnv p (Map.insert k (Right v) m) d

add_to_deallocation_list :: LIdentifier -> Env -> Env
add_to_deallocation_list k (ChildEnv p m d) = ChildEnv p m (k:d)

get_deallocation_list :: Env -> [LIdentifier]
get_deallocation_list (ChildEnv _ _ d) = d
get_deallocation_list RootEnv = []
