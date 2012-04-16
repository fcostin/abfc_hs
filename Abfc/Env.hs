module Abfc.Env (
    Env,
    root,
    end,
    begin,
    get_address,
    get_constant,
    outer_get_address,
    set_address,
    set_constant,
    add_to_deallocation_list,
    get_deallocation_list) where

import Abfc.Macros

import qualified Data.Map as Map

type Bindings a = Map.Map LIdentifier a

data Env =
    ChildEnv Env (Bindings LAddress) [LIdentifier] |
    RootEnv (Bindings LConstant)

root :: Env
root = RootEnv Map.empty

end :: Env -> Maybe Env
end (ChildEnv p _ _) = Just p
end (RootEnv _) = Nothing

begin :: Env -> Env
begin (RootEnv m) = ChildEnv (RootEnv m) Map.empty []
begin (ChildEnv p m d) = ChildEnv (ChildEnv p m d) Map.empty []

get_address :: LIdentifier -> Env -> Maybe LAddress
get_address k (ChildEnv p m _) =
    case Map.lookup k m of
        Just x -> Just x
        Nothing -> get_address k p
get_address k (RootEnv _) = Nothing

get_constant :: LIdentifier -> Env -> Maybe LConstant
get_constant k (ChildEnv p _ _) = get_constant k p
get_constant k (RootEnv m) = Map.lookup k m

outer_get_address :: LIdentifier -> Env -> Maybe LAddress
outer_get_address k (ChildEnv p _ _) = get_address k p
outer_get_address k (RootEnv _) = Nothing

set_address :: LIdentifier -> LAddress -> Env -> Env
set_address k v (ChildEnv p m d) = ChildEnv p (Map.insert k v m) d

set_constant :: LIdentifier -> LConstant -> Env -> Env
set_constant k v (ChildEnv p m d) = ChildEnv (set_constant k v p) m d
set_constant k v (RootEnv m) = RootEnv (Map.insert k v m)

add_to_deallocation_list :: LIdentifier -> Env -> Env
add_to_deallocation_list k (ChildEnv p m d) = ChildEnv p m (k:d)

get_deallocation_list :: Env -> [LIdentifier]
get_deallocation_list (ChildEnv _ _ d) = d
get_deallocation_list (RootEnv _) = []
