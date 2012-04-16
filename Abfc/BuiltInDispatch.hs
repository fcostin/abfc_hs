module Abfc.BuiltInDispatch where

import qualified Abfc.Machine as Bf
import qualified Abfc.MachineCodegen as Codegen
import qualified Abfc.Allocator as Alloc
import Abfc.ResolvedArg

call_built_in :: String -> [ResolvedArg] -> Bf.Machine -> Alloc.Allocator -> (String, Bf.Machine, Alloc.Allocator)

call_built_in "CLEAR" [AddressLiteral dst] machine alloc = Codegen.clear dst machine alloc
call_built_in "BEGIN_LOOP" [AddressLiteral dst] machine alloc = Codegen.begin_loop dst machine alloc
call_built_in "END_LOOP" [AddressLiteral dst] machine alloc = Codegen.end_loop dst machine alloc
call_built_in "COPY" [AddressLiteral src, AddressLiteral dst] machine alloc = Codegen.copy src dst machine alloc

call_built_in name args machine alloc = (("unknown call " ++ name ++ " with args " ++ show args), machine, alloc)
