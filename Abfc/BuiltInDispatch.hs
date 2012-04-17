module Abfc.BuiltInDispatch where

import qualified Abfc.Machine as Bf
import qualified Abfc.MachineCodegen as C
import qualified Abfc.Allocator as Alloc
import Abfc.ResolvedArg

call_built_in :: String -> [ResolvedArg] -> Bf.Machine -> Alloc.Allocator -> (String, Bf.Machine, Alloc.Allocator)



call_built_in "BEGIN_LOOP" [AddressLiteral dst] bf alloc = C.begin_loop dst bf alloc
call_built_in "END_LOOP" [AddressLiteral dst] bf alloc = C.end_loop dst bf alloc



call_built_in "CLEAR" [AddressLiteral dst] bf alloc = C.clear dst bf alloc



call_built_in "DESTRUCTIVE_ADD" [AddressLiteral src, AddressLiteral dst] bf alloc = C.destructive_add src dst bf alloc
call_built_in "DESTRUCTIVE_SUB" [AddressLiteral src, AddressLiteral dst] bf alloc = C.destructive_sub src dst bf alloc



call_built_in "MOVE" [AddressLiteral src, AddressLiteral dst] bf alloc = C.move src dst bf alloc
call_built_in "COPY" [AddressLiteral src, AddressLiteral dst] bf alloc = C.copy src dst bf alloc



call_built_in "STACK_ADD" [AddressLiteral src, AddressLiteral dst] bf alloc = C.stack_add src dst bf alloc
call_built_in "STACK_SUB" [AddressLiteral src, AddressLiteral dst] bf alloc = C.stack_sub src dst bf alloc



call_built_in "CONSTANT_ADD" [IntLiteral n, AddressLiteral dst] bf alloc = C.constant_add n dst bf alloc
call_built_in "CONSTANT_SUB" [IntLiteral n, AddressLiteral dst] bf alloc = C.constant_sub n dst bf alloc



call_built_in "AS_LOGICAL" [AddressLiteral src, AddressLiteral dst] bf alloc = C.as_logical src dst bf alloc
call_built_in "LOGICAL_NOT" [AddressLiteral src, AddressLiteral dst] bf alloc = C.logical_not src dst bf alloc 
call_built_in "LOGICAL_OR" [AddressLiteral src_a, AddressLiteral src_b, AddressLiteral dst] bf alloc = C.logical_or src_a src_b dst bf alloc
call_built_in "LOGICAL_AND" [AddressLiteral src_a, AddressLiteral src_b, AddressLiteral dst] bf alloc = C.logical_and src_a src_b dst bf alloc



call_built_in "GET_CHAR" [AddressLiteral dst] bf alloc = C.get_char dst bf alloc
call_built_in "PUT_CHAR" [AddressLiteral src] bf alloc = C.put_char src bf alloc



call_built_in "PUT_STRING_CONSTANT" [StringLiteral s] bf alloc = C.put_string_constant s bf alloc



call_built_in "GROW_STACK" [IntLiteral n] bf alloc = C.grow_stack n bf alloc
call_built_in "SHRINK_STACK" [IntLiteral n] bf alloc = C.shrink_stack n bf alloc



call_built_in name args _ _ = error ("Error: undefined builtin call: " ++ (show (name, args)))

