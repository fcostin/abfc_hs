module Abfc.Allocator (
    Allocator,
    initial,
    next_free_cell,
    allocated_cells,
    mark_cell_used,
    free_cell) where

import qualified Data.Set as Set

import Abfc.Macros

type Allocator = Set.Set Int

initial :: Allocator
initial = Set.empty

next_free_cell :: Allocator -> LAddress
next_free_cell alloc = StackAddressConstant (if Set.null alloc then 0 else ((Set.findMax alloc) + 1))

allocated_cells :: Allocator -> [LAddress]
allocated_cells alloc = map StackAddressConstant (Set.toAscList alloc)

mark_cell_used :: LAddress -> Allocator -> Allocator
mark_cell_used (StackAddressConstant c) alloc = Set.insert c alloc

free_cell :: LAddress -> Allocator -> Allocator
free_cell (StackAddressConstant c) alloc = Set.delete c alloc
