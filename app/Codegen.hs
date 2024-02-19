{-
   This Source Code Form is subject to the terms of the Mozilla Public
   License v. 2.0. If a copy of the MPL was not distributed with this
   file You can obtain one at https:--mozilla.org/MPL/2.0/.
-}

module Codegen where
import Header
import Data.Word (Word8)


go :: [Stmt U] -> SLC [Word8]
go = undefined

data Op = ReqOp             -- 0x00
        | RegionOp          -- 0x01
        | HeapOp            -- 0x02
        | CapOp             -- 0x03
        | CapLEOp           -- 0x04
        | UniqueOp          -- 0x05
        | RWOp              -- 0x06
        | BothOp            -- 0x07
        | HandleOp          -- 0x08
        | I32Op             -- 0x09
        | EndFunctionOp     -- 0x0A
        | MutOp             -- 0x0B
        | TupleOp Int       -- 0x0C
        | ArrOp             -- 0x0D
        | AllOp             -- 0x0E
        | SomeOp            -- 0x0F
        | EmosOp            -- 0x10
        | FuncOp Int        -- 0x11
        | CTGetOp Int       -- 0x12
        | CTPopOp           -- 0x13
        | UnpackOp          -- 0x14
        | GetOp Int         -- 0x15
        | InitOp Int        -- 0x16
        | MallocOp          -- 0x17
        | ProjOp Int        -- 0x18
        | CallOp            -- 0x19
        | PrintOp           -- 0x1A
        | LitOp Int         -- 0x1B
        | GlobalFuncOp Int  -- 0x1C
        | HaltOp Int        -- 0x1D
        | PackOp            -- 0x1E
        | Word32Op          -- 0x1F
        | Word64Op          -- 0x20
        | PtrOp             -- 0x21
        | ReprsOp Int       -- 0x22
        | NewRgnOp          -- 0x23
        | FreeRgnOp         -- 0x24

