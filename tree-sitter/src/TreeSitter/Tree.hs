{-# LANGUAGE DeriveGeneric #-}
module TreeSitter.Tree
( Tree
, withRootNode
, ts_tree_delete
, ts_tree_root_node_p
-- * Editing the tree
, TSInputEdit(..)
, ts_tree_edit
) where

import Foreign
import GHC.Generics
import TreeSitter.Node

-- | This type is uninhabited and used only for type safety within 'Ptr' values.
data Tree

withRootNode :: Ptr Tree -> (Ptr Node -> IO a) -> IO a
withRootNode tree action = alloca $ \ ptr -> do
  ts_tree_root_node_p tree ptr
  action ptr

foreign import ccall safe "ts_tree_delete" ts_tree_delete :: Ptr Tree -> IO ()
foreign import ccall unsafe "src/bridge.c ts_tree_root_node_p" ts_tree_root_node_p :: Ptr Tree -> Ptr Node -> IO ()

-- ---------------------------------------------------------------------

-- typedef struct {
--   uint32_t start_byte;
--   uint32_t old_end_byte;
--   uint32_t new_end_byte;
--   TSPoint start_point;
--   TSPoint old_end_point;
--   TSPoint new_end_point;
-- } TSInputEdit;
data TSInputEdit = TSInputEdit
  { editStartByte   :: !Word32
  , editOldEndByte  :: !Word32
  , editNewEndByte  :: !Word32
  , editStartPoint  :: !TSPoint
  , editOldEndPoint :: !TSPoint
  , editNewEndPoint :: !TSPoint
  }
  deriving (Show, Eq, Generic)

-- void ts_tree_edit(TSTree *, const TSInputEdit *);
foreign import ccall safe "ts_tree_edit" ts_tree_edit :: Ptr Tree -> Ptr TSInputEdit -> IO ()

-- ---------------------------------------------------------------------

instance Storable TSInputEdit where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 12 + 3 * 8
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSInputEdit <$> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
                                  <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSInputEdit sb oeb neb sp oep nep) = flip evalStruct ptr $ do
    pokeStruct sb
    pokeStruct oeb
    pokeStruct neb
    pokeStruct sp
    pokeStruct oep
    pokeStruct nep
  {-# INLINE poke #-}

-- ---------------------------------------------------------------------
