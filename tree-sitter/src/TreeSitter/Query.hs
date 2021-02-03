{-# LANGUAGE DeriveGeneric #-}

module TreeSitter.Query
  (
    TSQuery
  , TSQueryCursor
  , TSQueryError(..)
  , TSQueryMatch(..)
  , TSQueryCapture(..)
  , ts_query_new
  , ts_query_cursor_new
  , ts_query_cursor_delete
  , ts_query_cursor_exec_p
  , ts_query_cursor_next_match
  ) where

import Foreign
import Foreign.C
import GHC.Generics
import TreeSitter.Language
import TreeSitter.Node

-- ---------------------------------------------------------------------

-- | A tree-sitter query.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data TSQuery

-- | A tree-sitter query cursor.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data TSQueryCursor

-- ---------------------------------------------------------------------

-- typedef enum {
--   TSQueryErrorNone = 0,
--   TSQueryErrorSyntax,
--   TSQueryErrorNodeType,
--   TSQueryErrorField,
--   TSQueryErrorCapture,
-- } TSQueryError;

data TSQueryError
  = TSQueryErrorNone
  | TSQueryErrorSyntax
  | TSQueryErrorNodeType
  | TSQueryErrorField
  | TSQueryErrorCapture
  deriving (Show, Eq, Generic)

instance Enum TSQueryError where
  toEnum 0 = TSQueryErrorNone
  toEnum 1 = TSQueryErrorSyntax
  toEnum 2 = TSQueryErrorNodeType
  toEnum 3 = TSQueryErrorField
  toEnum _ = TSQueryErrorCapture

  fromEnum TSQueryErrorNone     = 0
  fromEnum TSQueryErrorSyntax   = 1
  fromEnum TSQueryErrorNodeType = 2
  fromEnum TSQueryErrorField    = 3
  fromEnum TSQueryErrorCapture  = 4

-- ---------------------------------------------------------------------

-- typedef struct {
--   uint32_t id;
--   uint16_t pattern_index;
--   uint16_t capture_count;
--   const TSQueryCapture *captures;
-- } TSQueryMatch;

data TSQueryMatch = TSQueryMatch
  { qmId :: Word32,
    qmPatternIndex :: Word16,
    qmCaptureCount :: Word16,
    qmCaptures :: Ptr TSQueryCapture
  }
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------

instance Storable TSQueryMatch where
  alignment _ = alignment (undefined :: TSQueryMatch)
  {-# INLINE alignment #-}
  sizeOf _ = 4 + 2 * 2 + 8
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSQueryMatch <$> peekStruct
                                   <*> peekStruct
                                   <*> peekStruct
                                   <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryMatch i p cc c) = flip evalStruct ptr $ do
    pokeStruct i
    pokeStruct p
    pokeStruct cc
    pokeStruct c
  {-# INLINE poke #-}

-- ---------------------------------------------------------------------

-- typedef struct {
--   TSNode node;
--   uint32_t index;
-- } TSQueryCapture;

data TSQueryCapture = TSQueryCapture
  { qcNode :: TSNode,
    qcIndex :: Word32
  }
  deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------

instance Storable TSQueryCapture where
  alignment _ = alignment (undefined :: TSQueryCapture)
  {-# INLINE alignment #-}
  sizeOf _ = (4 * 4 + 2 * 8) + 4
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSQueryCapture <$> peekStruct
                                     <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSQueryCapture n i) = flip evalStruct ptr $ do
    pokeStruct n
    pokeStruct i
  {-# INLINE poke #-}

-- ---------------------------------------------------------------------

-- ts_query_new
-- TSQuery * ts_query_new
--   ( const TSLanguage *language
--   , const char *source
--   , uint32_t source_len
--   , uint32_t *error_offset
--   , TSQueryError *error_type)

foreign import ccall safe "ts_query_new" ts_query_new
  :: Ptr Language -> CString -> Word32
  -> Ptr Word32 -> Ptr TSQueryError -> IO (Ptr TSQuery)

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

-- ts_query_pattern_count
-- ts_query_capture_count
-- ts_query_string_count
-- ts_query_start_byte_for_pattern
-- ts_query_predicates_for_pattern
-- ts_query_capture_name_for_id
-- ts_query_string_value_for_id
-- ts_query_disable_capture
-- ts_query_disable_pattern
-- ---------------------------------------------------------------------

{-
 * Create a new cursor for executing a given query.
 *
 * The cursor stores the state that is needed to iteratively search
 * for matches. To use the query cursor, first call `ts_query_cursor_exec`
 * to start running a given query on a given syntax node. Then, there are
 * two options for consuming the results of the query:
 * 1. Repeatedly call `ts_query_cursor_next_match` to iterate over all of the
 *    the *matches* in the order that they were found. Each match contains the
 *    index of the pattern that matched, and an array of captures. Because
 *    multiple patterns can match the same set of nodes, one match may contain
 *    captures that appear *before* some of the captures from a previous match.
 * 2. Repeatedly call `ts_query_cursor_next_capture` to iterate over all of the
 *    individual *captures* in the order that they appear. This is useful if
 *    don't care about which pattern matched, and just want a single ordered
 *    sequence of captures.
 *
 * If you don't care about consuming all of the results, you can stop calling
 * `ts_query_cursor_next_match` or `ts_query_cursor_next_capture` at any point.
 *  You can then start executing another query on another node by calling
 *  `ts_query_cursor_exec` again.
TSQueryCursor *ts_query_cursor_new(void);
 -}
-- ts_query_cursor_new
foreign import ccall safe "ts_query_cursor_new" ts_query_cursor_new
  :: IO (Ptr TSQueryCursor)

-- ---------------------------------------------------------------------
{-
 * Delete a query cursor, freeing all of the memory that it used.
 */
void ts_query_cursor_delete(TSQueryCursor *);
-}
-- ts_query_cursor_delete
foreign import ccall safe "ts_query_cursor_delete" ts_query_cursor_delete
  :: Ptr TSQueryCursor -> IO ()

-- ---------------------------------------------------------------------

{-
 * Start running a given query on a given node.
 */
void ts_query_cursor_exec(TSQueryCursor *, const TSQuery *, TSNode);
-}
-- ts_query_cursor_exec
foreign import ccall safe "src/bridge.c ts_query_cursor_exec_p" ts_query_cursor_exec_p
  :: Ptr TSQueryCursor -> Ptr TSQuery -> Ptr TSNode -> IO ()

-- ---------------------------------------------------------------------
-- ts_query_cursor_set_byte_range
-- ts_query_cursor_set_point_range
-- ---------------------------------------------------------------------

{-
 * Advance to the next match of the currently running query.
 *
 * If there is a match, write it to `*match` and return `true`.
 * Otherwise, return `false`.
 *
bool ts_query_cursor_next_match(TSQueryCursor *, TSQueryMatch *match);
-}
-- ts_query_cursor_next_match
foreign import ccall safe "ts_query_cursor_next_match" ts_query_cursor_next_match
  :: Ptr TSQueryCursor -> Ptr TSQueryMatch -> IO Bool

-- ---------------------------------------------------------------------
-- void ts_query_cursor_remove_match(TSQueryCursor *, uint32_t id);
-- ts_query_cursor_remove_match
-- ---------------------------------------------------------------------
-- ts_query_cursor_next_capture

-- ---------------------------------------------------------------------

-- This section based on
-- https://stackoverflow.com/questions/46760015/haskell-ffi-using-data-types-in-a-c-program

type C_TSQueryError = CInt

instance Storable TSQueryError where
  sizeOf _ = sizeOf (undefined :: C_TSQueryError)
  alignment _ = alignment (undefined :: C_TSQueryError)
  peek ptr = genToEnum <$> peek (castPtr ptr :: Ptr C_TSQueryError)
  poke ptr val = poke (castPtr ptr :: Ptr C_TSQueryError) (genFromEnum val)

genToEnum :: (Integral a, Enum b) => a -> b
genToEnum = toEnum . fromIntegral

genFromEnum :: (Integral a, Enum b) => b -> a
genFromEnum = fromIntegral . fromEnum

-- ---------------------------------------------------------------------
