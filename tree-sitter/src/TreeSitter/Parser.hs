{-# LANGUAGE DeriveGeneric #-}
module TreeSitter.Parser
( Parser
, withParser
, withParseTree
, ts_parser_new
, ts_parser_halt_on_error
, ts_parser_parse_string
, ts_parser_parse_p1
, TSHInput(..)
, Payload
, ReadFunction
, mkReadFunction
, TSInputEncoding(..)
, ts_parser_delete
, ts_parser_set_language
, ts_parser_timeout_micros
, ts_parser_set_timeout_micros
, ts_parser_log_to_stderr
) where

import Control.Exception as Exc
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign
import Foreign.C
import GHC.Generics
import TreeSitter.Language
import TreeSitter.Node
import TreeSitter.Tree

-- | A tree-sitter parser.
--
--   This type is uninhabited and used only for type safety within 'Ptr' values.
data Parser

withParser :: Ptr Language -> (Ptr Parser -> IO a) -> IO a
withParser language action = Exc.bracket
  ts_parser_new
  ts_parser_delete
  $ \ parser -> do
    _ <- ts_parser_set_language parser language
    action parser

withParseTree :: Ptr Parser -> ByteString -> (Ptr Tree -> IO a) -> IO a
withParseTree parser bytestring action =
  unsafeUseAsCStringLen bytestring $ \ (source, len) -> Exc.bracket
    (ts_parser_parse_string parser nullPtr source len)
    releaseTree
    action
  where releaseTree t
          | t == nullPtr = pure ()
          | otherwise    = ts_tree_delete t

foreign import ccall safe "ts_parser_new" ts_parser_new :: IO (Ptr Parser)
foreign import ccall safe "ts_parser_halt_on_error" ts_parser_halt_on_error :: Ptr Parser -> CBool -> IO ()
foreign import ccall safe "ts_parser_parse_string" ts_parser_parse_string :: Ptr Parser -> Ptr Tree -> CString -> Int -> IO (Ptr Tree)
foreign import ccall safe "ts_parser_delete" ts_parser_delete :: Ptr Parser -> IO ()
foreign import ccall safe "ts_parser_set_language" ts_parser_set_language :: Ptr Parser -> Ptr Language -> IO Bool
foreign import ccall safe "ts_parser_timeout_micros" ts_parser_timeout_micros :: Ptr Parser -> IO Word64
foreign import ccall safe "ts_parser_set_timeout_micros" ts_parser_set_timeout_micros :: Ptr Parser -> Word64 -> IO ()

foreign import ccall safe "src/bridge.c ts_parser_log_to_stderr" ts_parser_log_to_stderr :: Ptr Parser -> IO ()

foreign import ccall safe "src/bridge.c ts_parser_parse_p1" ts_parser_parse_p1 :: Ptr Parser -> Ptr Tree -> Ptr TSHInput -> IO (Ptr Tree)

-- ---------------------------------------------------------------------
{-

typedef struct {
  void *payload;
  const char *(*read)(void *payload, uint32_t byte_index, TSPoint position, uint32_t *bytes_read);
  TSInputEncoding encoding;
} TSInput;
-}

type Payload = TSHInput

-- | TSHInput is like TSInput, but the ReadFunction uses Ptr TSPoint, not bare TSPoint
data TSHInput = TSHInput
  { inputPayload  :: !(Ptr CChar)
  , inputRead     :: !(FunPtr ReadFunction)
  , inputEncoding :: !TSInputEncoding
  }
  deriving (Show, Eq, Generic)

-- | reader: payload byte_offset position bytes_read
type ReadFunction = (Ptr Payload -> Word32 -> Ptr TSPoint -> Ptr Word32 -> IO CString)
{-
From https://tree-sitter.docsforge.com/master/api/ts_parser_parse/

read: A function to retrieve a chunk of text at a given byte offset
and (row, column) position. The function should return a pointer to
the text and write its length to the bytes_read pointer. The parser
does not take ownership of this buffer; it just borrows it until it
has finished reading it. The function should write a zero value to the
bytes_read pointer to indicate the end of the document.
-}

foreign import ccall "wrapper"
  mkReadFunction :: ReadFunction -> IO (FunPtr ReadFunction)

data TSInputEncoding
  = TSInputEncodingUTF8
  | TSInputEncodingUTF16
  deriving (Show, Eq, Generic)

instance Enum TSInputEncoding where
  toEnum 0 = TSInputEncodingUTF8
  toEnum _ = TSInputEncodingUTF16

  fromEnum TSInputEncodingUTF8  = 0
  fromEnum TSInputEncodingUTF16 = 1

-- ---------------------------------------------------------------------

instance Storable TSHInput where
  alignment _ = alignment (0 :: Int32)
  {-# INLINE alignment #-}
  sizeOf _ = 2*4 + 1
  {-# INLINE sizeOf #-}
  peek = evalStruct $ TSHInput <$> peekStruct
                               <*> peekStruct
                               <*> peekStruct
  {-# INLINE peek #-}
  poke ptr (TSHInput p r e) = flip evalStruct ptr $ do
    pokeStruct p
    pokeStruct r
    pokeStruct e
  {-# INLINE poke #-}

-- ---------------------------------------------------------------------

-- This section based on
-- https://stackoverflow.com/questions/46760015/haskell-ffi-using-data-types-in-a-c-program

type C_TSInputEncoding = CInt

instance Storable TSInputEncoding where
  sizeOf _ = sizeOf (undefined :: C_TSInputEncoding)
  alignment _ = alignment (undefined :: C_TSInputEncoding)
  peek ptr = genToEnum <$> peek (castPtr ptr :: Ptr C_TSInputEncoding)
  poke ptr val = poke (castPtr ptr :: Ptr C_TSInputEncoding) (genFromEnum val)

genToEnum :: (Integral a, Enum b) => a -> b
genToEnum = toEnum . fromIntegral

genFromEnum :: (Integral a, Enum b) => b -> a
genFromEnum = fromIntegral . fromEnum

-- ---------------------------------------------------------------------
