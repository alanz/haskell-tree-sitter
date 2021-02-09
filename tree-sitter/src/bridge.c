#include "tree_sitter/api.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

typedef struct Node {
  TSNode node;
  const char *type;
  TSSymbol symbol;
  TSPoint endPoint;
  uint32_t endByte;
  uint32_t childCount;
  const char *fieldName;
  bool     isNamed;
  bool     isExtra;
} Node;

void log_to_stdout(void *payload, TSLogType type, const char *message) {
  printf("%s\n", message);
}

void ts_parser_log_to_stderr(TSParser *parser) {
  ts_parser_set_logger(parser, (TSLogger) {.log = log_to_stdout, .payload = NULL});
}

static inline void ts_node_poke(const char *fieldName, TSNode node, Node *out) {
  out->node = node;
  out->symbol = ts_node_symbol(node);
  out->type = ts_node_type(node);
  out->endPoint = ts_node_end_point(node);
  out->endByte = ts_node_end_byte(node);
  out->childCount = ts_node_child_count(node);
  out->fieldName = fieldName;
  out->isNamed = ts_node_is_named(node);
  out->isExtra = ts_node_is_extra(node);
}

void ts_node_poke_p(TSNode *node, Node *out) {
  assert(node != NULL);
  ts_node_poke(NULL, *node, out);
}

void ts_tree_root_node_p(TSTree *tree, Node *outNode) {
  assert(tree != NULL);
  assert(outNode != NULL);
  TSNode root = ts_tree_root_node(tree);
  assert(root.id != NULL);
  ts_node_poke(NULL, root, outNode);
}

void ts_node_copy_child_nodes(const TSNode *parentNode, Node *outChildNodes) {
  assert(parentNode != NULL);
  assert(outChildNodes != NULL);
  TSTreeCursor curse = ts_tree_cursor_new(*parentNode);

  if (ts_tree_cursor_goto_first_child(&curse)) {
    do {
      TSNode current = ts_tree_cursor_current_node(&curse);
      ts_node_poke(ts_tree_cursor_current_field_name(&curse), current, outChildNodes);
      outChildNodes++;
    } while (ts_tree_cursor_goto_next_sibling(&curse));
  }

  ts_tree_cursor_delete(&curse);
}

size_t sizeof_tsnode() {
  return sizeof(TSNode);
}

size_t sizeof_tspoint() {
  return sizeof(TSPoint);
}

size_t sizeof_node() {
  return sizeof(Node);
}

size_t sizeof_tstreecursor() {
  return sizeof(TSTreeCursor);
}


void ts_tree_cursor_new_p(TSNode *node, TSTreeCursor *outCursor) {
  assert(node != NULL);
  assert(outCursor != NULL);
  *outCursor = ts_tree_cursor_new(*node);
}

void ts_tree_cursor_reset_p(TSTreeCursor *cursor, TSNode *node) {
  assert(cursor != NULL);
  assert(node != NULL);
  ts_tree_cursor_reset(cursor, *node);
}

bool ts_tree_cursor_current_node_p(const TSTreeCursor *cursor, Node *outNode) {
  assert(cursor != NULL);
  assert(outNode != NULL);
  TSNode tsNode = ts_tree_cursor_current_node(cursor);
  if (!ts_node_is_null(tsNode)) {
    ts_node_poke(ts_tree_cursor_current_field_name(cursor), tsNode, outNode);
  }
  return false;
}


uint32_t ts_tree_cursor_copy_child_nodes(TSTreeCursor *cursor, Node *outChildNodes) {
  assert(cursor != NULL);
  assert(outChildNodes != NULL);
  uint32_t count = 0;

  if (ts_tree_cursor_goto_first_child(cursor)) {
    do {
      TSNode current = ts_tree_cursor_current_node(cursor);
      const char *fieldName = ts_tree_cursor_current_field_name(cursor);
      if (fieldName || (ts_node_is_named(current) && !ts_node_is_extra(current))) {
        ts_node_poke(fieldName, current, outChildNodes);
        count++;
        outChildNodes++;
      }
    } while (ts_tree_cursor_goto_next_sibling(cursor));
    ts_tree_cursor_goto_parent(cursor);
  }
  return count;
}

char *ts_node_string_p(TSNode *node) {
  assert(node != NULL);
  return ts_node_string(*node);
}

char *ts_node_string_extra_p(TSNode *node) {
  assert(node != NULL);
  return ts_node_string_extra(*node);
}

bool ts_node_is_missing_p(TSNode *node) {
  assert(node != NULL);
  return ts_node_is_missing(*node);
}

bool ts_node_has_error_p(TSNode *node) {
  assert(node != NULL);
  return ts_node_has_error(*node);
}

char *ts_node_string_diagnostics_p(TSNode *node) {
  assert(node != NULL);
  return ts_node_string_diagnostics(*node);
}

TSTree *ts_parser_parse_p(
  TSParser *self,
  const TSTree *old_tree,
  TSInput *input) {
  assert(self != NULL);
  // old_tree is allowed to be null
  assert(input != NULL);
  return ts_parser_parse(self, old_tree, *input);
}

/*
There is a problem with haskell FFI, in that parameters can either be
basic, known types, or pointers to structs.
The TSInput structure has a callback function which is

  const char *(*read)(void *payload, uint32_t byte_index, TSPoint position, uint32_t *bytes_read);

The position parameter is a TSPoint, rather than *TSPoint. This means
FFI (to AZ understanding) cannot be used directly. But it is a
callback, so we need to wrap the haskell callback.

We create a structure for use in haskell instead, and the acceptable
type signature, and then tap dance.
*/


// This structure can be provided by haskell FFI, the callback uses a pointer for position.
typedef struct {
  void *payload;
  const char *(*haskell_read)(void *payload, uint32_t byte_index, TSPoint *position, uint32_t *bytes_read);
  TSInputEncoding encoding;
} TSHInput;

const char *wrapped_read(void *hread,
                   uint32_t byte_index, TSPoint position, uint32_t *bytes_read) {
  fprintf( stderr, "*****wrapped_read (%p)\n", (void *) hread);
  TSHInput * hinput = (TSHInput *)hread;
  fprintf( stderr, "*****wrapped_read (%p,%p,%d)\n",
           (void *)hinput->payload, (void *)hinput->haskell_read, hinput->encoding);
  fprintf( stderr, "*****wrapped_read [%s]\n", hinput->payload);

  // Call the haskell function with address of position
  return (hinput->haskell_read)(hread, byte_index, &position, bytes_read);
}

TSTree *ts_parser_parse_p1(
  TSParser *self,
  const TSTree *old_tree,
  TSHInput *hinput)
{
  TSInput input;
  /* fprintf( stderr, "*****ts_parser_parse_p1 entered\n"); */
  /* fprintf( stderr, "*****ts_parser_parse_p1 hinput=%p\n", (void *)hinput); */
  /* fprintf( stderr, "*****ts_parser_parse_p1 (%p,%p,%d)\n", (void *) hinput->payload, (void *)hinput->haskell_read, hinput->encoding); */

  /* fprintf( stderr, "*****ts_parser_parse_p1 str [%s]\n", hinput->payload); */
  assert(self != NULL);
  // old_tree is allowed to be null
  assert(hinput != NULL);
  // Set the callback wrapper
  input.payload  = (void *)hinput;
  input.read     = wrapped_read;
  input.encoding = hinput->encoding;
  return ts_parser_parse(self, old_tree, input);
}

void ts_query_cursor_exec_p(TSQueryCursor *cursor, const TSQuery *query, TSNode *node)
{
  ts_query_cursor_exec(cursor, query, *node);
}

