
#include <stdint.h>

typedef struct document document;
typedef struct node node;

// Pair of indexes into the input string
typedef struct
{
    int32_t start;
    int32_t length;
} str;

typedef struct
{
    str name;
    str value;
} attr;

// Convention: if slen can be -1 for nul-terminated, or given explicitly

// Parse a document, returns a potentially invalid document - use document_error to check
// Must use document_free to release the memory.
document* document_parse(char* s, int slen);

// Free the memory returned from document_parse.
void document_free(document* d);

// generate a fresh document with the same semantics
// requires an input buffer, returns the size of the rendered document
int document_render(document* d, char* buffer, int length);

// return either NULL (successful parse) or the error message
char* document_error(document* d);

// return the root node of the document - imagine the document is wrapped in <>$DOC</> tags
node* document_node(document* d);

// Get the span of a node
str node_inner(node* n);
str node_outer(node* n);

// List all items within a node
node* node_children(document* d, node* n, int* res);
attr* node_attributes(document* d, node* n, int* res);

// Search for given strings within a node
node* node_firstChildBy(document* d, node* n, char* s, int slen);
node* node_nextChildBy(document* d, node* parent, node* prev, char* s, int slen);
attr* node_attributeBy(document* d, node* n, char* s, int slen);
