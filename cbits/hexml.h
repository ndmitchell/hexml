
#include <stdint.h>

typedef struct document document;

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

typedef struct
{
    str name; // tag name, e.g. <[foo]>
    str inner; // inner text, <foo>[bar]</foo>
    str outer; // outer text, [<foo>bar</foo>]
    // Not usable, but required to get the correct sizing when returning arrays of nodes
    str attrs; // all the attributes, in the attribute buffer (not usable)
    str nodes; // all the nodes, in the node buffer (not usable)
} node;

// Convention: if slen can be -1 for nul-terminated, or given explicitly

// Parse a document, returns a potentially invalid document - use document_error to check
// Must use document_free to release the memory (even if invalid).
// The string must be nul terminated, e.g. slen != -1 ==> slen[0]
document* hexml_document_parse(const char* s, int slen);

// Free the memory returned from document_parse.
void hexml_document_free(document* d);

// Generate a fresh string with the same semantics as the node.
// Requires an input buffer, returns the size of the rendered document.
// Requires the string passed to document_parse to be valid.
int hexml_node_render(const document* d, const node* n, char* buffer, int length);

// Return either NULL (successful parse) or the error message.
char* hexml_document_error(const document* d);

// Return the root node of the document - imagine the document is wrapped in <>$DOC</> tags.
node* hexml_document_node(const document* d);

// List all items within a node
node* hexml_node_children(const document* d, const node* n, int* res);
attr* hexml_node_attributes(const document* d, const node* n, int* res);

// Search for given strings within a node, note that prev may be NULL
// Requires the string passed to document_parse to be valid.
node* hexml_node_child(const document* d, const node* parent, const node* prev, const char* s, int slen);
attr* hexml_node_attribute(const document* d, const node* n, const char* s, int slen);
