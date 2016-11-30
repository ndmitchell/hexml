#include "hexml.h"
#include <malloc.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

typedef int bool;

/////////////////////////////////////////////////////////////////////
// TYPES

int static inline end(str s) { return s.start + s.length; }

str static inline start_length(int32_t start, int32_t length)
{
    if (start < 0 || length < 0) assert(0);
    str res;
    res.start = start;
    res.length = length;
    return res;
}

str static inline start_end(int32_t start, int32_t end)
{
    return start_length(start, end - start);
}

typedef struct
{
    int size;
    int used;
    attr* attrs; // dynamically allocated buffer
    attr* alloc; // what to call free on
} attr_buffer;

typedef struct
{
    // have a cursor at the front, which is all the stuff I have written out, final
    // have a cursor at the end, which is stack scoped, so children write out, then I do
    // when you commit, you copy over from end to front

    // nodes 
    int size;
    int used_front; // front entries, stored for good
    int used_back; // back entries, stack based, copied into front
    node* nodes; // dynamically allocated buffer
    node* alloc; // what to call free on
} node_buffer;


struct document
{
    char* body; // pointer to initial argument, not owned by us

    // things only used while parsing
    char* cursor; // pointer to where we are in body
    char* end; // pointer to one past the last char
    // if cursor is > end we have gone past the end

    char* error_message;
    node_buffer nodes;
    attr_buffer attrs;
};

int static inline doc_length(document* d) { return (int) (d->end - d->body); }
int static inline doc_position(document* d) { return (int) (d->cursor - d->body); }


/////////////////////////////////////////////////////////////////////
// RENDER CODE

typedef struct
{
    document* d;
    char* buffer;
    int length;
    int cursor;
} render;

void static inline render_char(render* r, char c)
{
    if (r->cursor < r->length)
        r->buffer[r->cursor] = c;
    r->cursor++;
}

void static inline bound(char* msg, int index, int mn, int mx)
{
    if (index < mn || index > mx)
    {
        //printf("Bounds checking failed %s, got %i, which should be in %i:%i\n", msg, index, mn, mx);
        assert(0);
    }
}

void static inline bound_str(char* msg, str s, int mn, int mx)
{
    if (s.length < 0) assert(0);
    bound(msg, s.start, mn, mx);
    bound(msg, end(s), mn, mx);
}

void render_str(render* r, str s)
{
    bound_str("render_str", s, 0, doc_length(r->d));
    for (int i = 0; i < s.length; i++)
        render_char(r, r->d->body[s.start + i]);
}

void render_tag(render* r, node* n);

void render_content(render* r, node* n)
{
    bound_str("render_conent inner", n->inner, 0, doc_length(r->d));
    bound_str("render_conent nodes", n->nodes, 0, r->d->nodes.used_front);
    bound_str("render_conent attrs", n->attrs, 0, r->d->attrs.used);

    int done = n->inner.start;
    for (int i = 0; i < n->nodes.length; i++)
    {
        node* x = &r->d->nodes.nodes[n->nodes.start + i];
        render_str(r, start_end(done, x->outer.start));
        done = end(x->outer);
        render_tag(r, x);
    }
    render_str(r, start_end(done, end(n->inner)));
}

void render_tag(render* r, node* n)
{
    render_char(r, '<');
    render_str(r, n->name);
    for (int i = 0; i < n->attrs.length; i++)
    {
        attr* x = &r->d->attrs.attrs[n->attrs.start + i];
        render_char(r, ' ');
        render_str(r, x->name);
        render_char(r, '=');
        render_char(r, '\"');
        render_str(r, x->value);
        render_char(r, '\"');
    }
    render_char(r, '>');
    render_content(r, n);
    render_char(r, '<');
    render_char(r, '/');
    render_str(r, n->name);
    render_char(r, '>');
}

int node_render(document* d, node* n, char* buffer, int length)
{
    render r;
    r.d = d;
    r.buffer = buffer;
    r.length = length;
    r.cursor = 0;
    // The root node (and only the root node) has an empty length, so just render its innards
    if (n->name.length == 0)
        render_content(&r, n);
    else
        render_tag(&r, n);
    return r.cursor;
}


/////////////////////////////////////////////////////////////////////
// NOT THE PARSER

char* document_error(document* d){return d->error_message;}
node* document_node(document* d){return &d->nodes.nodes[0];}

void document_free(document* d)
{
    free(d->error_message);
    free(d->nodes.alloc);
    free(d->attrs.alloc);
    free(d);
}

node* node_children(document* d, node* n, int* res)
{
    *res = n->nodes.length;
    return &d->nodes.nodes[n->nodes.start];
}

attr* node_attributes(document* d, node* n, int* res)
{
    *res = n->attrs.length;
    return &d->attrs.attrs[n->attrs.start];
}


attr* node_attributeBy(document* d, node* n, char* s, int slen)
{
    if (slen == -1) slen = (int) strlen(s);
    const int limit = end(n->attrs);
    for (int i = n->attrs.start; i < limit; i++)
    {
        attr* r = &d->attrs.attrs[i];
        if (r->name.length == slen && memcmp(s, &d->body[r->name.start], slen) == 0)
            return r;
    }
    return NULL;
}

// Search for given strings within a node
node* node_childBy(document* d, node* parent, node* prev, char* s, int slen)
{
    if (slen == -1) slen = (int) strlen(s);
    int i = prev == NULL ? parent->nodes.start : (int) (prev + 1 - d->nodes.nodes);
    const int limit = end(parent->nodes);
    for (; i < limit; i++)
    {
        node* r = &d->nodes.nodes[i];
        if (r->name.length == slen && memcmp(s, &d->body[r->name.start], slen) == 0)
            return r;
    }
    return NULL;
}


/////////////////////////////////////////////////////////////////////
// PARSE TABLE

const char tag_name1 = 0x1;
const char tag_name = 0x2;
const char tag_space = 0x4;

char parse_table[256];

void init_parse_table()
{
    static bool done = 0;
    if (done) return;
    for (int i = 0; i < 256; i++)
    {
        bool name1 = i == ':' || i == '_' || (i >= 'a' && i <= 'z') || (i >= 'A' && i <= 'Z');
        bool name = name1 || i == '-' || (i >= '0' && i <= '9');
        bool space = i == ' ' || i == '\t' || i == '\r' || i == '\n';
        parse_table[i] = (name1 ? tag_name1 : 0) | (name ? tag_name : 0) | (space ? tag_space : 0);
    }
    done = 1;
}

bool static inline is(char c, char tag) { return parse_table[c] & tag; }
bool static inline is_name1(char c) { return is(c, tag_name1); }
bool static inline is_name(char c) { return is(c, tag_name); }
bool static inline is_space(char c) { return is(c, tag_space); }


/////////////////////////////////////////////////////////////////////
// PARSER COMBINATORS

char static inline peekAt(document* d, int i) { return d->cursor[i]; }
void static inline skip(document* d, int i) { d->cursor += i; }
char static inline peek(document* d) { return peekAt(d, 0); }
char static inline get(document* d) { char c = peek(d); skip(d, 1); return c; }

// Remove whitespace characters from the cursor while they are still whitespace
void static inline trim(document* d)
{
    while (is_space(peek(d)))
        skip(d, 1);
}

// Find this character form the cursor onwards, if true adjust the cursor to that char, otherwise leave it at the end
bool find(document* d, char c)
{
    char* x = memchr(d->cursor, c, d->end - d->cursor);
    if (x == NULL)
    {
        d->cursor = d->end;
        return 0;
    }
    else
    {
        d->cursor = x;
        return 1;
    }
}


/////////////////////////////////////////////////////////////////////
// PARSING CODE

void static inline node_alloc(node_buffer* b, int ask)
{
    int space = b->size - b->used_back - b->used_front;
    if (space >= ask) return;
    int size2 = (b->size + 1000 + ask) * 2;
    node* buf2 = malloc(size2 * sizeof(node));
    memcpy(buf2, b->nodes, b->used_front * sizeof(node));
    memcpy(&buf2[size2 - b->used_back], &b->nodes[b->size - b->used_back], b->used_back * sizeof(node));
    free(b->alloc);
    b->size = size2;
    b->nodes = buf2;
    b->alloc = buf2;
}

void static inline attr_alloc(attr_buffer* b, int ask)
{
    int space = b->size - b->used;
    if (space >= ask) return;
    int size2 = (b->size + 1000 + ask) * 2;
    attr* buf2 = malloc(size2 * sizeof(attr));
    memcpy(buf2, b->attrs, b->used * sizeof(attr));
    free(b->alloc);
    b->size = size2;
    b->attrs = buf2;
    b->alloc = buf2;
}

void set_error(document* d, char* msg)
{
    if (d->error_message != NULL) return; // keep the first error message
    d->error_message = malloc(strlen(msg)+1);
    strcpy(d->error_message, msg);
}

// you now expect a name, perhaps preceeded by whitespace
// the name may be empty
str static inline parse_name(document* d)
{
    int start = doc_position(d);
    if (!is_name1(peek(d)))
        return start_length(start, 0);
    skip(d, 1);
    while (is_name(peek(d)))
        skip(d, 1);
    return start_end(start, doc_position(d));
}

str static inline parse_attrval(document* d)
{
    trim(d);
    if (get(d) != '=')
    {
        set_error(d, "Expected = in attribute, but missing");
        return start_length(0, 0);
    }
    trim(d);
    char c = peek(d);
    if (c == '\"' || c == '\'')
    {
        skip(d, 1);
        int start = doc_position(d);
        if (!find(d, c))
        {
            set_error(d, "Couldn't find closing attribute bit");
            return start_length(0, 0);
        }
        skip(d, 1);
        return start_end(start, doc_position(d) - 1);
    }
    else
    {
        set_error(d, "Invalid attribute");
        return start_length(0, 0);
    }
}



// seen a tag name, now looking for attributes terminated by >
// puts the attributes it finds in the attribute buffer
str static inline parse_attributes(document* d)
{
    str res;
    res.start = d->attrs.used;
    for (int i = 0; ; i++)
    {
        trim(d);
        str name = parse_name(d);
        if (name.length == 0) break;
        attr_alloc(&d->attrs, 1);
        d->attrs.attrs[d->attrs.used].name = name;
        d->attrs.attrs[d->attrs.used].value = parse_attrval(d);
        d->attrs.used++;
    }
    res.length = d->attrs.used - res.start;
    return res;
}

str parse_content(document* d);


// Add a new entry into tag, am at a '<'
void static inline parse_tag(document* d)
{
    node_alloc(&d->nodes, 1);
    d->nodes.used_back++;
    node* me = &d->nodes.nodes[d->nodes.size - d->nodes.used_back];

    me->outer.start = doc_position(d);
    char c = get(d);
    assert(c == '<');
    if (peek(d) == '?') skip(d, 1);
    me->name = parse_name(d);
    me->attrs = parse_attributes(d);

    c = get(d);
    if ((c == '/' || c == '?') && peek(d) == '>')
    {
        skip(d, 1);
        me->nodes = start_length(0, 0);
        me->outer.length = start_end(me->outer.start, doc_position(d)).length;
        me->inner = start_length(doc_position(d), 0);
        return;
    }
    else if (c != '>')
    {
        set_error(d, "Gunk at the end of the tag");
        return;
    }
    me->inner.start = doc_position(d);
    str content = parse_content(d);

    // parse_content may have allocated more nodes, so recompute me
    me = &d->nodes.nodes[d->nodes.size - d->nodes.used_back];
    me->nodes = content;
    me->inner.length = start_end(me->inner.start, doc_position(d)).length;

    if (d->error_message != NULL) return;
    if (peek(d) == '<' && peekAt(d, 1) == '/')
    {
        skip(d, 2);
        if (d->end - d->cursor >= me->name.length &&
            memcmp(d->cursor, &d->body[me->name.start], me->name.length) == 0)
        {
            skip(d, me->name.length);
            trim(d);
            if (get(d) == '>')
            {
                me->outer.length = start_end(me->outer.start, doc_position(d)).length;
                return;
            }
        }
        set_error(d, "Mismatch in closing tags");
        return;
    }
    set_error(d, "Weirdness when trying to close tags");
}

// Parser until </, return the index of your node children
// Not inline as it is recursive
str parse_content(document* d)
{
    int before = d->nodes.used_back;
    while (d->error_message == NULL)
    {
        // only < can have any impact
        if (!find(d, '<'))
        {
            break;
        }
        else if (peekAt(d, 1) == '/')
        {
            // have found a </
            break;
        }
        else if (peekAt(d, 1) == '!' && peekAt(d, 2) == '-' && peekAt(d, 3) == '-')
        {
            skip(d, 3);
            // you can't reuse the two '-' characters for the closing as well
            if (peekAt(d, 0) == '\0' || peekAt(d, 1) == '\0')
            {
                set_error(d, "Didn't get a closing comment");
                return start_end(0, 0);
            }
            skip(d, 2);
            while (1)
            {
                if (!find(d, '>'))
                {
                    set_error(d, "Didn't get a closing comment");
                    return start_end(0, 0);
                }
                skip(d, 1);
                if (peekAt(d, -3) == '-' && peekAt(d, -2))
                    break;
            }
        }
        else
        {
            parse_tag(d);
        }
    }
    int diff = d->nodes.used_back - before;
    node_alloc(&d->nodes, diff);
    str res = start_length(d->nodes.used_front, diff);
    for (int i = 0; i < diff; i++)
        d->nodes.nodes[d->nodes.used_front + i] = d->nodes.nodes[d->nodes.size - d->nodes.used_back + diff - 1 - i];
    d->nodes.used_front += diff;
    d->nodes.used_back -= diff;
    return res;
}

// Based on looking at ~50Kb XML documents, they seem to have ~700 attributes
// and ~300 nodes, so size appropriately to cope with that.
typedef struct
{
    document document;
    attr attrs[1000];
    node nodes[500];
} buffer;

document* document_parse(char* s, int slen)
{
    if (slen == -1) slen = (int) strlen(s);
    assert(s[slen] == 0);
    init_parse_table();

    buffer* buf = malloc(sizeof(buffer));
    document* d = &buf->document;
    d->body = s;
    d->cursor = s;
    d->end = &s[slen];
    d->error_message = NULL;
    d->attrs.size = 1000;
    d->attrs.used = 0;
    d->attrs.attrs = buf->attrs;
    d->attrs.alloc = NULL;
    d->nodes.size = 500;
    d->nodes.used_back = 0;
    d->nodes.used_front = 1;
    d->nodes.nodes = buf->nodes;
    d->nodes.alloc = NULL;

    d->nodes.nodes[0].name = start_length(0, 0);
    d->nodes.nodes[0].outer = start_length(0, slen);
    d->nodes.nodes[0].inner = start_length(0, slen);
    d->nodes.nodes[0].attrs = start_length(0, 0);
    d->nodes.nodes[0].nodes = parse_content(d);

    if (d->cursor < d->end && d->error_message == NULL)
    {
        set_error(d, "Trailing junk at the end of the document");
    }
    return d;
}
