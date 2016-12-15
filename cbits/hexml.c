#include "hexml.h"
#include <malloc.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

typedef int bool;

/////////////////////////////////////////////////////////////////////
// TYPES

static inline int end(str s) { return s.start + s.length; }

static inline str start_length(int32_t start, int32_t length)
{
    assert(start >= 0 && length >= 0);
    str res;
    res.start = start;
    res.length = length;
    return res;
}

static inline str start_end(int32_t start, int32_t end)
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
    const char* body; // pointer to initial argument, not owned by us

    // things only used while parsing
    const char* cursor; // pointer to where we are in body
    const char* end; // pointer to one past the last char
    // if cursor is > end we have gone past the end

    char* error_message;
    node_buffer nodes;
    attr_buffer attrs;
};

static inline int doc_length(const document* d) { return (int) (d->end - d->body); }
static inline int doc_position(const document* d) { return (int) (d->cursor - d->body); }


/////////////////////////////////////////////////////////////////////
// RENDER CODE

typedef struct
{
    const document* d;
    char* buffer;
    int length;
    int cursor;
} render;

static inline void render_char(render* r, char c)
{
    if (r->cursor < r->length)
        r->buffer[r->cursor] = c;
    r->cursor++;
}

static inline void bound(int idx, int mn, int mx)
{
    assert(idx >= mn && idx <= mx);
}

static inline void bound_str(str s, int mn, int mx)
{
    assert(s.length >= 0);
    bound(s.start, mn, mx);
    bound(end(s), mn, mx);
}

static void render_str(render* r, str s)
{
    bound_str(s, 0, doc_length(r->d));
    for (int i = 0; i < s.length; i++)
        render_char(r, r->d->body[s.start + i]);
}

static void render_tag(render* r, const node* n);

static void render_content(render* r, const node* n)
{
    bound_str(n->inner, 0, doc_length(r->d));
    bound_str(n->nodes, 0, r->d->nodes.used_front);
    bound_str(n->attrs, 0, r->d->attrs.used);

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

static void render_tag(render* r, const node* n)
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

int hexml_node_render(const document* d, const node* n, char* buffer, int length)
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

char* hexml_document_error(const document* d){return d->error_message;}
node* hexml_document_node(const document* d){return &d->nodes.nodes[0];}

void hexml_document_free(document* d)
{
    free(d->error_message);
    free(d->nodes.alloc);
    free(d->attrs.alloc);
    free(d);
}

node* hexml_node_children(const document* d, const node* n, int* res)
{
    *res = n->nodes.length;
    return &d->nodes.nodes[n->nodes.start];
}

attr* hexml_node_attributes(const document* d, const node* n, int* res)
{
    *res = n->attrs.length;
    return &d->attrs.attrs[n->attrs.start];
}


attr* hexml_node_attribute(const document* d, const node* n, const char* s, int slen)
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
node* hexml_node_child(const document* d, const node* parent, const node* prev, const char* s, int slen)
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

static const char tag_name1 = 0x1;
static const char tag_name = 0x2;
static const char tag_space = 0x4;

static char parse_table[256];

static void init_parse_table()
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

static inline bool is(char c, char tag) { return parse_table[c] & tag; }
static inline bool is_name1(char c) { return is(c, tag_name1); }
static inline bool is_name(char c) { return is(c, tag_name); }
static inline bool is_space(char c) { return is(c, tag_space); }


/////////////////////////////////////////////////////////////////////
// PARSER COMBINATORS

static inline char peek_at(const document* d, int i) { return d->cursor[i]; }
static inline char peek(const document* d) { return peek_at(d, 0); }
static inline void skip(document* d, int i) { d->cursor += i; }
static inline char get(document* d) { char c = peek(d); skip(d, 1); return c; }

// Remove whitespace characters from the cursor while they are still whitespace
static inline void trim(document* d)
{
    while (is_space(peek(d)))
        skip(d, 1);
}

// Find this character form the cursor onwards, if true adjust the cursor to that char, otherwise leave it at the end
static bool find(document* d, char c)
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

static inline void node_alloc(node_buffer* b, int ask)
{
    int space = b->size - b->used_back - b->used_front;
    if (space >= ask) return;
    int size2 = (b->size + 1000 + ask) * 2;
    node* buf2 = malloc(size2 * sizeof(node));
    assert(buf2);
    memcpy(buf2, b->nodes, b->used_front * sizeof(node));
    memcpy(&buf2[size2 - b->used_back], &b->nodes[b->size - b->used_back], b->used_back * sizeof(node));
    free(b->alloc);
    b->size = size2;
    b->nodes = buf2;
    b->alloc = buf2;
}

static inline void attr_alloc(attr_buffer* b, int ask)
{
    int space = b->size - b->used;
    if (space >= ask) return;
    int size2 = (b->size + 1000 + ask) * 2;
    attr* buf2 = malloc(size2 * sizeof(attr));
    assert(buf2);
    memcpy(buf2, b->attrs, b->used * sizeof(attr));
    free(b->alloc);
    b->size = size2;
    b->attrs = buf2;
    b->alloc = buf2;
}

static void set_error(document* d, const char* msg)
{
    if (d->error_message != NULL) return; // keep the first error message
    d->error_message = malloc(strlen(msg)+1);
    assert(d->error_message);
    strcpy(d->error_message, msg);
}

// you now expect a name, perhaps preceeded by whitespace
// the name may be empty
static inline str parse_name(document* d)
{
    int start = doc_position(d);
    if (!is_name1(peek(d)))
        return start_length(start, 0);
    skip(d, 1);
    while (is_name(peek(d)))
        skip(d, 1);
    return start_end(start, doc_position(d));
}

static inline str parse_attrval(document* d)
{
    trim(d);
    if (peek(d) != '=')
    {
        set_error(d, "Expected = in attribute, but missing");
        return start_length(0, 0);
    }
    skip(d, 1);
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
static inline str parse_attributes(document* d)
{
    int start = d->attrs.used;
    while (d->error_message == NULL)
    {
        trim(d);
        str name = parse_name(d);
        if (name.length == 0) break;
        attr_alloc(&d->attrs, 1);
        d->attrs.attrs[d->attrs.used].name = name;
        d->attrs.attrs[d->attrs.used].value = parse_attrval(d);
        d->attrs.used++;
    }
    return start_end(start, d->attrs.used);
}

static str parse_content(document* d);


// Add a new entry into tag, am at a '<'
static inline void parse_tag(document* d)
{
    node_alloc(&d->nodes, 1);
    d->nodes.used_back++;
    node* me = &d->nodes.nodes[d->nodes.size - d->nodes.used_back];

    me->outer.start = doc_position(d);
    char c = get(d);
    assert(c == '<');
    if (peek(d) == '?') skip(d, 1);
    me->name = parse_name(d);
    if (me->name.length == 0)
    {
        set_error(d, "Missing tag name");
        return;
    }
    me->attrs = parse_attributes(d);
    if (d->error_message != NULL) return;

    c = peek(d);
    if ((c == '/' || c == '?') && peek_at(d, 1) == '>')
    {
        skip(d, 2);
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
    else
        skip(d, 1);
    me->inner.start = doc_position(d);
    str content = parse_content(d);

    // parse_content may have allocated more nodes, so recompute me
    me = &d->nodes.nodes[d->nodes.size - d->nodes.used_back];
    me->nodes = content;
    me->inner.length = start_end(me->inner.start, doc_position(d)).length;

    if (d->error_message != NULL) return;
    if (peek(d) == '<' && peek_at(d, 1) == '/')
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
static str parse_content(document* d)
{
    int before = d->nodes.used_back;
    while (d->error_message == NULL)
    {
        // only < can have any impact
        if (!find(d, '<'))
        {
            break;
        }
        else if (peek_at(d, 1) == '/')
        {
            // have found a </
            break;
        }
        else if (peek_at(d, 1) == '!' && peek_at(d, 2) == '-' && peek_at(d, 3) == '-')
        {
            skip(d, 3);
            // you can't reuse the two '-' characters for the closing as well
            if (peek_at(d, 0) == '\0' || peek_at(d, 1) == '\0')
            {
                set_error(d, "Didn't get a closing comment");
                return start_end(0, 0);
            }
            skip(d, 2);
            for (;;)
            {
                if (!find(d, '>'))
                {
                    set_error(d, "Didn't get a closing comment");
                    return start_end(0, 0);
                }
                skip(d, 1);
                if (peek_at(d, -3) == '-' && peek_at(d, -2))
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

document* hexml_document_parse(const char* s, int slen)
{
    if (slen == -1) slen = (int) strlen(s);
    assert(s[slen] == 0);
    init_parse_table();

    buffer* buf = malloc(sizeof(buffer));
    assert(buf);
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
    
    // Introduce an intermediate result, otherwise behaviour is undefined
    // because there is no guaranteed ordering between LHS and RHS evaluation
    str content = parse_content(d);
    d->nodes.nodes[0].nodes = content;

    if (d->cursor < d->end)
        set_error(d, "Trailing junk at the end of the document");
    return d;
}
