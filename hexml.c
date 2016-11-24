#include "hexml.h"
#include <malloc.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

/////////////////////////////////////////////////////////////////////
// TYPES

str mkStr(int32_t start, int32_t length)
{
    str res;
    res.start = start;
    res.length = length;
    return res;
}

typedef struct
{
    int size;
    int used;
    attr* attrs; // dynamically allocated buffer
} attr_buffer;


struct node
{
    str name; // tag name, e.g. <[foo]>
    str outer; // outer text, [<foo>bar</foo>]
    str inner; // inner text, <foo>[bar]</foo>
    str attrs; // all the attributes, in the attribute buffer
    str nodes; // all the nodes, in the node buffer
};

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
} node_buffer;


struct document
{
    char* body; // pointer to initial argument, not owned by us

    int cursor; // things only used while parsing
    int length;

    char* error_message;
    node* node;
    node_buffer nodes;
    attr_buffer attrs;
};


/////////////////////////////////////////////////////////////////////
// NOT THE PARSER

char* document_error(document* d){return d->error_message;}
node* document_node(document* d){return d->node;}
str node_inner(node* n){return n->inner;}
str node_outer(node* n){return n->outer;}

void document_free(document* d)
{
    free(d->error_message);
    free(d->nodes.nodes);
    free(d->attrs.attrs);
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
    assert(0);
}

// Search for given strings within a node
node* node_firstChildBy(document* d, node* n, char* s, int slen)
{
    if (slen == -1) slen = strlen(s);
    int i = n->nodes.start;
    int limit = i + n->nodes.length;
    for (; i < limit; i++)
    {
        if (d->nodes.nodes[i].name.length == slen &&
            memcmp(s, &d->body[d->nodes.nodes[i].name.start], slen) == 0)
            return &d->nodes.nodes[i];
    }
    return NULL;
}

node* node_nextChildBy(document* d, node* parent, node* prev, char* s, int slen)
{
    assert(0);
}


/////////////////////////////////////////////////////////////////////
// PARSER PRIMITIVES

int has(document* d, int ask){return (d->length - d->cursor) >= ask;}

void trim(document* d)
{
    while (has(d, 1) && isspace(grab(d->body[*p]))
        *p += 1;
}



void node_alloc(node_buffer* b, int ask)
{
    int space = b->size - b->used_back - b->used_front;
    if (space >= ask) return;
    assert(0);
}

void attr_alloc(attr_buffer* b, int ask)
{
    int space = b->size - b->used;
    if (space >= ask) return;
    assert(0);
}

void lexeme(char* msg, document* d, str s)
{
    printf("Lexeme: %s, %i:%i, %.*s\n", msg, s.start, s.length, s.length, &d->body[s.start]);
}

int isName(char c)
{
    return isalnum(c) || c == '?';
}

// you now expect a name, perhaps preceeded by whitespace
// the name may be empty
str parse_name(document* d, int* p)
{
    printf("Before trim: %i\n", *p);
    trim(d, p);
    printf("After trim: %i %c\n", *p, d->body[*p]);
    str res;
    res.start = *p;
    while (*p < d->bodylen && isName(d->body[*p]))
        *p += 1;
    printf("After trim: %i\n", *p);
    res.length = *p - res.start;
    lexeme("parse_name", d, res);
    return res;
}

str parse_attrval(document* d, int* p)
{
    trim(d, p);
    str res;
    if (*p < d->bodylen || d->body[*p] != '=') return mkStr(0,0);
    trim(d, p);
    return parse_name(d, p);
}


// seen a tag name, now looking for attributes terminated by >
// puts the attributes it finds in the attribute buffer
void parse_attributes(document* s, int* p)
{


}

void parse_content(document* d, int* p);


// Add a new entry into tag, am at a '<'
void parse_tag(document* d, int* p)
{
    printf("Before parse_tag %i\n", *p);
    assert(d->body[*p] == '<');

    node_alloc(&d->nodes, 1);
    int me = d->nodes.used_back;
    d->nodes.used_back++;

    *p += 1;
    printf("After increment %i\n", *p);
    d->nodes.nodes[me].name = parse_name(d, p);

    d->nodes.nodes[me].attrs.start = d->attrs.used;
    parse_attributes(d, p);
    d->nodes.nodes[me].attrs.length = d->attrs.used;

    if (d->bodylen - *p >= 2 &&
        (d->body[*p] == '/' || d->body[*p] == '?') &&
        d->body[*p + 1] == '>')
    {
        *p += 2;
        return;
    }

    d->nodes.nodes[me].nodes.start = 42;
    parse_content(d, p);
    d->nodes.nodes[me].nodes.length = 42;

    if (d->error_message != NULL) return;
    if (d->bodylen - *p >= 2 &&
        d->body[*p] == '<' &&
        d->body[*p + 1] == '/')
    {
        *p += 2;
        str close = parse_name(d, p);
        lexeme("openning tag", d, d->nodes.nodes[me].name);
        lexeme("closing tag", d, close);
        trim(d, p);
        if (close.length == d->nodes.nodes[me].name.length &&
            memcmp(&d->body[close.start], &d->body[d->nodes.nodes[me].name.start], close.length) == 0)
            return;
        d->error_message = strdup("Mismatch in closing tags");
        return;
    }
    d->error_message = strdup("Weirdness when trying to close tags");
}

// Parser until </
void parse_content(document* d, int* p)
{
    int before = d->nodes.used_back;
    while (d->error_message == NULL)
    {
        // only < can have any impact
        char* res = memchr(&d->body[*p], '<', d->bodylen - *p);
        printf("%i - %i = %i\n", d->bodylen, *p, d->bodylen - *p);
        if (res == NULL)
        {
            *p = d->bodylen;
            printf("break 0\n");
            break;
        }
        *p = res - d->body;
        if (*p + 1 == d->bodylen)
        {
            // final character of string is <
            printf("break 1\n");
            break;
        }
        else if (d->body[*p + 1] == '/')
        {
            // have found a </
            printf("break 2\n");
            break;
        }
        else
        {
            printf("calling parse_tag\n");
            parse_tag(d, p);
        }
    }
    int diff = d->nodes.used_back - before;
    node_alloc(&d->nodes, diff);

//    for (int i = 0; i < diff; i++)
//        d->nodes.nodes[d->nodes.used_front + i] = d->nodes.nodes[d->nodes.size - d->nodes.used_back - i];
    d->nodes.used_front += diff;
    d->nodes.used_back -= diff;
}

document* document_parse(char* s, int slen)
{
    if (slen == -1) slen = strlen(s);

    document* d = malloc(sizeof(document));
    d->body = s;
    d->bodylen = slen;
    d->error_message = NULL;
    d->node = NULL;
    d->attrs.size = 0;
    d->attrs.used = 0;
    d->attrs.attrs = NULL;
    d->nodes.size = 1000;
    d->nodes.used_back = 0;
    d->nodes.used_front = 1;
    d->nodes.nodes = malloc(sizeof(node) * 1000);

    d->nodes.nodes[0].name = mkStr(0, 0);
    d->nodes.nodes[0].outer = mkStr(0, slen);
    d->nodes.nodes[0].inner = mkStr(0, slen);
    d->nodes.nodes[0].attrs = mkStr(0, 0);
    int p = 0;
    parse_content(d, &p);
    if (p != slen && d->error_message == NULL)
    {
        printf("%i vs %i\n", p, slen);
        d->error_message = strdup("Trailing junk at the end of the document");
    }
    return d;
}
