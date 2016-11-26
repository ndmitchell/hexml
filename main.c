#include "hexml.h"
#include <stdio.h>
#include <malloc.h>

void main()
{
    setbuf(stdout, NULL);
    char* body = "<?xml version=\"1.0\" lang=\"ja\"?><foo><bar baz=\"qux\">quux</bar><bar baz=\"bar\">hoge</bar><piyo>&gt;piyopiyo&lt;</piyo></foo><foo2 />";
    body = "<test>X</test> ";
    document* doc = document_parse(body, -1);
    char* err = document_error(doc);
    if (err == NULL)
    {
        int len = document_render(doc, NULL, 0);
        char* s = malloc(len + 1);
        document_render(doc, s, len);
        s[len] = 0;
        printf("Parse successful, %s\n", s);
    }
    else
        printf("Parse failed, %s\n", err);
}
