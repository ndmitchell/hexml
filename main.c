#include "hexml.h"
#include <stdio.h>

void main()
{
    char* body = "<?xml version=\"1.0\" lang=\"ja\"?><foo><bar baz=\"qux\">quux</bar><bar baz=\"bar\">hoge</bar><piyo>&gt;piyopiyo&lt;</piyo></foo><foo2 />";
    document* doc = document_parse(body, -1);
    char* err = document_error(doc);
    if (err == NULL)
        printf("Parse successful\n");
    else
        printf("Parse failed, %s\n", err);
}
