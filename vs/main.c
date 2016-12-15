#include "hexml.h"
#include <stdio.h>
#include <malloc.h>
#include <time.h>
#include <string.h>
#include <assert.h>

char* readFile(char* file)
{
    FILE* f = fopen(file, "rb");
    fseek(f, 0, SEEK_END);
    int len = ftell(f);
    fseek(f, 0, SEEK_SET);
    char* res = malloc(len + 1);
    assert(res);
    res[len] = 0;
    size_t n = fread(res, 1, len, f);
    assert(n == len);
    fclose(f);
    return res;
}

char* example = "<?xml version=\"1.0\" lang=\"ja\"?><foo><bar baz=\"qux\">quux</bar><bar baz=\"bar\">hoge</bar><piyo>&gt;piyopiyo&lt;</piyo></foo><foo2 />";

int main(int argc, char **argv)
{
    setbuf(stdout, NULL);
    char* body = argv[1] == NULL ? example : readFile(argv[1]);
    document* doc = hexml_document_parse(body, -1);
    char* err = hexml_document_error(doc);
    if (err == NULL)
    {
        int len = hexml_node_render(doc, hexml_document_node(doc), NULL, 0);
        char* s = malloc(len + 1);
        assert(s);
        hexml_node_render(doc, hexml_document_node(doc), s, len);
        s[len] = 0;
        printf("Parse successful\n"); // , %s\n", s);
        return 0;
    }
    else
    {
        printf("Parse failed, %s\n", err);
        return 1;
    }
}
