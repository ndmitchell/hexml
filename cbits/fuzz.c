#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hexml.c"

int main(int ac, char** av)
{
#ifdef __AFL_HAVE_MANUAL_CONTROL
    __AFL_INIT();
#endif

    FILE *f = fopen(av[1], "rb");
    fseek(f, 0, SEEK_END);
    size_t fsize = ftell(f);
    rewind(f);

    char* string = malloc(fsize+1);
    memset(string, 0, fsize+1);
    (void) fread(string, fsize, 1, f);
    fclose(f);

    document *doc = hexml_document_parse(string, fsize);
    hexml_document_free(doc);
    free(string);
    return 0;
}
