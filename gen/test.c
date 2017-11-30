
#include <stdio.h>
#define ARGUMENTS

#define parse_table hexml_parse_table
#define parser hexml_parser

#define abort(x) return x
#define NameStart printf("NameStart %s\n", p)
#define NameEnd printf("NameEnd %s\n", p)
#define QuoteStart printf("QuoteStart %s\n", p)
#define QuoteEnd printf("QuoteEnd %s\n", p)
#define AttribsStart printf("AttribsStart %s\n", p)
#define AttribsEnd printf("AttribsEnd %s\n", p)
#define Tag printf("Tag %s\n", p)
#define hexml_Tag printf("Tag %s\n", p)
#define TagComment printf("TagComment %s\n", p)
#define TagOpen printf("TagOpen %s\n", p)
#define TagClose printf("TagClose %s\n", p)
#define TagOpenClose printf("TagOpenClose %s\n", p)

#include "test.h"


void main()
{
  const char* res = hexml_parser("<test foo='123 xyz'/>");
  printf("Result %s\n", res);
}
