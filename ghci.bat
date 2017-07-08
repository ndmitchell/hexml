gcc -O2 -std=c99 -c cbits\hexml.c -o cbits\hexml.o -Wall -Wextra -Werror && ghci.exe cbits\hexml.o %*
