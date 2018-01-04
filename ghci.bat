gcc -O2 -std=c99 -c cbits\hexml.c -o cbits\hexml.o -Wall -Wextra -Werror && ghci.exe cbits\hexml.o %*
rem gcc -O2 -std=c99 -c gen\test.c -o gen\test.o -Wall -Wextra -Werror && ghci.exe gen\test.o %*
