rem sos -p .*.c -c "go 2>&1 | head -n12"
gcc -c hexml.c && gcc -c main.c && gcc -o hexml hexml.o main.o && hexml
