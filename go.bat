rem sos -p .*.c -c "go 2>&1 | head -n12"
gcc -c hexml.c -O3 && gcc -c main.c -O3 && gcc -o hexml hexml.o main.o && hexml
