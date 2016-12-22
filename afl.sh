# Script based on code from https://github.com/ndmitchell/hexml/issues/6
# More examples of things to do are at that ticket
set -eu

# First install AFL
if ! command -v afl-clang-fast > /dev/null ; then
    wget http://lcamtuf.coredump.cx/afl/releases/afl-latest.tgz
    tar xf afl-latest.tgz
    cd afl-*
    make
    cd llvm_mode && make && cd ..
    sudo make install
    cd libdislocator && make && sudo make install && cd ..
    cd ..
fi

# Compile it
AFL_HARDEN=1 afl-clang-fast -O2 -Icbits cbits/fuzz.c -o $PWD/hexml-fuzz

# Fuzz it
if ! grep -q core /proc/sys/kernel/core_pattern ; then
    echo core | sudo tee /proc/sys/kernel/core_pattern
fi
AFL_EXIT_WHEN_DONE=1 AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so afl-fuzz -T hexml -x /usr/local/share/afl/dictionaries/xml.dict -i $PWD/xml -o $PWD/afl-results -- $PWD/hexml-fuzz @@
cat afl-results/fuzzer_stats

# Minimize failures
# $ AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so afl-cmin -i results/crashes/ -o results.shrunk -- $PWD/a.out @@
# $ mkdir results.min
# $ for x in `ls results.shrunk`; do AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so afl-tmin -i results.shrunk/$x -o results.min/$x -- $PWD/a.out @@; done
# $ AFL_PRELOAD=/usr/local/lib/afl/libdislocator.so afl-analyze -i results.shrunk/id:000013,sig:11,src:000002,op:havoc,rep:4 -- ./a.out @@
