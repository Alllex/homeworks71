TEMPLATE = app
CONFIG += console
CONFIG -= qt

SOURCES += main.c \
    stack.c \
    memory.c \
    commands.c \
    errors.c \
    vm.c \
    structures.c \
    parser.c \
    interpreter.c

HEADERS += \
    stack.h \
    memory.h \
    commands.h \
    errors.h \
    vm.h \
    structures.h \
    parser.h \
    interpreter.h

OTHER_FILES += \
    pr/testSWP.svm \
    pr/test.svm \
    pr/sumToX.svm \
    pr/loop.svm \
    pr/lcm.svm \
    pr/isPrime.svm \
    pr/gcd2.svm \
    pr/gcd.svm \
    pr/aplusbstack.svm \
    pr/aplusbmem.svm \
    pr/AaddBsqr.svm \
    pr/fib.svm \
    pr/prime.svm \
    tests/testSWP.svm \
    tests/test.svm \
    tests/sumToX.svm \
    tests/loop.svm \
    tests/lcm.svm \
    tests/isPrime.svm \
    tests/gcd2.svm \
    tests/aplusbstack.svm \
    tests/aplusbmem.svm \
    tests/AaddBsqr.svm \
    pr/gpd.svm

