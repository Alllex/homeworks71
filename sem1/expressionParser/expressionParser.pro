TEMPLATE = app
CONFIG += console
CONFIG -= qt

SOURCES += main.cpp \
    stateAutomate.cpp \
    parser.cpp

OTHER_FILES += \
    rules

HEADERS += \
    stateAutomate.h \
    parser.h

