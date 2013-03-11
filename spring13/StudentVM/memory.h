/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#ifndef MEMORY_H
#define MEMORY_H

#include <stdio.h>
#include <stdlib.h>

typedef struct Memory *pMemory;

pMemory createMemory(unsigned int size);
void deleteMemory(pMemory memory);
int getItemMemory(pMemory memory, unsigned int address);
void setItemMemory(pMemory memory, unsigned int address, int value);
int isValidAddress(pMemory memory, unsigned int address);

void printMemoryState(FILE *output, pMemory memory);

#endif // MEMORY_H
