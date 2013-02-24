/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "memory.h"

#define PAGE_MEMORY_SIZE 10

typedef struct Memory
{
    int **cell;
    unsigned int size; // (maximum is 2^32)
    unsigned int amountPages;
} Memory;

pMemory createMemory(unsigned int size)
{
    unsigned int i = 0;
    pMemory memory = (pMemory) malloc(sizeof(Memory));
    memory->size = size;
    memory->amountPages = ((size - 1) / PAGE_MEMORY_SIZE) + 1;
    memory->cell = (int **) malloc(sizeof(int *) * memory->amountPages);
    for (i = 0; i < memory->amountPages; ++i)
    {
        memory->cell[i] = (int *) calloc(PAGE_MEMORY_SIZE, sizeof(int));
    }
    return memory;
}

void deleteMemory(pMemory memory)
{
    unsigned int i = 0;
    for (i = 0; i < memory->amountPages; ++i)
    {
        free(memory->cell[i]);
    }
    free(memory->cell);
}

int getItemMemory(pMemory memory, unsigned int address)
{
    return (address > memory->size) ? 0 : memory->cell[(address) / PAGE_MEMORY_SIZE][(address) % PAGE_MEMORY_SIZE];
}

void setItemMemory(pMemory memory, unsigned int address, int value)
{
    if (address <= memory->size)
    {
        memory->cell[(address) / PAGE_MEMORY_SIZE][(address) % PAGE_MEMORY_SIZE] = value;
    }
}

void printMemoryState(FILE *output, pMemory memory)
{
    unsigned int p = 0;
    unsigned int i = 0;
    fprintf(output, "---------------------\n");
    fprintf(output, "Memory state: size(%d)\n", memory->size);
    for (p = 0; p < memory->amountPages; ++p)
    {
        for (i = 0; i < PAGE_MEMORY_SIZE; ++i)
        {
            fprintf(output, "[#%d][%d]\n", p * PAGE_MEMORY_SIZE + i, memory->cell[p][i]);
        }
    }
    fprintf(output, "---------------------\n");
}

int isValidAddress(pMemory memory, unsigned int address)
{
    return address <= memory->size;
}

