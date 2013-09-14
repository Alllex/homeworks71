/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#ifndef STACK_H
#define STACK_H

#include <stdio.h>
#include <stdlib.h>

typedef struct Stack *pStack;

pStack createStack(void);
void deleteStack(pStack stack);
void clearStack(pStack stack);
void pushStack(pStack stack, const int value);
int popStack(pStack stack);
int peekStack(pStack stack);
int isEmptyStack(pStack stack);
int hasItemsStack(pStack stack, const int amount);
int getSizeStack(pStack stack);

void printStackState(FILE *output, pStack stack);

#endif // STACK_H
