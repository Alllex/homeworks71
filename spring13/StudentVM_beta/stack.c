/*******************************
    project: StudentVM_beta
    author: Alex Semin
    place: SPbU MM171
    date:  2013
*******************************/

#include "stack.h"

typedef struct Item
{
    int value;
    struct Item *next;
} Item;

typedef struct Stack
{
    Item *head;
    int amountItems;
} Stack;

pStack createStack(void)
{
    pStack stack = (pStack) malloc(sizeof(Stack));
    stack->head = NULL;
    stack->amountItems = 0;
    return stack;
}

void deleteStack(pStack stack)
{
    clearStack(stack);
    free(stack);
}

void clearStack(pStack stack)
{
    while (stack->head != NULL)
    {
        popStack(stack);
    }
}

void pushStack(pStack stack, const int value)
{
    Item *item = (Item *) malloc(sizeof(Item));
    item->value = value;
    item->next = stack->head;
    stack->head = item;
    stack->amountItems++;
}

int popStack(pStack stack)
{
    int result = 0;
    if (stack->head != NULL)
    {
        Item *currect = stack->head;
        result = currect->value;
        stack->head = currect->next;
        free(currect);
        stack->amountItems--;
    }
    return result;
}

int peekStack(pStack stack)
{
    return (stack->head == NULL) ? 0 : stack->head->value;
}

int isEmptyStack(pStack stack)
{
    return stack->amountItems == 0;
}

int hasItemsStack(pStack stack, const int amount)
{
    return amount <= stack->amountItems;
}

int getSizeStack(pStack stack)
{
    return stack->amountItems;
}

void printStackState(FILE *output, pStack stack)
{
    Item *item = stack->head;
    fprintf(output, "---------------------\n");
    fprintf(output, "Stack state:\n");
    while (item != NULL)
    {
        fprintf(output, "[%d]\n", item->value);
        item = item->next;
    }
    fprintf(output, "Amount elements (%d)\n", getSizeStack(stack));
    fprintf(output, "---------------------\n");
}
