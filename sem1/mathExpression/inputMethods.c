#include "inputMethods.h"

#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>

// this function provide reading very long string to \n
char *getString(FILE *input)
{
    const int sizeStep = 256;
    int countChars = 0;
    int currentSize = sizeStep;
    char *string = (char *) malloc(currentSize * sizeof(char));
    char c = '0';
    while (!feof(input) && ((c = fgetc(input)) != '\n'))
    {
        if (countChars == currentSize)
        {
            currentSize += sizeStep;
            string = realloc(string, currentSize * sizeof(char));
        }
        string[countChars++] = c;
    }
    if (feof(input))
    {
        string[countChars - 1] = '\0';
    }
    else
    {
        string[countChars] = '\0';
    }
    return string;
}

// not nessery to include malloc libraries to another files
void clearString(char *line)
{
    free(line);
}
