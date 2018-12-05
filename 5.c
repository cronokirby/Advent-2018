#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int opposite(char c1, char c2) {
    char diff = c1 - c2;
    return diff == 32 || diff == -32;
}

// Stack must be as big as data is long
int count_canceled(char* data, char* stack, char ignore) {
    int sp = -1;
    for (; *data != 0; ++data) {
        if (ignore) {
            char d = ignore - *data;
            if (d == 0 || d == 32) {
                continue;
            }
        }

        if (sp < 0) {
            ++sp;
            stack[sp] = *data;
        } else if (opposite(stack[sp], *data)) {
            --sp;
        } else {
            ++sp;
            stack[sp] = *data;
        }
    }
    return sp + 1;
}

int main() {
    FILE* fp = fopen("5.txt", "r");
    fseek(fp, 0L, SEEK_END);
    int size = ftell(fp);
    rewind(fp);
    char* data = calloc(size, sizeof(char));
    char* stack = calloc(size, sizeof(char));
    if (fgets(data, size + 1, fp) == NULL) {
        return 1;
    }
    fclose(fp);

    printf("Solution 1: %d\n", count_canceled(data, stack, 0));

    int min = -1;
    for (char c = 'a'; c <= 'z'; ++c) {
        int count = count_canceled(data, stack, c);
        if (min < 0 || count < min) {
            min = count;
        }
    }
    printf("Solution 2: %d\n", min);

    free(data);
    free(stack);
}
