#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int opposite(char c1, char c2) {
    char diff = c1 - c2;
    return diff == 32 || diff == -32;
}

int count_canceled(char* data, size_t size) {
    char** stack = calloc(size, sizeof(char*));
    for (int i = 0; i < size; ++i) {
        stack[i] = NULL;
    }
    int sp = 0;
    for (; *data != 0; ++data) {
        if (stack[sp] == NULL) {
            stack[sp] = data;
        } else if (opposite(*stack[sp], *data)) {
            stack[sp] = NULL;
            if (sp > 0) {
                --sp;
            }
        } else {
            ++sp;
            stack[sp] = data;
        }
    }
    free(stack);
    return sp + 1;
}

int main() {
    FILE* fp = fopen("5.txt", "r");
    fseek(fp, 0L, SEEK_END);
    int size = ftell(fp);
    rewind(fp);
    char* data = calloc(size, sizeof(char));
    char* filtered = calloc(size, sizeof(char));
    if (fgets(data, size + 1, fp) != NULL) {
        fclose(fp);
        int count = count_canceled(data, size);
        printf("Solution 1: %d\n", count);
        int min = -1;
        for (char bad = 'a'; bad <= 'z'; ++bad) {
            int i = 0;
            for (char* cp = data;; ++cp) {
                char c = *cp;
                if (c == 0) {
                   filtered[i] == 0;
                   break; 
                }
                if (c != bad && c + 32 != bad) {
                    filtered[i] = c;
                    ++i;
                }
            }
            int count = count_canceled(filtered, strlen(filtered));
            if (min < 0 || count < min) {
                min = count;
            }
        }
        printf("Solution 2: %d\n", min);
    };
}
