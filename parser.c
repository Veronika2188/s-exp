#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct SExp SExp;
struct SExp {
    char* type;
    void* value;
};

SExp* create_atom(char* value) {
    SExp* atom = (SExp*)malloc(sizeof(SExp));
    atom->type = "atom";
    atom->value = value;
    return atom;
}

SExp* create_list(SExp** children, int children_count) {
    SExp* list = (SExp*)malloc(sizeof(SExp));
    list->type = "list";
    list->value = children;
    return list;
}

char* sexp_to_string(SExp* sexp) {
    if (strcmp(sexp->type, "atom") == 0) {
        return (char*)sexp->value;
    } else {
        SExp** children = (SExp**)sexp->value;
        char* result = (char*)malloc(sizeof(char) * 1024);
        strcpy(result, "(");
        int i = 0;
        while (children[i] != NULL) {
            strcat(result, sexp_to_string(children[i]));
            i++;
        }
        strcat(result, ")");
        return result;
    }
}

char** tokenize(char* input, int* token_count) {
    char* input_copy = strdup(input);
    char* stringWithSpaces = input_copy;
    char* token;
     size_t pos;
    while ((pos = strcspn(stringWithSpaces, "(")) != strlen(stringWithSpaces)) {
        stringWithSpaces[pos] = '\0';
        char* temp = strdup(stringWithSpaces);
        stringWithSpaces += pos + 1;
        char* temp2 = (char*)malloc(sizeof(char) * (strlen(temp) + 4));
        strcpy(temp2, temp);
        strcat(temp2, " ( ");
        free(temp);
        stringWithSpaces = temp2;
    }
    while ((pos = strcspn(stringWithSpaces, ")")) != strlen(stringWithSpaces)) {
        stringWithSpaces[pos] = '\0';
        char* temp = strdup(stringWithSpaces);
        stringWithSpaces += pos + 1;
        char* temp2 = (char*)malloc(sizeof(char) * (strlen(temp) + 4));
        strcpy(temp2, temp);
        strcat(temp2, " ) ");
        free(temp);
        stringWithSpaces = temp2;
    }
    
    char** tokens = (char**)malloc(sizeof(char*) * 100);
    *token_count = 0;
    token = strtok(stringWithSpaces, " ");
    while (token != NULL) {
        if (strlen(token) > 0) {
            tokens[*token_count] = strdup(token);
            (*token_count)++;
        }
        token = strtok(NULL, " ");
    }
    tokens[*token_count] = NULL;
    free(input_copy);
    return tokens;
}

SExp* parse_tokens(char** tokens, int* token_index) {
    if (tokens[*token_index] == NULL) {
        return NULL;
    }

    char* token = tokens[*token_index];
    (*token_index)++;
    if (strcmp(token, "(") == 0) {
        SExp** children = (SExp**)malloc(sizeof(SExp*) * 100);
        int children_count = 0;
        while (tokens[*token_index] != NULL && strcmp(tokens[*token_index], ")") != 0) {
            children[children_count] = parse_tokens(tokens, token_index);
            children_count++;
        }
        if (tokens[*token_index] == NULL) {
             return NULL;
        }
        (*token_index)++; // Remove ")"
        children[children_count] = NULL;
        return create_list(children, children_count);
    } else if (strcmp(token, ")") == 0) {
        return NULL;
    } else {
        return create_atom(token);
    }
}

SExp* parse(char* input) {
    int token_count;
    char** tokens = tokenize(input, &token_count);
    int token_index = 0;
    SExp* result = parse_tokens(tokens, &token_index);
    for (int i = 0; i < token_count; i++) {
        free(tokens[i]);
    }
    free(tokens);
    return result;
}

int main() {
    char* input = "(+ 1 (* 2 3))";
    SExp* result = parse(input);
    char* result_string = sexp_to_string(result);
    printf("%s\n", result_string);
    free(result_string);
    return 0;
}
