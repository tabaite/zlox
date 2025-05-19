#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include "scanner.h"

char *ReadFileContents(const char *filename);
void PrintToken(struct Token token);

int main(int argc, char *argv[]) {
    // Disable output buffering
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    if (argc < 3) {
        fprintf(stderr, "Usage: ./your_program tokenize <filename>\n");
        return 1;
    }

    const char *command = argv[1];

    int exitCode = 0;

    if (strcmp(command, "tokenize") == 0) {
        // You can use print statements as follows for debugging, they'll be visible when running tests.
        fprintf(stderr, "Logs from your program will appear here!\n");

        char *file_contents = ReadFileContents(argv[2]);

        if (strlen(file_contents) <= 0) {
            struct ScanningContext context = NewScanningContext("");
            PrintToken(ScanUntilNextToken(&context));
            return 0;
        }

        struct ScanningContext context = NewScanningContext(file_contents);

        for(struct Token current = NewToken(NONE); current.Type != END; current = ScanUntilNextToken(&context)) {
            if(current.Type == INVALID) {
                exitCode = 65;
            }
            PrintToken(current);
        }

        struct Token end = NewToken(END);
        PrintToken(end);
        free(file_contents);
    } else {
        fprintf(stderr, "Unknown command: %s\n", command);
        return 1;
    }

    return exitCode;
}

char *ReadFileContents(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (file == NULL) {
        fprintf(stderr, "Error reading file: %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *file_contents = malloc(file_size + 1);
    if (file_contents == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(file_contents, 1, file_size, file);
    if (bytes_read < file_size) {
        fprintf(stderr, "Error reading file contents\n");
        free(file_contents);
        fclose(file);
        return NULL;
    }

    file_contents[file_size] = '\0';
    fclose(file);

    return file_contents;
}

void PrintToken(struct Token token) {
    switch(token.Type) {
        case BANG:
            printf("BANG ! null\n");
            return;
        case BANG_EQUAL:
            printf("BANG_EQUAL != null\n");
            return;
        case LESS:
            printf("LESS < null\n");
            return;
        case LESS_EQUAL:
            printf("LESS_EQUAL <= null\n");
            return;
        case GREATER:
            printf("GREATER > null\n");
            return;
        case GREATER_EQUAL:
            printf("GREATER_EQUAL >= null\n");
            return;
        case EQUAL:
            printf("EQUAL = null\n");
            return;
        case EQUAL_EQUAL:
            printf("EQUAL_EQUAL == null\n");
            return;
        case LEFT_PAREN:
            printf("LEFT_PAREN ( null\n");
            return;
        case RIGHT_PAREN:
            printf("RIGHT_PAREN ) null\n");
            return;
        case LEFT_BRACE:
            printf("LEFT_BRACE { null\n");
            return;
        case RIGHT_BRACE:
            printf("RIGHT_BRACE } null\n");
            return;
        case COMMA:
            printf("COMMA , null\n");
            return;
        case DOT:
            printf("DOT . null\n");
            return;
        case MINUS:
            printf("MINUS - null\n");
            return;
        case PLUS:
            printf("PLUS + null\n");
            return;
        case SEMICOLON:
            printf("SEMICOLON ; null\n");
            return;
        case STAR:
            printf("STAR * null\n");
            return;
        case SLASH:
            printf("SLASH / null\n");
            return;
        case IDENTIFIER: {
            char* str = token.Position;
            uint32_t len = token.Length;
            printf("IDENTIFIER %.*s null\n", len, str);
            return;
        }
        case STRING: {
            // yep
            char* str = token.Position;
            uint32_t len = token.Length;
            printf("STRING \"%.*s\" %.*s\n", len, str, len, str);
            return;
        }
        case NUMBER: {
            char* str = token.Position;
            uint32_t len = token.Length;

            char afterToken = str[len];
            str[len] = '\0';
            double converted = atof(str);
            str[len] = afterToken;

            if (fabs(converted - (int)converted) < 1e-6) {
                printf("NUMBER %.*s %.1f\n", len, str, converted);
            } else {
                // Floating point precision errors mean that printing the converted float
                // produces wrong answers for numbers such as 123.456.
                printf("NUMBER %.*s %.*s\n", len, str, len, str);
            }
            return;
        }
        case END:
            printf("EOF  null\n");
            return;
    }
}