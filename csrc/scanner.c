#include <stdio.h>
#include <stdint.h>
#include "scanner.h"
int IsAlpha(char c) {
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
            c == '_';
}
int IsNumeric(char c) {
    return (c >= '0' && c <= '9');
}
int IsAlphaNumeric(char c) {
    return IsAlpha(c) || IsNumeric(c);
}

struct Token ScanUntilNextToken(struct ScanningContext *context) {
    char* start = context->BackingString;

    // When we reach the null terminator, we return an END (EOF is taken)
    // token. IF THE USER ATTEMPTS TO CALL SCAN AFTERWARDS IT WILL RESULT
    // IN A SEGFAULT.

    int i = 0;
    for(; start[i]; i++) {
#pragma region Identifiers/keywords
        if (IsAlpha(start[i])) {
            int len = 0;

            for (; start[i+len]; len++)
            {
                if (!IsAlphaNumeric(start[i+len])) {
                    context->BackingString = start+i+len;
                    return NewIdentifierToken(start+i, len);
                }
            }
            context->BackingString = start+i+len;
            return NewIdentifierToken(start+i, len);
        }
#pragma endregion
#pragma region Number literals
        if (IsNumeric(start[i])) {
            int len = 0;
            int seenDecimal = 0;

            for (; start[i+len]; len++)
            {
                if (start[i+len] == '.') {
                    if (seenDecimal) {
                        context->BackingString = start+i+len;
                        return NewNumberToken(start+i, len);
                    }
                    if (!IsNumeric(start[i+len+1])) {
                        context->BackingString = start+i+len;
                        return NewNumberToken(start+i, len);
                    }
                    seenDecimal = 1;
                    continue;
                } else if (!IsNumeric(start[i+len])) {
                    // end of int
                    // This looks really sketchy, but len isn't incremented on this default case,
                    // so start+i+len points to the last element (meaning the token will exclude it)
                    context->BackingString = start+i+len;
                    return NewNumberToken(start+i, len);
                }
            }
            context->BackingString = start+i+len;
            return NewNumberToken(start+i, len);
        }
#pragma endregion
        // Other tokens are handled in a massive switch statement
        switch(start[i]) {
#pragma region Multiple-character tokens
            case '<':
                switch(start[i+1]) {
                    // <=
                    case '=':
                        // End of token, start at character after
                        context->BackingString = start+i+2;
                        return NewToken(LESS_EQUAL);
                    default:
                        // We don't know whether this char is a token, so start here next time
                        context->BackingString = start+i+1;
                        return NewToken(LESS);
                }
            case '>':
                switch(start[i+1]) {
                    // >=
                    case '=':
                        // End of token, start at character after
                        context->BackingString = start+i+2;
                        return NewToken(GREATER_EQUAL);
                    default:
                        // We don't know whether this char is a token, so start here next time
                        context->BackingString = start+i+1;
                        return NewToken(GREATER);
                }
            case '!':
                switch(start[i+1]) {
                    // !=
                    case '=':
                        // End of token, start at character after
                        context->BackingString = start+i+2;
                        return NewToken(BANG_EQUAL);
                    default:
                        // We don't know whether this char is a token, so start here next time
                        context->BackingString = start+i+1;
                        return NewToken(BANG);
                }
            case '=':
                switch(start[i+1]) {
                    // ==
                    case '=':
                        // End of token, start at character after
                        context->BackingString = start+i+2;
                        return NewToken(EQUAL_EQUAL);
                    default:
                        // We don't know whether this char is a token, so start here next time
                        context->BackingString = start+i+1;
                        return NewToken(EQUAL);
                }
            case '/':
                switch(start[i+1]) {
                    // Comment
                    case '/': {
                        int subt = i;
                        int seenNewLine = 0;
                        for(; start[subt]; subt++)
                        {
                            if(start[subt] == '\n') {
                                // Set total to where we are right now,
                                // and effectively reset the state.
                                // total gets incremented after we break
                                context->LineNumber++;
                                seenNewLine = 1;
                                break;
                            }
                        }
                        i = subt;

                        // There's nothing left.
                        if (!seenNewLine) {
                            context->BackingString = start+subt;
                            return NewToken(END);
                        }
                        break;
                    }
                    default:
                        // We don't know whether this char is a token, so start here next time
                        context->BackingString = start+i+1;
                        return NewToken(SLASH);
                }
                break;
#pragma endregion
#pragma region String literals
            case '"': {
                int strline = context->LineNumber;

                // don't mistakenly start at the starting quote
                int subt = i+1;

                for(; start[subt]; subt++)
                {
                    if(start[subt] == '\n') {
                        context->LineNumber++;
                    }
                    switch (start[subt]) {
                        case '"':
                            context->BackingString = start+subt+1;
                            return NewStringToken(start+i+1, subt-i-1);
                    };
                }
                // No terminator found.
                fprintf(stderr, "[line %d] Error: Unterminated string.\n", strline);
                context->BackingString = start+subt;
                return NewToken(INVALID);
            }
#pragma endregion
#pragma region One character tokens
            case '\t':
            case ' ':
            // windows shenanigans
            case '\r':
                break;
            case '\n':
                context->LineNumber++;
                break;
            case '(':
                context->BackingString = start+i+1;
                return NewToken(LEFT_PAREN);
            case ')':
                context->BackingString = start+i+1;
                return NewToken(RIGHT_PAREN);
            case '{':
                context->BackingString = start+i+1;
                return NewToken(LEFT_BRACE);
            case '}':
                context->BackingString = start+i+1;
                return NewToken(RIGHT_BRACE);
            case ',':
                context->BackingString = start+i+1;
                return NewToken(COMMA);
            case '.':
                context->BackingString = start+i+1;
                return NewToken(DOT);
            case '-':
                context->BackingString = start+i+1;
                return NewToken(MINUS);
            case '+':
                context->BackingString = start+i+1;
                return NewToken(PLUS);
            case ';':
                context->BackingString = start+i+1;
                return NewToken(SEMICOLON);
            case '*':
                context->BackingString = start+i+1;
                return NewToken(STAR);
#pragma endregion
            // Right now, we assume that any other character is a non-valid character.
            // This won't pick up the null terminator,
            // as that will cause the loop to break before this is
            // called.
            default:
                context->BackingString = start+i+1;
                fprintf(stderr, "[line %d] Error: Unexpected character: %c\n", context->LineNumber, start[i]);
                return NewToken(INVALID);
        }
    }
    return NewToken(END);
}

struct ScanningContext NewScanningContext(char *backingString) {
    struct ScanningContext context = { .BackingString = backingString, .LineNumber = 1 };
    return context;
}

void InitScanningContext(struct ScanningContext *context, char* backingString) {
    context->BackingString = "";
    context->LineNumber = 1;
}

struct Token NewToken(enum TokenType token) {
    struct Token result = { .Type = token };
    return result;
}
struct Token NewStringToken(char *start, uint32_t length) {
    struct Token result = {
        .Type = STRING,
        .Length = length,
        .Position = start,
    };
    return result;
}
struct Token NewNumberToken(char *start, uint32_t length) {
    struct Token result = {
        .Type = NUMBER,
        .Length = length,
        .Position = start,
    };
    return result;
}
struct Token NewIdentifierToken(char *start, uint32_t length) {
    struct Token result = {
        .Type = IDENTIFIER,
        .Length = length,
        .Position = start,
    };
    return result;
}
struct Token NewKeywordToken(enum TokenType keyword, char *start, uint32_t length) {
    struct Token result = {
        .Type = keyword,
        .Length = length,
        .Position = start,
    };
    return result;
}