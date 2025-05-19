#ifndef _SCANNER
#define _SCANNER
#include <stdint.h>
enum TokenType {
    // Invalid tokens can be handled however we want.
    // However, it is not really in the best interest
    // of the user to stop after only 1 invalid has been found.
    END, INVALID,

    // An initial value provided for looping.
    // ScanUntilNextToken will never return this.
    NONE,

    // single-character tokens
    LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
    COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

    // one or two character tokens
    BANG, BANG_EQUAL,
    EQUAL, EQUAL_EQUAL,
    GREATER, GREATER_EQUAL,
    LESS, LESS_EQUAL,

    // literals
    // all number literals are floats internally so don't worry
    IDENTIFIER, STRING, NUMBER,

    // keywords
    AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
    PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,
};

struct ScanningContext {
    char *BackingString;
    int LineNumber;
};

struct Token {
    enum TokenType Type;
    // A union representing the value.
    // The struct is arranged the way it is to prevent the compiler padding this to 24 bytes.
    // Strings and identifiers use the Item::Position field in conjunction with Length.
    // Numbers use the Item::Number field, and do not use Length.
    // Invalids do not use anything.

    // Single-char tokens do not use these fields.
    uint32_t Length;
    char *Position;
};

/// TODO: Add a struct which stores a union to
/// either an error (storing line num and the unexpected token),
/// an identifier (storing the name), or a regular token

struct Token NewToken(enum TokenType token);
struct Token NewStringToken(char *start, uint32_t length);
struct Token NewNumberToken(char *start, uint32_t length);
struct Token NewIdentifierToken(char *start, uint32_t length);
struct Token NewKeywordToken(enum TokenType keyword, char *start, uint32_t length);

/// @brief Take in a pointer to a ScanningContext.
/// The context will be mutated so that the BackingString
/// starts directly after the found token.
struct Token ScanUntilNextToken(struct ScanningContext *context);

/// @brief Create a new scanning context given a backing string (STACK-ALLOCATED)
struct ScanningContext NewScanningContext(char *backingString);
/// @brief Initialize a scanning context, given a pointer.
void InitScanningContext(struct ScanningContext *context, char *backingString);

#endif