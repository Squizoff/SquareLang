#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>
#include <stdint.h>

typedef struct
{
	const char* src;
	size_t		pos;
	int			line;
} Lexer;

typedef enum
{
	TK_EOF,
	TK_IDENT,
	TK_NUMBER,
	TK_OP,
	TK_KW_EXTERN,
	TK_KW_RETURN,
	TK_KW_IF,
	TK_KW_ELSE,
	TK_KW_WHILE,
	TK_STRING,
	TK_CHAR,
	TK_COMMENT,
	TK_KW_LOCAL,
	TK_KW_INCLUDE
} TokenKind;

typedef struct
{
	TokenKind kind;
	char*	  str;
	int64_t	  num;
	int		  line;
} Token;

static inline void lexer_init( Lexer* lx, const char* src )
{
	lx->src = src;
	lx->pos = 0;
	lx->line = 1;
}

Token lex_next( Lexer* lx );

void token_free( Token* t );

#endif /* LEXER_H */
