#include "lexer.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void* safe_malloc( size_t n )
{
	void* p = malloc( n );
	if ( !p ) {
		fprintf( stderr, "FATAL: out of memory (malloc %zu)\n", n );
		exit( 1 );
	}
	return p;
}
static void* safe_realloc( void* ptr, size_t n )
{
	void* p = realloc( ptr, n );
	if ( !p ) {
		fprintf( stderr, "FATAL: out of memory (realloc %zu)\n", n );
		exit( 1 );
	}
	return p;
}
static char* strdup_or_die( const char* s )
{
	if ( !s )
		return NULL;
	char* r = strdup( s );
	if ( !r ) {
		fprintf( stderr, "FATAL: out of memory (strdup)\n" );
		exit( 1 );
	}
	return r;
}

/* free token string */
void token_free( Token* t )
{
	if ( !t )
		return;
	if ( t->str ) {
		free( t->str );
		t->str = NULL;
	}
}

static inline char peek( const Lexer* lx, size_t off )
{
	size_t p = lx->pos + off;
	if ( lx->src == NULL )
		return '\0';
	return lx->src[p];
}

static void skip_ws( Lexer* lx )
{
	while ( peek( lx, 0 ) ) {
		char c = peek( lx, 0 );
		if ( isspace( (unsigned char) c ) ) {
			if ( c == '\n' )
				lx->line++;
			lx->pos++;
			continue;
		}
		break;
	}
}
static int is_ident_start( char c )
{
	return isalpha( (unsigned char) c ) || c == '_';
}
static int is_ident_char( char c )
{
	return isalnum( (unsigned char) c ) || c == '_';
}

Token lex_next( Lexer* lx )
{
	skip_ws( lx );
	Token t;
	t.line = lx->line;
	t.kind = TK_EOF;
	t.str = NULL;
	t.num = 0;
	char c = peek( lx, 0 );
	if ( c == 0 ) {
		t.kind = TK_EOF;
		return t;
	}

	/* // single-line comment */
	if ( c == '/' && peek( lx, 1 ) == '/' ) {
		size_t st = lx->pos + 2;
		lx->pos = st;
		size_t end = st;
		while ( peek( lx, end - st ) && lx->src[end] != '\n' )
			end++;
		size_t len = end - st;
		t.str = safe_malloc( len + 1 );
		memcpy( t.str, lx->src + st, len );
		t.str[len] = '\0';
		t.kind = TK_COMMENT;
		lx->pos = end;
		if ( peek( lx, 0 ) == '\n' ) {
			lx->line++;
			lx->pos++;
		}
		return t;
	}

	if ( c == '/' && peek( lx, 1 ) == '*' ) {
		lx->pos += 2;
		size_t st = lx->pos;
		size_t end = st;
		while ( peek( lx, 0 ) ) {
			if ( lx->src[end] == '*' && peek( lx, end - st + 1 ) == '/' ) {
				break;
			}
			if ( lx->src[end] == '\n' )
				lx->line++;
			end++;
		}
		/* check termination */
		if ( lx->src[end] == '\0' || !( lx->src[end] == '*' && lx->src[end + 1] == '/' ) ) {
			fprintf( stderr, "Unterminated block comment starting at line %d\n", lx->line );
			exit( 1 );
		}
		size_t len = end - st;
		t.str = safe_malloc( len + 1 );
		memcpy( t.str, lx->src + st, len );
		t.str[len] = '\0';
		lx->pos = end + 2;
		t.kind = TK_COMMENT;
		return t;
	}

	/* identifier / keyword */
	if ( is_ident_start( c ) ) {
		size_t st = lx->pos;
		while ( peek( lx, 0 ) && is_ident_char( peek( lx, 0 ) ) )
			lx->pos++;
		size_t len = lx->pos - st;
		t.str = safe_malloc( len + 1 );
		memcpy( t.str, lx->src + st, len );
		t.str[len] = '\0';

		/* map keywords to TK_KW_* tokens (parser relies on these) */
		if ( strcmp( t.str, "extern" ) == 0 ) {
			t.kind = TK_KW_EXTERN;
		} else if ( strcmp( t.str, "return" ) == 0 ) {
			t.kind = TK_KW_RETURN;
		} else if ( strcmp( t.str, "if" ) == 0 ) {
			t.kind = TK_KW_IF;
		} else if ( strcmp( t.str, "else" ) == 0 ) {
			t.kind = TK_KW_ELSE;
		} else if ( strcmp( t.str, "while" ) == 0 ) {
			t.kind = TK_KW_WHILE;
		} else if ( strcmp( t.str, "local" ) == 0 ) {
			t.kind = TK_KW_LOCAL;
		} else if ( strcmp( t.str, "include" ) == 0 ) {
			t.kind = TK_KW_INCLUDE;
		} else {
			t.kind = TK_IDENT;
		}
		return t;
	}

	/* numbers (support negative literal like -123) */
	if ( isdigit( (unsigned char) c ) || ( c == '-' && isdigit( (unsigned char) peek( lx, 1 ) ) ) ) {
		size_t st = lx->pos;
		if ( peek( lx, 0 ) == '-' )
			lx->pos++;
		while ( isdigit( (unsigned char) peek( lx, 0 ) ) )
			lx->pos++;
		size_t len = lx->pos - st;
		char*  tmp = safe_malloc( len + 1 );
		memcpy( tmp, lx->src + st, len );
		tmp[len] = '\0';
		t.kind = TK_NUMBER;
		t.num = atoll( tmp );
		t.str = tmp; /* transfer ownership; token_free will free */
		return t;
	}

	/* char literal */
	if ( c == '\'' ) {
		lx->pos++;
		char val = 0;
		if ( peek( lx, 0 ) == '\\' ) {
			lx->pos++;
			char esc = peek( lx, 0 );
			if ( esc == 'n' )
				val = '\n';
			else if ( esc == 't' )
				val = '\t';
			else
				val = esc;
			lx->pos++;
		} else {
			val = peek( lx, 0 );
			lx->pos++;
		}
		if ( peek( lx, 0 ) != '\'' ) {
			fprintf( stderr, "Unterminated char literal at line %d\n", lx->line );
			exit( 1 );
		}
		lx->pos++;
		t.kind = TK_CHAR;
		t.num = (int64_t) val;
		return t;
	}

	/* string literal */
	if ( c == '"' ) {
		lx->pos++;
		size_t cap = 64, outi = 0;
		char*  buf = safe_malloc( cap );
		while ( peek( lx, 0 ) && peek( lx, 0 ) != '"' ) {
			if ( peek( lx, 0 ) == '\\' && peek( lx, 1 ) ) {
				lx->pos++;
				char esc = peek( lx, 0 );
				char add = ( esc == 'n' ) ? '\n' : ( esc == 't' ) ? '\t' : esc;
				if ( outi + 1 >= cap ) {
					cap *= 2;
					buf = safe_realloc( buf, cap );
				}
				buf[outi++] = add;
				lx->pos++;
			} else {
				if ( peek( lx, 0 ) == '\n' ) {
					/* unterminated string (newline inside) */
					fprintf( stderr, "Unterminated string literal at line %d\n", lx->line );
					exit( 1 );
				}
				if ( outi + 1 >= cap ) {
					cap *= 2;
					buf = safe_realloc( buf, cap );
				}
				buf[outi++] = peek( lx, 0 );
				lx->pos++;
			}
		}
		if ( peek( lx, 0 ) != '"' ) {
			fprintf( stderr, "Unterminated string literal at line %d\n", lx->line );
			exit( 1 );
		}
		lx->pos++;
		buf[outi] = '\0';
		t.kind = TK_STRING;
		t.str = buf;
		return t;
	}

	/* ellipsis ... */
	if ( peek( lx, 0 ) == '.' && peek( lx, 1 ) == '.' && peek( lx, 2 ) == '.' ) {
		lx->pos += 3;
		t.kind = TK_OP;
		t.str = strdup_or_die( "..." );
		return t;
	}

	/* operators: check two-char ops first (==, !=, <=, >=, &&, ||, ->) */
	char n = peek( lx, 1 );
	if ( ( c == '=' && n == '=' ) || ( c == '!' && n == '=' ) || ( c == '<' && n == '=' ) || ( c == '>' && n == '=' ) || ( c == '&' && n == '&' )
		|| ( c == '|' && n == '|' ) || ( c == '-' && n == '>' ) ) {
		t.kind = TK_OP;
		t.str = safe_malloc( 3 );
		t.str[0] = c;
		t.str[1] = n;
		t.str[2] = '\0';
		lx->pos += 2;
		return t;
	}

	/* single-char operator set (common tokens) */
	const char* ops = "+-*/%(){};=,<>&|!?:[][].*";
	/* note: ops string is just a convenience; we accept anything as OP as fallback below */
	if ( strchr( "+-*/%(){};=,<>&|!?:[].*", c ) ) {
		t.kind = TK_OP;
		t.str = safe_malloc( 2 );
		t.str[0] = c;
		t.str[1] = '\0';
		lx->pos++;
		return t;
	}

	/* fallback: return single char as operator */
	t.kind = TK_OP;
	t.str = safe_malloc( 2 );
	t.str[0] = c;
	t.str[1] = '\0';
	lx->pos++;
	return t;
}
