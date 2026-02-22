#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_MACROS 256
#define MAX_MACRO_NAME 64
#define MAX_MACRO_VALUE 256

typedef struct
{
	char name[MAX_MACRO_NAME];
	char value[MAX_MACRO_VALUE];
} Macro;

static Macro  macros[MAX_MACROS];
static size_t macro_count = 0;

/* ================= utility ================= */

static void ensure_capacity( char** buf, size_t* cap, size_t need )
{
	if ( *cap >= need )
		return;
	size_t newcap = ( *cap == 0 ) ? 4096 : *cap;
	while ( newcap < need )
		newcap *= 2;
	char* n = realloc( *buf, newcap );
	if ( !n ) {
		perror( "realloc" );
		exit( 1 );
	}
	*buf = n;
	*cap = newcap;
}

/* ================= file read ================= */

static char* read_file( const char* filename )
{
	FILE* f = fopen( filename, "rb" );
	if ( !f ) {
		perror( filename );
		exit( 1 );
	}

	size_t cap = 0;
	size_t len = 0;
	char*  buf = NULL;
	char   tmp[4096];

	while ( 1 ) {
		size_t r = fread( tmp, 1, sizeof( tmp ), f );
		if ( r == 0 )
			break;
		ensure_capacity( &buf, &cap, len + r + 1 );
		memcpy( buf + len, tmp, r );
		len += r;
	}

	if ( !buf ) {
		buf = malloc( 1 );
		if ( !buf ) {
			perror( "malloc" );
			exit( 1 );
		}
		buf[0] = 0;
	} else {
		buf[len] = 0;
	}

	fclose( f );
	return buf;
}

/* ================= macros ================= */

static void define_macro( const char* name, const char* value )
{
	for ( size_t i = 0; i < macro_count; i++ ) {
		if ( !strcmp( macros[i].name, name ) ) {
			strncpy( macros[i].value, value, MAX_MACRO_VALUE - 1 );
			macros[i].value[MAX_MACRO_VALUE - 1] = 0;
			return;
		}
	}
	if ( macro_count >= MAX_MACROS ) {
		fprintf( stderr, "Too many macros\n" );
		exit( 1 );
	}
	strncpy( macros[macro_count].name, name, MAX_MACRO_NAME - 1 );
	macros[macro_count].name[MAX_MACRO_NAME - 1] = 0;
	strncpy( macros[macro_count].value, value, MAX_MACRO_VALUE - 1 );
	macros[macro_count].value[MAX_MACRO_VALUE - 1] = 0;
	macro_count++;
}

static void undef_macro( const char* name )
{
	for ( size_t i = 0; i < macro_count; i++ ) {
		if ( !strcmp( macros[i].name, name ) ) {
			macros[i] = macros[macro_count - 1];
			macro_count--;
			return;
		}
	}
}

static int is_macro_defined( const char* name )
{
	for ( size_t i = 0; i < macro_count; i++ )
		if ( !strcmp( macros[i].name, name ) )
			return 1;
	return 0;
}

/* ================= macro substitution ================= */

static void substitute_macros( char** text )
{
	char*  buf = *text;
	size_t cap = strlen( buf ) + 1;
	size_t len = strlen( buf );

	for ( size_t m = 0; m < macro_count; m++ ) {
		const char* name = macros[m].name;
		const char* val = macros[m].value;
		size_t		nlen = strlen( name );
		size_t		vlen = strlen( val );

		for ( size_t i = 0; i + nlen <= len; ) {
			if ( memcmp( buf + i, name, nlen ) == 0 && ( i == 0 || !isalnum( (unsigned char) buf[i - 1] ) )
				&& !isalnum( (unsigned char) buf[i + nlen] ) ) {

				size_t newlen = len - nlen + vlen;
				if ( newlen + 1 > cap ) {
					while ( cap < newlen + 1 )
						cap *= 2;
					buf = realloc( buf, cap );
					if ( !buf ) {
						perror( "realloc" );
						exit( 1 );
					}
				}

				memmove( buf + i + vlen, buf + i + nlen, len - i - nlen + 1 );
				memcpy( buf + i, val, vlen );
				len = newlen;
				i += vlen;
			} else {
				i++;
			}
		}
	}

	*text = buf;
}

/* ================= preprocess ================= */

char* preprocess_file( const char* filename )
{
	char* src = read_file( filename );

	size_t out_cap = 4096;
	size_t out_pos = 0;
	char*  out = malloc( out_cap );
	if ( !out ) {
		perror( "malloc" );
		exit( 1 );
	}

	int skip_stack[128] = { 0 };
	int skip_level = 0;

	for ( size_t i = 0; src[i]; ) {
		size_t j = i;
		while ( src[j] && ( src[j] == ' ' || src[j] == '\t' || src[j] == '\r' ) )
			j++;

		int skipping = skip_level ? skip_stack[skip_level - 1] : 0;

		if ( src[j] == '#' && isalpha( (unsigned char) src[j + 1] ) ) {
			i = j + 1;
			char   directive[32];
			size_t dlen = 0;

			while ( isalnum( (unsigned char) src[i] ) || src[i] == '_' ) {
				if ( dlen + 1 < sizeof( directive ) )
					directive[dlen++] = src[i];
				i++;
			}
			directive[dlen] = 0;

			while ( isspace( (unsigned char) src[i] ) && src[i] != '\n' )
				i++;

			int parent_skip = skip_level ? skip_stack[skip_level - 1] : 0;

			if ( !strcmp( directive, "include" ) ) {
				if ( !parent_skip ) {
					if ( src[i] != '"' ) {
						fprintf( stderr, "Expected \" after #include\n" );
						exit( 1 );
					}
					i++;
					char   fname[256];
					size_t flen = 0;
					while ( src[i] && src[i] != '"' ) {
						if ( flen + 1 < sizeof( fname ) )
							fname[flen++] = src[i];
						i++;
					}
					fname[flen] = 0;
					if ( src[i] != '"' ) {
						fprintf( stderr, "Unterminated #include\n" );
						exit( 1 );
					}
					i++;

					const char* dot = strrchr( fname, '.' );
					int			is_square = dot && !strcmp( dot, ".square" );

					if ( is_square ) {
						char line[512];
						int	 n = snprintf( line, sizeof( line ), "include \"%s\";\n", fname );
						ensure_capacity( &out, &out_cap, out_pos + n + 1 );
						memcpy( out + out_pos, line, n );
						out_pos += n;
					} else {
						char* inc = preprocess_file( fname );
						substitute_macros( &inc );
						size_t l = strlen( inc );
						ensure_capacity( &out, &out_cap, out_pos + l + 1 );
						memcpy( out + out_pos, inc, l );
						out_pos += l;
						free( inc );
					}
				}
			} else if ( !strcmp( directive, "define" ) && !parent_skip ) {
				char name[64], value[256] = "";
				sscanf( src + i, "%63s %255[^\n]", name, value );
				define_macro( name, value );
			} else if ( !strcmp( directive, "undef" ) && !parent_skip ) {
				char name[64];
				sscanf( src + i, "%63s", name );
				undef_macro( name );
			} else if ( !strcmp( directive, "ifdef" ) ) {
				char name[64];
				sscanf( src + i, "%63s", name );
				skip_stack[skip_level++] = parent_skip || !is_macro_defined( name );
			} else if ( !strcmp( directive, "ifndef" ) ) {
				char name[64];
				sscanf( src + i, "%63s", name );
				skip_stack[skip_level++] = parent_skip || is_macro_defined( name );
			} else if ( !strcmp( directive, "else" ) && skip_level ) {
				int parent = ( skip_level > 1 ) ? skip_stack[skip_level - 2] : 0;
				skip_stack[skip_level - 1] = parent ? 1 : !skip_stack[skip_level - 1];
			} else if ( !strcmp( directive, "endif" ) && skip_level ) {
				skip_level--;
			}

			while ( src[i] && src[i] != '\n' )
				i++;
			if ( src[i] == '\n' )
				i++;
		} else {
			if ( !skipping ) {
				ensure_capacity( &out, &out_cap, out_pos + 2 );
				out[out_pos++] = src[i];
			}
			i++;
		}
	}

	out[out_pos] = 0;
	substitute_macros( &out );

	free( src );
	return out;
}
