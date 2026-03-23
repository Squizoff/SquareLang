#include "preprocessor.h"
#include "ast.h"
#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void codegen_all( const char* out_filename );

int main( int argc, char** argv )
{
	if ( argc != 3 ) {
		fprintf( stderr, "Usage: %s input.square output.asm\n", argv[0] );
		return 1;
	}

	char* buf = preprocess_file( argv[1] );

	if ( (unsigned char) buf[0] == 0xEF && (unsigned char) buf[1] == 0xBB && (unsigned char) buf[2] == 0xBF ) {
		memmove( buf, buf + 3, strlen( buf + 3 ) + 1 );
	}

	Parser p;
	p.lx.src = buf;
	p.lx.pos = 0;
	p.lx.line = 1;

	parser_init( &p );

	parse_program( &p );

	if ( parser_error_count() > 0 ) {
		free( buf );
		return 1;
	}

	check_all_calls();
	if ( parser_error_count() > 0 ) {
		free( buf );
		return 1;
	}
	codegen_all( argv[2] );
	printf( "Compiled %s -> %s\n", argv[1], argv[2] );

	free( buf );
	return 0;
}
