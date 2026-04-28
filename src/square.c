#include "preprocessor.h"
#include "ast.h"
#include "optimize.h"
#include "parser.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void codegen_all( const char* out_filename );

int main( int argc, char** argv )
{
	int			optimize_enabled = 0;
	const char* input_filename = NULL;
	const char* output_filename = NULL;

	if ( argc == 4 && strcmp( argv[1], "-O" ) == 0 ) {
		optimize_enabled = 1;
		input_filename = argv[2];
		output_filename = argv[3];
	} else if ( argc == 3 ) {
		input_filename = argv[1];
		output_filename = argv[2];
	} else {
		fprintf( stderr, "Usage: %s [-O] input.square output.asm\n", argv[0] );
		return 1;
	}

	char* buf = preprocess_file( input_filename );

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
	if ( optimize_enabled )
		optimize_all();
	codegen_all( output_filename );
	printf( "Compiled %s -> %s\n", input_filename, output_filename );

	free( buf );
	return 0;
}
