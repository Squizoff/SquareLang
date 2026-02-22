#ifndef PARSER_H
#define PARSER_H

#include "ast.h"

Func** parse_file_into_funcs_from_buffer( const char* buf, int* out_cnt );
void parse_program( Parser* p );
void check_all_calls( void );

void parser_init( Parser* p );

extern Func** funcs;
extern int	  func_cnt;

#endif /* PARSER_H */
