#include "ast.h"
#include "parser.h"
#include "preprocessor.h"
#include "lexer.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

Func** funcs = NULL;
int	   func_cnt = 0;

StructDef** g_structs = NULL;
int			g_struct_cnt = 0;

static StructDef* find_struct_by_name( const char* name )
{
	if ( !name )
		return NULL;
	for ( int i = 0; i < g_struct_cnt; i++ ) {
		if ( g_structs[i] && g_structs[i]->name && strcmp( g_structs[i]->name, name ) == 0 )
			return g_structs[i];
	}
	return NULL;
}

static void add_struct_def( StructDef* s )
{
	g_structs = realloc( g_structs, sizeof( StructDef* ) * ( g_struct_cnt + 1 ) );
	g_structs[g_struct_cnt++] = s;
}

static void nexttok( Parser* p )
{
	token_free( &p->tok );
	p->tok = lex_next( &p->lx );
}
static int accept_op( Parser* p, const char* s )
{
	if ( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, s ) == 0 ) {
		nexttok( p );
		return 1;
	}
	return 0;
}
static void expect_op( Parser* p, const char* s )
{
	if ( !accept_op( p, s ) ) {
		fprintf( stderr, "Parse error line %d: expected '%s', got '%s'\n", p->tok.line, s, p->tok.str ? p->tok.str : "<null>" );
		exit( 1 );
	}
}
static int is_kw( Parser* p, TokenKind k )
{
	return p->tok.kind == k;
}
static int is_ident( Parser* p )
{
	return p->tok.kind == TK_IDENT;
}
static char* consume_ident( Parser* p )
{
	if ( !is_ident( p ) ) {
		fprintf( stderr, "Parse error line %d: expected ident\n", p->tok.line );
		exit( 1 );
	}
	char* r = strdup( p->tok.str );
	nexttok( p );
	return r;
}

static int is_type_name( const char* s )
{
	if ( !s )
		return 0;
	if ( strcmp( s, "void" ) == 0 || strcmp( s, "int32_t" ) == 0 || strcmp( s, "int64_t" ) == 0 || strcmp( s, "int8_t" ) == 0
		|| strcmp( s, "char" ) == 0 || strcmp( s, "uint8_t" ) == 0 || strcmp( s, "uint32_t" ) == 0 || strcmp( s, "uint64_t" ) == 0
		|| strcmp( s, "size_t" ) == 0 || strcmp( s, "unsigned" ) == 0 || strcmp( s, "const" ) == 0 || strcmp( s, "struct" ) == 0 )
		return 1;
	if ( find_struct_by_name( s ) )
		return 1;
	return 0;
}

static int size_of_type( const Type* t )
{
	if ( !t )
		return 0;
	int ptr_size = (int) sizeof( void* );
	if ( t->ptr_level > 0 )
		return ptr_size;

	switch ( t->base ) {
		case TY_I32:
		case TY_U32:
			return 4;
		case TY_I64:
		case TY_U64:
			return 8;
		case TY_I8:
			return 1;
		case TY_CHAR:
			return 1;
		case TY_USIZE:
			return ptr_size;
		case TY_STRUCT: {
			StructDef* sd = find_struct_by_name( t->struct_name );
			if ( !sd ) {
				return ptr_size;
			}
			return sd->size;
		}
		default:
			return 4;
	}
}

static void compute_struct_layout( StructDef* sd )
{
	if ( !sd )
		return;
	int offset = 0;
	int ptr_size = (int) sizeof( void* );
	for ( int i = 0; i < sd->field_cnt; i++ ) {
		StructField* f = sd->fields[i];
		int			 fsz = size_of_type( &f->type );
		int			 align = ( fsz > ptr_size ) ? ptr_size : fsz;
		if ( align <= 0 )
			align = 1;
		int rem = offset % align;
		if ( rem )
			offset += ( align - rem );
		f->offset = offset;
		offset += fsz;
	}
	int rem = offset % ptr_size;
	if ( rem )
		offset += ( ptr_size - rem );
	sd->size = offset;
}

static Type parse_type( Parser* p )
{
	Type t;
	memset( &t, 0, sizeof( Type ) );
	t.ptr_level = 0;
	t.is_const = 0;
	t.is_unsigned = 0;
	t.struct_name = NULL;

	int saw_something = 1;
	while ( saw_something ) {
		saw_something = 0;
		if ( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "const" ) == 0 ) {
			t.is_const = 1;
			nexttok( p );
			saw_something = 1;
		}
		if ( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "unsigned" ) == 0 ) {
			t.is_unsigned = 1;
			nexttok( p );
			saw_something = 1;
		}
	}

	if ( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "struct" ) == 0 ) {
		nexttok( p );
		if ( p->tok.kind != TK_IDENT ) {
			fprintf( stderr, "Parse error line %d: expected struct name\n", p->tok.line );
			exit( 1 );
		}
		char* sname = strdup( p->tok.str );
		nexttok( p );

		if ( accept_op( p, "{" ) ) {
			StructField** fields = NULL;
			int			  fcnt = 0;
			while ( !( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, "}" ) == 0 ) ) {
				while ( p->tok.kind == TK_COMMENT )
					nexttok( p );

				if ( p->tok.kind == TK_OP && strcmp( p->tok.str, "}" ) == 0 )
					break;

				Type  ftype = parse_type( p );
				char* fname = consume_ident( p );

				while ( p->tok.kind == TK_COMMENT )
					nexttok( p );

				expect_op( p, ";" );
				StructField* sf = malloc( sizeof( StructField ) );
				sf->name = fname;
				sf->type = ftype;
				fields = realloc( fields, sizeof( StructField* ) * ( fcnt + 1 ) );
				fields[fcnt++] = sf;
			}
			expect_op( p, "}" );
			accept_op( p, ";" );

			StructDef* sd = malloc( sizeof( StructDef ) );
			sd->name = sname;
			sd->fields = fields;
			sd->field_cnt = fcnt;
			add_struct_def( sd );

			t.base = TY_STRUCT;
			t.struct_name = strdup( sd->name );
			return t;
		} else {
			if ( !find_struct_by_name( sname ) ) {
				StructDef* sd = malloc( sizeof( StructDef ) );
				sd->name = strdup( sname );
				sd->fields = NULL;
				sd->field_cnt = 0;
				compute_struct_layout( sd );
				add_struct_def( sd );
			}
			t.base = TY_STRUCT;
			t.struct_name = sname;
			while ( p->tok.kind == TK_OP && strcmp( p->tok.str, "*" ) == 0 ) {
				t.ptr_level++;
				nexttok( p );
			}
			return t;
		}
	}

	if ( p->tok.kind == TK_IDENT
		&& ( strcmp( p->tok.str, "void" ) == 0 || strcmp( p->tok.str, "int32_t" ) == 0 || strcmp( p->tok.str, "int64_t" ) == 0
			|| strcmp( p->tok.str, "int8_t" ) == 0 || strcmp( p->tok.str, "uint8_t" ) == 0 || strcmp( p->tok.str, "uint32_t" ) == 0
			|| strcmp( p->tok.str, "uint64_t" ) == 0 || strcmp( p->tok.str, "char" ) == 0 || strcmp( p->tok.str, "size_t" ) == 0
			|| find_struct_by_name( p->tok.str ) ) ) {
		if ( strcmp( p->tok.str, "void" ) == 0 ) {
			t.base = TY_VOID;
		} else if ( strcmp( p->tok.str, "int32_t" ) == 0 ) {
			t.base = TY_I32;
		} else if ( strcmp( p->tok.str, "int64_t" ) == 0 ) {
			t.base = TY_I64;
		} else if ( strcmp( p->tok.str, "int8_t" ) == 0 ) {
			t.base = TY_I8;
		} else if ( strcmp( p->tok.str, "uint8_t" ) == 0 ) {
			t.base = TY_U8;
			t.is_unsigned = 1;
		} else if ( strcmp( p->tok.str, "uint32_t" ) == 0 ) {
			t.base = TY_U32;
			t.is_unsigned = 1;
		} else if ( strcmp( p->tok.str, "uint64_t" ) == 0 ) {
			t.base = TY_U64;
			t.is_unsigned = 1;
		} else if ( strcmp( p->tok.str, "char" ) == 0 ) {
			t.base = TY_CHAR;
		} else if ( strcmp( p->tok.str, "size_t" ) == 0 ) {
			t.base = TY_USIZE;
			t.is_unsigned = 1;
		} else if ( find_struct_by_name( p->tok.str ) ) {
			t.base = TY_STRUCT;
			t.struct_name = strdup( p->tok.str );
		} else {
			fprintf( stderr, "Unhandled type token '%s' at line %d\n", p->tok.str ? p->tok.str : "<null>", p->tok.line );
			exit( 1 );
		}
		nexttok( p );

		while ( p->tok.kind == TK_OP && strcmp( p->tok.str, "*" ) == 0 ) {
			t.ptr_level++;
			nexttok( p );
		}

		return t;
	}

	fprintf( stderr, "Parse error line %d: explicit type required (got '%s')\n", p->tok.line, p->tok.str ? p->tok.str : "<null>" );
	exit( 1 );
}

static Type make_vararg_type( void )
{
	Type t;
	memset( &t, 0, sizeof( Type ) );
	t.base = TY_VARARG;
	return t;
}

static void free_temp_type( Type* t )
{
	if ( t && t->struct_name ) {
		free( t->struct_name );
		t->struct_name = NULL;
	}
}

static Expr*  parse_expr( Parser* p );
static Stmt*  parse_stmt( Parser* p );
static Stmt** parse_block( Parser* p, int* out_cnt );

static Expr* make_num_expr( int64_t value )
{
	Expr* e = calloc( 1, sizeof( Expr ) );
	e->kind = EX_NUM;
	e->num = value;
	return e;
}

static Expr* wrap_member_access( Expr* target, char* member )
{
	Expr* idx = calloc( 1, sizeof( Expr ) );
	idx->kind = EX_STR;
	idx->var = member;

	Expr* node = calloc( 1, sizeof( Expr ) );
	node->kind = EX_INDEX;
	node->index.target = target;
	node->index.idx = idx;
	return node;
}

static Stmt* parse_local_declaration_with_type( Parser* p, Type vart )
{
	Stmt* s = calloc( 1, sizeof( Stmt ) );
	if ( p->tok.kind != TK_IDENT ) {
		fprintf( stderr, "Parse error line %d: expected identifier after type\n", p->tok.line );
		exit( 1 );
	}
	char* name = consume_ident( p );

	Expr* init = NULL;
	if ( accept_op( p, "=" ) ) {
		init = parse_expr( p );
		expect_op( p, ";" );
	} else {
		expect_op( p, ";" );
		init = make_num_expr( 0 );
	}

	s->kind = ST_ASSIGN;
	s->assign.var = name;
	s->assign.expr = init;
	s->assign.vartype = vart;
	return s;
}

static void parse_function_params( Parser* p, char*** out_args, Type** out_arg_types, int* out_nargs )
{
	char** args = NULL;
	Type*  arg_types = NULL;
	int	   nargs = 0;

	expect_op( p, "(" );
	if ( !accept_op( p, ")" ) ) {
		while ( 1 ) {
			if ( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, "..." ) == 0 ) {
				nexttok( p );
				args = realloc( args, sizeof( char* ) * ( nargs + 1 ) );
				args[nargs] = strdup( "..." );
				arg_types = realloc( arg_types, sizeof( Type ) * ( nargs + 1 ) );
				arg_types[nargs] = make_vararg_type();
				nargs++;
				expect_op( p, ")" );
				break;
			}

			if ( !( p->tok.kind == TK_IDENT && is_type_name( p->tok.str ) ) ) {
				fprintf( stderr, "Parse error line %d: argument type required\n", p->tok.line );
				exit( 1 );
			}
			Type  at = parse_type( p );
			char* aname = consume_ident( p );

			args = realloc( args, sizeof( char* ) * ( nargs + 1 ) );
			args[nargs] = aname;
			arg_types = realloc( arg_types, sizeof( Type ) * ( nargs + 1 ) );
			arg_types[nargs] = at;
			nargs++;
			if ( accept_op( p, ")" ) )
				break;
			expect_op( p, "," );
		}
	}

	*out_args = args;
	*out_arg_types = arg_types;
	*out_nargs = nargs;
}

static void ensure_unique_param_names( const char* fname, char** args, Type* arg_types, int nargs, int line )
{
	for ( int i = 0; i < nargs; i++ ) {
		if ( arg_types && arg_types[i].base == TY_VARARG )
			continue;
		for ( int j = i + 1; j < nargs; j++ ) {
			if ( arg_types && arg_types[j].base == TY_VARARG )
				continue;
			if ( strcmp( args[i], args[j] ) == 0 ) {
				fprintf( stderr, "Parse error line %d: duplicate argument name '%s' in function '%s'\n", line, args[j], fname );
				exit( 1 );
			}
		}
	}
}

static Func* new_func( char* name, char** args, Type* arg_types, int nargs, Stmt** body, int body_cnt, int is_local, Type ret_type, int has_types )
{
	Func* f = malloc( sizeof( Func ) );
	f->name = name;
	f->orig_name = strdup( name );
	f->args = args;
	f->nargs = nargs;
	f->arg_types = ( nargs > 0 ) ? arg_types : NULL;
	f->body = body;
	f->body_cnt = body_cnt;
	f->is_local = is_local ? 1 : 0;
	f->src_file = NULL;
	f->ret_type = ret_type;
	f->has_types = has_types;
	return f;
}

static void append_func( Func*** list, int* count, Func* f )
{
	*list = realloc( *list, sizeof( Func* ) * ( *count + 1 ) );
	( *list )[( *count )++] = f;
}

static void fill_assignment_target( Parser* p, Expr* lhs, Stmt* s )
{
	if ( lhs->kind == EX_VAR ) {
		s->assign.var = lhs->var;
		s->assign.index = NULL;
		return;
	}

	if ( lhs->kind == EX_INDEX ) {
		Expr* outer_idx = lhs->index.idx;
		Expr* base = lhs;
		while ( base->kind == EX_INDEX && base->index.target->kind == EX_INDEX )
			base = base->index.target;
		if ( base->kind != EX_INDEX || base->index.target->kind != EX_VAR ) {
			fprintf( stderr, "Parse error line %d: invalid LHS in assignment\n", p->tok.line );
			exit( 1 );
		}
		s->assign.var = base->index.target->var;
		s->assign.index = outer_idx;
		return;
	}

	fprintf( stderr, "Parse error line %d: invalid lvalue in assignment\n", p->tok.line );
	exit( 1 );
}

Expr* parse_primary( Parser* p )
{
	if ( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, "&" ) == 0 ) {
		nexttok( p );
		Expr* operand = parse_primary( p );
		Expr* ue = calloc( 1, sizeof( Expr ) );
		ue->kind = EX_UNARY;
		ue->unary.op = '&';
		ue->unary.operand = operand;
		return ue;
	}

	if ( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "sizeof" ) == 0 ) {
		nexttok( p );
		expect_op( p, "(" );
		if ( !( p->tok.kind == TK_IDENT && is_type_name( p->tok.str ) ) && !( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "struct" ) == 0 ) ) {
			fprintf( stderr, "Parse error line %d: sizeof expects a type\n", p->tok.line );
			exit( 1 );
		}
		Type t = parse_type( p );
		expect_op( p, ")" );
		Expr* ne = make_num_expr( size_of_type( &t ) );
		free_temp_type( &t );
		return ne;
	}

	if ( p->tok.kind == TK_NUMBER ) {
		Expr* e = make_num_expr( p->tok.num );
		nexttok( p );
		return e;
	}

	if ( p->tok.kind == TK_IDENT ) {
		char* name = strdup( p->tok.str );
		nexttok( p );

		Expr* e = calloc( 1, sizeof( Expr ) );
		if ( accept_op( p, "(" ) ) {
			Expr** args = NULL;
			int	   nargs = 0;
			if ( !accept_op( p, ")" ) ) {
				while ( 1 ) {
					Expr* a = parse_expr( p );
					args = realloc( args, sizeof( Expr* ) * ( nargs + 1 ) );
					args[nargs++] = a;
					if ( accept_op( p, ")" ) )
						break;
					expect_op( p, "," );
				}
			}
			e->kind = EX_CALL;
			e->call.name = name;
			e->call.args = args;
			e->call.nargs = nargs;
		} else {
			e->kind = EX_VAR;
			e->var = name;
		}

		while ( accept_op( p, "[" ) ) {
			Expr* idx = parse_expr( p );
			expect_op( p, "]" );
			Expr* new_e = calloc( 1, sizeof( Expr ) );
			new_e->kind = EX_INDEX;
			new_e->index.target = e;
			new_e->index.idx = idx;
			e = new_e;
		}

		while ( p->tok.kind == TK_OP && p->tok.str && ( strcmp( p->tok.str, "." ) == 0 || strcmp( p->tok.str, "->" ) == 0 ) ) {
			nexttok( p );
			if ( !is_ident( p ) ) {
				fprintf( stderr, "Parse error line %d: expected identifier after member access operator\n", p->tok.line );
				exit( 1 );
			}
			e = wrap_member_access( e, consume_ident( p ) );
		}

		return e;
	}

	if ( p->tok.kind == TK_STRING ) {
		Expr* e = calloc( 1, sizeof( Expr ) );
		e->kind = EX_STR;
		e->var = strdup( p->tok.str );
		nexttok( p );
		return e;
	}

	if ( accept_op( p, "(" ) ) {
		if ( p->tok.kind == TK_IDENT && is_type_name( p->tok.str ) ) {
			Type ignored = parse_type( p );
			expect_op( p, ")" );
			free_temp_type( &ignored );
			return parse_primary( p );
		}
		Expr* e = parse_expr( p );
		expect_op( p, ")" );
		return e;
	}

	if ( p->tok.kind == TK_CHAR ) {
		Expr* e = make_num_expr( p->tok.num );
		nexttok( p );
		return e;
	}

	fprintf( stderr, "Parse primary error at line %d token '%s'\n", p->tok.line, p->tok.str ? p->tok.str : "<null>" );
	exit( 1 );
}

/* binary precedence parser */
static int prec_op( const char* op )
{
	if ( strcmp( op, "||" ) == 0 )
		return 1;
	if ( strcmp( op, "&&" ) == 0 )
		return 2;
	if ( strcmp( op, "==" ) == 0 || strcmp( op, "!=" ) == 0 )
		return 5;
	if ( strcmp( op, "<" ) == 0 || strcmp( op, "<=" ) == 0 || strcmp( op, ">" ) == 0 || strcmp( op, ">=" ) == 0 )
		return 5;
	if ( strcmp( op, "+" ) == 0 || strcmp( op, "-" ) == 0 )
		return 10;
	if ( strcmp( op, "*" ) == 0 || strcmp( op, "/" ) == 0 || strcmp( op, "%" ) == 0 )
		return 20;
	return 0;
}
static int is_binary_op_char( char c )
{
	return c == '+' || c == '-' || c == '*' || c == '/' || c == '%' || c == '<' || c == '>' || c == '=' || c == '&' || c == '|';
}
static Expr* parse_binop_rhs( Parser* p, int expr_prec, Expr* lhs )
{
	while ( p->tok.kind == TK_OP ) {
		const char* opstr = p->tok.str;
		if ( strcmp( opstr, ";" ) == 0 || strcmp( opstr, ")" ) == 0 || strcmp( opstr, "," ) == 0 )
			break;
		int tok_prec = prec_op( opstr );
		if ( tok_prec == 0 || tok_prec < expr_prec )
			break;
		if ( !is_binary_op_char( opstr[0] ) ) {
			fprintf( stderr, "Unexpected operator string while parsing binary op: '%s'\n", opstr );
			break;
		}
		char opbuf[3] = { 0 };
		strncpy( opbuf, opstr, 2 );
		opbuf[2] = 0;
		nexttok( p );
		Expr* rhs = parse_primary( p );
		while ( p->tok.kind == TK_OP && prec_op( p->tok.str ) > tok_prec )
			rhs = parse_binop_rhs( p, prec_op( p->tok.str ), rhs );
		Expr* newlhs = malloc( sizeof( Expr ) );
		newlhs->kind = EX_BINOP;
		newlhs->bin.op = opbuf[0];
		newlhs->bin.a = lhs;
		newlhs->bin.b = rhs;
		lhs = newlhs;
	}
	return lhs;
}
static Expr* parse_expr( Parser* p )
{
	Expr* lhs = parse_primary( p );
	return parse_binop_rhs( p, 0, lhs );
}

static Stmt** parse_block( Parser* p, int* out_cnt )
{
	Stmt** arr = NULL;
	int	   cnt = 0;
	expect_op( p, "{" );
	if ( accept_op( p, "}" ) ) {
		*out_cnt = 0;
		return NULL;
	}
	do {
		Stmt* s = parse_stmt( p );
		if ( s ) {
			arr = realloc( arr, sizeof( Stmt* ) * ( cnt + 1 ) );
			arr[cnt++] = s;
		}
	} while ( !accept_op( p, "}" ) );
	*out_cnt = cnt;
	return arr;
}

Stmt* parse_stmt( Parser* p )
{
	if ( p->tok.kind == TK_IDENT && strcmp( p->tok.str, "struct" ) == 0 ) {
		Type ignored = parse_type( p );
		free_temp_type( &ignored );
		return NULL;
	}

	if ( p->tok.kind == TK_IDENT && is_type_name( p->tok.str ) ) {
		Type vart = parse_type( p );
		return parse_local_declaration_with_type( p, vart );
	}

	if ( is_kw( p, TK_KW_LOCAL ) ) {
		nexttok( p );
		Type vart = parse_type( p );
		return parse_local_declaration_with_type( p, vart );
	}

	if ( is_kw( p, TK_KW_RETURN ) ) {
		Stmt* s = calloc( 1, sizeof( Stmt ) );
		nexttok( p );
		Expr* e = NULL;
		if ( !( p->tok.kind == TK_OP && strcmp( p->tok.str, ";" ) == 0 ) )
			e = parse_expr( p );
		expect_op( p, ";" );
		s->kind = ST_RETURN;
		s->ret = e;
		return s;
	}

	if ( is_kw( p, TK_KW_IF ) ) {
		Stmt* s = calloc( 1, sizeof( Stmt ) );
		nexttok( p );
		expect_op( p, "(" );
		Expr* cond = parse_expr( p );
		expect_op( p, ")" );
		int	   then_cnt;
		Stmt** then_stmts = parse_block( p, &then_cnt );
		Stmt** else_stmts = NULL;
		int	   else_cnt = 0;
		if ( is_kw( p, TK_KW_ELSE ) ) {
			nexttok( p );
			else_stmts = parse_block( p, &else_cnt );
		}
		s->kind = ST_IF;
		s->ifs.cond = cond;
		s->ifs.then_stmts = then_stmts;
		s->ifs.then_cnt = then_cnt;
		s->ifs.else_stmts = else_stmts;
		s->ifs.else_cnt = else_cnt;
		return s;
	}

	if ( is_kw( p, TK_KW_WHILE ) ) {
		Stmt* s = calloc( 1, sizeof( Stmt ) );
		nexttok( p );
		expect_op( p, "(" );
		Expr* cond = parse_expr( p );
		expect_op( p, ")" );
		int	   body_cnt;
		Stmt** body = parse_block( p, &body_cnt );
		s->kind = ST_WHILE;
		s->wh.cond = cond;
		s->wh.body = body;
		s->wh.body_cnt = body_cnt;
		return s;
	}

	if ( p->tok.kind == TK_IDENT ) {
		Expr* lhs = parse_primary( p );
		if ( accept_op( p, "=" ) ) {
			Stmt* s = calloc( 1, sizeof( Stmt ) );
			Expr* rhs = parse_expr( p );
			expect_op( p, ";" );
			s->kind = ST_ASSIGN;
			fill_assignment_target( p, lhs, s );
			s->assign.expr = rhs;
			return s;
		} else {
			Stmt* s = calloc( 1, sizeof( Stmt ) );
			expect_op( p, ";" );
			s->kind = ST_EXPR;
			s->expr = lhs;
			return s;
		}
	}

	if ( p->tok.kind == TK_COMMENT ) {
		const char* src = p->tok.str ? p->tok.str : "";
		size_t		len = strlen( src );
		size_t		bufcap = len * 2 + 32;
		char*		buf = malloc( bufcap );
		size_t		outi = 0;
		if ( len == 0 ) {
			outi += snprintf( buf + outi, bufcap - outi, "; \n" );
		} else {
			size_t i = 0;
			while ( i <= len ) {
				size_t j = i;
				while ( j < len && src[j] != '\n' )
					j++;
				outi += snprintf( buf + outi, bufcap - outi, "; " );
				size_t copylen = j - i;
				if ( copylen > 0 )
					memcpy( buf + outi, src + i, copylen ), outi += copylen;
				buf[outi++] = '\n';
				i = j + 1;
			}
		}
		buf[outi] = '\0';
		Stmt* s = calloc( 1, sizeof( Stmt ) );
		s->kind = ST_ASM;
		s->asm_code = buf;
		nexttok( p );
		return s;
	}

	if ( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, ";" ) == 0 ) {
		nexttok( p );
		return NULL;
	}

	Stmt* s = calloc( 1, sizeof( Stmt ) );
	Expr* e = parse_expr( p );
	expect_op( p, ";" );
	s->kind = ST_EXPR;
	s->expr = e;
	return s;
}

/* rename local calls after include mangling */
static void mangle_calls_in_expr( Expr* e, const char* from, const char* to )
{
	if ( !e )
		return;
	switch ( e->kind ) {
		case EX_CALL:
			if ( strcmp( e->call.name, from ) == 0 ) {
				free( e->call.name );
				e->call.name = strdup( to );
			}
			for ( int i = 0; i < e->call.nargs; i++ )
				mangle_calls_in_expr( e->call.args[i], from, to );
			break;
		case EX_BINOP:
			mangle_calls_in_expr( e->bin.a, from, to );
			mangle_calls_in_expr( e->bin.b, from, to );
			break;
		default:
			break;
	}
}
static void mangle_calls_in_stmt( Stmt* s, const char* from, const char* to )
{
	if ( !s )
		return;
	switch ( s->kind ) {
		case ST_EXPR:
			mangle_calls_in_expr( s->expr, from, to );
			break;
		case ST_ASSIGN:
			mangle_calls_in_expr( s->assign.expr, from, to );
			break;
		case ST_RETURN:
			mangle_calls_in_expr( s->ret, from, to );
			break;
		case ST_IF:
			mangle_calls_in_expr( s->ifs.cond, from, to );
			for ( int i = 0; i < s->ifs.then_cnt; i++ )
				mangle_calls_in_stmt( s->ifs.then_stmts[i], from, to );
			for ( int i = 0; i < s->ifs.else_cnt; i++ )
				mangle_calls_in_stmt( s->ifs.else_stmts[i], from, to );
			break;
		case ST_WHILE:
			mangle_calls_in_expr( s->wh.cond, from, to );
			for ( int i = 0; i < s->wh.body_cnt; i++ )
				mangle_calls_in_stmt( s->wh.body[i], from, to );
			break;
		default:
			break;
	}
}
static void mangle_calls_in_func_body( Func* f, const char* from, const char* to )
{
	for ( int i = 0; i < f->body_cnt; i++ )
		mangle_calls_in_stmt( f->body[i], from, to );
}

/* return non-zero when the next tokens look like a function header */
static int lookahead_is_function( Parser* p )
{
	Parser cp = *p;
	if ( cp.tok.str )
		cp.tok.str = strdup( p->tok.str );

	int is_func = 0;

	if ( cp.tok.kind == TK_IDENT && is_type_name( cp.tok.str ) ) {
		nexttok( &cp );
	}

	if ( cp.tok.kind != TK_IDENT )
		goto done;

	nexttok( &cp );

	if ( !( cp.tok.kind == TK_OP && cp.tok.str && strcmp( cp.tok.str, "(" ) == 0 ) )
		goto done;

	nexttok( &cp );

	int depth = 1;
	while ( cp.tok.kind != TK_EOF && depth > 0 ) {
		if ( cp.tok.kind == TK_OP && cp.tok.str ) {
			if ( strcmp( cp.tok.str, "(" ) == 0 )
				depth++;
			else if ( strcmp( cp.tok.str, ")" ) == 0 )
				depth--;
		}
		nexttok( &cp );
	}

	if ( depth != 0 )
		goto done;

	if ( cp.tok.kind == TK_OP && cp.tok.str && ( strcmp( cp.tok.str, "{" ) == 0 || strcmp( cp.tok.str, ";" ) == 0 ) )
		is_func = 1;

done:
	if ( cp.tok.str )
		free( cp.tok.str );
	return is_func;
}

Func** parse_file_into_funcs_from_buffer( const char* buf, int* out_cnt )
{
	Parser ip;
	lexer_init( &ip.lx, buf );

	ip.tok.str = NULL;
	ip.tok.kind = TK_EOF;
	ip.tok.num = 0;
	ip.tok.line = 1;
	nexttok( &ip );

	Func** local_funcs = NULL;
	int	   local_cnt = 0;

	while ( ip.tok.kind != TK_EOF ) {
		int is_local_decl = 0;
		if ( is_kw( &ip, TK_KW_LOCAL ) ) {
			is_local_decl = 1;
			nexttok( &ip );
		}

		if ( is_kw( &ip, TK_KW_EXTERN ) ) {
			nexttok( &ip );
			if ( !( ip.tok.kind == TK_IDENT && is_type_name( ip.tok.str ) ) ) {
				fprintf( stderr, "Parse error line %d: function return type required\n", ip.tok.line );
				exit( 1 );
			}
			Type   ret = parse_type( &ip );
			char*  fname = consume_ident( &ip );
			char** args = NULL;
			int	   nargs = 0;
			Type*  arg_types = NULL;
			parse_function_params( &ip, &args, &arg_types, &nargs );
			expect_op( &ip, ";" );

			Func* f = new_func( fname, args, arg_types, nargs, NULL, 0, is_local_decl, ret, 1 );
			append_func( &local_funcs, &local_cnt, f );
			continue;
		}

		if ( lookahead_is_function( &ip ) ) {
			if ( !( ip.tok.kind == TK_IDENT && is_type_name( ip.tok.str ) ) ) {
				fprintf( stderr, "Parse error line %d: function return type required\n", ip.tok.line );
				exit( 1 );
			}
			Type   ret = parse_type( &ip );
			char*  fname = consume_ident( &ip );
			char** args = NULL;
			int	   nargs = 0;
			Type*  arg_types = NULL;
			parse_function_params( &ip, &args, &arg_types, &nargs );

			if ( accept_op( &ip, ";" ) ) {
				Func* f = new_func( fname, args, arg_types, nargs, NULL, 0, is_local_decl, ret, 1 );
				append_func( &local_funcs, &local_cnt, f );
				continue;
			}

			int	   body_cnt;
			Stmt** body = parse_block( &ip, &body_cnt );
			Func*  f = new_func( fname, args, arg_types, nargs, body, body_cnt, is_local_decl, ret, 1 );
			append_func( &local_funcs, &local_cnt, f );
			continue;
		}

		Stmt* s = parse_stmt( &ip );
		(void) s;
	}

	token_free( &ip.tok );
	*out_cnt = local_cnt;
	return local_funcs;
}

static int next_include_id = 0;
void	   parse_program( Parser* p )
{
	static Stmt** top_stmts = NULL;
	static int	  top_cnt = 0;

	while ( p->tok.kind != TK_EOF ) {
		if ( is_kw( p, TK_KW_INCLUDE ) ) {
			nexttok( p );
			if ( p->tok.kind != TK_STRING ) {
				fprintf( stderr, "Parse error line %d: include expects a string literal\n", p->tok.line );
				exit( 1 );
			}
			char* inc_fname = strdup( p->tok.str );
			nexttok( p );
			expect_op( p, ";" );
			char*  inc_buf = preprocess_file( inc_fname );
			int	   inc_cnt = 0;
			Func** inc_funcs = parse_file_into_funcs_from_buffer( inc_buf, &inc_cnt );
			for ( int i = 0; i < inc_cnt; i++ ) {
				if ( inc_funcs[i]->src_file )
					free( inc_funcs[i]->src_file );
				inc_funcs[i]->src_file = strdup( inc_fname );
			}
			char mod_pref[64];
			snprintf( mod_pref, sizeof( mod_pref ), "__inc%d_", next_include_id++ );
			for ( int i = 0; i < inc_cnt; i++ ) {
				if ( inc_funcs[i]->is_local ) {
					char* newname = malloc( strlen( mod_pref ) + strlen( inc_funcs[i]->orig_name ) + 1 );
					strcpy( newname, mod_pref );
					strcat( newname, inc_funcs[i]->orig_name );
					free( inc_funcs[i]->name );
					inc_funcs[i]->name = newname;
				}
			}
			for ( int t = 0; t < inc_cnt; t++ ) {
				Func*		target = inc_funcs[t];
				const char* from = target->orig_name;
				const char* to = target->name;
				for ( int j = 0; j < inc_cnt; j++ )
					mangle_calls_in_func_body( inc_funcs[j], from, to );
			}
			for ( int i = 0; i < inc_cnt; i++ )
				append_func( &funcs, &func_cnt, inc_funcs[i] );
			free( inc_funcs );
			free( inc_buf );
			free( inc_fname );
			continue;
		}

		int is_local_decl = 0;
		if ( is_kw( p, TK_KW_LOCAL ) ) {
			is_local_decl = 1;
			nexttok( p );
		}

		if ( is_kw( p, TK_KW_EXTERN ) ) {
			nexttok( p );
			Type   ret = parse_type( p );
			char*  fname = consume_ident( p );
			char** args = NULL;
			int	   nargs = 0;
			Type*  arg_types = NULL;
			parse_function_params( p, &args, &arg_types, &nargs );
			expect_op( p, ";" );
			Func* f = new_func( fname, args, arg_types, nargs, NULL, 0, is_local_decl, ret, 1 );
			append_func( &funcs, &func_cnt, f );
			continue;
		}

		if ( lookahead_is_function( p ) ) {
			Type  ret = parse_type( p );
			char* fname = consume_ident( p );

			for ( int i = 0; i < func_cnt; i++ ) {
				if ( funcs[i] && funcs[i]->name && strcmp( funcs[i]->name, fname ) == 0 ) {
					fprintf( stderr, "Parse error line %d: function '%s' already defined\n", p->tok.line, fname );
					free( fname );
					exit( 1 );
				}
			}

			char** args = NULL;
			int	   nargs = 0;
			Type*  arg_types = NULL;
			parse_function_params( p, &args, &arg_types, &nargs );
			if ( accept_op( p, ";" ) ) {
				Func* f = new_func( fname, args, arg_types, nargs, NULL, 0, is_local_decl, ret, 1 );
				append_func( &funcs, &func_cnt, f );
				continue;
			}

			ensure_unique_param_names( fname, args, arg_types, nargs, p->tok.line );
			int	   body_cnt;
			Stmt** body = parse_block( p, &body_cnt );
			Func*  f = new_func( fname, args, arg_types, nargs, body, body_cnt, is_local_decl, ret, 1 );
			append_func( &funcs, &func_cnt, f );
			continue;
		}

		if ( p->tok.kind == TK_OP && p->tok.str && strcmp( p->tok.str, "}" ) == 0 ) {
			fprintf( stderr, "Unmatched '}' at line %d\n", p->tok.line );
			exit( 1 );
		}

		Stmt* s = parse_stmt( p );
		top_stmts = realloc( top_stmts, sizeof( Stmt* ) * ( top_cnt + 1 ) );
		top_stmts[top_cnt++] = s;
		if ( p->tok.kind == TK_EOF && top_cnt > 0 ) {
			int has_main = 0;
			for ( int i = 0; i < func_cnt; i++ ) {
				if ( strcmp( funcs[i]->name, "main" ) == 0 ) {
					has_main = 1;
					break;
				}
			}

			if ( !has_main ) {
				fprintf( stderr, "Parse error: top-level statements are not allowed without explicit main()\n" );
				exit( 1 );
			}
		}
	}

	token_free( &p->tok );
}

/* validate that every call references a visible function */
static Func* find_local_definition( const char* name )
{
	for ( int i = 0; i < func_cnt; i++ ) {
		Func* f = funcs[i];
		if ( f->is_local && f->orig_name && strcmp( f->orig_name, name ) == 0 )
			return f;
	}
	return NULL;
}
static void check_calls_in_expr( Expr* e, const char* in_func )
{
	if ( !e )
		return;
	switch ( e->kind ) {
		case EX_CALL: {
			int found = 0;
			for ( int i = 0; i < func_cnt; i++ )
				if ( funcs[i] && funcs[i]->name && strcmp( funcs[i]->name, e->call.name ) == 0 ) {
					found = 1;
					break;
				}
			if ( !found ) {
				Func* local = find_local_definition( e->call.name );
				if ( local ) {
					if ( local->src_file )
						fprintf( stderr,
							"Undefined function '%s' called in function '%s': a local function '%s' is defined in included file '%s' and is not "
							"visible outside that module.\n",
							e->call.name, in_func ? in_func : "<unknown>", e->call.name, local->src_file );
					else
						fprintf( stderr,
							"Undefined function '%s' called in function '%s': a local function '%s' is defined in another module and is not visible "
							"here.\n",
							e->call.name, in_func ? in_func : "<unknown>", e->call.name );
				} else
					fprintf( stderr, "Undefined function '%s' called in function '%s'\n", e->call.name, in_func ? in_func : "<unknown>" );
				exit( 1 );
			}
			for ( int i = 0; i < e->call.nargs; i++ )
				check_calls_in_expr( e->call.args[i], in_func );
			break;
		}
		case EX_BINOP:
			check_calls_in_expr( e->bin.a, in_func );
			check_calls_in_expr( e->bin.b, in_func );
			break;
		default:
			break;
	}
}
static void check_calls_in_stmt( Stmt* s, const char* in_func )
{
	if ( !s )
		return;
	switch ( s->kind ) {
		case ST_EXPR:
			check_calls_in_expr( s->expr, in_func );
			break;
		case ST_ASSIGN:
			check_calls_in_expr( s->assign.expr, in_func );
			break;
		case ST_RETURN:
			check_calls_in_expr( s->ret, in_func );
			break;
		case ST_IF:
			check_calls_in_expr( s->ifs.cond, in_func );
			for ( int i = 0; i < s->ifs.then_cnt; i++ )
				check_calls_in_stmt( s->ifs.then_stmts[i], in_func );
			for ( int i = 0; i < s->ifs.else_cnt; i++ )
				check_calls_in_stmt( s->ifs.else_stmts[i], in_func );
			break;
		case ST_WHILE:
			check_calls_in_expr( s->wh.cond, in_func );
			for ( int i = 0; i < s->wh.body_cnt; i++ )
				check_calls_in_stmt( s->wh.body[i], in_func );
			break;
		default:
			break;
	}
}
void check_all_calls( void )
{
	for ( int i = 0; i < func_cnt; i++ ) {
		Func* f = funcs[i];
		if ( !f->body )
			continue;
		for ( int j = 0; j < f->body_cnt; j++ )
			check_calls_in_stmt( f->body[j], f->name );
	}
}

void parser_init( Parser* p )
{
	p->tok.str = NULL;
	p->tok.kind = TK_EOF;
	p->tok.num = 0;
	p->tok.line = 1;
	nexttok( p );
}
