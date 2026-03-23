#include "ast.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern Func** funcs;
extern int	  func_cnt;

static FILE* outf = NULL;
static int	 gen_id = 0;
static int	 lbl_id = 0;

/* tiny utils */
static char* newtemp( void )
{
	char b[32];
	snprintf( b, sizeof b, "%%t%d", gen_id++ );
	return strdup( b );
}
static char* newlbl( const char* p )
{
	char b[64];
	snprintf( b, sizeof b, "%s_%d", p, lbl_id++ );
	return strdup( b );
}
static void emit( const char* fmt, ... )
{
	if ( !outf )
		return;
	va_list ap;
	va_start( ap, fmt );
	vfprintf( outf, fmt, ap );
	fprintf( outf, "\n" );
	va_end( ap );
}
static char* append_arglist( char* base, const char* part )
{
	size_t la = base ? strlen( base ) : 0;
	size_t lb = part ? strlen( part ) : 0;
	char*  n = realloc( base, la + lb + 3 );
	if ( !n ) {
		perror( "realloc" );
		exit( 1 );
	}
	if ( la == 0 )
		n[0] = '\0';
	if ( la )
		strcat( n, ", " );
	strcat( n, part );
	return n;
}
static char* detect_host_triple( void )
{
	FILE* p = popen( "clang -dumpmachine 2>/dev/null", "r" );
	if ( !p )
		return strdup( "x86_64-pc-linux-gnu" );
	char buf[256];
	if ( !fgets( buf, sizeof buf, p ) ) {
		pclose( p );
		return strdup( "x86_64-pc-linux-gnu" );
	}
	pclose( p );
	char* nl = strchr( buf, '\n' );
	if ( nl )
		*nl = 0;
	return strdup( buf[0] ? buf : "x86_64-pc-linux-gnu" );
}

/* string constants */
typedef struct
{
	char* lbl;
	char* val;
	int	  len;
} StrConst;
static StrConst* sct = NULL;
static int		 sct_n = 0, sct_cap = 0;
static void		 ensure_sct( void )
{
	if ( sct_n >= sct_cap ) {
		sct_cap = sct_cap ? sct_cap * 2 : 8;
		sct = realloc( sct, sizeof *sct * sct_cap );
		if ( !sct ) {
			perror( "realloc" );
			exit( 1 );
		}
	}
}
static char* register_string( const char* v )
{
	if ( !v )
		v = "";
	for ( int i = 0; i < sct_n; i++ )
		if ( sct[i].val && strcmp( sct[i].val, v ) == 0 )
			return sct[i].lbl;
	ensure_sct();
	char tmp[64];
	snprintf( tmp, sizeof tmp, ".str.%d", sct_n );
	sct[sct_n].lbl = strdup( tmp );
	sct[sct_n].val = strdup( v );
	sct[sct_n].len = (int) strlen( v ) + 1;
	sct_n++;
	return sct[sct_n - 1].lbl;
}

typedef struct
{
	char* name;
	char* alloca_name;
	int	  ty;
} LocSym;
static LocSym* locals = NULL;
static int	   loc_n = 0, loc_cap = 0;
static void	   ensure_loc( void )
{
	if ( loc_n >= loc_cap ) {
		loc_cap = loc_cap ? loc_cap * 2 : 16;
		locals = realloc( locals, sizeof *locals * loc_cap );
		if ( !locals ) {
			perror( "realloc" );
			exit( 1 );
		}
	}
}
static char* find_alloca( const char* name )
{
	for ( int i = 0; i < loc_n; i++ )
		if ( locals[i].name && strcmp( locals[i].name, name ) == 0 )
			return locals[i].alloca_name;
	return NULL;
}
static int find_type( const char* name )
{
	for ( int i = 0; i < loc_n; i++ )
		if ( locals[i].name && strcmp( locals[i].name, name ) == 0 )
			return locals[i].ty;
	return 0;
}
static void add_mapping( const char* name, const char* alloca_name, int ty )
{
	ensure_loc();
	locals[loc_n].name = strdup( name );
	locals[loc_n].alloca_name = strdup( alloca_name );
	locals[loc_n].ty = ty;
	loc_n++;
}
static char* sanitize_name( const char* name )
{
	if ( !name )
		return strdup( "unnamed" );
	size_t len = strlen( name );
	char*  buf = malloc( len * 3 + 1 ); // worst case: non-alnum -> _XX
	char*  p = buf;
	for ( size_t i = 0; i < len; i++ ) {
		unsigned char c = (unsigned char) name[i];
		if ( ( c >= 'a' && c <= 'z' ) || ( c >= 'A' && c <= 'Z' ) || ( c >= '0' && c <= '9' ) || c == '_' ) {
			*p++ = c;
		} else {
			p += sprintf( p, "_%02X", c );
		}
	}
	*p = '\0';
	return buf;
}
static char* alloc_local( const char* name, int ty )
{
	if ( find_alloca( name ) ) {
		fprintf( stderr, "Local redefinition: %s\n", name );
		exit( 1 );
	}
	ensure_loc();
	char  buf[128];
	char* safe_name = sanitize_name( name );
	snprintf( buf, sizeof buf, "%%_%s.ptr%d", safe_name, loc_n );
	free( safe_name );

	locals[loc_n].name = strdup( name );
	locals[loc_n].alloca_name = strdup( buf );
	locals[loc_n].ty = ty;
	loc_n++;
	return strdup( buf );
}
static void free_locals( void )
{
	if ( !locals )
		return;
	for ( int i = 0; i < loc_n; i++ ) {
		free( locals[i].name );
		free( locals[i].alloca_name );
	}
	free( locals );
	locals = NULL;
	loc_n = loc_cap = 0;
}

/* store full Type for locals so codegen can know struct names/ptr levels etc */
typedef struct
{
	char* name;
	Type  t;
} LocType;
static LocType* loc_types = NULL;
static int		loc_types_n = 0, loc_types_cap = 0;
static void		ensure_loc_types( void )
{
	if ( loc_types_n >= loc_types_cap ) {
		loc_types_cap = loc_types_cap ? loc_types_cap * 2 : 16;
		loc_types = realloc( loc_types, sizeof *loc_types * loc_types_cap );
		if ( !loc_types ) {
			perror( "realloc" );
			exit( 1 );
		}
	}
}
static void add_local_type( const char* name, Type t )
{
	ensure_loc_types();
	loc_types[loc_types_n].name = strdup( name );
	loc_types[loc_types_n].t = t; /* shallow copy */
	loc_types_n++;
}
static Type* find_local_type( const char* name )
{
	for ( int i = 0; i < loc_types_n; i++ )
		if ( loc_types[i].name && strcmp( loc_types[i].name, name ) == 0 )
			return &loc_types[i].t;
	return NULL;
}

/* helpers to compute sizes/offsets for struct fields (simple model) */
static int ptr_size_in_bytes( void )
{
	return (int) sizeof( void* );
}

static StructDef* lookup_struct_by_name( const char* name );

static int size_of_type_internal( Type* t )
{
	if ( !t )
		return ptr_size_in_bytes();
	if ( t->ptr_level > 0 )
		return ptr_size_in_bytes();

	switch ( t->base ) {
		case TY_I32:
			return 4;
		case TY_I64:
			return 8;
		case TY_I8:
			return 1;
		case TY_CHAR:
			return 1;
		case TY_USIZE:
			return ptr_size_in_bytes();
		case TY_STRUCT: {
			if ( !t->struct_name )
				return ptr_size_in_bytes();
			StructDef* sd = lookup_struct_by_name( t->struct_name );
			if ( !sd )
				return ptr_size_in_bytes();
			int offset = 0;
			int ps = ptr_size_in_bytes();
			for ( int i = 0; i < sd->field_cnt; i++ ) {
				Type fty = sd->fields[i]->type;
				int	 fsz = size_of_type_internal( &fty );
				int	 align = ( fsz > ps ) ? ps : fsz;
				if ( align <= 0 )
					align = 1;
				int rem = offset % align;
				if ( rem )
					offset += ( align - rem );
				offset += fsz;
			}
			int rem = offset % ps;
			if ( rem )
				offset += ( ps - rem );
			return offset;
		}
		default:
			return 4;
	}
}

/* helper: map AST Type -> storage code & llvm type string.
   returns storage code: 0=i32,1=i64,2=i8*,3=i8,4=struct */
static int type_to_storage_and_llvm( Type t, char* buf, size_t bufsz )
{
	if ( t.ptr_level > 0 ) {
		snprintf( buf, bufsz, "i8*" );
		return 2;
	}
	switch ( t.base ) {
		case TY_VOID:
			snprintf( buf, bufsz, "void" );
			return -1;
		case TY_I32:
			snprintf( buf, bufsz, "i32" );
			return 0;
		case TY_I64:
			snprintf( buf, bufsz, "i64" );
			return 1;
		case TY_I8:
			snprintf( buf, bufsz, "i8" );
			return 3;
		case TY_CHAR:
			snprintf( buf, bufsz, "i8" );
			return 3;
		case TY_STRUCT: {
			Type tmp = t; /* shallow copy */
			int	 size = size_of_type_internal( &tmp );
			if ( size <= 0 ) {
				/* fallback to pointer-size */
				size = ptr_size_in_bytes();
			}
			snprintf( buf, bufsz, "[%d x i8]", size );
			return 4;
		}
		case TY_USIZE:
			snprintf( buf, bufsz, "i64" );
			return 1;
		default:
			snprintf( buf, bufsz, "i32" );
			return 0;
	}
}

extern StructDef** g_structs;
extern int		   g_struct_cnt;

static StructDef* lookup_struct_by_name( const char* name )
{
	if ( !name )
		return NULL;
	for ( int i = 0; i < g_struct_cnt; i++ ) {
		if ( g_structs[i] && g_structs[i]->name && strcmp( g_structs[i]->name, name ) == 0 )
			return g_structs[i];
	}
	return NULL;
}

/* find field offset (bytes) inside struct; returns -1 on error */
static int find_field_offset( const char* struct_name, const char* field_name )
{
	if ( !struct_name || !field_name )
		return -1;
	StructDef* sd = lookup_struct_by_name( struct_name );
	if ( !sd )
		return -1;
	int offset = 0;
	int ps = ptr_size_in_bytes();
	for ( int i = 0; i < sd->field_cnt; i++ ) {
		StructField* f = sd->fields[i];
		int			 fsz = size_of_type_internal( &f->type );
		int			 align = ( fsz > ps ) ? ps : fsz;
		if ( align <= 0 )
			align = 1;
		int rem = offset % align;
		if ( rem )
			offset += ( align - rem );
		if ( strcmp( f->name, field_name ) == 0 ) {
			return offset;
		}
		offset += fsz;
	}
	return -1;
}

/* forward */
static char* gen_expr( Expr* e, int* is_ptr );
static Func* find_func_by_name( const char* name )
{
	for ( int i = 0; i < func_cnt; i++ )
		if ( funcs[i] && funcs[i]->name && strcmp( funcs[i]->name, name ) == 0 )
			return funcs[i];
	return NULL;
}

/* conv wants: 1=i8*, 2=i64, 0=i32 (already above) */
static char* conv( const char* name, int src_kind, int want_kind )
{
	if ( src_kind == want_kind )
		return strdup( name );

	char* tmp = newtemp();

	if ( want_kind == 1 ) { // want i8*
		if ( src_kind == 2 )
			emit( "  %s = inttoptr i64 %s to i8*", tmp, name );
		else if ( src_kind == 0 )
			emit( "  %s = inttoptr i32 %s to i8*", tmp, name );
		else if ( src_kind == 3 ) { /* byte -> sign-extend then inttoptr */
			char* t1 = newtemp();
			emit( "  %s = sext i8 %s to i32", t1, name );
			emit( "  %s = inttoptr i32 %s to i8*", tmp, t1 );
			free( t1 );
		} else // src_kind == 1
			return strdup( name );
		return tmp;
	} else if ( want_kind == 2 ) { // want i64
		if ( src_kind == 0 )
			emit( "  %s = sext i32 %s to i64", tmp, name );
		else if ( src_kind == 1 )
			emit( "  %s = ptrtoint i8* %s to i64", tmp, name );
		else if ( src_kind == 3 )
			emit( "  %s = sext i8 %s to i64", tmp, name );
		return tmp;
	} else { // want i32
		if ( src_kind == 2 )
			emit( "  %s = trunc i64 %s to i32", tmp, name );
		else if ( src_kind == 1 ) {
			char* t2 = newtemp();
			emit( "  %s = ptrtoint i8* %s to i64", tmp, name );
			emit( "  %s = trunc i64 %s to i32", t2, tmp );
			free( tmp );
			tmp = t2;
		} else if ( src_kind == 3 ) {
			/* i8 already, sext to i32 */
			emit( "  %s = sext i8 %s to i32", tmp, name );
		} else {
			/* already i32 */
			return strdup( name );
		}
		return tmp;
	}
}

/* get expression type (best-effort): returns Type by value (shallow). On unknown returns default int32. */
static Type get_expr_type( Expr* e )
{
	Type def;
	memset( &def, 0, sizeof( def ) );
	def.base = TY_I32;
	def.ptr_level = 0;

	if ( !e )
		return def;

	if ( e->kind == EX_VAR ) {
		Type* t = find_local_type( e->var );
		if ( t )
			return *t;
		return def;
	}

	if ( e->kind == EX_INDEX ) {
		/* member access: if idx is string -> field type */
		if ( e->index.idx && e->index.idx->kind == EX_STR ) {
			const char* field = e->index.idx->var;
			Type		base = get_expr_type( e->index.target );
			if ( base.ptr_level > 0 && base.base == TY_STRUCT ) {
				StructDef* sd = lookup_struct_by_name( base.struct_name );
				if ( !sd )
					return def;
				for ( int i = 0; i < sd->field_cnt; i++ )
					if ( strcmp( sd->fields[i]->name, field ) == 0 )
						return sd->fields[i]->type;
				return def;
			} else if ( base.base == TY_STRUCT ) {
				StructDef* sd = lookup_struct_by_name( base.struct_name );
				if ( !sd )
					return def;
				for ( int i = 0; i < sd->field_cnt; i++ )
					if ( strcmp( sd->fields[i]->name, field ) == 0 )
						return sd->fields[i]->type;
				return def;
			}
		} else {
			def.base = TY_I8;
			return def;
		}
	}

	if ( e->kind == EX_CALL ) {
		Func* f = find_func_by_name( e->call.name );
		if ( f && f->has_types )
			return f->ret_type;
	}

	if ( e->kind == EX_UNARY ) {
		if ( e->unary.op == '&' ) {
			Type t = get_expr_type( e->unary.operand );
			t.ptr_level++;
			return t;
		} else if ( e->unary.op == '*' ) {
			Type t = get_expr_type( e->unary.operand );
			if ( t.ptr_level > 0 )
				t.ptr_level--;
			return t;
		}
	}

	return def;
}

static char* gen_lvalue_address( Expr* e )
{
	if ( !e )
		return strdup( "0" );
	if ( e->kind == EX_UNARY && e->unary.op == '*' ) {
		int	  k = 0;
		char* v = gen_expr( e->unary.operand, &k );
		char* ptr = v;
		if ( k != 1 ) {
			ptr = conv( v, k, 1 );
			free( v );
		}
		return ptr;
	}
	if ( e->kind == EX_VAR ) {
		const char* v = e->var;
		char*		an = find_alloca( v );
		if ( !an ) {
			return strdup( "0" );
		}
		int stor = find_type( v );
		/* try to detect a full Type entry for this local */
		Type* lt = find_local_type( v );

		if ( stor == 2 ) {
			char* tmp = newtemp();
			emit( "  %s = load i8*, i8** %s", tmp, an );
			return tmp;
		} else if ( stor == 4 || ( lt && lt->base == TY_STRUCT && lt->ptr_level == 0 ) ) {
			int size = ptr_size_in_bytes();
			if ( lt )
				size = size_of_type_internal( lt );
			if ( size <= 0 )
				size = ptr_size_in_bytes();
			char* tmp = newtemp();
			emit( "  %s = bitcast [%d x i8]* %s to i8*", tmp, size, an );
			return tmp;
		} else {
			char*		tmp = newtemp();
			const char* from = ( stor == 1 ) ? "i64*" : "i32*";
			emit( "  %s = bitcast %s %s to i8*", tmp, from, an );
			return tmp;
		}
	}

	if ( e->kind == EX_INDEX ) {
		if ( e->index.idx && e->index.idx->kind == EX_STR ) {
			Expr* base = e->index.target;
			char* base_addr = gen_lvalue_address( base ); /* i8* */
			if ( !base_addr )
				return strdup( "0" );
			Type		base_type = get_expr_type( base );
			const char* struct_name = NULL;
			if ( base_type.base == TY_STRUCT )
				struct_name = base_type.struct_name;
			else if ( base_type.ptr_level > 0 && base_type.base == TY_STRUCT )
				struct_name = base_type.struct_name;
			else {
				if ( base->kind == EX_VAR ) {
					Type* lt = find_local_type( base->var );
					if ( lt && lt->base == TY_STRUCT )
						struct_name = lt->struct_name;
				}
			}
			int offset = -1;
			if ( struct_name )
				offset = find_field_offset( struct_name, e->index.idx->var );
			if ( offset < 0 )
				offset = 0;
			char* gep = newtemp();
			emit( "  %s = getelementptr i8, i8* %s, i64 %d", gep, base_addr, offset );
			free( base_addr );
			return gep;
		} else {
			Expr* base = e->index.target;
			int	  bk = 0;
			char* basev = gen_expr( base, &bk );
			char* baseptr = basev;
			if ( bk != 1 ) {
				char* convb = conv( basev, bk, 1 );
				free( basev );
				baseptr = convb;
			}
			int	  ik = 0;
			char* idxv = gen_expr( e->index.idx, &ik );
			char* idx64 = conv( idxv, ik, 2 );
			free( idxv );
			char* gep = newtemp();
			emit( "  %s = getelementptr i8, i8* %s, i64 %s", gep, baseptr, idx64 );
			free( baseptr );
			free( idx64 );
			return gep;
		}
	}
	return strdup( "0" );
}

/* gen_call: uses declared function types if available; supports varargs properly.
   No hardcoded known_sig; default: declare as vararg if extern lacks types. */
static char* gen_call( Expr* callexpr, int* is_ptr )
{
	if ( !callexpr ) {
		*is_ptr = 0;
		return strdup( "0" );
	}
	int	   n = callexpr->call.nargs;
	char** vals = n ? malloc( sizeof( char* ) * n ) : NULL;
	int*   kinds = n ? malloc( sizeof( int ) * n ) : NULL;
	for ( int i = 0; i < n; i++ )
		vals[i] = gen_expr( callexpr->call.args[i], &kinds[i] );
	const char* fname = callexpr->call.name;
	Func*		fdecl = find_func_by_name( fname );

	/* prepare arglist and argtypes-only string for fallback scenarios */
	char* arglist = NULL;
	char* argtypes_only = NULL;

	/* if function declared with types, handle fixed params + varargs */
	if ( fdecl && fdecl->has_types ) {
		/* locate vararg position, if any */
		int vararg_pos = -1;
		for ( int j = 0; j < fdecl->nargs; j++ ) {
			if ( fdecl->arg_types[j].base == TY_VARARG ) {
				vararg_pos = j;
				break;
			}
		}
		/* build arglist respecting fixed parameter types; varargs left to runtime kinds */
		for ( int i = 0; i < n; i++ ) {
			char buf[512] = { 0 };
			char typestr[64] = { 0 };
			if ( vararg_pos == -1 && i < fdecl->nargs ) {
				/* normal fixed param */
				Type want = fdecl->arg_types[i];
				int	 want_kind = ( want.ptr_level > 0 ) ? 1 : ( want.base == TY_I64 ? 2 : ( want.base == TY_I8 || want.base == TY_CHAR ? 3 : 0 ) );
				if ( want_kind == 1 ) {
					char* v = conv( vals[i], kinds[i], 1 );
					snprintf( buf, sizeof buf, "i8* %s", v );
					snprintf( typestr, sizeof typestr, "i8*" );
					if ( kinds[i] != 1 )
						free( v );
				} else if ( want_kind == 2 ) {
					char* v = conv( vals[i], kinds[i], 2 );
					snprintf( buf, sizeof buf, "i64 %s", v );
					snprintf( typestr, sizeof typestr, "i64" );
					if ( kinds[i] != 2 )
						free( v );
				} else if ( want_kind == 3 ) {
					/* char/byte -> pass promoted to i32 in expressions but if declared as i8 parameter, keep i8 */
					char* v = vals[i];
					if ( kinds[i] != 3 ) {
						/* convert to i32 then trunc later - but for parameter passing keep as i32? keep simplicity: convert to i32 and let store handle */
						char* convv = conv( vals[i], kinds[i], 0 );
						snprintf( buf, sizeof buf, "i32 %s", convv );
						snprintf( typestr, sizeof typestr, "i32" );
						free( convv );
					} else {
						snprintf( buf, sizeof buf, "i8 %s", v );
						snprintf( typestr, sizeof typestr, "i8" );
					}
				} else {
					char* v = conv( vals[i], kinds[i], 0 );
					snprintf( buf, sizeof buf, "i32 %s", v );
					snprintf( typestr, sizeof typestr, "i32" );
					free( v );
				}
			} else if ( vararg_pos != -1 && i < vararg_pos ) {
				/* fixed param before vararg */
				Type want = fdecl->arg_types[i];
				int	 want_kind = ( want.ptr_level > 0 ) ? 1 : ( want.base == TY_I64 ? 2 : ( want.base == TY_I8 || want.base == TY_CHAR ? 3 : 0 ) );
				if ( want_kind == 1 ) {
					char* v = conv( vals[i], kinds[i], 1 );
					snprintf( buf, sizeof buf, "i8* %s", v );
					snprintf( typestr, sizeof typestr, "i8*" );
					if ( kinds[i] != 1 )
						free( v );
				} else if ( want_kind == 2 ) {
					char* v = conv( vals[i], kinds[i], 2 );
					snprintf( buf, sizeof buf, "i64 %s", v );
					snprintf( typestr, sizeof typestr, "i64" );
					if ( kinds[i] != 2 )
						free( v );
				} else if ( want_kind == 3 ) {
					char* v = vals[i];
					if ( kinds[i] != 3 ) {
						char* convv = conv( vals[i], kinds[i], 0 );
						snprintf( buf, sizeof buf, "i32 %s", convv );
						snprintf( typestr, sizeof typestr, "i32" );
						free( convv );
					} else {
						snprintf( buf, sizeof buf, "i8 %s", v );
						snprintf( typestr, sizeof typestr, "i8" );
					}
				} else {
					char* v = conv( vals[i], kinds[i], 0 );
					snprintf( buf, sizeof buf, "i32 %s", v );
					snprintf( typestr, sizeof typestr, "i32" );
					free( v );
				}
			} else {
				/* varargs part (or function has fewer declared args than called) - use actual runtime kinds */
				if ( kinds[i] == 1 ) {
					snprintf( buf, sizeof buf, "i8* %s", vals[i] );
					snprintf( typestr, sizeof typestr, "i8*" );
				} else if ( kinds[i] == 2 ) {
					snprintf( buf, sizeof buf, "i64 %s", vals[i] );
					snprintf( typestr, sizeof typestr, "i64" );
				} else if ( kinds[i] == 3 ) {
					/* char promoted to i32 in varargs typically */
					char* convv = conv( vals[i], kinds[i], 0 );
					snprintf( buf, sizeof buf, "i32 %s", convv );
					snprintf( typestr, sizeof typestr, "i32" );
					free( convv );
				} else {
					/* default integer to i32 */
					char* v = conv( vals[i], kinds[i], 0 );
					snprintf( buf, sizeof buf, "i32 %s", v );
					snprintf( typestr, sizeof typestr, "i32" );
					free( v );
				}
			}
			arglist = append_arglist( arglist, buf );
			argtypes_only = append_arglist( argtypes_only, typestr );
		}
		/* build function param-type-signature string (fixed params only) */
		char* paramsig = NULL;
		int	  fixed_count = ( vararg_pos == -1 ) ? fdecl->nargs : vararg_pos;
		for ( int j = 0; j < fixed_count; j++ ) {
			char pbuf[64];
			Type want = fdecl->arg_types[j];
			int	 want_kind = ( want.ptr_level > 0 ) ? 1 : ( want.base == TY_I64 ? 2 : ( want.base == TY_I8 || want.base == TY_CHAR ? 3 : 0 ) );
			if ( want_kind == 1 )
				snprintf( pbuf, sizeof pbuf, "i8*" );
			else if ( want_kind == 2 )
				snprintf( pbuf, sizeof pbuf, "i64" );
			else if ( want_kind == 3 )
				snprintf( pbuf, sizeof pbuf, "i32" ); /* promote char param to i32 signature for varargs compatibility */
			else
				snprintf( pbuf, sizeof pbuf, "i32" );
			paramsig = append_arglist( paramsig, pbuf );
		}
		if ( vararg_pos != -1 ) {
			/* append varargs marker */
			if ( paramsig && paramsig[0] != '\0' )
				; /* keep comma separation when we use in emit */
				  /* we'll later use "(<params>, ...)" */
		}

		/* determine return handling */
		if ( fdecl->ret_type.base == TY_VOID && fdecl->ret_type.ptr_level == 0 ) {
			/* void return */
			if ( paramsig && vararg_pos != -1 )
				emit( "  call void (%s, ...) @%s(%s)", paramsig, fname, arglist ? arglist : "" );
			else if ( paramsig )
				emit( "  call void (%s) @%s(%s)", paramsig, fname, arglist ? arglist : "" );
			else
				emit( "  call void @%s(%s)", fname, arglist ? arglist : "" );
			for ( int i = 0; i < n; i++ )
				free( vals[i] );
			free( vals );
			free( kinds );
			free( arglist );
			free( argtypes_only );
			if ( paramsig )
				free( paramsig );
			*is_ptr = 0;
			return strdup( "0" );
		} else {
			/* non-void */
			char  retbuf[64];
			int	  retstorage = type_to_storage_and_llvm( fdecl->ret_type, retbuf, sizeof retbuf );
			char* r = newtemp();
			if ( retstorage == 1 ) {
				if ( paramsig && vararg_pos != -1 )
					emit( "  %s = call i64 (%s, ...) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else if ( paramsig )
					emit( "  %s = call i64 (%s) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else
					emit( "  %s = call i64 @%s(%s)", r, fname, arglist ? arglist : "" );
				*is_ptr = 2;
			} else if ( retstorage == 2 ) { /* pointer? ptr_level>0 yields i8* above, but type_to_storage returned 2 only for ptr -> handled as 2 */
				if ( paramsig && vararg_pos != -1 )
					emit( "  %s = call i8* (%s, ...) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else if ( paramsig )
					emit( "  %s = call i8* (%s) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else
					emit( "  %s = call i8* @%s(%s)", r, fname, arglist ? arglist : "" );
				*is_ptr = 1;
			} else {
				/* return i32 default */
				if ( paramsig && vararg_pos != -1 )
					emit( "  %s = call i32 (%s, ...) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else if ( paramsig )
					emit( "  %s = call i32 (%s) @%s(%s)", r, paramsig, fname, arglist ? arglist : "" );
				else
					emit( "  %s = call i32 @%s(%s)", r, fname, arglist ? arglist : "" );
				*is_ptr = 0;
			}
			for ( int i = 0; i < n; i++ )
				free( vals[i] );
			free( vals );
			free( kinds );
			free( arglist );
			free( argtypes_only );
			if ( paramsig )
				free( paramsig );
			return r;
		}
	}

	/* No function declaration with types - fallback: use actual runtime kinds to build param list and call */
	for ( int i = 0; i < n; i++ ) {
		char buf[256] = { 0 };
		char typestr[64] = { 0 };
		if ( kinds[i] == 1 ) {
			snprintf( buf, sizeof buf, "i8* %s", vals[i] );
			snprintf( typestr, sizeof typestr, "i8*" );
		} else if ( kinds[i] == 2 ) {
			snprintf( buf, sizeof buf, "i64 %s", vals[i] );
			snprintf( typestr, sizeof typestr, "i64" );
		} else if ( kinds[i] == 3 ) {
			/* byte -> promote to i32 for varargs/default */
			char* convv = conv( vals[i], kinds[i], 0 );
			snprintf( buf, sizeof buf, "i32 %s", convv );
			snprintf( typestr, sizeof typestr, "i32" );
			free( convv );
		} else {
			char* v = conv( vals[i], kinds[i], 0 );
			snprintf( buf, sizeof buf, "i32 %s", v );
			snprintf( typestr, sizeof typestr, "i32" );
			free( v );
		}
		arglist = append_arglist( arglist, buf );
		argtypes_only = append_arglist( argtypes_only, typestr );
	}

	/* emit call using explicit function type composed from argtypes_only */
	char* functypesig = NULL;
	if ( argtypes_only && argtypes_only[0] != '\0' )
		functypesig = strdup( argtypes_only );
	/* default return i32 */
	char* res = newtemp();
	if ( functypesig && functypesig[0] != '\0' )
		emit( "  %s = call i32 (%s) @%s(%s)", res, functypesig, fname, arglist ? arglist : "" );
	else
		emit( "  %s = call i32 @%s(%s)", res, fname, arglist ? arglist : "" );

	for ( int i = 0; i < n; i++ )
		free( vals[i] );
	free( vals );
	free( kinds );
	free( arglist );
	free( argtypes_only );
	if ( functypesig )
		free( functypesig );
	*is_ptr = 0;
	return res;
}

/* gen_expr (loads depend on storage ty mapping: 2->i8*,1->i64,0->i32,3->i8) */
static char* gen_expr( Expr* e, int* kind )
{
	if ( !e ) {
		*kind = 0;
		return strdup( "0" );
	}
	switch ( e->kind ) {
		case EX_NUM: {
			char b[32];
			snprintf( b, sizeof b, "%lld", (long long) e->num );
			*kind = 0;
			return strdup( b );
		}
		case EX_STR: {
			char* lbl = register_string( e->var );
			int	  len = 0;
			for ( int i = 0; i < sct_n; i++ )
				if ( sct[i].lbl && strcmp( sct[i].lbl, lbl ) == 0 ) {
					len = sct[i].len;
					break;
				}
			if ( !len )
				len = (int) strlen( e->var ) + 1;
			char* tmp = newtemp();
			emit( "  %s = getelementptr inbounds [%d x i8], [%d x i8]* @%s, i32 0, i32 0", tmp, len, len, lbl );
			*kind = 1;
			return tmp;
		}
		case EX_VAR: {
			char* a = find_alloca( e->var );
			if ( !a ) {
				*kind = 0;
				return strdup( "0" );
			}
			char* tmp = newtemp();
			int	  ty = find_type( e->var );
			if ( ty == 2 ) {
				emit( "  %s = load i8*, i8** %s", tmp, a );
				*kind = 1;
			} else if ( ty == 1 ) {
				emit( "  %s = load i64, i64* %s", tmp, a );
				*kind = 2;
			} else if ( ty == 3 ) {
				char* loadb = newtemp();
				emit( "  %s = load i8, i8* %s", loadb, a );
				char* sext = newtemp();
				emit( "  %s = sext i8 %s to i32", sext, loadb );
				free( loadb );
				*kind = 0;
				return sext;
			} else {
				emit( "  %s = load i32, i32* %s", tmp, a );
				*kind = 0;
			}
			return tmp;
		}
		case EX_CALL:
			return gen_call( e, kind );
		case EX_BINOP: {
			int	  lk = 0, rk = 0;
			char* L = gen_expr( e->bin.a, &lk );
			char* R = gen_expr( e->bin.b, &rk );
			char* Ln = conv( L, lk, 0 );
			if ( Ln != L ) {
				free( L );
				L = Ln;
			}
			char* Rn = conv( R, rk, 0 );
			if ( Rn != R ) {
				free( R );
				R = Rn;
			}
			char* out = newtemp();
			if ( e->bin.op == '+' )
				emit( "  %s = add i32 %s, %s", out, L, R );
			else if ( e->bin.op == '-' )
				emit( "  %s = sub i32 %s, %s", out, L, R );
			else if ( e->bin.op == '*' )
				emit( "  %s = mul i32 %s, %s", out, L, R );
			else if ( e->bin.op == '/' )
				emit( "  %s = sdiv i32 %s, %s", out, L, R );
			else
				emit( "  %s = sub i32 %s, %s", out, L, R );
			free( L );
			free( R );
			*kind = 0;
			return out;
		}
		case EX_INDEX: {
			if ( e->index.idx && e->index.idx->kind == EX_STR ) {
				char* addr = gen_lvalue_address( e );
				Type  ftype = get_expr_type( e );
				char  llvmty[64];
				int	  wantstor = type_to_storage_and_llvm( ftype, llvmty, sizeof llvmty );
				char  addrcast[128];
				snprintf( addrcast, sizeof addrcast, "%s*", llvmty );
				char* addr_tmp = newtemp();
				emit( "  %s = bitcast i8* %s to %s", addr_tmp, addr, addrcast );
				char* valtmp = newtemp();
				if ( wantstor == 1 ) {
					emit( "  %s = load i64, i64* %s", valtmp, addr_tmp );
					*kind = 2;
				} else if ( wantstor == 2 ) {
					emit( "  %s = load i8*, i8** %s", valtmp, addr_tmp );
					*kind = 1;
				} else if ( wantstor == 3 ) {
					char* loadb = newtemp();
					emit( "  %s = load i8, i8* %s", loadb, addr_tmp );
					char* sext = newtemp();
					emit( "  %s = sext i8 %s to i32", sext, loadb );
					free( loadb );
					free( addr );
					free( addr_tmp );
					*kind = 0;
					return sext;
				} else {
					emit( "  %s = load i32, i32* %s", valtmp, addr_tmp );
					*kind = 0;
				}
				free( addr );
				free( addr_tmp );
				return valtmp;
			} else {
				char* addr = gen_lvalue_address( e );
				char* tmp = newtemp();
				emit( "  %s = load i8, i8* %s", tmp, addr );
				char* out = newtemp();
				emit( "  %s = sext i8 %s to i32", out, tmp );
				free( tmp );
				free( addr );
				*kind = 0;
				return out;
			}
		}
		case EX_UNARY: {
			char  op = e->unary.op;
			Expr* operand = e->unary.operand;
			if ( op == '&' ) {
				char* addr = gen_lvalue_address( operand );
				*kind = 1;
				return addr;
			} else if ( op == '*' ) {
				int	  pk = 0;
				char* p = gen_expr( operand, &pk );
				char* ptr = p;
				if ( pk != 1 ) {
					ptr = conv( p, pk, 1 );
					free( p );
				}

				Type t = get_expr_type( operand );
				if ( t.ptr_level > 0 )
					t.ptr_level--;
				else {
					t.base = TY_I8;
					t.ptr_level = 0;
				}

				char llvmty[64];
				int	 stor = type_to_storage_and_llvm( t, llvmty, sizeof llvmty );
				if ( stor == 4 ) {
					/* struct value is not supported as a first-class expression; return address */
					*kind = 1;
					return ptr;
				}

				char ptrty[128];
				snprintf( ptrty, sizeof ptrty, "%s*", llvmty );
				char* addrcast = newtemp();
				emit( "  %s = bitcast i8* %s to %s", addrcast, ptr, ptrty );

				if ( stor == 1 ) {
					char* out = newtemp();
					emit( "  %s = load i64, i64* %s", out, addrcast );
					free( addrcast );
					free( ptr );
					*kind = 2;
					return out;
				} else if ( stor == 2 ) {
					char* out = newtemp();
					emit( "  %s = load i8*, i8** %s", out, addrcast );
					free( addrcast );
					free( ptr );
					*kind = 1;
					return out;
				} else if ( stor == 3 ) {
					char* loadb = newtemp();
					emit( "  %s = load i8, i8* %s", loadb, addrcast );
					char* sext = newtemp();
					emit( "  %s = sext i8 %s to i32", sext, loadb );
					free( loadb );
					free( addrcast );
					free( ptr );
					*kind = 0;
					return sext;
				} else {
					char* out = newtemp();
					emit( "  %s = load i32, i32* %s", out, addrcast );
					free( addrcast );
					free( ptr );
					*kind = 0;
					return out;
				}
			} else if ( op == '!' ) {
				int	  k = 0;
				char* v = gen_expr( operand, &k );
				char* cmp = newtemp();
				if ( k == 1 ) {
					char* t = newtemp();
					emit( "  %s = ptrtoint i8* %s to i64", t, v );
					emit( "  %s = icmp eq i64 %s, 0", cmp, t );
					free( t );
				} else if ( k == 2 ) {
					emit( "  %s = icmp eq i64 %s, 0", cmp, v );
				} else if ( k == 3 ) {
					emit( "  %s = icmp eq i8 %s, 0", cmp, v );
				} else {
					emit( "  %s = icmp eq i32 %s, 0", cmp, v );
				}
				char* out = newtemp();
				emit( "  %s = zext i1 %s to i32", out, cmp );
				free( v );
				free( cmp );
				*kind = 0;
				return out;
			} else {
				fprintf( stderr, "codegen: unsupported unary operator '%c'\\n", op );
				*kind = 0;
				return strdup( "0" );
			}
		}
		default:
			*kind = 0;
			return strdup( "0" );
	}
}

/* statements & functions */
static void gen_stmt( Stmt* s );
static int  cur_ret_is_void = 0;
static void gen_func( Func* f )
{
	free_locals();
	if ( loc_types ) {
		for ( int i = 0; i < loc_types_n; i++ )
			free( loc_types[i].name );
		free( loc_types );
		loc_types = NULL;
		loc_types_n = loc_types_cap = 0;
	}

	if ( !f )
		return;
	if ( !f->body ) {
		/* extern declare using types if present */
		if ( f->has_types && f->arg_types ) {
			char retbuf[64];
			int	 retstorage = type_to_storage_and_llvm( f->ret_type, retbuf, sizeof retbuf );
			/* build arg signature but respect varargs marker */
			char* argsig = NULL;
			int	  vararg_pos = -1;
			for ( int i = 0; i < f->nargs; i++ ) {
				if ( f->arg_types[i].base == TY_VARARG ) {
					vararg_pos = i;
					break;
				}
				char a_buf[64];
				int	 astor = type_to_storage_and_llvm( f->arg_types[i], a_buf, sizeof a_buf );
				char tmp[128];
				snprintf( tmp, sizeof tmp, "%s", a_buf );
				argsig = append_arglist( argsig, tmp );
			}
			if ( vararg_pos != -1 ) {
				if ( argsig && argsig[0] != '\0' )
					argsig = append_arglist( argsig, "..." );
				else
					argsig = strdup( "..." );
			}
			if ( f->ret_type.base == TY_VOID && f->ret_type.ptr_level == 0 )
				emit( "declare void @%s(%s)", f->name, argsig ? argsig : "" );
			else
				emit( "declare %s @%s(%s)", retbuf, f->name, argsig ? argsig : "" );
			free( argsig );
			return;
		} else {
			/* no type info: declare as vararg to be permissive */
			emit( "declare i32 @%s(...)", f->name );
			return;
		}
	}

	/* define function: use explicit types if present, else default to i32 return + i64 args (legacy) */
	char* argsig = NULL;
	for ( int i = 0; i < f->nargs; i++ ) {
		char tmp[64];
		if ( f->has_types && f->arg_types ) {
			char abuf[64];
			type_to_storage_and_llvm( f->arg_types[i], abuf, sizeof abuf );
			snprintf( tmp, sizeof tmp, "%s %%a%d", abuf, i );
		} else {
			snprintf( tmp, sizeof tmp, "i64 %%a%d", i );
		}
		argsig = append_arglist( argsig, tmp );
	}
	char retbuf[64];
	if ( f->has_types ) {
		type_to_storage_and_llvm( f->ret_type, retbuf, sizeof retbuf );
	} else
		strcpy( retbuf, "i32" ); /* legacy default */
	emit( "define %s @%s(%s) {", retbuf, f->name, argsig ? argsig : "" );
	free( argsig );
	emit( "entry:" );
	cur_ret_is_void = ( f->has_types && f->ret_type.base == TY_VOID && f->ret_type.ptr_level == 0 );
	for ( int i = 0; i < f->nargs; i++ ) {
		char argname[64];
		snprintf( argname, sizeof argname, "arg.%d", i );
		int	 stor = 1;
		char llvmty[64];
		if ( f->has_types && f->arg_types )
			stor = type_to_storage_and_llvm( f->arg_types[i], llvmty, sizeof llvmty );
		else {
			stor = 1;
			strcpy( llvmty, "i64" );
		}
		char* an = alloc_local( argname, stor );
		emit( "  %s = alloca %s", an, llvmty );
		if ( strcmp( llvmty, "i8*" ) == 0 ) {
			emit( "  store i8* %%a%d, i8** %s", i, an );
		} else if ( strcmp( llvmty, "i64" ) == 0 ) {
			emit( "  store i64 %%a%d, i64* %s", i, an );
		} else if ( strcmp( llvmty, "i8" ) == 0 ) {
			emit( "  store i8 %%a%d, i8* %s", i, an );
		} else {
			emit( "  store i32 %%a%d, i32* %s", i, an );
		}
		char shortn[32];
		snprintf( shortn, sizeof shortn, "a%d", i );
		add_mapping( shortn, an, stor );
		if ( f->args && f->args[i] && f->args[i][0] ) {
			add_mapping( f->args[i], an, stor );
			if ( f->has_types && f->arg_types ) {
				add_local_type( f->args[i], f->arg_types[i] );
				add_local_type( shortn, f->arg_types[i] );
			}
		}
	}

	for ( int i = 0; i < f->body_cnt; i++ )
		gen_stmt( f->body[i] );

	if ( f->has_types && f->ret_type.base == TY_VOID && f->ret_type.ptr_level == 0 ) {
		emit( "  ret void" );
	} else {
		emit( "  ret i32 0" );
	}
	emit( "}" );
	cur_ret_is_void = 0;
}

/* gen_stmt */
static void gen_stmt( Stmt* s )
{
	if ( !s )
		return;
	switch ( s->kind ) {
		case ST_EXPR:
			if ( !s->expr )
				break;
			if ( s->expr->kind == EX_CALL ) {
				int	  d = 0;
				char* v = gen_call( s->expr, &d );
				free( v );
			} else {
				int	  d = 0;
				char* v = gen_expr( s->expr, &d );
				free( v );
			}
			break;
		case ST_ASSIGN: {
			if ( s->assign.lvalue && s->assign.lvalue->kind == EX_UNARY && s->assign.lvalue->unary.op == '*' ) {
				int	  k = 0;
				char* v = gen_expr( s->assign.expr, &k );
				char* addr = gen_lvalue_address( s->assign.lvalue );

				Type ltype = get_expr_type( s->assign.lvalue );
				char llvmty[64];
				int	 stor = type_to_storage_and_llvm( ltype, llvmty, sizeof llvmty );
				if ( stor == 4 ) {
					fprintf( stderr, "Assignment to struct via deref is not supported\n" );
					exit( 1 );
				}
				int want_kind
					= ( ltype.ptr_level > 0 ) ? 1 : ( ltype.base == TY_I64 ? 2 : ( ltype.base == TY_I8 || ltype.base == TY_CHAR ? 3 : 0 ) );

				char ptrty[128];
				snprintf( ptrty, sizeof ptrty, "%s*", llvmty );
				char* addrcast = newtemp();
				emit( "  %s = bitcast i8* %s to %s", addrcast, addr, ptrty );

				if ( want_kind == 1 ) {
					char* valtmp = v;
					if ( k != 1 )
						valtmp = conv( v, k, 1 );
					emit( "  store i8* %s, i8** %s", valtmp, addrcast );
					if ( valtmp != v )
						free( valtmp );
				} else if ( want_kind == 2 ) {
					char* valtmp = v;
					if ( k != 2 )
						valtmp = conv( v, k, 2 );
					emit( "  store i64 %s, i64* %s", valtmp, addrcast );
					if ( valtmp != v )
						free( valtmp );
				} else if ( want_kind == 3 ) {
					char* valtmp = v;
					if ( k != 0 )
						valtmp = conv( v, k, 0 );
					char* trunc_tmp = newtemp();
					emit( "  %s = trunc i32 %s to i8", trunc_tmp, valtmp );
					emit( "  store i8 %s, i8* %s", trunc_tmp, addrcast );
					if ( valtmp != v )
						free( valtmp );
					free( trunc_tmp );
				} else {
					char* valtmp = v;
					if ( k != 0 )
						valtmp = conv( v, k, 0 );
					emit( "  store i32 %s, i32* %s", valtmp, addrcast );
					if ( valtmp != v )
						free( valtmp );
				}

				free( addr );
				free( addrcast );
				free( v );
				break;
			}

			char* an = find_alloca( s->assign.var );
			int	  stor = 0;
			char  llvmty[64];

			if ( !an ) {
				stor = type_to_storage_and_llvm( s->assign.vartype, llvmty, sizeof llvmty );
				if ( s->assign.vartype.ptr_level == 0 && s->assign.vartype.base == TY_I8 ) {
					stor = 3;
					strcpy( llvmty, "i8" );
				}
				an = alloc_local( s->assign.var, stor );
				emit( "  %s = alloca %s", an, llvmty );
				add_local_type( s->assign.var, s->assign.vartype );
			} else {
				stor = find_type( s->assign.var );
				if ( stor < 0 || stor > 3 )
					stor = 0;
			}

			int	  k = 0;
			char* v = gen_expr( s->assign.expr, &k );

			if ( s->assign.index ) {
				if ( s->assign.index->kind == EX_STR ) {
					Expr basevar;
					memset( &basevar, 0, sizeof( basevar ) );
					basevar.kind = EX_VAR;
					basevar.var = strdup( s->assign.var );
					Expr idxexpr;
					memset( &idxexpr, 0, sizeof( idxexpr ) );
					idxexpr.kind = EX_STR;
					idxexpr.var = strdup( s->assign.index->var );
					Expr fullexpr;
					memset( &fullexpr, 0, sizeof( fullexpr ) );
					fullexpr.kind = EX_INDEX;
					fullexpr.index.target = &basevar;
					fullexpr.index.idx = &idxexpr;

					char* addr = gen_lvalue_address( &fullexpr );

					Type ftype = get_expr_type( &fullexpr );
					char fllvm[64];
					int	 fstor = type_to_storage_and_llvm( ftype, fllvm, sizeof fllvm );
					int	 want_kind
						= ( ftype.ptr_level > 0 ) ? 1 : ( ftype.base == TY_I64 ? 2 : ( ftype.base == TY_I8 || ftype.base == TY_CHAR ? 3 : 0 ) );

					char ptrty[128];
					snprintf( ptrty, sizeof ptrty, "%s*", fllvm );
					char* addrcast = newtemp();
					emit( "  %s = bitcast i8* %s to %s", addrcast, addr, ptrty );

					char* valtmp = v;
					if ( want_kind != k ) {
						valtmp = conv( v, k, want_kind );
					}

					if ( want_kind == 1 ) {
						emit( "  store i8* %s, i8** %s", valtmp, addrcast );
					} else if ( want_kind == 2 ) {
						emit( "  store i64 %s, i64* %s", valtmp, addrcast );
					} else if ( want_kind == 3 ) {
						/* want byte: ensure i32->trunc i8 */
						char* vt = valtmp;
						if ( want_kind != k ) {
							vt = conv( valtmp, k, 0 );
						}
						char* trunc_tmp = newtemp();
						emit( "  %s = trunc i32 %s to i8", trunc_tmp, vt );
						emit( "  store i8 %s, i8* %s", trunc_tmp, addrcast );
						if ( vt != valtmp )
							free( vt );
						free( trunc_tmp );
					} else {
						emit( "  store i32 %s, i32* %s", valtmp, addrcast );
					}

					if ( valtmp != v )
						free( valtmp );
					free( addr );
					free( addrcast );
					free( basevar.var );
					free( idxexpr.var );
				} else {
					if ( stor != 2 ) {
						fprintf( stderr, "Indexing non-pointer local: %s\n", s->assign.var );
						exit( 1 );
					}
					char* base_ptr = newtemp();
					emit( "  %s = load i8*, i8** %s", base_ptr, an );

					int	  ik = 0;
					char* idx = gen_expr( s->assign.index, &ik );
					char* idx_conv = conv( idx, ik, 2 );
					free( idx );

					char* gep = newtemp();
					emit( "  %s = getelementptr i8, i8* %s, i64 %s", gep, base_ptr, idx_conv );

					char* valtmp = v;
					if ( k != 0 )
						valtmp = conv( v, k, 0 );
					char* trunc_tmp = newtemp();
					emit( "  %s = trunc i32 %s to i8", trunc_tmp, valtmp );
					emit( "  store i8 %s, i8* %s", trunc_tmp, gep );

					if ( valtmp != v )
						free( valtmp );
					free( gep );
					free( idx_conv );
					free( base_ptr );
					free( trunc_tmp );
					free( v );
				}
			} else {
				char* tmp = v;
				if ( stor == 2 ) {
					if ( k != 1 )
						tmp = conv( v, k, 1 );
					emit( "  store i8* %s, i8** %s", tmp, an );
				} else if ( stor == 1 ) {
					if ( k != 2 )
						tmp = conv( v, k, 2 );
					emit( "  store i64 %s, i64* %s", tmp, an );
				} else if ( stor == 3 ) {
					char* valtmp = v;
					if ( k != 0 )
						valtmp = conv( v, k, 0 );
					char* trunc_tmp = newtemp();
					emit( "  %s = trunc i32 %s to i8", trunc_tmp, valtmp );
					emit( "  store i8 %s, i8* %s", trunc_tmp, an );
					if ( valtmp != v )
						free( valtmp );
					free( trunc_tmp );
				} else {
					if ( k != 0 )
						tmp = conv( v, k, 0 );
					emit( "  store i32 %s, i32* %s", tmp, an );
				}
				if ( tmp != v )
					free( tmp );
				free( v );
			}
			break;
		}
		case ST_RETURN: {
			if ( cur_ret_is_void ) {
				if ( s->ret ) {
					int	  k = 0;
					char* v = gen_expr( s->ret, &k );
					free( v );
				}
				emit( "  ret void" );
				break;
			}
			if ( !s->ret ) {
				emit( "  ret i32 0" );
				break;
			}
			int	  k = 0;
			char* v = gen_expr( s->ret, &k );
			char* tmp = v;
			if ( k != 0 )
				tmp = conv( v, k, 0 );
			emit( "  ret i32 %s", tmp );
			if ( tmp != v )
				free( tmp );
			free( v );
			break;
		}
		case ST_IF: {
			char *L = newlbl( "if_then" ), *E = newlbl( "if_else" ), *X = newlbl( "if_end" );
			int	  k = 0;
			char* c = gen_expr( s->ifs.cond, &k );
			char* cmp = newtemp();
			if ( k == 1 ) {
				char* t = newtemp();
				emit( "  %s = ptrtoint i8* %s to i64", t, c );
				emit( "  %s = icmp ne i64 %s, 0", cmp, t );
				free( t );
			} else if ( k == 2 )
				emit( "  %s = icmp ne i64 %s, 0", cmp, c );
			else
				emit( "  %s = icmp ne i32 %s, 0", cmp, c );
			emit( "  br i1 %s, label %%%s, label %%%s", cmp, L, E );
			emit( "%s:", L );
			for ( int i = 0; i < s->ifs.then_cnt; i++ )
				gen_stmt( s->ifs.then_stmts[i] );
			emit( "  br label %%%s", X );
			emit( "%s:", E );
			for ( int i = 0; i < s->ifs.else_cnt; i++ )
				gen_stmt( s->ifs.else_stmts[i] );
			emit( "  br label %%%s", X );
			emit( "%s:", X );
			free( L );
			free( E );
			free( X );
			free( c );
			free( cmp );
			break;
		}
		case ST_WHILE: {
			char *T = newlbl( "while_top" ), *B = newlbl( "while_body" ), *E = newlbl( "while_end" );
			emit( "  br label %%%s", T );
			emit( "%s:", T );
			int	  k = 0;
			char* c = gen_expr( s->wh.cond, &k );
			char* cmp = newtemp();
			if ( k == 1 ) {
				char* t = newtemp();
				emit( "  %s = ptrtoint i8* %s to i64", t, c );
				emit( "  %s = icmp ne i64 %s, 0", cmp, t );
				free( t );
			} else if ( k == 2 )
				emit( "  %s = icmp ne i64 %s, 0", cmp, c );
			else
				emit( "  %s = icmp ne i32 %s, 0", cmp, c );
			emit( "  br i1 %s, label %%%s, label %%%s", cmp, B, E );
			emit( "%s:", B );
			for ( int i = 0; i < s->wh.body_cnt; i++ )
				gen_stmt( s->wh.body[i] );
			emit( "  br label %%%s", T );
			emit( "%s:", E );
			free( T );
			free( B );
			free( E );
			free( c );
			free( cmp );
			break;
		}
		case ST_ASM:
			if ( s->asm_code && s->asm_code[0] )
				emit( "  ; inline asm: %s", s->asm_code );
			break;
		default:
			break;
	}
}

/* top-level codegen */
void codegen_all( const char* out_filename )
{
	if ( !out_filename )
		return;
	outf = fopen( out_filename, "w" );
	if ( !outf ) {
		perror( "fopen" );
		exit( 1 );
	}
	char* host = detect_host_triple();
	emit( "target triple = \"%s\"", host );
	free( host );
	emit( "" );
	for ( int i = 0; i < func_cnt; i++ )
		gen_func( funcs[i] );
	if ( sct_n > 0 )
		emit( "; -- string constants --" );
	for ( int i = 0; i < sct_n; i++ )
		emit( "@%s = private constant [%d x i8] c\"%s\\00\"", sct[i].lbl, sct[i].len, sct[i].val );
	fclose( outf );
	free_locals();
	if ( sct ) {
		for ( int i = 0; i < sct_n; i++ ) {
			free( sct[i].lbl );
			free( sct[i].val );
		}
		free( sct );
		sct = NULL;
	}
	sct_n = sct_cap = 0;

	if ( loc_types ) {
		for ( int i = 0; i < loc_types_n; i++ )
			free( loc_types[i].name );
		free( loc_types );
		loc_types = NULL;
		loc_types_n = loc_types_cap = 0;
	}
}
