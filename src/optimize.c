#include "optimize.h"

#include "ast.h"
#include "parser.h"

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct
{
	char*	name;
	int64_t value;
} ConstBinding;

typedef struct
{
	ConstBinding* items;
	int			  count;
} ConstEnv;

static int g_optimize_enabled = 0;

void set_optimize_enabled( int enabled )
{
	g_optimize_enabled = enabled != 0;
}

int optimize_enabled( void )
{
	return g_optimize_enabled;
}

static int64_t fold_binary( char op, int64_t lhs, int64_t rhs, int* ok )
{
	*ok = 1;
	switch ( op ) {
		case '+':
			return lhs + rhs;
		case '-':
			return lhs - rhs;
		case '*':
			return lhs * rhs;
		case '/':
			if ( rhs == 0 ) {
				*ok = 0;
				return 0;
			}
			return lhs / rhs;
		case '%':
			if ( rhs == 0 ) {
				*ok = 0;
				return 0;
			}
			return lhs % rhs;
		case '=':
			return lhs == rhs ? 1 : 0;
		case '<':
			return lhs < rhs ? 1 : 0;
		case '>':
			return lhs > rhs ? 1 : 0;
		case '&':
			return ( lhs != 0 && rhs != 0 ) ? 1 : 0;
		case '|':
			return ( lhs != 0 || rhs != 0 ) ? 1 : 0;
		default:
			*ok = 0;
			return 0;
	}
}

static Expr* make_num_expr( int64_t value )
{
	Expr* expr = calloc( 1, sizeof( *expr ) );
	if ( !expr )
		return NULL;
	expr->kind = EX_NUM;
	expr->num = value;
	return expr;
}

static int is_num_expr( const Expr* expr, int64_t* out_value )
{
	if ( !expr || expr->kind != EX_NUM )
		return 0;
	if ( out_value )
		*out_value = expr->num;
	return 1;
}

static int expr_has_side_effects( const Expr* expr )
{
	if ( !expr )
		return 0;

	switch ( expr->kind ) {
		case EX_CALL:
			return 1;
		case EX_BINOP:
			return expr_has_side_effects( expr->bin.a ) || expr_has_side_effects( expr->bin.b );
		case EX_UNARY:
			return expr_has_side_effects( expr->unary.operand );
		case EX_INDEX:
			return expr_has_side_effects( expr->index.target ) || expr_has_side_effects( expr->index.idx );
		default:
			return 0;
	}
}

static void env_clear( ConstEnv* env )
{
	if ( !env )
		return;

	for ( int i = 0; i < env->count; i++ )
		free( env->items[i].name );
	free( env->items );
	env->items = NULL;
	env->count = 0;
}

static int env_get( const ConstEnv* env, const char* name, int64_t* out_value )
{
	if ( !env || !name )
		return 0;

	for ( int i = env->count - 1; i >= 0; i-- ) {
		if ( strcmp( env->items[i].name, name ) == 0 ) {
			if ( out_value )
				*out_value = env->items[i].value;
			return 1;
		}
	}
	return 0;
}

static void env_forget( ConstEnv* env, const char* name )
{
	if ( !env || !name )
		return;

	for ( int i = 0; i < env->count; ) {
		if ( strcmp( env->items[i].name, name ) == 0 ) {
			free( env->items[i].name );
			memmove( &env->items[i], &env->items[i + 1], sizeof( env->items[i] ) * (size_t) ( env->count - i - 1 ) );
			env->count--;
			continue;
		}
		i++;
	}
}

static void env_set( ConstEnv* env, const char* name, int64_t value )
{
	if ( !env || !name )
		return;

	env_forget( env, name );
	env->items = realloc( env->items, sizeof( *env->items ) * (size_t) ( env->count + 1 ) );
	env->items[env->count].name = strdup( name );
	env->items[env->count].value = value;
	env->count++;
}

static ConstEnv env_clone( const ConstEnv* src )
{
	ConstEnv clone = { 0 };

	if ( !src || src->count == 0 )
		return clone;

	clone.items = calloc( (size_t) src->count, sizeof( *clone.items ) );
	if ( !clone.items )
		return clone;

	for ( int i = 0; i < src->count; i++ ) {
		clone.items[i].name = strdup( src->items[i].name );
		clone.items[i].value = src->items[i].value;
	}
	clone.count = src->count;
	return clone;
}

static Expr*  optimize_expr( Expr* expr, ConstEnv* env );
static Stmt*  optimize_stmt( Stmt* stmt, ConstEnv* env );
static Stmt** optimize_stmt_list( Stmt** stmts, int count, int* out_count, ConstEnv* env );

static Expr* optimize_expr( Expr* expr, ConstEnv* env )
{
	int64_t lhs = 0;
	int64_t rhs = 0;

	if ( !expr )
		return NULL;

	switch ( expr->kind ) {
		case EX_VAR:
			if ( env_get( env, expr->var, &lhs ) )
				return make_num_expr( lhs );
			return expr;

		case EX_BINOP:
			expr->bin.a = optimize_expr( expr->bin.a, env );
			expr->bin.b = optimize_expr( expr->bin.b, env );

			if ( is_num_expr( expr->bin.a, &lhs ) && is_num_expr( expr->bin.b, &rhs ) ) {
				int		ok = 0;
				int64_t value = fold_binary( expr->bin.op, lhs, rhs, &ok );
				if ( ok )
					return make_num_expr( value );
			}

			if ( expr->bin.op == '+' ) {
				if ( is_num_expr( expr->bin.b, &rhs ) && rhs == 0 )
					return expr->bin.a;
				if ( is_num_expr( expr->bin.a, &lhs ) && lhs == 0 )
					return expr->bin.b;
			}
			if ( expr->bin.op == '-' ) {
				if ( is_num_expr( expr->bin.b, &rhs ) && rhs == 0 )
					return expr->bin.a;
			}
			if ( expr->bin.op == '*' ) {
				if ( ( is_num_expr( expr->bin.a, &lhs ) && lhs == 0 ) || ( is_num_expr( expr->bin.b, &rhs ) && rhs == 0 ) )
					return make_num_expr( 0 );
				if ( is_num_expr( expr->bin.a, &lhs ) && lhs == 1 )
					return expr->bin.b;
				if ( is_num_expr( expr->bin.b, &rhs ) && rhs == 1 )
					return expr->bin.a;
			}
			if ( expr->bin.op == '/' ) {
				if ( is_num_expr( expr->bin.b, &rhs ) && rhs == 1 )
					return expr->bin.a;
			}
			if ( expr->bin.op == '=' && expr->bin.a == expr->bin.b )
				return make_num_expr( 1 );
			return expr;

		case EX_UNARY:
			expr->unary.operand = optimize_expr( expr->unary.operand, env );
			if ( expr->unary.op == '!' && is_num_expr( expr->unary.operand, &lhs ) )
				return make_num_expr( lhs == 0 ? 1 : 0 );
			if ( expr->unary.op == '*' && expr->unary.operand && expr->unary.operand->kind == EX_UNARY && expr->unary.operand->unary.op == '&' )
				return expr->unary.operand->unary.operand;
			if ( expr->unary.op == '&' && expr->unary.operand && expr->unary.operand->kind == EX_UNARY && expr->unary.operand->unary.op == '*' )
				return expr->unary.operand->unary.operand;
			return expr;

		case EX_CALL:
			for ( int i = 0; i < expr->call.nargs; i++ )
				expr->call.args[i] = optimize_expr( expr->call.args[i], env );
			return expr;

		case EX_INDEX:
			expr->index.target = optimize_expr( expr->index.target, env );
			expr->index.idx = optimize_expr( expr->index.idx, env );
			return expr;

		default:
			return expr;
	}
}

static Stmt* make_expr_stmt( Expr* expr )
{
	Stmt* stmt;

	if ( !expr )
		return NULL;

	stmt = calloc( 1, sizeof( *stmt ) );
	if ( !stmt )
		return NULL;
	stmt->kind = ST_EXPR;
	stmt->expr = expr;
	return stmt;
}

static Stmt* optimize_stmt( Stmt* stmt, ConstEnv* env )
{
	int64_t cond_value = 0;

	if ( !stmt )
		return NULL;

	switch ( stmt->kind ) {
		case ST_EXPR:
			stmt->expr = optimize_expr( stmt->expr, env );
			if ( !stmt->expr )
				return NULL;
			if ( expr_has_side_effects( stmt->expr ) )
				env_clear( env );
			if ( stmt->expr->kind == EX_NUM || stmt->expr->kind == EX_STR )
				return NULL;
			return stmt;

		case ST_ASSIGN:
			stmt->assign.expr = optimize_expr( stmt->assign.expr, env );
			stmt->assign.index = optimize_expr( stmt->assign.index, env );
			stmt->assign.lvalue = optimize_expr( stmt->assign.lvalue, env );

			if ( stmt->assign.var && !stmt->assign.index && ( !stmt->assign.lvalue || stmt->assign.lvalue->kind == EX_VAR ) ) {
				int64_t value = 0;
				if ( is_num_expr( stmt->assign.expr, &value ) )
					env_set( env, stmt->assign.var, value );
				else
					env_forget( env, stmt->assign.var );
			} else {
				env_clear( env );
			}
			return stmt;

		case ST_RETURN:
			stmt->ret = optimize_expr( stmt->ret, env );
			env_clear( env );
			return stmt;

		case ST_IF: {
			ConstEnv then_env;
			ConstEnv else_env;

			stmt->ifs.cond = optimize_expr( stmt->ifs.cond, env );
			then_env = env_clone( env );
			else_env = env_clone( env );
			stmt->ifs.then_stmts = optimize_stmt_list( stmt->ifs.then_stmts, stmt->ifs.then_cnt, &stmt->ifs.then_cnt, &then_env );
			stmt->ifs.else_stmts = optimize_stmt_list( stmt->ifs.else_stmts, stmt->ifs.else_cnt, &stmt->ifs.else_cnt, &else_env );
			env_clear( &then_env );
			env_clear( &else_env );

			if ( is_num_expr( stmt->ifs.cond, &cond_value ) ) {
				env_clear( env );
				if ( cond_value != 0 ) {
					if ( stmt->ifs.then_cnt == 0 )
						return NULL;
					if ( stmt->ifs.then_cnt == 1 )
						return stmt->ifs.then_stmts[0];
				} else {
					if ( stmt->ifs.else_cnt == 0 )
						return NULL;
					if ( stmt->ifs.else_cnt == 1 )
						return stmt->ifs.else_stmts[0];
				}
			}

			if ( stmt->ifs.then_cnt == 0 && stmt->ifs.else_cnt == 0 ) {
				if ( expr_has_side_effects( stmt->ifs.cond ) )
					return make_expr_stmt( stmt->ifs.cond );
				return NULL;
			}
			env_clear( env );
			return stmt;
		}

		case ST_WHILE: {
			ConstEnv loop_env;

			stmt->wh.cond = optimize_expr( stmt->wh.cond, env );
			loop_env = env_clone( env );
			stmt->wh.body = optimize_stmt_list( stmt->wh.body, stmt->wh.body_cnt, &stmt->wh.body_cnt, &loop_env );
			env_clear( &loop_env );
			if ( is_num_expr( stmt->wh.cond, &cond_value ) && cond_value == 0 )
				return NULL;
			env_clear( env );
			return stmt;
		}

		case ST_ASM:
			env_clear( env );
		default:
			return stmt;
	}
}

static Stmt** optimize_stmt_list( Stmt** stmts, int count, int* out_count, ConstEnv* env )
{
	Stmt** result = NULL;
	int	   result_count = 0;

	for ( int i = 0; i < count; i++ ) {
		Stmt* stmt = optimize_stmt( stmts[i], env );
		if ( !stmt )
			continue;

		result = realloc( result, sizeof( *result ) * (size_t) ( result_count + 1 ) );
		result[result_count++] = stmt;

		if ( stmt->kind == ST_RETURN )
			break;
	}

	*out_count = result_count;
	return result;
}

void optimize_all( void )
{
	for ( int i = 0; i < func_cnt; i++ ) {
		Func*	 func = funcs[i];
		ConstEnv env = { 0 };

		if ( !func || !func->body )
			continue;
		func->body = optimize_stmt_list( func->body, func->body_cnt, &func->body_cnt, &env );
		env_clear( &env );
	}
}
