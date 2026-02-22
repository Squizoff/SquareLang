#ifndef AST_H
#define AST_H

#include "lexer.h"

#include <stddef.h>
#include <stdint.h>

typedef enum
{
	TY_VOID = 0,
	TY_I32 = 1,
	TY_I64 = 2,
	TY_I8 = 3,
	TY_U8 = 4,
	TY_U32 = 5,
	TY_U64 = 6,
	TY_CHAR = 7,
	TY_USIZE = 8,
	TY_STRUCT = 9,
	TY_VARARG = 10
} BaseType;

typedef struct
{
	BaseType base;
	int		 ptr_level;
	int		 is_const;
	int		 is_unsigned;
	char*	 struct_name;
} Type;

typedef struct
{
	char* name;
	Type  type;
	int	  offset;
} StructField;

typedef struct
{
	char*		  name;
	StructField** fields;
	int			  field_cnt;
	int			  size;
} StructDef;

typedef enum
{
	EX_NUM,
	EX_VAR,
	EX_BINOP,
	EX_STR,
	EX_CALL,
	EX_INDEX,
	EX_UNARY
} ExprKind;

typedef struct Expr Expr;

typedef struct
{
	Expr* target;
	Expr* idx;
} ExprIndex;

struct Expr
{
	ExprKind kind;
	union {
		int64_t num;
		char*	var;
		struct
		{
			char  op;
			Expr *a, *b;
		} bin;
		struct
		{
			char*  name;
			Expr** args;
			int	   nargs;
		} call;
		ExprIndex index;
		struct
		{
			char		 op;
			struct Expr* operand;
		} unary;
	};
};

typedef enum
{
	ST_EXPR,
	ST_ASSIGN,
	ST_IF,
	ST_WHILE,
	ST_RETURN,
	ST_ASM
} StmtKind;

typedef struct Stmt Stmt;

struct Stmt
{
	StmtKind kind;
	union {
		Expr* expr;
		struct
		{
			char* var;
			Expr* expr;
			Expr* index;
			Type  vartype;
		} assign;
		struct
		{
			Expr*  cond;
			Stmt** then_stmts;
			int	   then_cnt;
			Stmt** else_stmts;
			int	   else_cnt;
		} ifs;
		struct
		{
			Expr*  cond;
			Stmt** body;
			int	   body_cnt;
		} wh;
		Expr* ret;
		char* asm_code;
	};
};

typedef struct
{
	char*  name;
	char*  orig_name;
	char** args;
	Type*  arg_types;
	int	   nargs;
	Stmt** body;
	int	   body_cnt;
	int	   is_local;
	char*  src_file;
	Type   ret_type;
	int	   has_types;
} Func;

typedef struct
{
	Token tok;
	Lexer lx;
} Parser;

extern StructDef** g_structs;
extern int		   g_struct_cnt;

#endif /* AST_H */
