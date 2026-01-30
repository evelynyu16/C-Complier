// compiler6.c — Jive interpreter with conditionals, loops, and boolean expressions

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>

// ===================== String =====================

typedef struct String {
    char *data;
    long count;
} String;

#define PRINT_STRING(s) (int)(s).count, (s).data
#define STRLIT(s) (String){ (char*)(s), (long)(sizeof(s)-1) }

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) { fprintf(stderr, "Out of memory\n"); exit(1); }
    return p;
}

static char *xstrndup_(const char *s, size_t n) {
    char *p = (char*)xmalloc(n + 1);
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

// ===================== Lexer =====================

typedef struct Loc {
    const char *file_name;
    long line;
    long col;
} Loc;

typedef enum Token_Kind {
    TOKEN_EOF = 0,
    TOKEN_KEYWORD,
    TOKEN_IDENT,
    TOKEN_TYPE,
    TOKEN_INTEGER,
    TOKEN_STRING,

    TOKEN_SYMBOL,       // operators etc

    TOKEN_OPEN_PAREN,   // (
    TOKEN_CLOSE_PAREN,  // )
    TOKEN_OPEN_BRACE,   // {
    TOKEN_CLOSE_BRACE,  // }
    TOKEN_COMMA,        // ,
    TOKEN_COLON,        // :
    TOKEN_ARROW         // ->
} Token_Kind;

typedef enum Keyword {
    KEYWORD_UNKNOWN = 0,
    KEYWORD_FN, KEYWORD_LET, KEYWORD_SET, KEYWORD_IF, KEYWORD_WHILE,
    KEYWORD_CALL, KEYWORD_RETURN, KEYWORD_TRUE, KEYWORD_FALSE
} Keyword;

typedef enum Type {
    TYPE_UNKNOWN = 0,
    TYPE_INT, TYPE_STR, TYPE_BOOL
} Type;

typedef struct Token {
    Token_Kind kind;
    String source; // textual lexeme
    Loc loc;
    union {
        long    long_value;  // INTEGER
        Keyword keyword;     // KEYWORD
        Type    type;        // TYPE
    };
} Token;

typedef struct Token_Array {
    Token *items;
    long count;
    long capacity;
} Token_Array;

static void token_array_init(Token_Array *arr) {
    arr->items = NULL;
    arr->count = 0;
    arr->capacity = 0;
}
static void token_array_push(Token_Array *arr, Token t) {
    if (arr->count == arr->capacity) {
        arr->capacity = arr->capacity ? arr->capacity * 2 : 64;
        arr->items = (Token*)realloc(arr->items, arr->capacity * sizeof(Token));
        if (!arr->items) { fprintf(stderr, "Out of memory\n"); exit(1); }
    }
    arr->items[arr->count++] = t;
}

typedef struct Scanner {
    const char *file_name;
    const char *start;
    const char *cur;
    const char *end;
    long line;
    long col;
} Scanner;

static void scanner_init(Scanner *s, const char *file_name, const char *buf, size_t len) {
    s->file_name = file_name;
    s->start = buf;
    s->cur   = buf;
    s->end   = buf + len;
    s->line  = 1;
    s->col   = 1;
}

static bool is_at_end(Scanner *s) { return s->cur >= s->end; }
static char peek(Scanner *s) { return is_at_end(s) ? '\0' : *s->cur; }
static char peek_next(Scanner *s) { return (s->cur + 1 >= s->end) ? '\0' : s->cur[1]; }

static char advance(Scanner *s) {
    if (is_at_end(s)) return '\0';
    char c = *s->cur++;
    if (c == '\n') { s->line++; s->col = 1; }
    else           { s->col++; }
    return c;
}

static void skip_ws_and_comments(Scanner *s) {
    for (;;) {
        char c = peek(s);
        if (c==' '||c=='\t'||c=='\r'||c=='\f'||c=='\v'||c=='\n') { advance(s); continue; }
        if (c == '/' && peek_next(s) == '/') {
            while (!is_at_end(s) && peek(s) != '\n') advance(s);
            continue;
        }
        break;
    }
}

static bool is_ident_start(char c) { return isalpha((unsigned char)c) || c=='_'; }
static bool is_ident_part (char c) { return isalnum((unsigned char)c) || c=='_'; }

typedef struct { const char *text; Keyword kw; } KwEnt;
static const KwEnt KW_TABLE[] = {
    {"fn", KEYWORD_FN},
    {"let", KEYWORD_LET},
    {"set", KEYWORD_SET},
    {"if", KEYWORD_IF},
    {"while", KEYWORD_WHILE},
    {"call", KEYWORD_CALL},
    {"return", KEYWORD_RETURN},
    {"true", KEYWORD_TRUE},
    {"false", KEYWORD_FALSE},
};

typedef struct { const char *text; Type ty; } TyEnt;
static const TyEnt TY_TABLE[] = {
    {"int",  TYPE_INT},
    {"str",  TYPE_STR},
    {"bool", TYPE_BOOL},
};

static Keyword lookup_keyword(const char *s, size_t n) {
    for (size_t i=0;i<sizeof(KW_TABLE)/sizeof(KW_TABLE[0]);++i)
        if (strlen(KW_TABLE[i].text)==n && strncmp(s,KW_TABLE[i].text,n)==0)
            return KW_TABLE[i].kw;
    return KEYWORD_UNKNOWN;
}
static Type lookup_type(const char *s, size_t n) {
    for (size_t i=0;i<sizeof(TY_TABLE)/sizeof(TY_TABLE[0]);++i)
        if (strlen(TY_TABLE[i].text)==n && strncmp(s,TY_TABLE[i].text,n)==0)
            return TY_TABLE[i].ty;
    return TYPE_UNKNOWN;
}

static Token make_token_basic(Token_Kind k, Scanner *s, const char *lexeme_start, size_t n, long line, long col) {
    Token t;
    t.kind = k;
    t.source.data = xstrndup_(lexeme_start, n);
    t.source.count = (long)n;
    t.loc.file_name = s->file_name;
    t.loc.line = line;
    t.loc.col = col;
    t.long_value = 0;
    t.keyword = KEYWORD_UNKNOWN;
    t.type = TYPE_UNKNOWN;
    return t;
}

static Token token_from_symbol(Scanner *s, const char *start, size_t n, long line, long col, Token_Kind kind) {
    return make_token_basic(kind, s, start, n, line, col);
}

static Token scan_number(Scanner *s) {
    long line = s->line, col = s->col;
    const char *start = s->cur;
    while (isdigit((unsigned char)peek(s))) advance(s);
    size_t n = (size_t)(s->cur - start);
    Token t = make_token_basic(TOKEN_INTEGER, s, start, n, line, col);
    char tmp[64]; size_t m = n < sizeof(tmp)-1 ? n : sizeof(tmp)-1;
    memcpy(tmp, start, m); tmp[m] = '\0';
    t.long_value = strtol(tmp, NULL, 10);
    return t;
}

static Token scan_ident_like(Scanner *s) {
    long line = s->line, col = s->col;
    const char *start = s->cur;
    advance(s);
    while (is_ident_part(peek(s))) advance(s);
    size_t n = (size_t)(s->cur - start);

    Keyword kw = lookup_keyword(start, n);
    if (kw != KEYWORD_UNKNOWN) {
        Token t = make_token_basic(TOKEN_KEYWORD, s, start, n, line, col);
        t.keyword = kw; return t;
    }
    Type ty = lookup_type(start, n);
    if (ty != TYPE_UNKNOWN) {
        Token t = make_token_basic(TOKEN_TYPE, s, start, n, line, col);
        t.type = ty; return t;
    }
    return make_token_basic(TOKEN_IDENT, s, start, n, line, col);
}

static Token scan_symbol(Scanner *s) {
    long line = s->line, col = s->col;
    const char *start = s->cur;
    char c = advance(s);

    // multi-char operators
    if (c=='-' && peek(s)=='>' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_ARROW); }
    if (c=='=' && peek(s)=='=' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }
    if (c=='!' && peek(s)=='=' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }
    if (c=='<' && peek(s)=='=' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }
    if (c=='>' && peek(s)=='=' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }
    if (c=='&' && peek(s)=='&' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }
    if (c=='|' && peek(s)=='|' ) { advance(s); return token_from_symbol(s,start,2,line,col,TOKEN_SYMBOL); }

    switch (c) {
        case '(': return token_from_symbol(s, start, 1, line, col, TOKEN_OPEN_PAREN);
        case ')': return token_from_symbol(s, start, 1, line, col, TOKEN_CLOSE_PAREN);
        case '{': return token_from_symbol(s, start, 1, line, col, TOKEN_OPEN_BRACE);
        case '}': return token_from_symbol(s, start, 1, line, col, TOKEN_CLOSE_BRACE);
        case ',': return token_from_symbol(s, start, 1, line, col, TOKEN_COMMA);
        case ':': return token_from_symbol(s, start, 1, line, col, TOKEN_COLON);
        default:  break;
    }
    return token_from_symbol(s, start, 1, line, col, TOKEN_SYMBOL);
}

static Token scan_token(Scanner *s) {
    skip_ws_and_comments(s);
    s->start = s->cur;
    if (is_at_end(s)) {
        Token t; t.kind = TOKEN_EOF;
        t.source.data = xstrndup_("", 0); t.source.count = 0;
        t.loc.file_name = s->file_name; t.loc.line = s->line; t.loc.col = s->col;
        return t;
    }
    char c = peek(s);
    if (is_ident_start(c)) return scan_ident_like(s);
    if (isdigit((unsigned char)c)) return scan_number(s);
    return scan_symbol(s);
}

static const char *kind_name(Token_Kind k) {
    switch (k) {
        case TOKEN_EOF:          return "EOF";
        case TOKEN_KEYWORD:      return "KEYWORD";
        case TOKEN_IDENT:        return "IDENT";
        case TOKEN_TYPE:         return "TYPE";
        case TOKEN_INTEGER:      return "INTEGER";
        case TOKEN_STRING:       return "STRING";
        case TOKEN_SYMBOL:       return "SYMBOL";
        case TOKEN_OPEN_PAREN:   return "(";
        case TOKEN_CLOSE_PAREN:  return ")";
        case TOKEN_OPEN_BRACE:   return "{";
        case TOKEN_CLOSE_BRACE:  return "}";
        case TOKEN_COMMA:        return ",";
        case TOKEN_COLON:        return ":";
        case TOKEN_ARROW:        return "->";
        default:                 return "?";
    }
}
static const char *kw_name(Keyword k) {
    switch (k) {
        case KEYWORD_FN: return "fn";
        case KEYWORD_LET: return "let";
        case KEYWORD_SET: return "set";
        case KEYWORD_IF: return "if";
        case KEYWORD_WHILE: return "while";
        case KEYWORD_CALL: return "call";
        case KEYWORD_RETURN: return "return";
        case KEYWORD_TRUE: return "true";
        case KEYWORD_FALSE: return "false";
        default: return "?";
    }
}
static const char *ty_name(Type t) {
    switch (t) {
        case TYPE_INT: return "int";
        case TYPE_STR: return "str";
        case TYPE_BOOL: return "bool";
        default: return "?";
    }
}

static void print_loc(Loc loc) { printf("%s:%ld:%ld", loc.file_name, loc.line, loc.col); }

static void print_token(const Token *t) {
    printf("%s:%ld:%ld\t%-10s\t", t->loc.file_name, t->loc.line, t->loc.col, kind_name(t->kind));
    switch (t->kind) {
        case TOKEN_KEYWORD: printf("%s\n", kw_name(t->keyword)); break;
        case TOKEN_TYPE:    printf("%s\n", ty_name(t->type)); break;
        case TOKEN_INTEGER: printf("%ld\n", t->long_value); break;
        case TOKEN_IDENT:
        case TOKEN_SYMBOL:
        case TOKEN_OPEN_PAREN: case TOKEN_CLOSE_PAREN:
        case TOKEN_OPEN_BRACE: case TOKEN_CLOSE_BRACE:
        case TOKEN_COMMA: case TOKEN_COLON:
        case TOKEN_ARROW:
            printf("%s\n", t->source.data); break;
        case TOKEN_EOF: printf("EOF\n"); break;
        default: printf("\n"); break;
    }
}

static void print_token_array(Token_Array a) {
    for (long i = 0; i < a.count; ++i) print_token(&a.items[i]);
}

static char *read_entire_file_to_buf(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); rewind(f);
    char *buf = (char*)xmalloc((size_t)sz + 1);
    size_t n = fread(buf, 1, (size_t)sz, f); fclose(f);
    buf[n] = '\0'; if (out_len) *out_len = n; return buf;
}

static Token_Array lex_file(const char *path) {
    size_t len = 0;
    char *buf = read_entire_file_to_buf(path, &len);
    if (!buf) { fprintf(stderr, "error: could not read '%s'\n", path); exit(1); }
    Token_Array toks; token_array_init(&toks);
    Scanner s; scanner_init(&s, path, buf, len);
    for (;;) {
        Token t = scan_token(&s);
        token_array_push(&toks, t);
        if (t.kind == TOKEN_EOF) break;
    }
    free(buf);
    return toks;
}

// ===================== AST =====================

typedef enum AST_Kind {
    AST_NONE,
    AST_PROGRAM,
    AST_FN,
    AST_PARAM,

    AST_STMT_BLOCK,
    AST_STMT_RETURN,
    AST_STMT_CALL,
    AST_STMT_LET,
    AST_STMT_SET,
    AST_STMT_IF,
    AST_STMT_WHILE,

    AST_EXPR_INT,
    AST_EXPR_BOOL,
    AST_EXPR_IDENT,
    AST_EXPR_BINOP,
    AST_EXPR_CALL,
} AST_Kind;

typedef enum OpKind {
    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD,
    OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
    OP_AND, OP_OR,
} OpKind;

struct AST_Node;

typedef struct AST_List {
    long count;
    struct AST_Node *first;
    struct AST_Node *last;
} AST_List;

typedef struct AST_Param {
    String name;
    Type type;
} AST_Param;

typedef struct AST_Call {
    String fn_name;
    AST_List args; // expressions
} AST_Call;

typedef struct AST_Fn_Data {
    String name;
    AST_List params; // PARAM nodes
    Type return_type;
    struct AST_Node *body; // BLOCK stmt node
} AST_Fn_Data;

typedef struct AST_Let {
    String name;
    Type type;
    struct AST_Node *init; // nullable expr
} AST_Let;

typedef struct AST_Set {
    String name;
    struct AST_Node *value; // expr
} AST_Set;

typedef struct AST_If {
    struct AST_Node *cond; // expr (bool)
    struct AST_Node *then_branch; // stmt
} AST_If;

typedef struct AST_While {
    struct AST_Node *cond; // expr (bool)
    struct AST_Node *body; // stmt (usually BLOCK)
} AST_While;

typedef struct AST_Bin {
    OpKind op;
    struct AST_Node *left;
    struct AST_Node *right;
} AST_Bin;

typedef struct AST_Node {
    AST_Kind kind;
    struct AST_Node *prev;
    struct AST_Node *next;
    union {
        AST_List program;    // PROGRAM list of FNs
        AST_Fn_Data fn;      // FN

        AST_Param param;     // PARAM

        AST_List block;      // STMT_BLOCK: list of statements

        struct { struct AST_Node *expr; } ret_stmt; // RETURN

        AST_Call call;       // CALL stmt or CALL expr

        AST_Let let_stmt;    // LET

        AST_Set set_stmt;    // SET

        AST_If if_stmt;      // IF

        AST_While while_stmt;// WHILE

        long int_value;      // INT
        bool bool_value;     // BOOL
        String ident_name;   // IDENT
        AST_Bin bin;         // BINOP
    };
} AST_Node;

static AST_Node *ast_make(AST_Kind kind) {
    AST_Node *n = (AST_Node*)calloc(1, sizeof(AST_Node));
    n->kind = kind;
    return n;
}

static void ast_list_append(AST_List *list, AST_Node *node) {
    node->prev = list->last;
    node->next = NULL;
    if (list->last) list->last->next = node;
    else            list->first = node;
    list->last = node;
    list->count++;
}

// ===================== Parser =====================

typedef struct Parser {
    Token_Array tokens;
    long tok_index;
    bool has_error;
} Parser;

static Token *peek_token(Parser *p, int off) {
    long idx = p->tok_index + off;
    if (idx >= p->tokens.count) return &p->tokens.items[p->tokens.count - 1];
    return &p->tokens.items[idx];
}

static void report_error(Parser *p, Token *tok, const char *msg) {
    print_loc(tok->loc);
    printf(": %s\n", msg);
    p->has_error = true;
}

static Token *expect_token(Parser *p, Token_Kind want) {
    Token *t = peek_token(p, 0);
    if (t->kind != want) {
        report_error(p, t, "unexpected token");
        printf("  expected %s, got %s\n", kind_name(want), kind_name(t->kind));
        return t;
    }
    ++p->tok_index;
    return t;
}

static Token *expect_keyword(Parser *p, Keyword want) {
    Token *t = expect_token(p, TOKEN_KEYWORD);
    if (p->has_error) return t;
    if (t->keyword != want) {
        report_error(p, t, "unexpected keyword");
        printf("  expected '%s'\n", kw_name(want));
    }
    return t;
}

static bool token_is_sym(Token *t, const char *s) {
    return t->kind == TOKEN_SYMBOL &&
           (long)strlen(s) == t->source.count &&
           strncmp(t->source.data, s, (size_t)t->source.count) == 0;
}

// ---- expression parsing with precedence ----
// primary: INT | true/false | IDENT | IDENT '(' args ')' | '(' expr ')'
// unary:   (not implemented)
// mul:     unary ( (*|/|%) unary )*
// add:     mul ( (+|-) mul )*
// rel:     add ( (<|<=|>|>=) add )?
// eq:      rel ( (==|!=) rel )?
// and:     eq ( && eq )*
// or:      and ( || and )*

static AST_Node *parse_expr(Parser *p); // forward

static AST_Node *parse_primary(Parser *p) {
    Token *t = peek_token(p, 0);

    if (t->kind == TOKEN_INTEGER) {
        ++p->tok_index;
        AST_Node *n = ast_make(AST_EXPR_INT);
        n->int_value = t->long_value;
        return n;
    }

    if (t->kind == TOKEN_KEYWORD && (t->keyword == KEYWORD_TRUE || t->keyword == KEYWORD_FALSE)) {
        ++p->tok_index;
        AST_Node *n = ast_make(AST_EXPR_BOOL);
        n->bool_value = (t->keyword == KEYWORD_TRUE);
        return n;
    }

    if (t->kind == TOKEN_IDENT) {
        Token *name_tok = t;
        ++p->tok_index;
        Token *next = peek_token(p, 0);

        if (next->kind == TOKEN_OPEN_PAREN) {
            ++p->tok_index; // '('
            AST_Node *call = ast_make(AST_EXPR_CALL);
            call->call.fn_name = (String){ name_tok->source.data, name_tok->source.count };
            call->call.args = (AST_List){0};

            Token *look = peek_token(p, 0);
            if (look->kind != TOKEN_CLOSE_PAREN) {
                for (;;) {
                    AST_Node *arg = parse_expr(p);
                    ast_list_append(&call->call.args, arg);
                    Token *sep = peek_token(p, 0);
                    if (sep->kind == TOKEN_COMMA) { ++p->tok_index; continue; }
                    if (sep->kind == TOKEN_CLOSE_PAREN) break;
                    report_error(p, sep, "expected ',' or ')' in call arguments");
                    break;
                }
            }
            expect_token(p, TOKEN_CLOSE_PAREN);
            return call;
        }

        AST_Node *id = ast_make(AST_EXPR_IDENT);
        id->ident_name = (String){ name_tok->source.data, name_tok->source.count };
        return id;
    }

    if (t->kind == TOKEN_OPEN_PAREN) {
        ++p->tok_index;
        AST_Node *inside = parse_expr(p);
        expect_token(p, TOKEN_CLOSE_PAREN);
        return inside;
    }

    report_error(p, t, "expected expression");
    return ast_make(AST_EXPR_INT);
}

static AST_Node *parse_mul(Parser *p) {
    AST_Node *node = parse_primary(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (token_is_sym(t, "*") || token_is_sym(t, "/") || token_is_sym(t, "%")) {
            OpKind op = token_is_sym(t, "*") ? OP_MUL : (token_is_sym(t, "/") ? OP_DIV : OP_MOD);
            ++p->tok_index;
            AST_Node *rhs = parse_primary(p);
            AST_Node *bin = ast_make(AST_EXPR_BINOP);
            bin->bin.op = op; bin->bin.left = node; bin->bin.right = rhs;
            node = bin;
        } else break;
    }
    return node;
}

static AST_Node *parse_add(Parser *p) {
    AST_Node *node = parse_mul(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (token_is_sym(t, "+") || token_is_sym(t, "-")) {
            OpKind op = token_is_sym(t, "+") ? OP_ADD : OP_SUB;
            ++p->tok_index;
            AST_Node *rhs = parse_mul(p);
            AST_Node *bin = ast_make(AST_EXPR_BINOP);
            bin->bin.op = op; bin->bin.left = node; bin->bin.right = rhs;
            node = bin;
        } else break;
    }
    return node;
}

static AST_Node *parse_rel(Parser *p) {
    AST_Node *node = parse_add(p);
    Token *t = peek_token(p, 0);
    if (token_is_sym(t, "<") || token_is_sym(t, "<=") || token_is_sym(t, ">") || token_is_sym(t, ">=")) {
        OpKind op = OP_LT;
        if (token_is_sym(t, "<")) op = OP_LT;
        else if (token_is_sym(t, "<=")) op = OP_LE;
        else if (token_is_sym(t, ">")) op = OP_GT;
        else if (token_is_sym(t, ">=")) op = OP_GE;
        ++p->tok_index;
        AST_Node *rhs = parse_add(p);
        AST_Node *bin = ast_make(AST_EXPR_BINOP);
        bin->bin.op = op; bin->bin.left = node; bin->bin.right = rhs;
        node = bin;
    }
    return node;
}

static AST_Node *parse_eq(Parser *p) {
    AST_Node *node = parse_rel(p);
    Token *t = peek_token(p, 0);
    if (token_is_sym(t, "==") || token_is_sym(t, "!=")) {
        OpKind op = token_is_sym(t, "==") ? OP_EQ : OP_NE;
        ++p->tok_index;
        AST_Node *rhs = parse_rel(p);
        AST_Node *bin = ast_make(AST_EXPR_BINOP);
        bin->bin.op = op; bin->bin.left = node; bin->bin.right = rhs;
        node = bin;
    }
    return node;
}

static AST_Node *parse_and(Parser *p) {
    AST_Node *node = parse_eq(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (!token_is_sym(t, "&&")) break;
        ++p->tok_index;
        AST_Node *rhs = parse_eq(p);
        AST_Node *bin = ast_make(AST_EXPR_BINOP);
        bin->bin.op = OP_AND; bin->bin.left = node; bin->bin.right = rhs;
        node = bin;
    }
    return node;
}

static AST_Node *parse_or(Parser *p) {
    AST_Node *node = parse_and(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (!token_is_sym(t, "||")) break;
        ++p->tok_index;
        AST_Node *rhs = parse_and(p);
        AST_Node *bin = ast_make(AST_EXPR_BINOP);
        bin->bin.op = OP_OR; bin->bin.left = node; bin->bin.right = rhs;
        node = bin;
    }
    return node;
}

static AST_Node *parse_expr(Parser *p) {
    return parse_or(p);
}

// ---- parameters, statements, functions ----

static AST_Node *parse_param(Parser *p) {
    Token *name_tok = expect_token(p, TOKEN_IDENT);
    expect_token(p, TOKEN_COLON);
    Token *ty_tok = expect_token(p, TOKEN_TYPE);

    AST_Node *param = ast_make(AST_PARAM);
    param->param.name = (String){ name_tok->source.data, name_tok->source.count };
    param->param.type = ty_tok->type;
    return param;
}

static void parse_param_list(Parser *p, AST_List *out_params) {
    *out_params = (AST_List){0};
    Token *t = peek_token(p, 0);
    if (t->kind == TOKEN_CLOSE_PAREN) return;

    for (;;) {
        AST_Node *param = parse_param(p);
        ast_list_append(out_params, param);
        Token *next = peek_token(p, 0);
        if (next->kind == TOKEN_COMMA) { ++p->tok_index; continue; }
        if (next->kind == TOKEN_CLOSE_PAREN) break;
        report_error(p, next, "expected ',' or ')' in parameter list");
        break;
    }
}

static AST_Node *parse_statement(Parser *p); // forward

static AST_Node *parse_block(Parser *p) {
    AST_Node *blk = ast_make(AST_STMT_BLOCK);
    blk->block = (AST_List){0};

    expect_token(p, TOKEN_OPEN_BRACE);
    if (p->has_error) return blk;

    for (;;) {
        Token *t = peek_token(p, 0);
        if (t->kind == TOKEN_CLOSE_BRACE) { ++p->tok_index; break; }
        AST_Node *st = parse_statement(p);
        if (p->has_error) break;
        ast_list_append(&blk->block, st);
    }
    return blk;
}

static AST_Node *parse_return_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_RETURN);
    AST_Node *ret = ast_make(AST_STMT_RETURN);
    ret->ret_stmt.expr = parse_expr(p);
    return ret;
}

static AST_Node *parse_call_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_CALL);
    Token *name_tok = expect_token(p, TOKEN_IDENT);
    expect_token(p, TOKEN_OPEN_PAREN);

    AST_Node *call = ast_make(AST_STMT_CALL);
    call->call.fn_name = (String){ name_tok->source.data, name_tok->source.count };
    call->call.args = (AST_List){0};

    Token *t = peek_token(p, 0);
    if (t->kind != TOKEN_CLOSE_PAREN) {
        for (;;) {
            AST_Node *arg = parse_expr(p);
            ast_list_append(&call->call.args, arg);
            Token *next = peek_token(p, 0);
            if (next->kind == TOKEN_COMMA) { ++p->tok_index; continue; }
            if (next->kind == TOKEN_CLOSE_PAREN) break;
            report_error(p, next, "expected ',' or ')' in call");
            break;
        }
    }
    expect_token(p, TOKEN_CLOSE_PAREN);
    return call;
}

static AST_Node *parse_let_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_LET);
    Token *name_tok = expect_token(p, TOKEN_IDENT);
    expect_token(p, TOKEN_COLON);
    Token *ty_tok = expect_token(p, TOKEN_TYPE);

    AST_Node *st = ast_make(AST_STMT_LET);
    st->let_stmt.name = (String){ name_tok->source.data, name_tok->source.count };
    st->let_stmt.type = ty_tok->type;
    st->let_stmt.init = NULL;

    Token *t = peek_token(p, 0);
    if (token_is_sym(t, "=")) {
        ++p->tok_index;
        st->let_stmt.init = parse_expr(p);
    }
    return st;
}

static AST_Node *parse_set_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_SET);
    Token *name_tok = expect_token(p, TOKEN_IDENT);

    Token *eq = peek_token(p, 0);
    if (!token_is_sym(eq, "=")) {
        report_error(p, eq, "expected '=' after set <ident>");
        return ast_make(AST_STMT_SET);
    }
    ++p->tok_index;

    AST_Node *st = ast_make(AST_STMT_SET);
    st->set_stmt.name = (String){ name_tok->source.data, name_tok->source.count };
    st->set_stmt.value = parse_expr(p);
    return st;
}

static AST_Node *parse_if_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_IF);
    AST_Node *st = ast_make(AST_STMT_IF);
    st->if_stmt.cond = parse_expr(p);
    st->if_stmt.then_branch = parse_statement(p);
    return st;
}

static AST_Node *parse_while_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_WHILE);
    AST_Node *st = ast_make(AST_STMT_WHILE);
    st->while_stmt.cond = parse_expr(p);

    Token *t = peek_token(p, 0);
    if (t->kind == TOKEN_OPEN_BRACE) st->while_stmt.body = parse_block(p);
    else st->while_stmt.body = parse_statement(p);
    return st;
}

static AST_Node *parse_statement(Parser *p) {
    Token *t = peek_token(p, 0);

    if (t->kind == TOKEN_OPEN_BRACE) return parse_block(p);

    if (t->kind == TOKEN_KEYWORD) {
        switch (t->keyword) {
            case KEYWORD_RETURN: return parse_return_stmt(p);
            case KEYWORD_CALL:   return parse_call_stmt(p);
            case KEYWORD_LET:    return parse_let_stmt(p);
            case KEYWORD_SET:    return parse_set_stmt(p);
            case KEYWORD_IF:     return parse_if_stmt(p);
            case KEYWORD_WHILE:  return parse_while_stmt(p);
            default: break;
        }
    }

    report_error(p, t, "unsupported statement (expected return/call/let/set/if/while or block)");
    return ast_make(AST_NONE);
}

static AST_Node *parse_fn_def(Parser *p) {
    AST_Node *fn = ast_make(AST_FN);

    expect_keyword(p, KEYWORD_FN);
    Token *name = expect_token(p, TOKEN_IDENT);

    expect_token(p, TOKEN_OPEN_PAREN);
    parse_param_list(p, &fn->fn.params);
    expect_token(p, TOKEN_CLOSE_PAREN);

    expect_token(p, TOKEN_ARROW);
    Token *ret_ty = expect_token(p, TOKEN_TYPE);

    fn->fn.name = (String){ name->source.data, name->source.count };
    fn->fn.return_type = ret_ty->type;

    fn->fn.body = parse_block(p);
    return fn;
}

typedef struct Parse_Result { AST_Node *ast; bool success; } Parse_Result;

static Parse_Result parse_program(Token_Array tokens) {
    Parser p = { .tokens = tokens, .tok_index = 0, .has_error = false };
    Parse_Result r;
    r.ast = ast_make(AST_PROGRAM);
    r.ast->program = (AST_List){0};

    for (;;) {
        Token *t = peek_token(&p, 0);
        if (t->kind == TOKEN_EOF) break;
        AST_Node *fn = parse_fn_def(&p);
        ast_list_append(&r.ast->program, fn);
        if (p.has_error) break;
    }

    r.success = !p.has_error;
    return r;
}

// ---- AST ---------

static const char *op_name(OpKind op) {
    switch (op) {
        case OP_ADD: return "+";
        case OP_SUB: return "-";
        case OP_MUL: return "*";
        case OP_DIV: return "/";
        case OP_MOD: return "%";
        case OP_EQ:  return "==";
        case OP_NE:  return "!=";
        case OP_LT:  return "<";
        case OP_LE:  return "<=";
        case OP_GT:  return ">";
        case OP_GE:  return ">=";
        case OP_AND: return "&&";
        case OP_OR:  return "||";
        default: return "?";
    }
}

static void print_ast(AST_Node *n, int depth) {
    if (!n) return;
    for (int i = 0; i < depth; ++i) printf("  ");
    switch (n->kind) {
        case AST_PROGRAM:
            printf("PROGRAM\n");
            for (AST_Node *f = n->program.first; f; f = f->next) print_ast(f, depth+1);
            break;
        case AST_FN:
            printf("FN %.*s(", PRINT_STRING(n->fn.name));
            for (AST_Node *p = n->fn.params.first; p; p = p->next) {
                printf("%.*s:%s", PRINT_STRING(p->param.name), ty_name(p->param.type));
                if (p->next) printf(", ");
            }
            printf(") -> %s\n", ty_name(n->fn.return_type));
            print_ast(n->fn.body, depth+1);
            break;
        case AST_STMT_BLOCK:
            printf("BLOCK\n");
            for (AST_Node *s = n->block.first; s; s = s->next) print_ast(s, depth+1);
            break;
        case AST_STMT_RETURN:
            printf("RETURN\n");
            print_ast(n->ret_stmt.expr, depth+1);
            break;
        case AST_STMT_CALL:
            printf("CALL_STMT %.*s\n", PRINT_STRING(n->call.fn_name));
            for (AST_Node *a = n->call.args.first; a; a = a->next) print_ast(a, depth+1);
            break;
        case AST_STMT_LET:
            printf("LET %.*s : %s\n", PRINT_STRING(n->let_stmt.name), ty_name(n->let_stmt.type));
            if (n->let_stmt.init) print_ast(n->let_stmt.init, depth+1);
            break;
        case AST_STMT_SET:
            printf("SET %.*s\n", PRINT_STRING(n->set_stmt.name));
            print_ast(n->set_stmt.value, depth+1);
            break;
        case AST_STMT_IF:
            printf("IF\n");
            print_ast(n->if_stmt.cond, depth+1);
            print_ast(n->if_stmt.then_branch, depth+1);
            break;
        case AST_STMT_WHILE:
            printf("WHILE\n");
            print_ast(n->while_stmt.cond, depth+1);
            print_ast(n->while_stmt.body, depth+1);
            break;
        case AST_EXPR_INT:
            printf("INT %ld\n", n->int_value);
            break;
        case AST_EXPR_BOOL:
            printf("BOOL %s\n", n->bool_value ? "true" : "false");
            break;
        case AST_EXPR_IDENT:
            printf("IDENT %.*s\n", PRINT_STRING(n->ident_name));
            break;
        case AST_EXPR_CALL:
            printf("CALL_EXPR %.*s\n", PRINT_STRING(n->call.fn_name));
            for (AST_Node *a = n->call.args.first; a; a = a->next) print_ast(a, depth+1);
            break;
        case AST_EXPR_BINOP:
            printf("BINOP %s\n", op_name(n->bin.op));
            print_ast(n->bin.left, depth+1);
            print_ast(n->bin.right, depth+1);
            break;
        default:
            printf("UNKNOWN(%d)\n", n->kind);
            break;
    }
}

// ===================== Interpreter =====================

typedef struct EnvEntry {
    String name;
    long value; // all values as long; booleans are 0/1
} EnvEntry;

typedef struct Env {
    EnvEntry *items;
    long count;
    long capacity;
} Env;

static void env_add(Env *env, String name, long value) {
    if (env->count == env->capacity) {
        env->capacity = env->capacity ? env->capacity * 2 : 8;
        env->items = (EnvEntry*)realloc(env->items, env->capacity * sizeof(EnvEntry));
        if (!env->items) { fprintf(stderr, "Out of memory\n"); exit(1); }
    }
    env->items[env->count].name = name;
    env->items[env->count].value = value;
    env->count++;
}

static bool env_lookup(Env *env, String name, long *out_val) {
    for (long i = env->count - 1; i >= 0; --i) {
        EnvEntry *e = &env->items[i];
        if (e->name.count == name.count &&
            strncmp(e->name.data, name.data, (size_t)name.count) == 0) {
            *out_val = e->value;
            return true;
        }
    }
    return false;
}

static void env_set(Env *env, String name, long value) {
    for (long i = env->count - 1; i >= 0; --i) {
        EnvEntry *e = &env->items[i];
        if (e->name.count == name.count &&
            strncmp(e->name.data, name.data, (size_t)name.count) == 0) {
            e->value = value;
            return;
        }
    }
    fprintf(stderr, "runtime error: set of unknown variable '%.*s'\n", PRINT_STRING(name));
    exit(1);
}

static AST_Node *find_function(AST_Node *program, String name) {
    for (AST_Node *fn = program->program.first; fn; fn = fn->next) {
        if (fn->kind != AST_FN) continue;
        if (fn->fn.name.count == name.count &&
            strncmp(fn->fn.name.data, name.data, (size_t)name.count) == 0) {
            return fn;
        }
    }
    return NULL;
}

typedef struct ExecResult {
    bool did_return;
    long value;
} ExecResult;

static long eval_expr(AST_Node *program, AST_Node *expr, Env *env);

static long call_function(AST_Node *program, AST_Node *fn, long *arg_values, long arg_count);

static long truthy(long v) { return v != 0; }

static long eval_bin(OpKind op, long a, long b) {
    switch (op) {
        case OP_ADD: return a + b;
        case OP_SUB: return a - b;
        case OP_MUL: return a * b;
        case OP_DIV: if (b == 0) { fprintf(stderr, "runtime error: division by zero\n"); exit(1); } return a / b;
        case OP_MOD: if (b == 0) { fprintf(stderr, "runtime error: modulo by zero\n"); exit(1); } return a % b;

        case OP_EQ:  return (a == b) ? 1 : 0;
        case OP_NE:  return (a != b) ? 1 : 0;
        case OP_LT:  return (a <  b) ? 1 : 0;
        case OP_LE:  return (a <= b) ? 1 : 0;
        case OP_GT:  return (a >  b) ? 1 : 0;
        case OP_GE:  return (a >= b) ? 1 : 0;

        case OP_AND: return (truthy(a) && truthy(b)) ? 1 : 0;
        case OP_OR:  return (truthy(a) || truthy(b)) ? 1 : 0;
        default: break;
    }
    fprintf(stderr, "runtime error: unknown operator\n");
    exit(1);
}

static long eval_expr(AST_Node *program, AST_Node *expr, Env *env) {
    switch (expr->kind) {
        case AST_EXPR_INT: return expr->int_value;
        case AST_EXPR_BOOL: return expr->bool_value ? 1 : 0;
        case AST_EXPR_IDENT: {
            long v = 0;
            if (!env_lookup(env, expr->ident_name, &v)) {
                fprintf(stderr, "runtime error: unknown variable '%.*s'\n", PRINT_STRING(expr->ident_name));
                exit(1);
            }
            return v;
        }
        case AST_EXPR_BINOP: {
            long a = eval_expr(program, expr->bin.left, env);
            if (expr->bin.op == OP_AND) {
                if (!truthy(a)) return 0;
                long b = eval_expr(program, expr->bin.right, env);
                return eval_bin(OP_AND, a, b);
            }
            if (expr->bin.op == OP_OR) {
                if (truthy(a)) return 1;
                long b = eval_expr(program, expr->bin.right, env);
                return eval_bin(OP_OR, a, b);
            }
            long b = eval_expr(program, expr->bin.right, env);
            return eval_bin(expr->bin.op, a, b);
        }
        case AST_EXPR_CALL: {
            AST_Node *fn_def = find_function(program, expr->call.fn_name);
            if (!fn_def) {
                fprintf(stderr, "runtime error: unknown function '%.*s'\n", PRINT_STRING(expr->call.fn_name));
                exit(1);
            }
            long argc = expr->call.args.count;
            long *args = (long*)xmalloc(sizeof(long) * (size_t)argc);
            long i = 0;
            for (AST_Node *a = expr->call.args.first; a; a = a->next) {
                args[i++] = eval_expr(program, a, env);
            }
            long result = call_function(program, fn_def, args, argc);
            free(args);
            return result;
        }
        default:
            fprintf(stderr, "runtime error: unsupported expr kind %d\n", expr->kind);
            exit(1);
    }
}

static ExecResult exec_stmt(AST_Node *program, AST_Node *stmt, Env *env);

static ExecResult exec_block(AST_Node *program, AST_Node *blk, Env *env) {
    ExecResult r = {0};
    for (AST_Node *s = blk->block.first; s; s = s->next) {
        r = exec_stmt(program, s, env);
        if (r.did_return) return r;
    }
    return r;
}

static ExecResult exec_stmt(AST_Node *program, AST_Node *stmt, Env *env) {
    ExecResult r = {0};
    switch (stmt->kind) {
        case AST_STMT_BLOCK:
            return exec_block(program, stmt, env);

        case AST_STMT_RETURN:
            r.did_return = true;
            r.value = eval_expr(program, stmt->ret_stmt.expr, env);
            return r;

        case AST_STMT_CALL: {
            AST_Node *fn_def = find_function(program, stmt->call.fn_name);
            if (!fn_def) {
                fprintf(stderr, "runtime error: unknown function '%.*s'\n", PRINT_STRING(stmt->call.fn_name));
                exit(1);
            }
            long argc = stmt->call.args.count;
            long *args = (long*)xmalloc(sizeof(long) * (size_t)argc);
            long i = 0;
            for (AST_Node *a = stmt->call.args.first; a; a = a->next) {
                args[i++] = eval_expr(program, a, env);
            }
            (void)call_function(program, fn_def, args, argc);
            free(args);
            return r;
        }

        case AST_STMT_LET: {
            long init = 0;
            if (stmt->let_stmt.init) init = eval_expr(program, stmt->let_stmt.init, env);
            env_add(env, stmt->let_stmt.name, init);
            return r;
        }

        case AST_STMT_SET: {
            long v = eval_expr(program, stmt->set_stmt.value, env);
            env_set(env, stmt->set_stmt.name, v);
            return r;
        }

        case AST_STMT_IF: {
            long c = eval_expr(program, stmt->if_stmt.cond, env);
            if (truthy(c)) return exec_stmt(program, stmt->if_stmt.then_branch, env);
            return r;
        }

        case AST_STMT_WHILE: {
            for (;;) {
                long c = eval_expr(program, stmt->while_stmt.cond, env);
                if (!truthy(c)) break;
                ExecResult inner = exec_stmt(program, stmt->while_stmt.body, env);
                if (inner.did_return) return inner;
            }
            return r;
        }

        default:
            fprintf(stderr, "runtime error: unsupported statement kind %d\n", stmt->kind);
            exit(1);
    }
}

static long call_function(AST_Node *program, AST_Node *fn, long *arg_values, long arg_count) {
    Env env = {0};

    long idx = 0;
    for (AST_Node *p = fn->fn.params.first; p; p = p->next) {
        if (idx >= arg_count) {
            fprintf(stderr, "runtime error: not enough arguments for '%.*s'\n", PRINT_STRING(fn->fn.name));
            exit(1);
        }
        env_add(&env, p->param.name, arg_values[idx]);
        idx++;
    }
    if (idx != arg_count) {
        fprintf(stderr, "runtime error: too many arguments for '%.*s'\n", PRINT_STRING(fn->fn.name));
        exit(1);
    }

    if (!fn->fn.body || fn->fn.body->kind != AST_STMT_BLOCK) {
        fprintf(stderr, "runtime error: function '%.*s' has no block body\n", PRINT_STRING(fn->fn.name));
        exit(1);
    }

    ExecResult r = exec_block(program, fn->fn.body, &env);
    if (!r.did_return) {
        fprintf(stderr, "runtime error: function '%.*s' did not return\n", PRINT_STRING(fn->fn.name));
        exit(1);
    }
    return r.value;
}

// ===================== Main =====================

typedef struct Options {
    const char *in_file;
    bool dump_tokens;
    bool dump_ast;
} Options;

static void print_usage(const char *prog) {
    printf("Usage: %s input.jive [--tokens] [--ast]\n", prog);
}

int main(int argc, const char **argv) {
    Options opt = {0};
    if (argc < 2) { print_usage(argv[0]); return 1; }
    opt.in_file = argv[1];
    for (int i = 2; i < argc; ++i) {
        if (strcmp(argv[i], "--tokens") == 0) opt.dump_tokens = true;
        else if (strcmp(argv[i], "--ast") == 0) opt.dump_ast = true;
        else printf("WARNING: unknown flag '%s'\n", argv[i]);
    }

    Token_Array toks = lex_file(opt.in_file);
    if (opt.dump_tokens) {
        printf("=== TOKENS ===\n");
        print_token_array(toks);
    }

    Parse_Result pr = parse_program(toks);
    if (!pr.success) {
        printf("ERROR: failed to parse program.\n");
        return 1;
    }

    if (opt.dump_ast) {
        printf("=== AST ===\n");
        print_ast(pr.ast, 0);
    }

    AST_Node *program = pr.ast;
    AST_Node *main_fn = find_function(program, STRLIT("main"));
    if (!main_fn) {
        fprintf(stderr, "error: no 'main' function defined\n");
        return 1;
    }

    long result = call_function(program, main_fn, NULL, 0);
    printf("%ld\n", result);
    return 0;
}
