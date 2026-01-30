//Sorry!!! I was focusing on the pitch event at school for my research project/startup
//and didn't want to risk resubmit the work late last week. I should have reached out and work out a timeline instead.
//when I could actually sit down and really fix the code. I apologize for the inconvenience caused.
//Here is the fixed version of compiler3.c, lexer.c and parser.c according to your comments. 
//Reviewing concepts for finals helped me understood some of my mistakesand I hope this version is satisfactory.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>

// ===================== String =====================
#ifndef JIVE_STRING_C
#define JIVE_STRING_C

typedef struct String {
    char *data;
    long count;
} String;

#define PRINT_STRING(s) (int)(s).count, (s).data

#endif // JIVE_STRING_C

// ===================== Lexer =====================
#ifndef JIVE_LEXER_C
#define JIVE_LEXER_C

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
    TOKEN_OPEN_BRACKET, // [
    TOKEN_CLOSE_BRACKET,// ]
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
    struct String source; // textual lexeme (ident/symbol/etc.)
    Loc        loc;
    union {
        long    long_value;  // INTEGER
        Keyword keyword;     // KEYWORD
        Type    type;        // TYPE
    };
} Token;

typedef struct Token_Array {
    Token *items;
    long   count;
    long   capacity;
} Token_Array;

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

// ----- token builders -----
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

static Token scan_string(Scanner *s) {
    long line = s->line, col = s->col;
    advance(s); // skip opening "

    size_t cap = 32, n = 0;
    char *buf = (char*)xmalloc(cap);
    bool terminated = false;

    while (!is_at_end(s)) {
        char c = advance(s);
        if (c == '"') { terminated = true; break; }
        if (c == '\\') {
            if (is_at_end(s)) break;
            char e = advance(s);
            switch (e) {
                case 'n': c = '\n'; break;
                case 't': c = '\t'; break;
                case 'r': c = '\r'; break;
                case '\\': c = '\\'; break;
                case '"': c = '"'; break;
                default:  c = e; break;
            }
        }
        if (n + 1 >= cap) { cap *= 2; buf = (char*)realloc(buf, cap); if (!buf) { fprintf(stderr,"OOM\n"); exit(1);} }
        buf[n++] = c;
    }
    if (!terminated) {
        fprintf(stderr, "%s:%ld:%ld: error: unterminated string literal\n", s->file_name, line, col);
    }
    buf[n] = '\0';

    Token t;
    t.kind = TOKEN_STRING;
    t.source.data = buf;
    t.source.count = (long)n;
    t.loc.file_name = s->file_name; t.loc.line = line; t.loc.col = col;
    return t;
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
        case '[': return token_from_symbol(s, start, 1, line, col, TOKEN_OPEN_BRACKET);
        case ']': return token_from_symbol(s, start, 1, line, col, TOKEN_CLOSE_BRACKET);
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
    if (c == '"') return scan_string(s);
    return scan_symbol(s);
}

static void lex_all(const char *file_name, const char *buf, size_t len, Token_Array *out) {
    Scanner s; scanner_init(&s, file_name, buf, len);
    token_array_init(out);
    for (;;) {
        Token t = scan_token(&s);
        token_array_push(out, t);
        if (t.kind == TOKEN_EOF) break;
    }
}

static const char *kind_name(Token_Kind k) {
    switch (k) {
        case TOKEN_EOF:          return "EOF";
        case TOKEN_KEYWORD:      return "KEYWORD";
        case TOKEN_IDENT:        return "IDENTIFIER";
        case TOKEN_TYPE:         return "TYPE";
        case TOKEN_INTEGER:      return "INTEGER";
        case TOKEN_STRING:       return "STRING";
        case TOKEN_SYMBOL:       return "SYMBOL";
        case TOKEN_OPEN_PAREN:   return "(";
        case TOKEN_CLOSE_PAREN:  return ")";
        case TOKEN_OPEN_BRACKET: return "[";
        case TOKEN_CLOSE_BRACKET:return "]";
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

static void print_token(const Token *t) {
    printf("%s:%ld:%ld\t", t->loc.file_name, t->loc.line, t->loc.col);
    printf("%-12s", kind_name(t->kind));
    switch (t->kind) {
        case TOKEN_KEYWORD: printf("\t%s\n", kw_name(t->keyword)); break;
        case TOKEN_TYPE:    printf("\t%s\n", ty_name(t->type)); break;
        case TOKEN_INTEGER: printf("\t%ld\n", t->long_value); break;
        case TOKEN_IDENT:
        case TOKEN_SYMBOL:
        case TOKEN_OPEN_PAREN: case TOKEN_CLOSE_PAREN:
        case TOKEN_OPEN_BRACKET: case TOKEN_CLOSE_BRACKET:
        case TOKEN_OPEN_BRACE: case TOKEN_CLOSE_BRACE:
        case TOKEN_COMMA: case TOKEN_COLON:
        case TOKEN_ARROW: case TOKEN_STRING:
            printf("\t%s\n", t->source.data); break;
        case TOKEN_EOF: printf("\n"); break;
        default: printf("\n"); break;
    }
}

#define STRLIT(s) (String){ (char*)(s), (long)(sizeof(s)-1) }

const String keyword_names[] = {
    STRLIT("?"),
    STRLIT("fn"),
    STRLIT("let"),
    STRLIT("set"),
    STRLIT("if"),
    STRLIT("while"),
    STRLIT("call"),
    STRLIT("return"),
    STRLIT("true"),
    STRLIT("false"),
};

const String type_names[] = {
    STRLIT("?"),
    STRLIT("int"),
    STRLIT("str"),
    STRLIT("bool"),
};

void print_token_kind(Token_Kind k) { printf("%s", kind_name(k)); }
void print_loc(Loc loc) { printf("%s:%ld:%ld", loc.file_name, loc.line, loc.col); }

void print_token_array(Token_Array a) {
    for (long i = 0; i < a.count; ++i) print_token(&a.items[i]);
}

static char *read_entire_file_to_buf(const char *path, size_t *out_len) {
    FILE *f = fopen(path, "rb"); if (!f) return NULL;
    fseek(f, 0, SEEK_END); long sz = ftell(f); rewind(f);
    char *buf = (char*)xmalloc((size_t)sz + 1);
    size_t n = fread(buf, 1, (size_t)sz, f); fclose(f);
    buf[n] = '\0'; if (out_len) *out_len = n; return buf;
}

Token_Array lex_file(const char *path) {
    size_t len = 0;
    char *buf = read_entire_file_to_buf(path, &len);
    if (!buf) { fprintf(stderr, "error: could not read '%s'\n", path); exit(1); }
    Token_Array toks; lex_all(path, buf, len, &toks);
    free(buf);
    return toks;
}

#endif // JIVE_LEXER_C

// ===================== Parser (with binary expressions) =====================
#ifndef JIVE_PARSER_C
#define JIVE_PARSER_C

typedef enum AST_Kind {
    AST_NONE,
    AST_PROGRAM,
    AST_FN,
    AST_RETURN,
    AST_INTEGER,
    AST_BINOP,
} AST_Kind;

typedef enum AST_BinOp {
    BIN_ADD,
    BIN_SUB,
    BIN_MUL,
    BIN_DIV,
    BIN_MOD
} AST_BinOp;

const char *ast_kind_as_cstr(AST_Kind kind) {
    switch (kind) {
        case AST_NONE:    return "NONE (ERROR!)";
        case AST_PROGRAM: return "PROGRAM";
        case AST_FN:      return "FN";
        case AST_RETURN:  return "RETURN";
        case AST_INTEGER: return "INTEGER";
        case AST_BINOP:   return "BINOP";
        default:          return "UNKNOWN (ERROR!)";
    }
}

typedef struct AST_Node AST_Node;

typedef struct AST_List {
    long count;
    AST_Node *first;
    AST_Node *last;
} AST_List;

typedef struct AST_Fn_Data {
    String  name;
    AST_List parameters;
    Type    return_type;
    AST_List body;
} AST_Fn_Data;

struct AST_Node {
    AST_Kind kind;
    AST_Node *prev;
    AST_Node *next;
    union {
        AST_List   program;     // AST_PROGRAM
        AST_Fn_Data fn;         // AST_FN
        AST_Node  *ret_expr;    // AST_RETURN
        long       int_value;   // AST_INTEGER
        struct {                // AST_BINOP
            AST_BinOp op;
            AST_Node *left;
            AST_Node *right;
        } bin;
    };
};

typedef struct Parser {
    Token_Array tokens;
    long tok_index;
    bool has_error;
} Parser;

static AST_Node *make_ast_node(AST_Kind kind) {
    AST_Node *n = (AST_Node*)calloc(1, sizeof(AST_Node));
    n->kind = kind;
    return n;
}

static void report_error(Parser *p, Token *tok, const char *msg) {
    print_loc(tok->loc);
    printf(": %s\n", msg);
    p->has_error = true;
}

static Token *peek_token(Parser *p, int off) {
    long idx = p->tok_index + off;
    if (idx >= p->tokens.count) return &p->tokens.items[p->tokens.count - 1];
    return &p->tokens.items[idx];
}

static Token *expect_token(Parser *p, Token_Kind want) {
    Token *t = peek_token(p, 0);
    if (t->kind != want) {
        report_error(p, t, "ERROR: Unexpected token");
        printf(" (expected ");
        print_token_kind(want);
        printf(", got %s)\n", kind_name(t->kind));
        return t;
    }
    ++p->tok_index;
    return t;
}

static Token *expect_keyword(Parser *p, Keyword want) {
    Token *t = expect_token(p, TOKEN_KEYWORD);
    if (p->has_error) return t;
    if (t->keyword != want) {
        report_error(p, t, "ERROR: Unexpected keyword");
        printf(" (expected '%s')\n", kw_name(want));
    }
    return t;
}

static void ast_list_append(AST_List *list, AST_Node *node) {
    node->prev = list->last;
    node->next = NULL;
    if (list->last) list->last->next = node;
    else            list->first = node;
    list->last = node;
    list->count++;
}

// ---- Expression parsing ----
// Grammar:
//
// expr  -> term ( ('+'|'-') term )*
// term  -> factor ( ('*'|'/'|'%') factor )*
// factor-> INTEGER | '(' expr ')'

static AST_Node *parse_expr(Parser *p); // forward

static bool token_is_op(Token *t, char c) {
    return t->kind == TOKEN_SYMBOL &&
           t->source.count == 1 &&
           t->source.data[0] == c;
}

static AST_Node *parse_factor(Parser *p) {
    Token *t = peek_token(p, 0);
    if (t->kind == TOKEN_INTEGER) {
        ++p->tok_index;
        AST_Node *n = make_ast_node(AST_INTEGER);
        n->int_value = t->long_value;
        return n;
    } else if (t->kind == TOKEN_OPEN_PAREN) {
        ++p->tok_index; // consume '('
        AST_Node *inside = parse_expr(p);
        if (!p->has_error) {
            expect_token(p, TOKEN_CLOSE_PAREN);
        }
        return inside;
    } else {
        report_error(p, t, "ERROR: expected expression");
        return make_ast_node(AST_NONE);
    }
}

static AST_Node *parse_term(Parser *p) {
    AST_Node *node = parse_factor(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (token_is_op(t, '*') || token_is_op(t, '/') || token_is_op(t, '%')) {
            char op_ch = t->source.data[0];
            ++p->tok_index; // consume operator
            AST_Node *rhs = parse_factor(p);

            AST_Node *bin = make_ast_node(AST_BINOP);
            bin->bin.left = node;
            bin->bin.right = rhs;
            switch (op_ch) {
                case '*': bin->bin.op = BIN_MUL; break;
                case '/': bin->bin.op = BIN_DIV; break;
                case '%': bin->bin.op = BIN_MOD; break;
            }
            node = bin;
        } else {
            break;
        }
    }
    return node;
}

static AST_Node *parse_expr(Parser *p) {
    AST_Node *node = parse_term(p);
    for (;;) {
        Token *t = peek_token(p, 0);
        if (token_is_op(t, '+') || token_is_op(t, '-')) {
            char op_ch = t->source.data[0];
            ++p->tok_index;
            AST_Node *rhs = parse_term(p);

            AST_Node *bin = make_ast_node(AST_BINOP);
            bin->bin.left = node;
            bin->bin.right = rhs;
            switch (op_ch) {
                case '+': bin->bin.op = BIN_ADD; break;
                case '-': bin->bin.op = BIN_SUB; break;
            }
            node = bin;
        } else {
            break;
        }
    }
    return node;
}

// ---- Stmts / functions ----

static AST_Node *parse_return_stmt(Parser *p) {
    expect_keyword(p, KEYWORD_RETURN);
    AST_Node *ret = make_ast_node(AST_RETURN);
    if (!p->has_error) ret->ret_expr = parse_expr(p);
    return ret;
}

static AST_List parse_block(Parser *p) {
    AST_List result = {0};

    expect_token(p, TOKEN_OPEN_BRACE);
    if (p->has_error) return result;

    Token *t = peek_token(p, 0);
    if (!(t->kind == TOKEN_KEYWORD && t->keyword == KEYWORD_RETURN)) {
        report_error(p, t, "ERROR: expected 'return' in function body");
        return result;
    }
    AST_Node *r = parse_return_stmt(p);
    if (!p->has_error) ast_list_append(&result, r);

    expect_token(p, TOKEN_CLOSE_BRACE);
    return result;
}

static AST_Node *parse_fn_def(Parser *p) {
    AST_Node *fn = make_ast_node(AST_FN);

    expect_keyword(p, KEYWORD_FN);                 if (p->has_error) return fn;
    Token *name = expect_token(p, TOKEN_IDENT);    if (p->has_error) return fn;

    expect_token(p, TOKEN_OPEN_PAREN);             if (p->has_error) return fn;
    expect_token(p, TOKEN_CLOSE_PAREN);            if (p->has_error) return fn;

    Token *arrow = peek_token(p, 0);
    if (arrow->kind != TOKEN_ARROW) {
        report_error(p, arrow, "ERROR: expected '-> int' after function header");
        return fn;
    }
    ++p->tok_index;

    Token *ret_ty = expect_token(p, TOKEN_TYPE);   if (p->has_error) return fn;
    if (ret_ty->type != TYPE_INT) {
        report_error(p, ret_ty, "ERROR: only 'int' return type is supported");
        return fn;
    }

    AST_List body = parse_block(p);                if (p->has_error) return fn;

    fn->fn.name = (String){ name->source.data, name->source.count };
    fn->fn.return_type = TYPE_INT;
    fn->fn.body = body;
    return fn;
}

typedef struct Parse_Result { AST_Node *ast; bool success; } Parse_Result;

Parse_Result parse_program(Token_Array tokens) {
    Parse_Result r = { .ast = make_ast_node(AST_PROGRAM), .success = true };
    Parser p = { .tokens = tokens, .tok_index = 0, .has_error = false };

    for (;;) {
        Token *t = peek_token(&p, 0);
        if (t->kind == TOKEN_EOF) break;
        AST_Node *f = parse_fn_def(&p);
        ast_list_append(&r.ast->program, f);
        if (p.has_error) break;
    }
    r.success = !p.has_error;
    return r;
}

static void print_ast_with_indent(AST_Node *node, int depth) {
    if (!node) return;
    switch (node->kind) {
        case AST_PROGRAM:
            printf("%*sprogram\n", 2*depth, "");
            for (AST_Node *n = node->program.first; n; n = n->next)
                print_ast_with_indent(n, depth + 1);
            break;
        case AST_FN:
            printf("%*sfn %.*s()\n", 2*depth, "", PRINT_STRING(node->fn.name));
            for (AST_Node *n = node->fn.body.first; n; n = n->next)
                print_ast_with_indent(n, depth + 1);
            break;
        case AST_RETURN:
            printf("%*sreturn\n", 2*depth, "");
            if (node->ret_expr) print_ast_with_indent(node->ret_expr, depth + 1);
            break;
        case AST_INTEGER:
            printf("%*sinteger %ld\n", 2*depth, "", node->int_value);
            break;
        case AST_BINOP:
            printf("%*sbinop ", 2*depth, "");
            switch (node->bin.op) {
                case BIN_ADD: printf("+\n"); break;
                case BIN_SUB: printf("-\n"); break;
                case BIN_MUL: printf("*\n"); break;
                case BIN_DIV: printf("/\n"); break;
                case BIN_MOD: printf("%%\n"); break;
            }
            print_ast_with_indent(node->bin.left, depth + 1);
            print_ast_with_indent(node->bin.right, depth + 1);
            break;
        default:
            printf("%*sUNHANDLED %d\n", 2*depth, "", node->kind);
            break;
    }
}
void print_ast(AST_Node *node) { print_ast_with_indent(node, 0); }

#endif // JIVE_PARSER_C

// ===================== Stack Machine IR =====================

#define da_append(da, item)                                                      \
do {                                                                             \
    if ((da)->count >= (da)->capacity) {                                         \
        (da)->capacity = (da)->capacity ? (da)->capacity*2 : 16;                 \
        (da)->items = realloc((da)->items, (da)->capacity*sizeof((da)->items[0]));\
    }                                                                            \
    (da)->items[(da)->count++] = (item);                                         \
} while (false)
#define da_pop(da) (da)->items[--(da)->count]

typedef enum SM_Kind {
    SM_NONE,
    SM_FN,
    SM_FN_END,
    SM_PUSH,
    SM_ADD,
    SM_SUB,
    SM_MUL,
    SM_DIV,
    SM_MOD,
    SM_RETURN,
} SM_Kind;

typedef struct SM_Instruction {
    SM_Kind kind;
    long int_value;   // for PUSH
    String name;      // for FN
} SM_Instruction;

typedef struct Stack_Machine_Instructions {
    SM_Instruction *items;
    long count, capacity;
} Stack_Machine_Instructions;

static SM_Instruction sm_make_fn(String name) { SM_Instruction i = { .kind = SM_FN, .name = name }; return i; }
static SM_Instruction sm_make_fn_end(void)   { SM_Instruction i = { .kind = SM_FN_END }; return i; }
static SM_Instruction sm_make_push(long v)   { SM_Instruction i = { .kind = SM_PUSH, .int_value = v }; return i; }
static SM_Instruction sm_make_add(void)      { SM_Instruction i = { .kind = SM_ADD }; return i; }
static SM_Instruction sm_make_sub(void)      { SM_Instruction i = { .kind = SM_SUB }; return i; }
static SM_Instruction sm_make_mul(void)      { SM_Instruction i = { .kind = SM_MUL }; return i; }
static SM_Instruction sm_make_div(void)      { SM_Instruction i = { .kind = SM_DIV }; return i; }
static SM_Instruction sm_make_mod(void)      { SM_Instruction i = { .kind = SM_MOD }; return i; }
static SM_Instruction sm_make_return(void)   { SM_Instruction i = { .kind = SM_RETURN }; return i; }

static void gen_expr(Stack_Machine_Instructions *out, AST_Node *expr)
{
    if (!expr) return;
    switch (expr->kind) {
        case AST_INTEGER:
            da_append(out, sm_make_push(expr->int_value));
            break;
        case AST_BINOP:
            // post-order: left, right, op
            gen_expr(out, expr->bin.left);
            gen_expr(out, expr->bin.right);
            switch (expr->bin.op) {
                case BIN_ADD: da_append(out, sm_make_add()); break;
                case BIN_SUB: da_append(out, sm_make_sub()); break;
                case BIN_MUL: da_append(out, sm_make_mul()); break;
                case BIN_DIV: da_append(out, sm_make_div()); break;
                case BIN_MOD: da_append(out, sm_make_mod()); break;
            }
            break;
        default:
            fprintf(stderr, "ERROR: Unsupported expression node for IR: %s\n",
                    ast_kind_as_cstr(expr->kind));
            break;
    }
}

Stack_Machine_Instructions generate_stack_machine_instructions(AST_Node *ast)
{
    Stack_Machine_Instructions ir = {0};
    if (!ast || ast->kind != AST_PROGRAM) {
        fprintf(stderr, "ERROR: IR gen expected PROGRAM root, got %s\n",
                ast ? ast_kind_as_cstr(ast->kind) : "(null)");
        return ir;
    }

    for (AST_Node *fn = ast->program.first; fn; fn = fn->next) {
        if (fn->kind != AST_FN) {
            fprintf(stderr, "ERROR: top-level must be functions\n");
            continue;
        }
        da_append(&ir, sm_make_fn(fn->fn.name));

        for (AST_Node *stmt = fn->fn.body.first; stmt; stmt = stmt->next) {
            if (stmt->kind != AST_RETURN) {
                fprintf(stderr, "ERROR: Only 'return <expr>' statements are supported.\n");
                continue;
            }
            gen_expr(&ir, stmt->ret_expr);
            da_append(&ir, sm_make_return());
        }

        da_append(&ir, sm_make_fn_end());
    }

    return ir;
}

static const char *sm_name(SM_Kind k) {
    switch (k) {
        case SM_FN: return "FN";
        case SM_FN_END: return "END_FN";
        case SM_PUSH: return "PUSH";
        case SM_ADD: return "ADD";
        case SM_SUB: return "SUB";
        case SM_MUL: return "MUL";
        case SM_DIV: return "DIV";
        case SM_MOD: return "MOD";
        case SM_RETURN: return "RETURN";
        default: return "UNKNOWN";
    }
}

void print_stack_machine(const Stack_Machine_Instructions *ir)
{
    for (long i = 0; i < ir->count; ++i) {
        const SM_Instruction *ins = &ir->items[i];
        switch (ins->kind) {
            case SM_FN:
                printf("FN %.*s\n", PRINT_STRING(ins->name));
                break;
            case SM_FN_END:
                printf("END_FN\n");
                break;
            case SM_PUSH:
                printf("PUSH %ld\n", ins->int_value);
                break;
            case SM_ADD: case SM_SUB: case SM_MUL:
            case SM_DIV: case SM_MOD: case SM_RETURN:
                printf("%s\n", sm_name(ins->kind));
                break;
            default:
                printf("??? (%d)\n", ins->kind);
                break;
        }
    }
}

// （可选）解释执行 stack machine，用来自己测试
typedef struct Stack { long *items; long count, capacity; } Stack;
static void s_push(Stack *s, long v){ da_append(s, v); }
static long s_pop(Stack *s){ return da_pop(s); }

long simulate_stack_machine(Stack_Machine_Instructions *ir)
{
    Stack st = {0};
    long last_ret = 0;
    for (long ip = 0; ip < ir->count; ++ip) {
        SM_Instruction in = ir->items[ip];
        switch (in.kind) {
            case SM_PUSH: s_push(&st, in.int_value); break;
            case SM_ADD:  { long b=s_pop(&st), a=s_pop(&st); s_push(&st, a+b); } break;
            case SM_SUB:  { long b=s_pop(&st), a=s_pop(&st); s_push(&st, a-b); } break;
            case SM_MUL:  { long b=s_pop(&st), a=s_pop(&st); s_push(&st, a*b); } break;
            case SM_DIV:  { long b=s_pop(&st), a=s_pop(&st); s_push(&st, a/b); } break;
            case SM_MOD:  { long b=s_pop(&st), a=s_pop(&st); s_push(&st, a%b); } break;
            case SM_RETURN:
                last_ret = s_pop(&st);
                break;
            default: break;
        }
    }
    return last_ret;
}

// ===================== Assembly codegen from Stack Machine =====================

bool generate_asm_from_ir(const Stack_Machine_Instructions *ir, FILE *out_file)
{
    if (!ir || !out_file) return false;

    // program preamble
    fprintf(out_file, "global _start\n\n");
    fprintf(out_file, "_start:\n");
    fprintf(out_file, "    call main\n");
    fprintf(out_file, "    mov rdi, rax\n");
    fprintf(out_file, "    mov rax, 60\n");
    fprintf(out_file, "    syscall\n\n");

    for (long i = 0; i < ir->count; ++i) {
        const SM_Instruction *ins = &ir->items[i];
        switch (ins->kind) {
            case SM_FN:
                fprintf(out_file, "%.*s:\n", PRINT_STRING(ins->name));
                break;
            case SM_FN_END:
                fprintf(out_file, "\n");
                break;
            case SM_PUSH:
                fprintf(out_file, "    push %ld\n", ins->int_value);
                break;
            case SM_ADD:
                fprintf(out_file, "    pop rcx\n");
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    add rax, rcx\n");
                fprintf(out_file, "    push rax\n");
                break;
            case SM_SUB:
                fprintf(out_file, "    pop rcx\n");
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    sub rax, rcx\n");
                fprintf(out_file, "    push rax\n");
                break;
            case SM_MUL:
                fprintf(out_file, "    pop rcx\n");
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    imul rcx\n");
                fprintf(out_file, "    push rax\n");
                break;
            case SM_DIV:
                fprintf(out_file, "    pop rcx\n");
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    cqo\n");
                fprintf(out_file, "    idiv rcx\n");
                fprintf(out_file, "    push rax\n");
                break;
            case SM_MOD:
                fprintf(out_file, "    pop rcx\n");
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    cqo\n");
                fprintf(out_file, "    idiv rcx\n");
                fprintf(out_file, "    push rdx\n");
                break;
            case SM_RETURN:
                fprintf(out_file, "    pop rax\n");
                fprintf(out_file, "    ret\n");
                break;
            default:
                break;
        }
    }

    return true;
}

// ===================== main =====================

typedef struct Options {
    const char *out_file_name;
    const char *in_file_name;
} Options;

static void print_usage(const char *program_name) {
    printf("Usage: %s input_file.jive [-o output_file.asm]\n", program_name);
}

int main(int arg_count, const char **args)
{
    Options options = {
        .out_file_name = "out.asm",
        .in_file_name = NULL,
    };

    int arg_index = 0;
    const char *program_name = args[arg_index++];

    while (arg_index < arg_count) {
        const char *arg = args[arg_index++];
        if (strcmp(arg, "-o") == 0) {
            if (arg_index < arg_count) {
                options.out_file_name = args[arg_index++];
            } else {
                printf("ERROR: Missing output file name after -o flag.\n");
                return 1;
            }
        } else if (options.in_file_name == NULL) {
            options.in_file_name = arg;
        } else {
            printf("WARNING: Unrecognized command line argument %s\n", arg);
        }
    }

    if (options.in_file_name == NULL) {
        printf("ERROR: No input file supplied.\n");
        print_usage(program_name);
        return 1;
    }

    // 1) Lex
    Token_Array tokens = lex_file(options.in_file_name);

   
    // printf("Tokens:\n");
    // print_token_array(tokens);

    // 2) Parse → AST (binary expressions))
    Parse_Result parse_result = parse_program(tokens);
    if (!parse_result.success) {
        printf("ERROR: Failed to parse.\n");
        return 1;
    }

    
    // printf("AST:\n");
    // print_ast(parse_result.ast);

    // 3) AST → Stack Machine IR
    Stack_Machine_Instructions ir = generate_stack_machine_instructions(parse_result.ast);

  
    // printf("Stack Machine:\n");
    // print_stack_machine(&ir);
    // printf("Simulated result = %ld\n", simulate_stack_machine(&ir));

    // 4) Stack Machine IR → x86-64 Assembly
    FILE *out_file = fopen(options.out_file_name, "w");
    if (!out_file) {
        printf("ERROR: Could not open %s for writing.\n", options.out_file_name);
        return 1;
    }
    if (!generate_asm_from_ir(&ir, out_file)) {
        printf("ERROR: Failed to generate assembly.\n");
        fclose(out_file);
        return 1;
    }
    fclose(out_file);

    printf("OK: wrote %s\n", options.out_file_name);
    return 0;
}
