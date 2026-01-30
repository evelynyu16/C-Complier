// jivec.c — single-file Jive compiler with let/set + stack machine IR + x86-64 asm
// Build: clang -std=c11 -Wall -Wextra -Werror -g jivec.c -o jivec

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdint.h>

// ===================== String =====================
typedef struct String {
    char *data;
    long count;
} String;

#define PRINT_STRING(s) (int)(s).count, (s).data

// ===================== Locations & Tokens (lexer) =====================
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

    TOKEN_SYMBOL,       // generic

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
    String     source; // textual lexeme
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
        exit(1);
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

static void print_token(const Token *t) {
    printf("%s:%ld:%ld\t", t->loc.file_name, t->loc.line, t->loc.col);
    printf("%-12s", kind_name(t->kind));
    switch (t->kind) {
        case TOKEN_KEYWORD:
            printf("\t%.*s\n", PRINT_STRING(t->source)); break;
        case TOKEN_TYPE:
            printf("\t%.*s\n", PRINT_STRING(t->source)); break;
        case TOKEN_INTEGER:
            printf("\t%ld\n", t->long_value); break;
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
    Token_Array toks; lex_all(path, buf, len, &toks);
    free(buf);
    return toks;
}

// ===================== Symbol table =====================
typedef struct Symbol {
    String name;
    long   offset; 
    long   size;   
} Symbol;

typedef struct SymBucket {
    bool   used;
    String key;
    Symbol value;
} SymBucket;

typedef struct Symbol_Table {
    SymBucket *buckets;
    long       count;
    long       capacity;
} Symbol_Table;

static uint64_t fnv1a64(const char *s, size_t n) {
    uint64_t h = 1469598103934665603ull;
    for (size_t i=0;i<n;i++) { h ^= (unsigned char)s[i]; h *= 1099511628211ull; }
    return h;
}

static void symtab_init(Symbol_Table *st, long initial_cap) {
    st->count = 0;
    st->capacity = (initial_cap < 16) ? 16 : initial_cap;
    st->buckets = (SymBucket*)calloc((size_t)st->capacity, sizeof(SymBucket));
}
static void symtab_free(Symbol_Table *st) {
    free(st->buckets);
    st->buckets = NULL; st->count = 0; st->capacity = 0;
}

static void symtab_rehash(Symbol_Table *st);

static bool symtab_lookup(Symbol_Table *st, String key, Symbol *out) {
    uint64_t h = fnv1a64(key.data, (size_t)key.count);
    long cap = st->capacity;
    for (long i=0;i<cap;i++) {
        long idx = (long)((h + (uint64_t)i) % (uint64_t)cap);
        SymBucket *b = &st->buckets[idx];
        if (!b->used) return false;
        if (b->key.count == key.count && strncmp(b->key.data, key.data, (size_t)key.count)==0) {
            if (out) *out = b->value;
            return true;
        }
    }
    return false;
}
static bool symtab_insert(Symbol_Table *st, String key, Symbol value) {
    if ((st->count*10) >= (st->capacity*7)) { // ~0.7 load
        symtab_rehash(st);
    }
    uint64_t h = fnv1a64(key.data, (size_t)key.count);
    long cap = st->capacity;
    for (long i=0;i<cap;i++) {
        long idx = (long)((h + (uint64_t)i) % (uint64_t)cap);
        SymBucket *b = &st->buckets[idx];
        if (!b->used) {
            b->used = true;
            b->key = key;
            b->value = value;
            st->count++;
            return true;
        }
        if (b->key.count == key.count && strncmp(b->key.data, key.data, (size_t)key.count)==0) {
            return false; // duplicate
        }
    }
    return false;
}
static void symtab_rehash(Symbol_Table *st) {
    SymBucket *old = st->buckets;
    long oldcap = st->capacity;
    st->capacity *= 2;
    st->buckets = (SymBucket*)calloc((size_t)st->capacity, sizeof(SymBucket));
    st->count = 0;
    for (long i=0;i<oldcap;i++) {
        if (old[i].used) {
            symtab_insert(st, old[i].key, old[i].value);
        }
    }
    free(old);
}

// wrappers
static bool sym_lookup(Symbol_Table *st, String name, Symbol *out) { return symtab_lookup(st, name, out); }
static bool sym_insert(Symbol_Table *st, String name, long offset, long size) {
    Symbol s = { .name = name, .offset = offset, .size = size };
    return symtab_insert(st, name, s);
}
static void sym_init(Symbol_Table *st) { symtab_init(st, 16); }
static void sym_free(Symbol_Table *st) { symtab_free(st); }

// ===================== AST & Parser =====================
typedef enum AST_Kind {
    AST_NONE,
    AST_PROGRAM,
    AST_FN,
    AST_TYPE,
    AST_RETURN,
    AST_INTEGER,
    AST_IDENT,
    AST_BINOP,
    AST_LET,
    AST_SET,
} AST_Kind;

typedef enum BinOp {
    BIN_ADD, BIN_SUB, BIN_MUL, BIN_DIV, BIN_MOD
} BinOp;

const char *ast_kind_as_cstr(AST_Kind k){
    switch(k){
        case AST_PROGRAM: return "PROGRAM";
        case AST_FN: return "FN";
        case AST_TYPE: return "TYPE";
        case AST_RETURN: return "RETURN";
        case AST_INTEGER: return "INTEGER";
        case AST_IDENT: return "IDENT";
        case AST_BINOP: return "BINOP";
        case AST_LET: return "LET";
        case AST_SET: return "SET";
        default: return "NONE";
    }
}

typedef struct AST_Node AST_Node;
typedef struct AST_List {
    long count;
    AST_Node *first;
    AST_Node *last;
} AST_List;

typedef struct AST_Fn_Data {
    String name;
    Type   return_type;
    AST_List body;
} AST_Fn_Data;

struct AST_Node {
    AST_Kind kind;
    AST_Node *prev, *next;
    Loc      loc;      // <--- added for precise error locations
    union {
        AST_List program;   // PROGRAM
        AST_Fn_Data fn;     // FN
        Type type;          // TYPE
        long int_value;     // INTEGER
        String ident;       // IDENT
        struct {            // BINOP
            BinOp op;
            AST_Node *lhs;
            AST_Node *rhs;
        } bin;
        AST_Node *ret_expr; // RETURN
        struct {            // LET
            String name;
            Type   type;
            AST_Node *init; // optional
        } let_;
        struct {            // SET
            String name;
            AST_Node *expr;
        } set_;
    };
};

typedef struct Parser {
    Token_Array tokens;
    long tok_index;
    bool has_error;
} Parser;

static AST_Node *make_node(AST_Kind k){
    AST_Node *n=calloc(1,sizeof(AST_Node));
    n->kind=k;
    n->loc.file_name = NULL;
    n->loc.line = 0;
    n->loc.col = 0;
    return n;
}

static void ast_list_append(AST_List *list, AST_Node *node){
    node->prev = list->last;
    node->next = NULL;
    if (list->last) list->last->next = node;
    else list->first = node;
    list->last = node;
    list->count++;
}

static Token *peek_token(Parser *p, int off){
    long i = p->tok_index + off;
    if (i >= p->tokens.count) i = p->tokens.count - 1;
    return &p->tokens.items[i];
}
static bool check(Parser *p, Token_Kind k){ return peek_token(p,0)->kind==k; }
static bool check_sym(Parser *p, const char *s){
    Token *t = peek_token(p,0);
    return (t->kind==TOKEN_SYMBOL || t->kind==TOKEN_OPEN_PAREN || t->kind==TOKEN_CLOSE_PAREN
            || t->kind==TOKEN_OPEN_BRACE || t->kind==TOKEN_CLOSE_BRACE)
        && strcmp(t->source.data, s)==0;
}
static Token *advance_tok(Parser *p){ return &p->tokens.items[p->tok_index++]; }

static void report(Parser *p, Token *t, const char *msg){
    fprintf(stderr, "%s:%ld:%ld: %s", t->loc.file_name, t->loc.line, t->loc.col, msg);
    p->has_error = true;
}

static Token *expect_kind(Parser *p, Token_Kind k, const char *what){
    Token *t = peek_token(p,0);
    if (t->kind != k) {
        report(p, t, "Unexpected token\n");
        fprintf(stderr, "  expected %s, got kind %d (%s)\n", what, (int)t->kind, t->source.data);
        return t;
    }
    advance_tok(p);
    return t;
}

static bool match_sym(Parser *p, const char *s){
    if (check_sym(p,s)) { advance_tok(p); return true; }
    return false;
}

static AST_Node *parse_expr(Parser *p); // fwd

static AST_Node *parse_primary(Parser *p){
    Token *t = peek_token(p,0);
    if (t->kind == TOKEN_INTEGER) {
        advance_tok(p);
        AST_Node *n = make_node(AST_INTEGER);
        n->int_value = t->long_value;
        n->loc = t->loc;
        return n;
    }
    if (t->kind == TOKEN_IDENT) {
        advance_tok(p);
        AST_Node *n = make_node(AST_IDENT);
        n->ident = t->source; // borrow string
        n->loc   = t->loc;
        return n;
    }
    if (match_sym(p, "(")) {
        AST_Node *inside = parse_expr(p);
        if (!match_sym(p, ")")) report(p, peek_token(p,0), "expected ')'\n");
        return inside;
    }
    report(p, t, "expected expression\n");
    return make_node(AST_NONE);
}

static AST_Node *parse_term(Parser *p){
    AST_Node *lhs = parse_primary(p);
    while (check_sym(p,"*") || check_sym(p,"/") || check_sym(p,"%")){
        Token *op = advance_tok(p);
        AST_Node *rhs = parse_primary(p);
        AST_Node *bin = make_node(AST_BINOP);
        bin->loc = op->loc;
        bin->bin.lhs = lhs;
        bin->bin.rhs = rhs;
        switch(op->source.data[0]){
            case '*': bin->bin.op = BIN_MUL; break;
            case '/': bin->bin.op = BIN_DIV; break;
            default:  bin->bin.op = BIN_MOD; break;
        }
        lhs = bin;
    }
    return lhs;
}

static AST_Node *parse_expr(Parser *p){
    AST_Node *lhs = parse_term(p);
    while (check_sym(p,"+") || check_sym(p,"-")){
        Token *op = advance_tok(p);
        AST_Node *rhs = parse_term(p);
        AST_Node *bin = make_node(AST_BINOP);
        bin->loc = op->loc;
        bin->bin.lhs = lhs;
        bin->bin.rhs = rhs;
        bin->bin.op = (op->source.data[0]=='+') ? BIN_ADD : BIN_SUB;
        lhs = bin;
    }
    return lhs;
}

static AST_Node *parse_statement(Parser *p);

static AST_List parse_block(Parser *p){
    AST_List list = {0};
    expect_kind(p, TOKEN_OPEN_BRACE, "'{'");
    while (!check(p, TOKEN_EOF) && !check(p, TOKEN_CLOSE_BRACE)) {
        AST_Node *st = parse_statement(p);
        if (st && st->kind!=AST_NONE) ast_list_append(&list, st);
    }
    expect_kind(p, TOKEN_CLOSE_BRACE, "'}'");
    return list;
}

static AST_Node *parse_return(Parser *p){
    expect_kind(p, TOKEN_KEYWORD, "return");
    // previous token was 'return'
    Token *rtok = &p->tokens.items[p->tok_index-1];
    if (rtok->keyword != KEYWORD_RETURN) {
        report(p, rtok, "expected 'return'\n");
        return make_node(AST_NONE);
    }
    AST_Node *n = make_node(AST_RETURN);
    n->loc = rtok->loc;
    if (!check(p, TOKEN_CLOSE_BRACE)) {
        n->ret_expr = parse_expr(p);
    }
    return n;
}

static AST_Node *parse_let(Parser *p){
    expect_kind(p, TOKEN_KEYWORD, "let");
    Token *ltok = &p->tokens.items[p->tok_index-1];
    if (ltok->keyword != KEYWORD_LET) { report(p, ltok, "expected 'let'\n"); return make_node(AST_NONE); }

    Token *name = expect_kind(p, TOKEN_IDENT, "identifier");
    expect_kind(p, TOKEN_COLON, "':'");
    Token *typ = expect_kind(p, TOKEN_TYPE, "type");

    AST_Node *n = make_node(AST_LET);
    n->loc = name->loc; // point to variable name
    n->let_.name = name->source;
    n->let_.type = typ->type;
    n->let_.init = NULL;

    if (match_sym(p, "=")) {
        n->let_.init = parse_expr(p);
    }
    return n;
}

static AST_Node *parse_set(Parser *p){
    expect_kind(p, TOKEN_KEYWORD, "set");
    Token *stok = &p->tokens.items[p->tok_index-1];
    if (stok->keyword != KEYWORD_SET) { report(p, stok, "expected 'set'\n"); return make_node(AST_NONE); }

    Token *name = expect_kind(p, TOKEN_IDENT, "identifier");
    if (!match_sym(p, "=")) { report(p, peek_token(p,0), "expected '=''\n"); }
    AST_Node *expr = parse_expr(p);

    AST_Node *n = make_node(AST_SET);
    n->loc = name->loc;
    n->set_.name = name->source;
    n->set_.expr = expr;
    return n;
}

static AST_Node *parse_statement(Parser *p){
    Token *t = peek_token(p,0);
    if (t->kind == TOKEN_KEYWORD) {
        if (t->keyword == KEYWORD_RETURN) return parse_return(p);
        if (t->keyword == KEYWORD_LET)    return parse_let(p);
        if (t->keyword == KEYWORD_SET)    return parse_set(p);
    }
    report(p, t, "expected statement (let/set/return)\n");
    advance_tok(p);
    return make_node(AST_NONE);
}

static AST_Node *parse_fn_def(Parser *p){
    Token *kw = expect_kind(p, TOKEN_KEYWORD, "fn");
    if (kw->keyword != KEYWORD_FN) { report(p, kw, "expected 'fn'\n"); return make_node(AST_NONE); }

    Token *name = expect_kind(p, TOKEN_IDENT, "identifier");
    expect_kind(p, TOKEN_OPEN_PAREN, "'('");
    expect_kind(p, TOKEN_CLOSE_PAREN,"')'");
    expect_kind(p, TOKEN_ARROW, "'->'");
    Token *rt = expect_kind(p, TOKEN_TYPE, "return type");

    AST_Node *fn = make_node(AST_FN);
    fn->loc = name->loc;
    fn->fn.name = name->source;
    fn->fn.return_type = rt->type;
    fn->fn.body = parse_block(p);
    return fn;
}

typedef struct Parse_Result {
    AST_Node *ast;
    bool success;
} Parse_Result;

static Parse_Result parse_program(Token_Array toks){
    Parser p = {.tokens=toks, .tok_index=0, .has_error=false};
    AST_Node *root = make_node(AST_PROGRAM);
    while (!p.has_error && !check(&p, TOKEN_EOF)) {
        AST_Node *fn = parse_fn_def(&p);
        ast_list_append(&root->program, fn);
    }
    return (Parse_Result){ .ast=root, .success = !p.has_error };
}

// for debugging
static const char* binop_name(BinOp op){
    switch(op){
        case BIN_ADD: return "+";
        case BIN_SUB: return "-";
        case BIN_MUL: return "*";
        case BIN_DIV: return "/";
        case BIN_MOD: return "%";
        default: return "?";
    }
}
static void print_ast_with_indent(AST_Node *node, int depth);
static void print_ast(AST_Node *node){ print_ast_with_indent(node, 0); }
static void print_ast_with_indent(AST_Node *node, int depth){
    if (!node) return;
    #define IND do{ for(int i=0;i<depth*2;i++) putchar(' ');}while(0)
    switch(node->kind){
        case AST_PROGRAM:
            IND; printf("program\n");
            for (AST_Node *fn=node->program.first; fn; fn=fn->next) print_ast_with_indent(fn, depth+1);
            break;
        case AST_FN:
            IND; printf("fn %.*s() -> int\n", PRINT_STRING(node->fn.name));
            for (AST_Node *s=node->fn.body.first; s; s=s->next) print_ast_with_indent(s, depth+1);
            break;
        case AST_RETURN:
            IND; printf("return\n");
            print_ast_with_indent(node->ret_expr, depth+1);
            break;
        case AST_INTEGER:
            IND; printf("integer %ld\n", node->int_value);
            break;
        case AST_IDENT:
            IND; printf("ident %.*s\n", PRINT_STRING(node->ident));
            break;
        case AST_BINOP:
            IND; printf("binop %s\n", binop_name(node->bin.op));
            print_ast_with_indent(node->bin.lhs, depth+1);
            print_ast_with_indent(node->bin.rhs, depth+1);
            break;
        case AST_LET:
            IND; printf("let %.*s: int\n", PRINT_STRING(node->let_.name));
            if (node->let_.init){ print_ast_with_indent(node->let_.init, depth+1); }
            break;
        case AST_SET:
            IND; printf("set %.*s =\n", PRINT_STRING(node->set_.name));
            print_ast_with_indent(node->set_.expr, depth+1);
            break;
        default:
            IND; printf("UNHANDLED %d\n", node->kind);
            break;
    }
    #undef IND
}

// ===================== Stack machine IR =====================
#define da_append(da, item) \
do { \
    if ((da)->count >= (da)->capacity) { \
        (da)->capacity = (da)->capacity ? (da)->capacity*2 : 16; \
        (da)->items = realloc((da)->items, (da)->capacity*sizeof((da)->items[0])); \
    } \
    (da)->items[(da)->count++] = (item); \
} while(false)

typedef enum SM_Kind {
    SM_NONE,
    SM_FN,         
    SM_END_FN,
    SM_RETURN,

    SM_PUSH,       
    SM_LOAD_LOCAL, 
    SM_STORE_LOCAL,

    SM_ADD, SM_SUB, SM_MUL, SM_DIV, SM_MOD,
} SM_Kind;

typedef struct SM_Instruction {
    SM_Kind kind;
    long    a;    
} SM_Instruction;

typedef struct Stack_Machine_Instructions {
    SM_Instruction *items;
    long count, capacity;
} Stack_Machine_Instructions;

// Constructors
static SM_Instruction sm_fn(long locals)       { SM_Instruction i={.kind=SM_FN,.a=locals}; return i; }
static SM_Instruction sm_endfn(void)           { SM_Instruction i={.kind=SM_END_FN}; return i; }
static SM_Instruction sm_ret(void)             { SM_Instruction i={.kind=SM_RETURN}; return i; }
static SM_Instruction sm_push(long v)          { SM_Instruction i={.kind=SM_PUSH,.a=v}; return i; }
static SM_Instruction sm_load(long slot)       { SM_Instruction i={.kind=SM_LOAD_LOCAL,.a=slot}; return i; }
static SM_Instruction sm_store(long slot)      { SM_Instruction i={.kind=SM_STORE_LOCAL,.a=slot}; return i; }
static SM_Instruction sm_add(void)             { SM_Instruction i={.kind=SM_ADD}; return i; }
static SM_Instruction sm_sub(void)             { SM_Instruction i={.kind=SM_SUB}; return i; }
static SM_Instruction sm_mul(void)             { SM_Instruction i={.kind=SM_MUL}; return i; }
static SM_Instruction sm_div(void)             { SM_Instruction i={.kind=SM_DIV}; return i; }
static SM_Instruction sm_mod(void)             { SM_Instruction i={.kind=SM_MOD}; return i; }

static const char* sm_name(SM_Kind k){
    switch(k){
        case SM_FN: return "FN";
        case SM_END_FN: return "END_FN";
        case SM_RETURN: return "RETURN";
        case SM_PUSH: return "PUSH";
        case SM_LOAD_LOCAL: return "LOADL";
        case SM_STORE_LOCAL: return "STOREL";
        case SM_ADD: return "ADD";
        case SM_SUB: return "SUB";
        case SM_MUL: return "MUL";
        case SM_DIV: return "DIV";
        case SM_MOD: return "MOD";
        default: return "?";
    }
}

static void print_sm(Stack_Machine_Instructions *sm){
    for(long i=0;i<sm->count;i++){
        SM_Instruction in = sm->items[i];
        switch(in.kind){
            case SM_FN:         printf("FN %ld\n", in.a); break;
            case SM_END_FN:     printf("END_FN\n"); break;
            case SM_RETURN:     printf("RETURN\n"); break;
            case SM_PUSH:       printf("PUSH %ld\n", in.a); break;
            case SM_LOAD_LOCAL: printf("LOAD_LOCAL %ld\n", in.a); break;
            case SM_STORE_LOCAL:printf("STORE_LOCAL %ld\n", in.a); break;
            case SM_ADD: case SM_SUB: case SM_MUL: case SM_DIV: case SM_MOD:
                                 printf("%s\n", sm_name(in.kind)); break;
            default:            printf("? (kind=%d)\n", (int)in.kind); break;
        }
    }
}

// ===================== IR generation (AST → IR with locations in errors) =====================
static void gen_expr(AST_Node *e, Stack_Machine_Instructions *out, Symbol_Table *st);

static long slot_index_from_offset(long offset){
    return (long)((-offset)/8) - 1; 
}

static void gen_stmt(AST_Node *s, Stack_Machine_Instructions *out, Symbol_Table *st, long *next_offset){
    switch(s->kind){
        case AST_LET: {
            Symbol dummy;
            if (sym_lookup(st, s->let_.name, &dummy)) {
                fprintf(stderr, "%s:%ld:%ld: error: variable '%.*s' already declared in this scope\n",
                        s->loc.file_name, s->loc.line, s->loc.col,
                        PRINT_STRING(s->let_.name));
                exit(1);
            }
            *next_offset -= 8;
            long offset = *next_offset; // -8, -16, ...
            sym_insert(st, s->let_.name, offset, 8);
            if (s->let_.init){
                gen_expr(s->let_.init, out, st);
                da_append(out, sm_store(slot_index_from_offset(offset)));
            }
        } break;
        case AST_SET: {
            Symbol sym;
            if (!sym_lookup(st, s->set_.name, &sym)) {
                fprintf(stderr, "%s:%ld:%ld: error: variable '%.*s' not declared\n",
                        s->loc.file_name, s->loc.line, s->loc.col,
                        PRINT_STRING(s->set_.name));
                exit(1);
            }
            gen_expr(s->set_.expr, out, st);
            da_append(out, sm_store(slot_index_from_offset(sym.offset)));
        } break;
        case AST_RETURN: {
            if (s->ret_expr) gen_expr(s->ret_expr, out, st);
            da_append(out, sm_ret());
        } break;
        default: break;
    }
}

static void gen_expr(AST_Node *e, Stack_Machine_Instructions *out, Symbol_Table *st){
    switch(e->kind){
        case AST_INTEGER:
            da_append(out, sm_push(e->int_value));
            break;
        case AST_IDENT: {
            Symbol sym;
            if (!sym_lookup(st, e->ident, &sym)) {
                fprintf(stderr, "%s:%ld:%ld: error: variable '%.*s' not declared\n",
                        e->loc.file_name, e->loc.line, e->loc.col,
                        PRINT_STRING(e->ident));
                exit(1);
            }
            da_append(out, sm_load(slot_index_from_offset(sym.offset)));
        } break;
        case AST_BINOP:
            gen_expr(e->bin.lhs, out, st);
            gen_expr(e->bin.rhs, out, st);
            switch(e->bin.op){
                case BIN_ADD: da_append(out, sm_add()); break;
                case BIN_SUB: da_append(out, sm_sub()); break;
                case BIN_MUL: da_append(out, sm_mul()); break;
                case BIN_DIV: da_append(out, sm_div()); break;
                case BIN_MOD: da_append(out, sm_mod()); break;
            }
            break;
        default:
            break;
    }
}

static Stack_Machine_Instructions generate_stack_machine(AST_Node *program){
    Stack_Machine_Instructions out = {0};
    if (!program || program->kind != AST_PROGRAM) return out;

    for (AST_Node *fn = program->program.first; fn; fn=fn->next){
        if (fn->kind != AST_FN) continue;
        Symbol_Table st; sym_init(&st);
        long next_offset = 0; 

        // first pass: count locals + duplicate check with location
        for (AST_Node *s=fn->fn.body.first; s; s=s->next){
            if (s->kind == AST_LET){
                Symbol dummy;
                if (sym_lookup(&st, s->let_.name, &dummy)) {
                    fprintf(stderr, "%s:%ld:%ld: error: duplicate '%.*s'\n",
                            s->loc.file_name, s->loc.line, s->loc.col,
                            PRINT_STRING(s->let_.name));
                    exit(1);
                } else {
                    next_offset -= 8;
                    sym_insert(&st, s->let_.name, next_offset, 8);
                }
            }
        }
        long locals = (-next_offset)/8; 

        sym_free(&st); sym_init(&st); next_offset = 0;
        da_append(&out, sm_fn(locals));
        for (AST_Node *s=fn->fn.body.first; s; s=s->next){
            gen_stmt(s, &out, &st, &next_offset);
        }
        da_append(&out, sm_endfn());
        sym_free(&st);
    }
    return out;
}

// ===================== IR → x86-64 assembly =====================
// Intel syntax, NASM-style, with _start calling main.

static long local_offset(long slot) {
    return (slot + 1) * 8; // 8 bytes per local
}

static bool generate_asm_from_stack_machine(Stack_Machine_Instructions *sm, FILE *out) {
    // Preamble: program entry
    fprintf(out, "global _start\n\n");
    fprintf(out, "_start:\n");
    fprintf(out, "    call main\n");
    fprintf(out, "    mov rdi, rax\n");
    fprintf(out, "    mov rax, 60\n");
    fprintf(out, "    syscall\n\n");

    long locals = 0;
    bool in_fn = false;
    bool first_fn = true;

    for (long i = 0; i < sm->count; ++i) {
        SM_Instruction ins = sm->items[i];
        switch (ins.kind) {
            case SM_FN: {
                locals = ins.a;
                // 本作业只需要 main，可以假定唯一函数是 main
                const char *fn_name = first_fn ? "main" : "main"; // 多函数不要求
                first_fn = false;

                fprintf(out, "%s:\n", fn_name);
                fprintf(out, "    push rbp\n");
                fprintf(out, "    mov rbp, rsp\n");
                if (locals > 0) {
                    fprintf(out, "    sub rsp, %ld\n", locals * 8);
                }
                in_fn = true;
            } break;

            case SM_END_FN: {
                if (in_fn) {
                    // 防御：如果没有显式 RETURN，就默认返回 0
                    fprintf(out, "    mov rax, 0\n");
                    fprintf(out, "    leave\n");
                    fprintf(out, "    ret\n");
                    in_fn = false;
                }
            } break;

            case SM_PUSH:
                fprintf(out, "    push %ld\n", ins.a);
                break;

            case SM_LOAD_LOCAL: {
                long off = local_offset(ins.a);
                fprintf(out, "    mov rax, [rbp-%ld]\n", off);
                fprintf(out, "    push rax\n");
            } break;

            case SM_STORE_LOCAL: {
                long off = local_offset(ins.a);
                fprintf(out, "    pop rax\n");
                fprintf(out, "    mov [rbp-%ld], rax\n", off);
            } break;

            case SM_ADD:
                fprintf(out, "    pop rbx\n");
                fprintf(out, "    pop rax\n");
                fprintf(out, "    add rax, rbx\n");
                fprintf(out, "    push rax\n");
                break;

            case SM_SUB:
                fprintf(out, "    pop rbx\n");
                fprintf(out, "    pop rax\n");
                fprintf(out, "    sub rax, rbx\n");
                fprintf(out, "    push rax\n");
                break;

            case SM_MUL:
                fprintf(out, "    pop rbx\n");
                fprintf(out, "    pop rax\n");
                fprintf(out, "    imul rax, rbx\n");
                fprintf(out, "    push rax\n");
                break;

            case SM_DIV:
            case SM_MOD:
                fprintf(out, "    pop rbx\n");  // divisor
                fprintf(out, "    pop rax\n");  // dividend
                fprintf(out, "    cqo\n");      // sign-extend rax into rdx:rax
                fprintf(out, "    idiv rbx\n");
                if (ins.kind == SM_DIV) {
                    fprintf(out, "    push rax\n");
                } else {
                    fprintf(out, "    push rdx\n");
                }
                break;

            case SM_RETURN:
                fprintf(out, "    pop rax\n");
                fprintf(out, "    leave\n");
                fprintf(out, "    ret\n");
                in_fn = false;
                break;

            default:
                fprintf(stderr, "ERROR: unhandled SM opcode %d in asm gen\n", (int)ins.kind);
                return false;
        }
    }

    return true;
}

// ===================== main: AST → IR → ASM =====================
int main(int argc, const char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s input.jive [output.asm]\n", argc>0?argv[0]:"jivec");
        return 1;
    }
    const char *in_file  = argv[1];
    const char *out_file = (argc >= 3) ? argv[2] : "out.asm";

    Token_Array tokens = lex_file(in_file);

    // 可选：想看词法输出就打开
    // print_token_array(tokens);

    Parse_Result pr = parse_program(tokens);
    if (!pr.success) {
        fprintf(stderr, "parse failed\n");
        return 1;
    }

    
    // print_ast(pr.ast);

    // AST → stack machine IR
    Stack_Machine_Instructions sm = generate_stack_machine(pr.ast);

  
    // print_sm(&sm);

    FILE *out = fopen(out_file, "w");
    if (!out) {
        fprintf(stderr, "ERROR: could not open %s for writing\n", out_file);
        return 1;
    }

    if (!generate_asm_from_stack_machine(&sm, out)) {
        fprintf(stderr, "ERROR: assembly generation failed\n");
        fclose(out);
        return 1;
    }
    fclose(out);

    printf("OK: wrote %s\n", out_file);
    return 0;
}
