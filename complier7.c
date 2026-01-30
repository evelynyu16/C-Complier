// compiler7_gol.c — Jive interpreter-style compiler supporting GOL syntax


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdint.h>

// ===================== Utilities& String =====================

static void *xmalloc(size_t n) {
  void *p = malloc(n);
  if (!p) { fprintf(stderr, "Out of memory\n"); exit(1); }
  return p;
}
static void *xcalloc(size_t n, size_t sz) {
  void *p = calloc(n, sz);
  if (!p) { fprintf(stderr, "Out of memory\n"); exit(1); }
  return p;
}
static char *xstrndup_(const char *s, size_t n) {
  char *p = (char*)xmalloc(n + 1);
  memcpy(p, s, n);
  p[n] = '\0';
  return p;
}

typedef struct String {
  char *data;
  long count;
} String;

#define STRLIT(s) (String){ (char*)(s), (long)(sizeof(s)-1) }
#define PRINT_STRING(s) (int)(s).count, (s).data

static bool streq(String a, String b) {
  return a.count == b.count && strncmp(a.data, b.data, (size_t)a.count) == 0;
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

  TOKEN_SYMBOL,        // operators like + - * / % == <= 

  TOKEN_OPEN_PAREN,    // (
  TOKEN_CLOSE_PAREN,   // )
  TOKEN_OPEN_BRACE,    // {
  TOKEN_CLOSE_BRACE,   // }
  TOKEN_OPEN_BRACKET,  // [
  TOKEN_CLOSE_BRACKET, // ]
  TOKEN_COMMA,         // ,
  TOKEN_COLON,         // :
  TOKEN_ARROW          // ->
} Token_Kind;

typedef enum Keyword {
  KEYWORD_UNKNOWN = 0,
  KEYWORD_FN,
  KEYWORD_LET,
  KEYWORD_SET,
  KEYWORD_IF,
  KEYWORD_ELSE,
  KEYWORD_WHILE,
  KEYWORD_CALL,
  KEYWORD_RETURN,
  KEYWORD_TRUE,
  KEYWORD_FALSE,
} Keyword;

typedef enum BaseType {
  BASE_UNKNOWN = 0,
  BASE_INT,
  BASE_STR,
  BASE_BOOL,
  BASE_ANY
} BaseType;

typedef struct TypeDesc {
  BaseType base;
  int indir; 
} TypeDesc;

typedef struct Token {
  Token_Kind kind;
  String source; // lexeme text 
  Loc loc;
  union {
    long long_value;
    Keyword keyword;
    BaseType type_base;
  };
} Token;

typedef struct Token_Array {
  Token *items;
  long count;
  long capacity;
} Token_Array;

static void token_array_init(Token_Array *a) {
  a->items = NULL; a->count = 0; a->capacity = 0;
}
static void token_array_push(Token_Array *a, Token t) {
  if (a->count == a->capacity) {
    a->capacity = a->capacity ? a->capacity * 2 : 64;
    a->items = (Token*)realloc(a->items, (size_t)a->capacity * sizeof(Token));
    if (!a->items) { fprintf(stderr, "Out of memory\n"); exit(1); }
  }
  a->items[a->count++] = t;
}

typedef struct Scanner {
  const char *file_name;
  const char *cur;
  const char *end;
  long line;
  long col;
} Scanner;

static void scanner_init(Scanner *s, const char *file_name, const char *buf, size_t len) {
  s->file_name = file_name;
  s->cur = buf;
  s->end = buf + len;
  s->line = 1;
  s->col = 1;
}

static bool is_at_end(Scanner *s) { return s->cur >= s->end; }
static char peek(Scanner *s) { return is_at_end(s) ? '\0' : *s->cur; }
static char peek_next(Scanner *s) { return (s->cur + 1 >= s->end) ? '\0' : s->cur[1]; }

static char advance(Scanner *s) {
  if (is_at_end(s)) return '\0';
  char c = *s->cur++;
  if (c == '\n') { s->line++; s->col = 1; }
  else s->col++;
  return c;
}

static void skip_ws_and_comments(Scanner *s) {
  for (;;) {
    char c = peek(s);
    if (c==' '||c=='\t'||c=='\r'||c=='\f'||c=='\v'||c=='\n') { advance(s); continue; }
    if (c=='/' && peek_next(s)=='/') {
      while (!is_at_end(s) && peek(s)!='\n') advance(s);
      continue;
    }
    break;
  }
}

static bool is_ident_start(char c) { return isalpha((unsigned char)c) || c=='_'; }
static bool is_ident_part(char c) { return isalnum((unsigned char)c) || c=='_'; }

typedef struct { const char *text; Keyword kw; } KwEnt;
static const KwEnt KW_TABLE[] = {
  {"fn", KEYWORD_FN},
  {"let", KEYWORD_LET},
  {"set", KEYWORD_SET},
  {"if", KEYWORD_IF},
  {"else", KEYWORD_ELSE},
  {"while", KEYWORD_WHILE},
  {"call", KEYWORD_CALL},
  {"return", KEYWORD_RETURN},
  {"true", KEYWORD_TRUE},
  {"false", KEYWORD_FALSE},
};

typedef struct { const char *text; BaseType ty; } TyEnt;
static const TyEnt TY_TABLE[] = {
  {"int", BASE_INT},
  {"str", BASE_STR},
  {"bool", BASE_BOOL},
  {"any", BASE_ANY},
};

static Keyword lookup_keyword(const char *s, size_t n) {
  for (size_t i=0;i<sizeof(KW_TABLE)/sizeof(KW_TABLE[0]);++i)
    if (strlen(KW_TABLE[i].text)==n && strncmp(s,KW_TABLE[i].text,n)==0)
      return KW_TABLE[i].kw;
  return KEYWORD_UNKNOWN;
}
static BaseType lookup_typebase(const char *s, size_t n) {
  for (size_t i=0;i<sizeof(TY_TABLE)/sizeof(TY_TABLE[0]);++i)
    if (strlen(TY_TABLE[i].text)==n && strncmp(s,TY_TABLE[i].text,n)==0)
      return TY_TABLE[i].ty;
  return BASE_UNKNOWN;
}

static Token make_token(Token_Kind k, Scanner *s, const char *start, size_t n, long line, long col) {
  Token t;
  t.kind = k;
  t.source.data = xstrndup_(start, n);
  t.source.count = (long)n;
  t.loc.file_name = s->file_name;
  t.loc.line = line;
  t.loc.col = col;
  t.long_value = 0;
  t.keyword = KEYWORD_UNKNOWN;
  t.type_base = BASE_UNKNOWN;
  return t;
}

static Token scan_number(Scanner *s) {
  long line = s->line, col = s->col;
  const char *start = s->cur;
  while (isdigit((unsigned char)peek(s))) advance(s);
  size_t n = (size_t)(s->cur - start);
  Token t = make_token(TOKEN_INTEGER, s, start, n, line, col);
  char tmp[64];
  size_t m = n < sizeof(tmp)-1 ? n : sizeof(tmp)-1;
  memcpy(tmp, start, m); tmp[m] = '\0';
  t.long_value = strtol(tmp, NULL, 10);
  return t;
}

static Token scan_string(Scanner *s) {
  long line = s->line, col = s->col;
  advance(s); // consume opening "
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
        default: c = e; break;
      }
    }
    if (n + 1 >= cap) {
      cap *= 2;
      buf = (char*)realloc(buf, cap);
      if (!buf) { fprintf(stderr, "Out of memory\n"); exit(1); }
    }
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
  t.loc.file_name = s->file_name;
  t.loc.line = line;
  t.loc.col = col;
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
    Token t = make_token(TOKEN_KEYWORD, s, start, n, line, col);
    t.keyword = kw;
    return t;
  }

  BaseType tb = lookup_typebase(start, n);
  if (tb != BASE_UNKNOWN) {
    Token t = make_token(TOKEN_TYPE, s, start, n, line, col);
    t.type_base = tb;
    return t;
  }

  return make_token(TOKEN_IDENT, s, start, n, line, col);
}

static Token scan_symbol(Scanner *s) {
  long line = s->line, col = s->col;
  const char *start = s->cur;
  char c = advance(s);

  // multi-char operators
  if (c=='-' && peek(s)=='>' ) { advance(s); return make_token(TOKEN_ARROW, s, start, 2, line, col); }
  if (c=='=' && peek(s)=='=' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }
  if (c=='!' && peek(s)=='=' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }
  if (c=='<' && peek(s)=='=' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }
  if (c=='>' && peek(s)=='=' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }
  if (c=='&' && peek(s)=='&' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }
  if (c=='|' && peek(s)=='|' ) { advance(s); return make_token(TOKEN_SYMBOL, s, start, 2, line, col); }

  // single-char punctuation
  switch (c) {
    case '(': return make_token(TOKEN_OPEN_PAREN, s, start, 1, line, col);
    case ')': return make_token(TOKEN_CLOSE_PAREN, s, start, 1, line, col);
    case '{': return make_token(TOKEN_OPEN_BRACE, s, start, 1, line, col);
    case '}': return make_token(TOKEN_CLOSE_BRACE, s, start, 1, line, col);
    case '[': return make_token(TOKEN_OPEN_BRACKET, s, start, 1, line, col);
    case ']': return make_token(TOKEN_CLOSE_BRACKET, s, start, 1, line, col);
    case ',': return make_token(TOKEN_COMMA, s, start, 1, line, col);
    case ':': return make_token(TOKEN_COLON, s, start, 1, line, col);
    default: break;
  }

  return make_token(TOKEN_SYMBOL, s, start, 1, line, col);
}

static Token scan_token(Scanner *s) {
  skip_ws_and_comments(s);
  if (is_at_end(s)) {
    Token t;
    t.kind = TOKEN_EOF;
    t.source.data = xstrndup_("", 0);
    t.source.count = 0;
    t.loc.file_name = s->file_name;
    t.loc.line = s->line;
    t.loc.col = s->col;
    return t;
  }

  char c = peek(s);
  if (is_ident_start(c)) return scan_ident_like(s);
  if (isdigit((unsigned char)c)) return scan_number(s);
  if (c == '"') return scan_string(s);
  return scan_symbol(s);
}

static const char *kind_name(Token_Kind k) {
  switch (k) {
    case TOKEN_EOF: return "EOF";
    case TOKEN_KEYWORD: return "KEYWORD";
    case TOKEN_IDENT: return "IDENT";
    case TOKEN_TYPE: return "TYPE";
    case TOKEN_INTEGER: return "INTEGER";
    case TOKEN_STRING: return "STRING";
    case TOKEN_SYMBOL: return "SYMBOL";
    case TOKEN_OPEN_PAREN: return "(";
    case TOKEN_CLOSE_PAREN: return ")";
    case TOKEN_OPEN_BRACE: return "{";
    case TOKEN_CLOSE_BRACE: return "}";
    case TOKEN_OPEN_BRACKET: return "[";
    case TOKEN_CLOSE_BRACKET: return "]";
    case TOKEN_COMMA: return ",";
    case TOKEN_COLON: return ":";
    case TOKEN_ARROW: return "->";
    default: return "?";
  }
}

static const char *kw_name(Keyword k) {
  switch (k) {
    case KEYWORD_FN: return "fn";
    case KEYWORD_LET: return "let";
    case KEYWORD_SET: return "set";
    case KEYWORD_IF: return "if";
    case KEYWORD_ELSE: return "else";
    case KEYWORD_WHILE: return "while";
    case KEYWORD_CALL: return "call";
    case KEYWORD_RETURN: return "return";
    case KEYWORD_TRUE: return "true";
    case KEYWORD_FALSE: return "false";
    default: return "?";
  }
}

static const char *base_name(BaseType b) {
  switch (b) {
    case BASE_INT: return "int";
    case BASE_STR: return "str";
    case BASE_BOOL: return "bool";
    case BASE_ANY: return "any";
    default: return "?";
  }
}

static void print_loc(Loc loc) { printf("%s:%ld:%ld", loc.file_name, loc.line, loc.col); }

static void print_token(const Token *t) {
  printf("%s:%ld:%ld\t%-8s\t", t->loc.file_name, t->loc.line, t->loc.col, kind_name(t->kind));
  switch (t->kind) {
    case TOKEN_KEYWORD: printf("%s\n", kw_name(t->keyword)); break;
    case TOKEN_TYPE: printf("%s\n", base_name(t->type_base)); break;
    case TOKEN_INTEGER: printf("%ld\n", t->long_value); break;
    default: printf("%s\n", t->source.data); break;
  }
}

static void print_token_array(Token_Array a) {
  for (long i=0;i<a.count;++i) print_token(&a.items[i]);
}

static char *read_entire_file(const char *path, size_t *out_len) {
  FILE *f = fopen(path, "rb");
  if (!f) return NULL;
  fseek(f, 0, SEEK_END);
  long sz = ftell(f);
  rewind(f);
  char *buf = (char*)xmalloc((size_t)sz + 1);
  size_t n = fread(buf, 1, (size_t)sz, f);
  fclose(f);
  buf[n] = '\0';
  if (out_len) *out_len = n;
  return buf;
}

static Token_Array lex_file(const char *path) {
  size_t len = 0;
  char *buf = read_entire_file(path, &len);
  if (!buf) { fprintf(stderr, "error: could not read '%s'\n", path); exit(1); }

  Token_Array toks;
  token_array_init(&toks);

  Scanner s;
  scanner_init(&s, path, buf, len);

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
  AST_NONE = 0,
  AST_PROGRAM,
  AST_FN,
  AST_PARAM,

  // statements
  AST_STMT_BLOCK,
  AST_STMT_LET,
  AST_STMT_SET,
  AST_STMT_SET_INDEX,
  AST_STMT_CALL,
  AST_STMT_RETURN,
  AST_STMT_IF,
  AST_STMT_WHILE,

  // expressions
  AST_EXPR_INT,
  AST_EXPR_BOOL,
  AST_EXPR_STR,
  AST_EXPR_IDENT,
  AST_EXPR_BINOP,
  AST_EXPR_CALL,
  AST_EXPR_INDEX
} AST_Kind;

typedef struct AST_Node AST_Node;

typedef struct AST_List {
  long count;
  AST_Node *first;
  AST_Node *last;
} AST_List;

typedef enum BinOp {
  OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD,
  OP_EQ, OP_NE, OP_LT, OP_LE, OP_GT, OP_GE,
  OP_AND, OP_OR
} BinOp;

typedef struct AST_Param {
  String name;
  TypeDesc type;
} AST_Param;

typedef struct AST_Call {
  String fn_name;
  AST_List args;
} AST_Call;

typedef struct AST_Fn {
  String name;
  AST_List params;    // PARAM
  bool has_return_type;
  TypeDesc return_type;
  AST_List body;      // statements
} AST_Fn;

typedef struct AST_Let {
  String name;
  TypeDesc type;
  AST_Node *init; 
} AST_Let;

typedef struct AST_Set {
  String name;
  AST_Node *value; 
} AST_Set;

typedef struct AST_SetIndex {
  AST_Node *base;   // expr 
  AST_Node *index;  // expr
  AST_Node *value;  // expr
} AST_SetIndex;

typedef struct AST_Return {
  AST_Node *expr;
} AST_Return;

typedef struct AST_If {
  AST_Node *cond;
  AST_Node *then_st; // statement 
  AST_Node *else_st; // optional statement or NULL
} AST_If;

typedef struct AST_While {
  AST_Node *cond;
  AST_List body;
} AST_While;

typedef struct AST_Block {
  AST_List body;
} AST_Block;

typedef struct AST_Bin {
  BinOp op;
  AST_Node *left;
  AST_Node *right;
} AST_Bin;

typedef struct AST_Index {
  AST_Node *base;
  AST_Node *index;
} AST_Index;

struct AST_Node {
  AST_Kind kind;
  AST_Node *prev;
  AST_Node *next;
  union {
    AST_List program;   // PROGRAM
    AST_Fn fn;          // FN
    AST_Param param;    // PARAM

    AST_Block block_st;
    AST_Let let_st;
    AST_Set set_st;
    AST_SetIndex set_index_st;
    AST_Call call;      // CALL stmt or CALL expr
    AST_Return ret_st;
    AST_If if_st;
    AST_While while_st;

    long int_value;
    bool bool_value;
    String str_value;   // owns char*
    String ident_name;
    AST_Bin bin;
    AST_Index index;
  };
};

static AST_Node *ast_make(AST_Kind k) {
  AST_Node *n = (AST_Node*)xcalloc(1, sizeof(AST_Node));
  n->kind = k;
  return n;
}

static void ast_list_append(AST_List *list, AST_Node *node) {
  node->prev = list->last;
  node->next = NULL;
  if (list->last) list->last->next = node;
  else list->first = node;
  list->last = node;
  list->count++;
}

// ===================== Parser =====================

typedef struct Parser {
  Token_Array tokens;
  long idx;
  bool has_error;
} Parser;

static Token *peek_tok(Parser *p, int off) {
  long i = p->idx + off;
  if (i >= p->tokens.count) return &p->tokens.items[p->tokens.count - 1];
  return &p->tokens.items[i];
}

static void parse_error(Parser *p, Token *t, const char *msg) {
  print_loc(t->loc);
  printf(": %s\n", msg);
  p->has_error = true;
}

static Token *expect(Parser *p, Token_Kind want) {
  Token *t = peek_tok(p, 0);
  if (t->kind != want) {
    parse_error(p, t, "unexpected token");
    printf("  expected %s, got %s\n", kind_name(want), kind_name(t->kind));
    return t;
  }
  p->idx++;
  return t;
}

static Token *expect_keyword(Parser *p, Keyword want) {
  Token *t = expect(p, TOKEN_KEYWORD);
  if (p->has_error) return t;
  if (t->keyword != want) {
    parse_error(p, t, "unexpected keyword");
    printf("  expected '%s'\n", kw_name(want));
  }
  return t;
}

static bool token_is_sym(Token *t, const char *s) {
  size_t n = strlen(s);
  return t->kind == TOKEN_SYMBOL && (size_t)t->source.count == n &&
         strncmp(t->source.data, s, n) == 0;
}

// ---- type parsing: int | str | bool | any | [type] | [[type]] ...
static TypeDesc parse_type(Parser *p) {
  TypeDesc td = { .base = BASE_UNKNOWN, .indir = 0 };

  while (peek_tok(p,0)->kind == TOKEN_OPEN_BRACKET) {
    p->idx++;
    td.indir++;
  }

  Token *t = expect(p, TOKEN_TYPE);
  td.base = t->type_base;

  for (int i=0; i<td.indir; ++i) {
    expect(p, TOKEN_CLOSE_BRACKET);
  }

  return td;
}

// ---- expression parsing (precedence) ----

static AST_Node *parse_expr(Parser *p);

static AST_Node *parse_primary(Parser *p) {
  Token *t = peek_tok(p, 0);

  if (t->kind == TOKEN_INTEGER) {
    p->idx++;
    AST_Node *n = ast_make(AST_EXPR_INT);
    n->int_value = t->long_value;
    return n;
  }

  if (t->kind == TOKEN_STRING) {
    p->idx++;
    AST_Node *n = ast_make(AST_EXPR_STR);
    n->str_value = (String){ t->source.data, t->source.count };
    return n;
  }

  if (t->kind == TOKEN_KEYWORD && (t->keyword == KEYWORD_TRUE || t->keyword == KEYWORD_FALSE)) {
    p->idx++;
    AST_Node *n = ast_make(AST_EXPR_BOOL);
    n->bool_value = (t->keyword == KEYWORD_TRUE);
    return n;
  }

  if (t->kind == TOKEN_IDENT) {
    Token *name = t;
    p->idx++;

    // call?
    if (peek_tok(p,0)->kind == TOKEN_OPEN_PAREN) {
      p->idx++; // '('
      AST_Node *call = ast_make(AST_EXPR_CALL);
      call->call.fn_name = (String){ name->source.data, name->source.count };
      call->call.args = (AST_List){0};

      if (peek_tok(p,0)->kind != TOKEN_CLOSE_PAREN) {
        for (;;) {
          AST_Node *arg = parse_expr(p);
          ast_list_append(&call->call.args, arg);
          Token *sep = peek_tok(p,0);
          if (sep->kind == TOKEN_COMMA) { p->idx++; continue; }
          if (sep->kind == TOKEN_CLOSE_PAREN) break;
          parse_error(p, sep, "expected ',' or ')'");
          break;
        }
      }
      expect(p, TOKEN_CLOSE_PAREN);

      while (peek_tok(p,0)->kind == TOKEN_OPEN_BRACKET) {
        p->idx++;
        AST_Node *idx = parse_expr(p);
        expect(p, TOKEN_CLOSE_BRACKET);
        AST_Node *ix = ast_make(AST_EXPR_INDEX);
        ix->index.base = call;
        ix->index.index = idx;
        call = ix;
      }

      return call;
    }

    AST_Node *id = ast_make(AST_EXPR_IDENT);
    id->ident_name = (String){ name->source.data, name->source.count };

    while (peek_tok(p,0)->kind == TOKEN_OPEN_BRACKET) {
      p->idx++;
      AST_Node *idx = parse_expr(p);
      expect(p, TOKEN_CLOSE_BRACKET);
      AST_Node *ix = ast_make(AST_EXPR_INDEX);
      ix->index.base = id;
      ix->index.index = idx;
      id = ix;
    }

    return id;
  }

  if (t->kind == TOKEN_OPEN_PAREN) {
    p->idx++;
    AST_Node *inside = parse_expr(p);
    expect(p, TOKEN_CLOSE_PAREN);

    while (peek_tok(p,0)->kind == TOKEN_OPEN_BRACKET) {
      p->idx++;
      AST_Node *idx = parse_expr(p);
      expect(p, TOKEN_CLOSE_BRACKET);
      AST_Node *ix = ast_make(AST_EXPR_INDEX);
      ix->index.base = inside;
      ix->index.index = idx;
      inside = ix;
    }
    return inside;
  }

  parse_error(p, t, "expected expression");
  return ast_make(AST_EXPR_INT);
}

static AST_Node *parse_mul(Parser *p) {
  AST_Node *node = parse_primary(p);
  for (;;) {
    Token *t = peek_tok(p,0);
    if (token_is_sym(t, "*") || token_is_sym(t, "/") || token_is_sym(t, "%")) {
      char op = t->source.data[0];
      p->idx++;
      AST_Node *rhs = parse_primary(p);
      AST_Node *bin = ast_make(AST_EXPR_BINOP);
      bin->bin.left = node;
      bin->bin.right = rhs;
      bin->bin.op = (op=='*')?OP_MUL : (op=='/')?OP_DIV : OP_MOD;
      node = bin;
    } else break;
  }
  return node;
}

static AST_Node *parse_add(Parser *p) {
  AST_Node *node = parse_mul(p);
  for (;;) {
    Token *t = peek_tok(p,0);
    if (token_is_sym(t, "+") || token_is_sym(t, "-")) {
      char op = t->source.data[0];
      p->idx++;
      AST_Node *rhs = parse_mul(p);
      AST_Node *bin = ast_make(AST_EXPR_BINOP);
      bin->bin.left = node;
      bin->bin.right = rhs;
      bin->bin.op = (op=='+')?OP_ADD:OP_SUB;
      node = bin;
    } else break;
  }
  return node;
}

static AST_Node *parse_cmp(Parser *p) {
  AST_Node *node = parse_add(p);
  for (;;) {
    Token *t = peek_tok(p,0);
    BinOp op = 0;
    bool is_cmp = true;

    if      (token_is_sym(t, "==")) op = OP_EQ;
    else if (token_is_sym(t, "!=")) op = OP_NE;
    else if (token_is_sym(t, "<"))  op = OP_LT;
    else if (token_is_sym(t, "<=")) op = OP_LE;
    else if (token_is_sym(t, ">"))  op = OP_GT;
    else if (token_is_sym(t, ">=")) op = OP_GE;
    else is_cmp = false;

    if (!is_cmp) break;

    p->idx++;
    AST_Node *rhs = parse_add(p);
    AST_Node *bin = ast_make(AST_EXPR_BINOP);
    bin->bin.left = node;
    bin->bin.right = rhs;
    bin->bin.op = op;
    node = bin;
  }
  return node;
}

static AST_Node *parse_and(Parser *p) {
  AST_Node *node = parse_cmp(p);
  while (token_is_sym(peek_tok(p,0), "&&")) {
    p->idx++;
    AST_Node *rhs = parse_cmp(p);
    AST_Node *bin = ast_make(AST_EXPR_BINOP);
    bin->bin.left = node;
    bin->bin.right = rhs;
    bin->bin.op = OP_AND;
    node = bin;
  }
  return node;
}

static AST_Node *parse_or(Parser *p) {
  AST_Node *node = parse_and(p);
  while (token_is_sym(peek_tok(p,0), "||")) {
    p->idx++;
    AST_Node *rhs = parse_and(p);
    AST_Node *bin = ast_make(AST_EXPR_BINOP);
    bin->bin.left = node;
    bin->bin.right = rhs;
    bin->bin.op = OP_OR;
    node = bin;
  }
  return node;
}

static AST_Node *parse_expr(Parser *p) { return parse_or(p); }

// ---- statements ----

static AST_Node *parse_stmt(Parser *p);

static AST_List parse_block_list(Parser *p) {
  AST_List body = {0};
  expect(p, TOKEN_OPEN_BRACE);
  if (p->has_error) return body;

  while (!p->has_error) {
    Token *t = peek_tok(p,0);
    if (t->kind == TOKEN_CLOSE_BRACE) { p->idx++; break; }
    AST_Node *st = parse_stmt(p);
    if (p->has_error) break;
    ast_list_append(&body, st);
  }
  return body;
}

static AST_Node *parse_stmt_block(Parser *p) {
  AST_Node *st = ast_make(AST_STMT_BLOCK);
  st->block_st.body = parse_block_list(p);
  return st;
}

static AST_Node *parse_stmt_return(Parser *p) {
  expect_keyword(p, KEYWORD_RETURN);
  AST_Node *st = ast_make(AST_STMT_RETURN);
  st->ret_st.expr = parse_expr(p);
  return st;
}

static AST_Node *parse_stmt_call(Parser *p) {
  expect_keyword(p, KEYWORD_CALL);
  Token *name = expect(p, TOKEN_IDENT);
  expect(p, TOKEN_OPEN_PAREN);

  AST_Node *st = ast_make(AST_STMT_CALL);
  st->call.fn_name = (String){ name->source.data, name->source.count };
  st->call.args = (AST_List){0};

  if (peek_tok(p,0)->kind != TOKEN_CLOSE_PAREN) {
    for (;;) {
      AST_Node *arg = parse_expr(p);
      ast_list_append(&st->call.args, arg);
      Token *sep = peek_tok(p,0);
      if (sep->kind == TOKEN_COMMA) { p->idx++; continue; }
      if (sep->kind == TOKEN_CLOSE_PAREN) break;
      parse_error(p, sep, "expected ',' or ')'");
      break;
    }
  }
  expect(p, TOKEN_CLOSE_PAREN);
  return st;
}

static AST_Node *parse_stmt_let(Parser *p) {
  expect_keyword(p, KEYWORD_LET);
  Token *name = expect(p, TOKEN_IDENT);
  expect(p, TOKEN_COLON);
  TypeDesc td = parse_type(p);

  AST_Node *st = ast_make(AST_STMT_LET);
  st->let_st.name = (String){ name->source.data, name->source.count };
  st->let_st.type = td;
  st->let_st.init = NULL;

  if (token_is_sym(peek_tok(p,0), "=")) {
    p->idx++;
    st->let_st.init = parse_expr(p);
  }
  return st;
}

static AST_Node *parse_stmt_set(Parser *p) {
  expect_keyword(p, KEYWORD_SET);

  Token *name = expect(p, TOKEN_IDENT);
  AST_Node *lhs_ident = ast_make(AST_EXPR_IDENT);
  lhs_ident->ident_name = (String){ name->source.data, name->source.count };

  if (peek_tok(p,0)->kind == TOKEN_OPEN_BRACKET) {
    p->idx++;
    AST_Node *idx = parse_expr(p);
    expect(p, TOKEN_CLOSE_BRACKET);

    Token *eq = expect(p, TOKEN_SYMBOL);
    if (!token_is_sym(eq, "=")) parse_error(p, eq, "expected '=' after set a[i]");

    AST_Node *rhs = parse_expr(p);

    AST_Node *st = ast_make(AST_STMT_SET_INDEX);
    st->set_index_st.base = lhs_ident;
    st->set_index_st.index = idx;
    st->set_index_st.value = rhs;
    return st;
  }

  Token *eq = expect(p, TOKEN_SYMBOL);
  if (!token_is_sym(eq, "=")) parse_error(p, eq, "expected '=' after set name");
  AST_Node *rhs = parse_expr(p);

  AST_Node *st = ast_make(AST_STMT_SET);
  st->set_st.name = (String){ name->source.data, name->source.count };
  st->set_st.value = rhs;
  return st;
}

static AST_Node *parse_stmt_if(Parser *p) {
  expect_keyword(p, KEYWORD_IF);
  AST_Node *cond = parse_expr(p);

  AST_Node *then_st = parse_stmt(p); // can be block or single stmt

  AST_Node *else_st = NULL;
  Token *t = peek_tok(p,0);
  if (t->kind == TOKEN_KEYWORD && t->keyword == KEYWORD_ELSE) {
    p->idx++;
    else_st = parse_stmt(p);
  }

  AST_Node *st = ast_make(AST_STMT_IF);
  st->if_st.cond = cond;
  st->if_st.then_st = then_st;
  st->if_st.else_st = else_st;
  return st;
}

static AST_Node *parse_stmt_while(Parser *p) {
  expect_keyword(p, KEYWORD_WHILE);
  AST_Node *cond = parse_expr(p);
  AST_List body = parse_block_list(p);

  AST_Node *st = ast_make(AST_STMT_WHILE);
  st->while_st.cond = cond;
  st->while_st.body = body;
  return st;
}

static AST_Node *parse_stmt(Parser *p) {
  Token *t = peek_tok(p,0);

  if (t->kind == TOKEN_OPEN_BRACE) {
    return parse_stmt_block(p);
  }

  if (t->kind == TOKEN_KEYWORD) {
    switch (t->keyword) {
      case KEYWORD_RETURN: return parse_stmt_return(p);
      case KEYWORD_CALL: return parse_stmt_call(p);
      case KEYWORD_LET: return parse_stmt_let(p);
      case KEYWORD_SET: return parse_stmt_set(p);
      case KEYWORD_IF: return parse_stmt_if(p);
      case KEYWORD_WHILE: return parse_stmt_while(p);
      default: break;
    }
  }

  parse_error(p, t, "unsupported statement");
  return ast_make(AST_NONE);
}

// ---- functions/program ----

static AST_Node *parse_param(Parser *p) {
  Token *name = expect(p, TOKEN_IDENT);
  expect(p, TOKEN_COLON);
  TypeDesc td = parse_type(p);

  AST_Node *n = ast_make(AST_PARAM);
  n->param.name = (String){ name->source.data, name->source.count };
  n->param.type = td;
  return n;
}

static void parse_param_list(Parser *p, AST_List *out) {
  *out = (AST_List){0};
  if (peek_tok(p,0)->kind == TOKEN_CLOSE_PAREN) return;

  for (;;) {
    AST_Node *param = parse_param(p);
    ast_list_append(out, param);
    Token *sep = peek_tok(p,0);
    if (sep->kind == TOKEN_COMMA) { p->idx++; continue; }
    if (sep->kind == TOKEN_CLOSE_PAREN) break;
    parse_error(p, sep, "expected ',' or ')' in parameter list");
    break;
  }
}

static AST_Node *parse_fn(Parser *p) {
  expect_keyword(p, KEYWORD_FN);
  Token *name = expect(p, TOKEN_IDENT);
  expect(p, TOKEN_OPEN_PAREN);

  AST_Node *fn = ast_make(AST_FN);
  fn->fn.name = (String){ name->source.data, name->source.count };
  parse_param_list(p, &fn->fn.params);
  expect(p, TOKEN_CLOSE_PAREN);

  // optional return annotation
  fn->fn.has_return_type = false;
  fn->fn.return_type = (TypeDesc){ .base = BASE_ANY, .indir = 0 };

  if (peek_tok(p,0)->kind == TOKEN_ARROW) {
    p->idx++;
    fn->fn.has_return_type = true;
    fn->fn.return_type = parse_type(p);
  }

  fn->fn.body = parse_block_list(p);
  return fn;
}

typedef struct Parse_Result {
  AST_Node *root;
  bool success;
} Parse_Result;

static Parse_Result parse_program(Token_Array toks) {
  Parser p = { .tokens = toks, .idx = 0, .has_error = false };
  AST_Node *root = ast_make(AST_PROGRAM);
  root->program = (AST_List){0};

  while (!p.has_error) {
    Token *t = peek_tok(&p,0);
    if (t->kind == TOKEN_EOF) break;
    AST_Node *fn = parse_fn(&p);
    ast_list_append(&root->program, fn);
  }

  return (Parse_Result){ .root = root, .success = !p.has_error };
}

// ===================== Interpreter =====================

typedef enum ValKind { V_INT, V_BOOL, V_STR, V_ARR, V_NONE } ValKind;

typedef struct ArrayVal {
  void *data;   // 8-byte slots
  long count;
  TypeDesc elem;
} ArrayVal;

typedef struct Value {
  ValKind kind;
  TypeDesc type;
  union {
    long i;
    bool b;
    char *s;
    ArrayVal arr;
  };
} Value;

static Value v_int(long x) { Value v={.kind=V_INT,.type={.base=BASE_INT,.indir=0},.i=x}; return v; }
static Value v_bool(bool x){ Value v={.kind=V_BOOL,.type={.base=BASE_BOOL,.indir=0},.b=x}; return v; }
static Value v_str(char *x){ Value v={.kind=V_STR,.type={.base=BASE_STR,.indir=0},.s=x}; return v; }
static Value v_none(void){ Value v={.kind=V_NONE,.type={.base=BASE_ANY,.indir=0}}; return v; }

static long truthy(Value v) {
  switch (v.kind) {
    case V_BOOL: return v.b ? 1 : 0;
    case V_INT: return v.i != 0;
    case V_STR: return v.s && v.s[0] != '\0';
    case V_ARR: return v.arr.data != NULL;
    default: return 0;
  }
}

typedef struct EnvEntry {
  String name;
  TypeDesc decl_type;
  Value val;
} EnvEntry;

typedef struct Env {
  EnvEntry *items;
  long count;
  long cap;
} Env;

static void env_push(Env *e, String name, TypeDesc td, Value v) {
  if (e->count == e->cap) {
    e->cap = e->cap ? e->cap*2 : 16;
    e->items = (EnvEntry*)realloc(e->items, (size_t)e->cap * sizeof(EnvEntry));
    if (!e->items) { fprintf(stderr, "Out of memory\n"); exit(1); }
  }
  e->items[e->count++] = (EnvEntry){ .name=name, .decl_type=td, .val=v };
}

static EnvEntry *env_find(Env *e, String name) {
  for (long i=e->count-1; i>=0; --i) {
    if (streq(e->items[i].name, name)) return &e->items[i];
  }
  return NULL;
}

static AST_Node *find_fn(AST_Node *program, String name) {
  for (AST_Node *f=program->program.first; f; f=f->next) {
    if (f->kind == AST_FN && streq(f->fn.name, name)) return f;
  }
  return NULL;
}

static Value eval_expr(AST_Node *program, Env *env, AST_Node *expr);
static Value call_function(AST_Node *program, AST_Node *fn, Value *args, long argc);

static Value builtin_call(String fn, Value *args, long argc) {
  if (streq(fn, STRLIT("print"))) {
    if (argc != 1) { fprintf(stderr, "runtime error: print expects 1 arg\n"); exit(1); }
    if (args[0].kind != V_STR) { fprintf(stderr, "runtime error: print expects str\n"); exit(1); }
    fputs(args[0].s ? args[0].s : "", stdout);
    return v_none();
  }
  if (streq(fn, STRLIT("print_int"))) {
    if (argc != 1) { fprintf(stderr, "runtime error: print_int expects 1 arg\n"); exit(1); }
    if (args[0].kind != V_INT) { fprintf(stderr, "runtime error: print_int expects int\n"); exit(1); }
    printf("%ld", args[0].i);
    return v_none();
  }
  if (streq(fn, STRLIT("print_nl"))) {
    if (argc != 0) { fprintf(stderr, "runtime error: print_nl expects 0 args\n"); exit(1); }
    fputc('\n', stdout);
    return v_none();
  }
  if (streq(fn, STRLIT("alloc"))) {
    if (argc != 1) { fprintf(stderr, "runtime error: alloc expects 1 arg\n"); exit(1); }
    if (args[0].kind != V_INT) { fprintf(stderr, "runtime error: alloc expects int\n"); exit(1); }
    long n = args[0].i;
    if (n < 0) n = 0;

    ArrayVal a;
    a.count = n;
    a.data = xcalloc((size_t)n, 8);
    a.elem = (TypeDesc){ .base = BASE_ANY, .indir = 0 };

    Value v = {0};
    v.kind = V_ARR;
    v.type = (TypeDesc){ .base = BASE_ANY, .indir = 1 };
    v.arr = a;
    return v;
  }

  fprintf(stderr, "runtime error: unknown builtin\n");
  exit(1);
}

static Value eval_binop(Value a, Value b, BinOp op) {
  if (op==OP_ADD || op==OP_SUB || op==OP_MUL || op==OP_DIV || op==OP_MOD) {
    if (a.kind != V_INT || b.kind != V_INT) { fprintf(stderr, "runtime error: arithmetic expects int\n"); exit(1); }
    switch (op) {
      case OP_ADD: return v_int(a.i + b.i);
      case OP_SUB: return v_int(a.i - b.i);
      case OP_MUL: return v_int(a.i * b.i);
      case OP_DIV: return v_int(b.i==0 ? 0 : a.i / b.i);
      case OP_MOD: return v_int(b.i==0 ? 0 : a.i % b.i);
      default: break;
    }
  }

  if (op==OP_EQ || op==OP_NE || op==OP_LT || op==OP_LE || op==OP_GT || op==OP_GE) {
    if (a.kind == V_INT && b.kind == V_INT) {
      bool r=false;
      switch (op) {
        case OP_EQ: r = (a.i == b.i); break;
        case OP_NE: r = (a.i != b.i); break;
        case OP_LT: r = (a.i <  b.i); break;
        case OP_LE: r = (a.i <= b.i); break;
        case OP_GT: r = (a.i >  b.i); break;
        case OP_GE: r = (a.i >= b.i); break;
        default: break;
      }
      return v_bool(r);
    }
    if (a.kind == V_BOOL && b.kind == V_BOOL) {
      bool r=false;
      switch (op) {
        case OP_EQ: r = (a.b == b.b); break;
        case OP_NE: r = (a.b != b.b); break;
        default:
          fprintf(stderr, "runtime error: only == != supported for bool\n");
          exit(1);
      }
      return v_bool(r);
    }
    fprintf(stderr, "runtime error: comparison type mismatch\n");
    exit(1);
  }

  if (op==OP_AND || op==OP_OR) {
    bool ta = truthy(a) != 0;
    bool tb = truthy(b) != 0;
    return v_bool(op==OP_AND ? (ta && tb) : (ta || tb));
  }

  fprintf(stderr, "runtime error: unknown binop\n");
  exit(1);
}

static Value read_array_slot(ArrayVal *a, long idx) {
  if (idx < 0 || idx >= a->count) {
    fprintf(stderr, "runtime error: array index out of bounds\n");
    exit(1);
  }
  uint64_t *slots = (uint64_t*)a->data;
  uint64_t raw = slots[idx];

  if (a->elem.base == BASE_STR) return v_str((char*)(uintptr_t)raw);
  if (a->elem.base == BASE_BOOL) return v_bool(raw != 0);
  return v_int((long)(int64_t)raw);
}

static void write_array_slot(ArrayVal *a, long idx, Value v) {
  if (idx < 0 || idx >= a->count) {
    fprintf(stderr, "runtime error: array index out of bounds\n");
    exit(1);
  }
  uint64_t *slots = (uint64_t*)a->data;

  if (v.kind == V_STR) {
    slots[idx] = (uint64_t)(uintptr_t)(v.s);
    a->elem.base = BASE_STR;
  } else if (v.kind == V_BOOL) {
    slots[idx] = (uint64_t)(v.b ? 1 : 0);
    a->elem.base = BASE_BOOL;
  } else if (v.kind == V_INT) {
    slots[idx] = (uint64_t)(int64_t)v.i;
    a->elem.base = BASE_INT;
  } else if (v.kind == V_ARR) {
    slots[idx] = (uint64_t)(uintptr_t)(v.arr.data);
    a->elem.base = BASE_ANY;
  } else {
    slots[idx] = 0;
  }
}

static Value eval_expr(AST_Node *program, Env *env, AST_Node *expr) {
  switch (expr->kind) {
    case AST_EXPR_INT: return v_int(expr->int_value);
    case AST_EXPR_BOOL: return v_bool(expr->bool_value);
    case AST_EXPR_STR: {
      char *cp = xstrndup_(expr->str_value.data, (size_t)expr->str_value.count);
      return v_str(cp);
    }
    case AST_EXPR_IDENT: {
      EnvEntry *e = env_find(env, expr->ident_name);
      if (!e) {
        fprintf(stderr, "runtime error: unknown variable: %.*s\n", PRINT_STRING(expr->ident_name));
        exit(1);
      }
      return e->val;
    }
    case AST_EXPR_BINOP: {
      Value a = eval_expr(program, env, expr->bin.left);
      Value b = eval_expr(program, env, expr->bin.right);
      return eval_binop(a, b, expr->bin.op);
    }
    case AST_EXPR_CALL: {
      long argc = expr->call.args.count;
      Value *args = (Value*)xmalloc(sizeof(Value) * (size_t)argc);
      long i=0;
      for (AST_Node *a=expr->call.args.first; a; a=a->next) args[i++] = eval_expr(program, env, a);

      if (streq(expr->call.fn_name, STRLIT("print")) ||
          streq(expr->call.fn_name, STRLIT("print_int")) ||
          streq(expr->call.fn_name, STRLIT("print_nl")) ||
          streq(expr->call.fn_name, STRLIT("alloc"))) {
        Value r = builtin_call(expr->call.fn_name, args, argc);
        free(args);
        return r;
      }

      AST_Node *fn = find_fn(program, expr->call.fn_name);
      if (!fn) {
        fprintf(stderr, "runtime error: unknown function: %.*s\n", PRINT_STRING(expr->call.fn_name));
        exit(1);
      }
      Value r = call_function(program, fn, args, argc);
      free(args);
      return r;
    }
    case AST_EXPR_INDEX: {
      Value base = eval_expr(program, env, expr->index.base);
      Value idxv = eval_expr(program, env, expr->index.index);
      if (base.kind != V_ARR) { fprintf(stderr, "runtime error: indexing requires array\n"); exit(1); }
      if (idxv.kind != V_INT) { fprintf(stderr, "runtime error: index must be int\n"); exit(1); }
      return read_array_slot(&base.arr, idxv.i);
    }
    default:
      fprintf(stderr, "runtime error: unsupported expression kind %d\n", (int)expr->kind);
      exit(1);
  }
}

typedef struct ExecResult {
  bool returned;
  Value value;
} ExecResult;

static ExecResult exec_stmt(AST_Node *program, Env *env, AST_Node *st);

static ExecResult exec_list(AST_Node *program, Env *env, AST_List body) {
  for (AST_Node *s=body.first; s; s=s->next) {
    ExecResult r = exec_stmt(program, env, s);
    if (r.returned) return r;
  }
  return (ExecResult){ .returned=false, .value=v_none() };
}

static ExecResult exec_stmt(AST_Node *program, Env *env, AST_Node *st) {
  switch (st->kind) {
    case AST_STMT_BLOCK: {
      return exec_list(program, env, st->block_st.body);
    }
    case AST_STMT_LET: {
      Value v = v_none();
      if (st->let_st.init) v = eval_expr(program, env, st->let_st.init);
      else {
        if (st->let_st.type.indir > 0) v = v_none();
        else if (st->let_st.type.base == BASE_BOOL) v = v_bool(false);
        else if (st->let_st.type.base == BASE_STR) v = v_str(xstrndup_("", 0));
        else v = v_int(0);
      }
      env_push(env, st->let_st.name, st->let_st.type, v);
      return (ExecResult){0};
    }
    case AST_STMT_SET: {
      EnvEntry *e = env_find(env, st->set_st.name);
      if (!e) {
        fprintf(stderr, "runtime error: set unknown variable: %.*s\n", PRINT_STRING(st->set_st.name));
        exit(1);
      }
      e->val = eval_expr(program, env, st->set_st.value);
      return (ExecResult){0};
    }
    case AST_STMT_SET_INDEX: {
      Value basev = eval_expr(program, env, st->set_index_st.base);
      Value idxv  = eval_expr(program, env, st->set_index_st.index);
      Value rhs   = eval_expr(program, env, st->set_index_st.value);

      if (basev.kind != V_ARR) { fprintf(stderr, "runtime error: set a[i] requires array\n"); exit(1); }
      if (idxv.kind != V_INT)  { fprintf(stderr, "runtime error: index must be int\n"); exit(1); }

      write_array_slot(&basev.arr, idxv.i, rhs);

      if (st->set_index_st.base->kind == AST_EXPR_IDENT) {
        EnvEntry *e = env_find(env, st->set_index_st.base->ident_name);
        if (!e) { fprintf(stderr, "runtime error: array variable not found\n"); exit(1); }
        e->val = basev;
      }
      return (ExecResult){0};
    }
    case AST_STMT_CALL: {
      long argc = st->call.args.count;
      Value *args = (Value*)xmalloc(sizeof(Value) * (size_t)argc);
      long i=0;
      for (AST_Node *a=st->call.args.first; a; a=a->next) args[i++] = eval_expr(program, env, a);

      if (streq(st->call.fn_name, STRLIT("print")) ||
          streq(st->call.fn_name, STRLIT("print_int")) ||
          streq(st->call.fn_name, STRLIT("print_nl")) ||
          streq(st->call.fn_name, STRLIT("alloc"))) {
        (void)builtin_call(st->call.fn_name, args, argc);
        free(args);
        return (ExecResult){0};
      }

      AST_Node *fn = find_fn(program, st->call.fn_name);
      if (!fn) {
        fprintf(stderr, "runtime error: unknown function in call stmt: %.*s\n", PRINT_STRING(st->call.fn_name));
        exit(1);
      }
      (void)call_function(program, fn, args, argc);
      free(args);
      return (ExecResult){0};
    }
    case AST_STMT_RETURN: {
      Value v = eval_expr(program, env, st->ret_st.expr);
      return (ExecResult){ .returned=true, .value=v };
    }
    case AST_STMT_IF: {
      Value c = eval_expr(program, env, st->if_st.cond);
      if (truthy(c)) {
        return exec_stmt(program, env, st->if_st.then_st);
      } else if (st->if_st.else_st) {
        return exec_stmt(program, env, st->if_st.else_st);
      }
      return (ExecResult){0};
    }
    case AST_STMT_WHILE: {
      while (true) {
        Value c = eval_expr(program, env, st->while_st.cond);
        if (!truthy(c)) break;
        ExecResult r = exec_list(program, env, st->while_st.body);
        if (r.returned) return r;
      }
      return (ExecResult){0};
    }
    default:
      fprintf(stderr, "runtime error: unsupported statement kind %d\n", (int)st->kind);
      exit(1);
  }
}

static Value call_function(AST_Node *program, AST_Node *fn, Value *args, long argc) {
  Env env = {0};

  long pcnt = fn->fn.params.count;
  if (pcnt != argc) {
    fprintf(stderr, "runtime error: wrong arg count calling %.*s (expected %ld, got %ld)\n",
            PRINT_STRING(fn->fn.name), pcnt, argc);
    exit(1);
  }

  long i=0;
  for (AST_Node *p=fn->fn.params.first; p; p=p->next) {
    env_push(&env, p->param.name, p->param.type, args ? args[i++] : v_none());
  }

  ExecResult r = exec_list(program, &env, fn->fn.body);

  if (r.returned) return r.value;

  // If function has explicit return type, require a return.
  if (fn->fn.has_return_type) {
    fprintf(stderr, "runtime error: function %.*s did not return\n", PRINT_STRING(fn->fn.name));
    exit(1);
  }

  // void function
  return v_none();
}

// ===================== Main =====================

typedef struct Options {
  const char *in_file;
  bool dump_tokens;
  bool dump_ast;
} Options;

static void usage(const char *prog) {
  printf("Usage: %s input.jive [--tokens] [--ast]\n", prog);
}

int main(int argc, const char **argv) {
  if (argc < 2) { usage(argv[0]); return 1; }

  Options opt = { .in_file = argv[1], .dump_tokens = false, .dump_ast = false };
  for (int i=2;i<argc;i++) {
    if (strcmp(argv[i], "--tokens")==0) opt.dump_tokens = true;
    else if (strcmp(argv[i], "--ast")==0) opt.dump_ast = true;
    else printf("WARNING: unknown flag '%s'\n", argv[i]);
  }

  Token_Array toks = lex_file(opt.in_file);
  if (opt.dump_tokens) {
    printf("=== TOKENS ===\n");
    print_token_array(toks);
  }

  Parse_Result pr = parse_program(toks);
  if (!pr.success) {
    printf("ERROR: parse failed.\n");
    return 1;
  }

   AST_Node *main_fn = find_fn(pr.root, STRLIT("main"));
  if (!main_fn) {
    fprintf(stderr, "error: no main() found\n");
    return 1;
  }

  // Run program. Output comes from built-in print/print_int/print_nl.
  (void)call_function(pr.root, main_fn, NULL, 0);

  return 0;
}
