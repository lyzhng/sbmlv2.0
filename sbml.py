# Code written by Lily Zhong (ID# 110959687)

import sys
import ply.lex as lex
import ply.yacc as yacc
from sys import argv
from exceptions import SyntaxError, SemanticError
from nodes import (
    ExprNode,
    VariableNode,
    ListNode,
    TupleNode,
    TupleNode,
    BinaryOpNode,
    ListAccessNode,
    PrintNode,
    WhileNode,
    IfNode,
    IfElseNode,
    BlockNode,
    ComparisonBinaryOpNode,
    ListAssignNode,
    UnaryOpNode
)

reserved = {
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE',
    'print': 'PRINT',
    'andalso': 'CONJUNCTIONOP',
    'orelse': 'DISJUNCTIONOP',
    'not': 'NOTOP',
    'in': 'INOP',
    'div': 'INTDIVOP',
    'mod': 'MODOP',
    'True': 'TRUE',
    'False': 'FALSE'
 }

tokens = [
    'VARIABLE',
    'INTEGER', 'REAL', 'STRING',
    'LPAREN', 'RPAREN',
    'HASH',
    'LBRACKET', 'RBRACKET',
    'LBRACE', 'RBRACE',
    'MULOP', 'EXPOP', 'DIVOP', 'PLUSOP', 'MINUSOP',
    'CONSOP',
    'LTOP', 'LTEOP', 'EQOP', 'NEQOP', 'GTEOP', 'GTOP',
    'COMMA', 'SEMICOLON',
    'ASSIGNOP',
 ] + list(reserved.values())

t_EQOP = r'\={2}'
t_EXPOP = r'\*{2}'
t_CONSOP = r'\:{2}'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_DIVOP = r'\/'
t_MULOP = r'\*'
t_PLUSOP = r'\+'
t_MINUSOP = r'\-'
t_LTOP = r'\<'
t_LTEOP = r'\<\='
t_NEQOP = r'\<\>'
t_GTEOP = r'\>\='
t_GTOP = r'\>'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'
t_HASH = r'\#'
t_ASSIGNOP = r'\='
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_ignore = ' \t'
    
def t_REAL(t):
    r'(([-]?([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+))([eE][-]?[0-9]+)?)'
    t.value = float(t.value)
    return t

def t_INTEGER(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\'[^\']*\'|\"[^\"]*\"'
    t.value = t.value
    return t

def t_VARIABLE(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.value = t.value
    t.type = reserved.get(t.value, 'VARIABLE')
    if t.value == 'True' or t.value == 'False':
        t.type = t.value.upper()
        t.value = t.value == 'True'
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    # lexing error
    t.lexer.skip(1)
    raise SyntaxError
    
lexer = lex.lex()
    
    
################## LEXING ##################
        
def p_start(p):
    'start : block'
    p[0] = p[1]
    
def p_block(p):
    'block : LBRACE stmts RBRACE'
    p[0] = p[2]

def p_stmts(p):
    '''
    stmts : stmt stmts
          | block stmts
          | stmt
    '''
    if len(p) == 3:
        if type(p[2]) is list:
            p[0] = [p[1]] + p[2]
        else:
            p[0] = [p[1]] + [p[2]]
    if len(p) == 2:
        p[0] = [p[1]]

def p_stmt(p):
    'stmt : expr SEMICOLON'
    p[0] = p[1]

def p_condition(p):
    'condition : LPAREN expr RPAREN'
    p[0] = p[2]

def p_if(p):
    'stmt : IF condition block'
    p[0] = IfNode(p[2], BlockNode(p[3]))

def p_ifelse(p):
    'stmt : IF condition block ELSE block'
    p[0] = IfElseNode(p[2], BlockNode(p[3]), BlockNode(p[5]))

def p_while(p):
    'stmt : WHILE condition block'
    p[0] = WhileNode(p[2], BlockNode(p[3]))

def p_print(p):
    'stmt : PRINT LPAREN expr RPAREN SEMICOLON'
    p[0] = PrintNode(p[3])
    
def p_paren(p):
    'expr : LPAREN expr RPAREN'
    p[0] = p[2]        
    
def p_assign(p):
    'stmt : VARIABLE ASSIGNOP expr SEMICOLON'
    p[0] = VariableNode(p[1], p[3])
    
def p_term(p):
    '''
    expr : STRING
         | INTEGER
         | REAL
         | TRUE
         | FALSE
    '''
    p[0] = ExprNode(p[1])
        
def p_var(p):
    'expr : VARIABLE'
    p[0] = ExprNode(p[1])    
        
def p_dynamic(p):
    '''
    expr : list
         | tup
         | listindex
         | tupindex        
    '''
    p[0] = ExprNode(p[1])
           
def p_not(p):
    'expr : NOTOP expr'
    p[0] = UnaryOpNode(p[2], p[1])
       
def p_uminus(p):
    'expr : MINUSOP expr %prec UMINUS'
    p[0] = UnaryOpNode(p[2], p[1])           
       
def p_binop(p):
    '''
    expr : expr PLUSOP expr
         | expr MINUSOP expr
         | expr MULOP expr
         | expr DIVOP expr
         | expr INTDIVOP expr
         | expr MODOP expr
         | expr EXPOP expr
         | expr CONJUNCTIONOP expr
         | expr DISJUNCTIONOP expr
         | expr INOP expr
         | expr CONSOP expr
    '''
    p[0] = BinaryOpNode(p[1], p[3], p[2])
       
def p_comparison(p):
    '''
    expr : expr EQOP expr
         | expr NEQOP expr
         | expr GTOP expr
         | expr GTEOP expr
         | expr LTOP expr
         | expr LTEOP expr
    '''        
    p[0] = ComparisonBinaryOpNode(p[1], p[3], p[2])

## Lists

def p_listassign(p):
    '''
    stmt : VARIABLE LBRACKET expr RBRACKET ASSIGNOP expr SEMICOLON
         | list LBRACKET expr RBRACKET ASSIGNOP expr SEMICOLON
    '''
    p[0] = ListAssignNode(p[1], p[3], p[6])

def p_list(p):
    '''
    list : LBRACKET listitems RBRACKET
         | LBRACKET RBRACKET
    '''
    if len(p) == 4:
        p[0] = ListNode(p[2])
    else:
        p[0] = ListNode([])
        
def p_listitems(p):
    '''
    listitems : listitems COMMA expr
              | expr
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_genindex(p):
    '''
    listindex : list LBRACKET expr RBRACKET
              | expr LBRACKET expr RBRACKET
    '''
    p[0] = ListAccessNode(p[1], p[3], 'listindex')


def p_listindex(p):
    'listindex : VARIABLE LBRACKET expr RBRACKET'
    p[0] = ListAccessNode(p[1], p[3], 'listindex')
    
## Tuples    
    
def p_tuple(p):
    '''
    tup : LPAREN tupitems RPAREN
        | LPAREN RPAREN
    '''
    if len(p) == 3:
        p[0] = TupleNode(())
    elif len(p) == 4:
        p[0] = TupleNode(tuple(p[2]))
    elif len(p[2]) == 1:
        p[0] = ExprNode(p[2][0])
                
def p_tupitems(p):
    '''
    tupitems : tupitems COMMA expr
             | tupitems COMMA
             | expr
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    elif len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = p[1] + [p[3]]
    
def p_tupindex(p):
    '''
    tupindex : HASH INTEGER LPAREN tupindex RPAREN
             | HASH INTEGER tup
    '''
    if len(p) == 6:
        p[0] = ListAccessNode(p[4], ExprNode(p[2] - 1), 'tupindex')
    else:
        p[0] = ListAccessNode(p[3], ExprNode(p[2] - 1), 'tupindex')

def p_error(p):
    raise SyntaxError

# PRECEDENCE
precedence = (
    ('left', 'DISJUNCTIONOP'),
    ('left', 'CONJUNCTIONOP'),
    ('left', 'NOTOP'),
    ('left', 'LTOP', 'LTEOP', 'EQOP', 'NEQOP', 'GTEOP', 'GTOP'),
    ('right', 'CONSOP'),
    ('left', 'INOP'),
    ('left', 'PLUSOP', 'MINUSOP'),
    ('left', 'MULOP', 'DIVOP', 'INTDIVOP', 'MODOP'),
    ('right', 'EXPOP'),
    ('right','UMINUS'),
    ('left', 'LBRACKET', 'RBRACKET'),
    ('left', 'HASH'),
    ('left', 'COMMA'),
    ('left', 'LPAREN', 'RPAREN'),
)

parser = yacc.yacc()

# FOR SUBMISSION
with open(argv[1], 'r') as file:
    try:
        content = file.read()
        result = BlockNode(parser.parse(content, debug=False))
        result.evaluate()
    except Exception as err:
        print(err)