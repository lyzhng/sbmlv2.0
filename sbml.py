# Code written by Lily Zhong (ID# 110959687)

import sys
import ply.lex as lex
import ply.yacc as yacc
from sys import argv

## Exceptions

class SemanticError(RuntimeError):
    def __init__(self):
        super(SemanticError, self).__init__('SEMANTIC ERROR')

class SyntaxError(Exception):
    def __init__(self):
        super(SyntaxError, self).__init__('SYNTAX ERROR')
        
DEBUG = len(sys.argv) > 1 and sys.argv[1] == '--debug'

## Globals

assignments = {}

## Classes

class ExprNode():
    def __init__(self, expr):
        self.expr = expr

    def evaluate(self):
        if type(self.expr) == str:
            single_quote = self.expr.startswith('\'') and self.expr.endswith('\'')
            double_quote = self.expr.startswith('\"') and self.expr.endswith('\"')
            if single_quote or double_quote: # actual string
                return self.expr[1:-1]
            else: # variable
                if self.expr in assignments:
                    return assignments[self.expr]
                else:
                    raise SemanticError
    
    def __repr__(self):
        return f'ExprNode(expr={self.expr})'

    
class VariableNode(ExprNode):
    def __init__(self, var_name, value):
        self.var_name = var_name
        self.value = value
        
    def evaluate(self):
        assignments[var_name] = value
        return assignments[var_name]
        
    def __repr__(self):
        return f'VariableNode(var_name={self.var_name}, value={self.value})'


class ListNode(ExprNode):
    def __init__(self, lst):
        self.lst = lst
    
    def evaluate(self):
        for item in self.lst:
            item.evaluate()
            
    def __repr__(self):
        return f'ListNode(lst={self.lst})'

class TupleNode(ExprNode):
    def __init__(self, tups):
        self.tups = tups
        
    def evaluate(self):
        for item in self.tups:
            item.evaluate()

    def __repr__(self):
        return f'TupleNode(tups={self.tups})'
    
class BinaryOpNode(ExprNode):
    def __init__(self, operand1, operand2, operator):
        self.operand1 = operand1
        self.operand2 = operand2
        self.operator = operator
        
    def evaluate(self):
        if self.operator == '+':
            both_numbers = _valid_types([self.operand1.expr, self.operand2.expr], [int, float])
            both_strings = _valid_types([self.operand1.expr, self.operand2.expr], [str])
            both_lists = _valid_types([self.operand1.expr, self.operand2.expr], [list])
            if both_numbers or both_strings or both_lists:
                return self.operand1.expr + self.operand2.expr
        elif self.operator == '-':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int, float]):
                return self.operand1.expr - self.operand2.expr
        elif self.operator == '*':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int, float]):
                return self.operand1.expr * self.operand2.expr
        elif self.operator == '/':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int, float]):
                if self.operand2.expr == 0:
                    raise SemanticError
                return self.operand1.expr / self.operand2.expr
        elif self.operator == 'div':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int]):
                if self.operand2.expr == 0:
                    raise SemanticError
                return self.operand1.expr // self.operand2.expr
        elif self.operator == 'mod':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int]):
                return self.operand1.expr % self.operand2.expr
        elif self.operator == '**':
            if _valid_types([self.operand1.expr, self.operand2.expr], [int, float]):
                return pow(self.operand1.expr, self.operand2.expr)
        elif self.operator == 'andalso':
            if _valid_types([self.operand1.expr, self.operand2.expr], [bool]):
                return self.operand1.expr and self.operand2.expr
        elif self.operator == 'orelse':
            if _valid_types([self.operand1.expr, self.operand2.expr], [bool]):
                return self.operand1.expr or self.operand2.expr
        elif self.operator == 'in':
            both_strings = _valid_types([self.operand1.expr, self.operand2.expr], [str])
            valid_operand1 = _valid_types([self.operand1.expr], [int, float, bool, str, list, tuple])
            valid_operand2 = _valid_types([self.operand2.expr], [list])
            if (valid_operand1 and valid_operand2) or both_strings:
                return self.operand1.expr in self.operand2.expr
        elif self.operator == '::':
            if _valid_types([self.operand1.expr], [int, float, bool, str, list, tuple]) and _valid_types([self.operand2.expr], [list]):
               return [self.operand1.expr] + self.operand2.expr
        elif self.operator == 'listindex':
            return self.operand1.lst[self.operand2.expr]
        elif self.operator == 'tupindex':
            return self.operand1.tups[self.operand2.expr]
        raise SemanticError  

    def __repr__(self):
        return f'BinaryOpNode(operand1={self.operand1}, operand2={self.operand2}, operator={self.operator})'


class ComparisonBinaryOpNode(BinaryOpNode):
    def __init__(self, operand1, operand2, operator):
        super().__init__(operand1, operand2, operator)
        
    def evaluate(self):
        if _valid_types([self.operand1.expr, self.operand2.expr], [int, float]) or _valid_types([self.operand1.expr, self.operand2.expr], [str]):
            if self.operator == '==':
               return self.operand1.expr == self.operand2.expr
            elif self.operator == '<>':
                return self.operand1.expr != self.operand2.expr
            elif self.operator == '>':
                return self.operand1.expr > self.operand2.expr
            elif self.operator == '>=':
                return self.operand1.expr >= self.operand2.expr
            elif self.operator == '<':
                return self.operand1.expr < self.operand2.expr
            elif self.operator == '<=':
                return self.operand1.expr <= self.operand2.expr
        raise SemanticError

    def __repr__(self):
        return f'BinaryOpNode(operand1={self.operand1}, operand2={self.operand2}, operator={self.operator})'

class UnaryOpNode(ExprNode):
    def __init__(self, operand, operator):
        self.operand = operand
        self.operator = operator
    
    def evaluate(self):
        if self.operator == 'not':
            if _valid_types([self.operand], [bool]):
                return not self.operand
        elif self.operator == '-':
            if _valid_types([self.operand], [int, float]):
                return self.operand * -1
      
    def __repr__(self):
        return f'UnaryOpNode(operand={self.operand}, operator={self.operator})'
    

class IfElseNode():
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block
        
    def evaluate(self):
        if self.condition.evaluate(): 
            self.if_block.evaluate()
        else:
            self.else_block.evaluate()

    def __repr__(self):
        return f'IfElseNode(condition={self.condition}, if_block={self.if_block}, else_block={self.else_block})'
    
class IfNode():
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block
    
    def evaluate(self):
        if self.condition.evaluate():
            self.block.evaluate()

    def __repr__(self):
        return f'IfNode(condition={self.condition}, block={self.block})'

class BlockNode():
    def __init__(self, statements):
        self.statements = statements
        
    def evaluate(self):
        for s in self.statements:
            s.evaluate()
            
    def __repr__(self):
        return f'BlockNode(statements={self.statements})'


class WhileNode():
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block

    def evaluate(self):
        while self.condition.evaluate():
            self.block.evaluate()

    def __repr__(self):
        return f'WhileNode(condition={self.condition}, block={self.block})'
    
## Print

class PrintNode():
    def __init__(self, expr):
        self.expr = expr
    
    def evaluate(self):
        evaluated = self.expr.evaluate()
        print(evaluated)

    def __repr__(self):
        return f'PrintNode(expr={self.expr})'

## Tokens

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
    'mod': 'MODOP'
 }

tokens = [
    'VARIABLE',
    'INTEGER', 'REAL', 'BOOLEAN', 'STRING',
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

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_DIVOP = r'\/'
t_MULOP = r'\*'
t_EXPOP = r'\*{2}'
t_PLUSOP = r'\+'
t_MINUSOP = r'\-'
t_LTOP = r'\<'
t_LTEOP = r'\<\='
t_EQOP = r'\={2}'
t_NEQOP = r'\<\>'
t_GTEOP = r'\>\='
t_GTOP = r'\>'
t_CONSOP = r'\:{2}'
t_COMMA = r'\,'
t_SEMICOLON = r'\;'
t_HASH = r'\#'
t_ASSIGNOP = r'\='
t_LBRACE = r'\{'
t_RBRACE = r'\}'

t_ignore = ' \t'

def t_VARIABLE(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'VARIABLE')
    t.value = t.value
    return t
    
## Data types

def t_REAL(t):
    r'(([-]?([0-9]+\.[0-9]*)|([0-9]*\.[0-9]+))([eE][-]?[0-9]+)?)'
    t.value = float(t.value)
    return t

def t_INTEGER(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_BOOLEAN(t):
    r'True|False'
    t.value = 'True' == t.value
    return t

def t_STRING(t):
    r'\'[^\']*\'|\"[^\"]*\"'
    t.value = t.value
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

## Lexing error

def t_error(t):
    t.lexer.skip(1)
    raise SyntaxError
    
lexer = lex.lex()

## PARSING
    
## Start statement    
    
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
        p[0] = [p[1]] + [p[2]]
    if len(p) == 2:
        p[0] = p[1]

def p_stmt(p):
    'stmt : expr SEMICOLON'
    p[0] = ExprNode(p[1])

def p_if(p):
    'stmt : IF expr block'
    p[0] = IfNode(ExprNode(p[2]), BlockNode(p[3]))

def p_ifelse(p):
    'stmt : IF expr block ELSE block'
    p[0] = IfElseNode(ExprNode(p[2]), BlockNode(p[3]), BlockNode(p[5]))

def p_while(p):
    'stmt : WHILE expr block'
    p[0] = WhileNode(ExprNode(p[2]), BlockNode(p[3]))

def p_print(p):
    'stmt : PRINT LPAREN expr RPAREN SEMICOLON'
    p[0] = PrintNode(p[3])
    
def p_paren(p):
    'expr : LPAREN expr RPAREN'
    p[0] = p[2]        
    
def p_assign(p):
    'stmt : VARIABLE ASSIGNOP expr SEMICOLON'
    p[0] = VariableNode(p[1], p[3])

## Wherever expr leads to... terminals and nested expressions 
    
def p_term(p):
    '''
    expr : STRING
         | INTEGER
         | REAL
         | BOOLEAN
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
    p[0] = p[1]
        
## Unary operations       
           
def p_not(p):
    'expr : NOTOP expr'
    p[0] = UnaryOpNode(p[2], p[1])
       
def p_uminus(p):
    'expr : MINUSOP expr %prec UMINUS'
    p[0] = UnaryOpNode(p[2], p[1])     
       
## Binary operations       
       
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
    p[0] = BinaryOpNode(ExprNode(p[1]), ExprNode(p[3]), p[2])
       
def p_comparison(p):
    '''
    expr : expr EQOP expr
         | expr NEQOP expr
         | expr GTOP expr
         | expr GTEOP expr
         | expr LTOP expr
         | expr LTEOP expr
    '''        
    p[0] = ComparisonBinaryOpNode(ExprNode([1]), ExprNode(p[3]), p[2])

## Lists

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
    listitems : listitems COMMA listitem
              | listitem
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[3]

def p_listitem(p):
    'listitem : expr'
    p[0] = [ExprNode(p[1])]

def p_genindex(p):
    '''
    listindex : list LBRACKET expr RBRACKET
              | expr LBRACKET expr RBRACKET
    '''
    is_int_index = type(p[3].expr) == int
    if isinstance(p[1], ListNode):
        if is_int_index and p[3].expr < 0 or p[3].expr >= len(p[1].lst): 
            raise SemanticError
        p[0] = BinaryOpNode(p[1], p[3], 'listindex')
        return
    else:
        if is_int_index and p[3] < 0 or p[3] >= len(p[1].expr): 
            raise SemanticError
        if _valid_types([p[3].expr], [int]) and _valid_types([p[1]], [str]):
            p[0] = BinaryOpNode(p[1], p[3], 'listindex')
            return
    raise SemanticError

    
## Tuples    
    
def p_tuple(p):
    '''
    tup : LPAREN tupitems RPAREN
        | LPAREN RPAREN
    '''
    if len(p) == 3:
        p[0] = ()
    elif len(p) == 4:
        p[0] = tuple(p[2])
    elif len(p[2]) == 1:
        p[0] = p[2][0]
                
def p_tupitems(p):
    '''
    tupitems : tupitems COMMA tupitem
             | tupitems COMMA
             | tupitem
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[3]                
                
def p_tupitem(p):
    'tupitem : expr'
    p[0] = [ExprNode(p[1])]
    
def p_tupindex(p):
    '''
    tupindex : HASH INTEGER LPAREN tupindex RPAREN
             | HASH INTEGER tup
    '''
    if len(p) == 6:
        if p[2] <= 0 or p[2] > len(p[4]):
            raise SemanticError
        p[0] = BinaryOpNode(TupleNode(p[4]), ExprNode(p[2] - 1), 'tupindex')
    else:
        if p[2] <= 0 or p[2] > len(p[3]):
            raise SemanticError
        p[0] = BinaryOpNode(TupleNode(p[3]), ExprNode(p[2] - 1), 'tupindex')

def _valid_types(arguments, types):
    """ Check if all the arguments are in the types list """
    for arg in arguments:
        if type(arg) not in types:
            return False
    return True

## Parsing error

def p_error(p):
    print(p)
    raise SyntaxError

## Precedence

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

if len(argv) != 2:
    print("Expecting filename in argv[1].")
    sys.exit()

# FOR SUBMISSION
# with open(argv[1], 'r') as file:
#     content = file.read()
#     try: 
#         result = parser.parse(content, debug=True)
#         print(f'RESULT: {result}')
#     except Exception as err:
#         print(err)


# FOR DEBUGGING PURPOSES
with open(argv[1], 'r') as file:
    content = file.read()
    result = parser.parse(content, debug=True)
    print(result)
    
