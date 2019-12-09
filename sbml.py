# Code written by Lily Zhong (ID# 110959687)

import sys
import ply.lex as lex
import ply.yacc as yacc
from sys import argv


class SemanticError(RuntimeError):
    def __init__(self):
        super(SemanticError, self).__init__('SEMANTIC ERROR')


class SyntaxError(Exception):
    def __init__(self):
        super(SyntaxError, self).__init__('SYNTAX ERROR')

        
DEBUG = len(sys.argv) > 2 and sys.argv[2] == '--debug'

assignments = {}


class ExprNode():
    def __init__(self, expr):
        self.expr = expr

    def evaluate(self):
        if type(self.expr) == str:
            single_quote = self.expr.startswith('\'') and self.expr.endswith('\'')
            double_quote = self.expr.startswith('\"') and self.expr.endswith('\"')
            if single_quote or double_quote: # actual string
                return self.expr[1:-1]
            elif not single_quote and not double_quote:
                if self.expr in assignments:
                    return assignments[self.expr]
            raise SemanticError
        if isinstance(self.expr, (ListNode, TupleNode, ListAccessNode)):
            return self.expr.evaluate()
        return self.expr
    
    def __repr__(self):
        return f'ExprNode(expr={self.expr})'

    
class VariableNode(ExprNode):
    def __init__(self, var_name, value):
        self.var_name = var_name
        self.value = value
        
    def evaluate(self):
        assignments[self.var_name] = self.value.evaluate()
        
    def __repr__(self):
        return f'VariableNode(var_name={self.var_name}, value={self.value})'


class ListNode(ExprNode):
    def __init__(self, lst):
        super().__init__(lst)
    
    def evaluate(self):
        return [item.evaluate() for item in self.expr]
            
    def __repr__(self):
        return f'ListNode(lst={self.expr})'


class TupleNode(ExprNode):
    def __init__(self, tups):
        super().__init__(tups)
        
    def evaluate(self):
        return tuple([item.evaluate() for item in self.expr])

    def __repr__(self):
        return f'TupleNode(tups={self.expr})'
    
    
class BinaryOpNode(ExprNode):
    def __init__(self, operand1, operand2, operator):
        self.operand1 = operand1
        self.operand2 = operand2
        self.operator = operator
        
    def evaluate(self):
        if self.operator == '+':
            both_numbers = _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float])
            both_strings = _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [str])
            both_lists = _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [list])
            if both_numbers or both_strings or both_lists:
                return self.operand1.evaluate() + self.operand2.evaluate()
        elif self.operator == '-':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float]):
                return self.operand1.evaluate() - self.operand2.evaluate()
        elif self.operator == '*':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float]):
                return self.operand1.evaluate() * self.operand2.evaluate()
        elif self.operator == '/':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float]):
                if self.operand2.evaluate() == 0:
                    raise SemanticError
                return self.operand1.evaluate() / self.operand2.evaluate()
        elif self.operator == 'div':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int]):
                if self.operand2.evaluate() == 0:
                    raise SemanticError
                return self.operand1.expr // self.operand2.expr
        elif self.operator == 'mod':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int]):
                return self.operand1.evaluate() % self.operand2.evaluate()
        elif self.operator == '**':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float]):
                return pow(self.operand1.evaluate(), self.operand2.evaluate())
        elif self.operator == 'andalso':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [bool]):
                return self.operand1.evaluate() and self.operand2.evaluate()
        elif self.operator == 'orelse':
            if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [bool]):
                return self.operand1.evaluate() or self.operand2.evaluate()
        elif self.operator == 'in':
            both_strings = _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [str])
            valid_operand1 = _valid_types([self.operand1.evaluate()], [int, float, bool, str, list, tuple])
            valid_operand2 = _valid_types([self.operand2.evaluate()], [list])
            if (valid_operand1 and valid_operand2) or both_strings:
                return self.operand1.evaluate() in self.operand2.evaluate()
        elif self.operator == '::':
            if _valid_types([self.operand1.evaluate()], [int, float, bool, str, list, tuple]) and _valid_types([self.operand2.evaluate()], [list]):
               return [self.operand1.evaluate()] + self.operand2.evaluate()
        raise SemanticError  

    def __repr__(self):
        return f'BinaryOpNode(operand1={self.operand1}, operand2={self.operand2}, operator={self.operator})'


class ListAccessNode(BinaryOpNode):
    def __init__(self, var_name, index, operator):
        super().__init__(var_name, index, operator)
        
    def evaluate(self):
        if type(self.operand1) == str:
            if self.operand1 in assignments:
                valid_operand1 = _valid_types([assignments[self.operand1]], [str, tuple, list])
                valid_operand2 = _valid_types([self.operand2.evaluate()], [int])
                if valid_operand1 and valid_operand2:
                    return assignments[self.operand1][self.operand2.evaluate()]
        if isinstance(self.operand1, ExprNode):
            valid_operand1 = _valid_types([self.operand1.evaluate()], [str, list, tuple])
            valid_operand2 = _valid_types([self.operand2.evaluate()], [int])
            if valid_operand1 and valid_operand2:
                if self.operand2.evaluate() < 0 or self.operand2.evaluate() >= len(self.operand1.evaluate()):
                    raise SemanticError
                lst = self.operand1.evaluate()
                return lst[self.operand2.evaluate()]
        raise SemanticError

    def __repr__(self):
        return f'ListAccessNode(operand1={self.operand1}, operand2={self.operand2}, operator={self.operator})'


class ComparisonBinaryOpNode(BinaryOpNode):
    def __init__(self, operand1, operand2, operator):
        super().__init__(operand1, operand2, operator)
        
    def evaluate(self):
        if _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [int, float]) or _valid_types([self.operand1.evaluate(), self.operand2.evaluate()], [str]):
            if self.operator == '==':
               return self.operand1.evaluate() == self.operand2.evaluate()
            elif self.operator == '<>':
                return self.operand1.evaluate() != self.operand2.evaluate()
            elif self.operator == '>':
                return self.operand1.evaluate() > self.operand2.evaluate()
            elif self.operator == '>=':
                return self.operand1.evaluate() >= self.operand2.evaluate()
            elif self.operator == '<':
                return self.operand1.evaluate() < self.operand2.evaluate()
            elif self.operator == '<=':
                return self.operand1.evaluate() <= self.operand2.evaluate()
        raise SemanticError

    def __repr__(self):
        return f'ComparisonBinaryOpNode(operand1={self.operand1}, operand2={self.operand2}, operator={self.operator})'


class UnaryOpNode(ExprNode):
    def __init__(self, operand, operator):
        self.operand = operand
        self.operator = operator
    
    def evaluate(self):
        if self.operator == 'not':
            if _valid_types([self.operand.evaluate()], [bool]):
                return not self.operand.evaluate()
        elif self.operator == '-':
            if _valid_types([self.operand.evaluate()], [int, float]):
                return self.operand.evaluate() * -1
      
    def __repr__(self):
        return f'UnaryOpNode(operand={self.operand}, operator={self.operator})'
    

class IfElseNode():
    def __init__(self, condition, if_block, else_block):
        self.condition = condition
        self.if_block = if_block
        self.else_block = else_block
        
    def evaluate(self):
        condition = self.condition.evaluate()
        if _valid_types([condition], [bool]):
            if condition:
                self.if_block.evaluate()
            else:
                self.else_block.evaluate()
            return
        raise SemanticError

    def __repr__(self):
        return f'IfElseNode(condition={self.condition}, if_block={self.if_block}, else_block={self.else_block})'
    
    
class IfNode():
    def __init__(self, condition, block):
        self.condition = condition
        self.block = block
    
    def evaluate(self):
        condition = self.condition.evaluate()
        if _valid_types([condition], [bool]):
            if condition:
                self.block.evaluate()
            return
        raise SemanticError

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
        if not _valid_types([self.condition.evaluate()], [bool]):
            raise SemanticError
        while _valid_types([self.condition.evaluate()], [bool]) and self.condition.evaluate():
            self.block.evaluate()
            
    def __repr__(self):
        return f'WhileNode(condition={self.condition}, block={self.block})'
    
    
class ListAssignNode():
    def __init__(self, data, index, value):
        self.data = data
        self.index = index
        self.value = value
        
    def evaluate(self):
        if isinstance(self.data, ListNode):
            self.data.evaluate()[self.index.evaluate()] = self.value.evaluate()
        else:
            lst = assignments[self.data]
            lst[self.index.evaluate()] = self.value.evaluate()
    
    
class PrintNode():
    def __init__(self, expr):
        self.expr = expr
    
    def evaluate(self):
        evaluated = self.expr.evaluate()
        print(evaluated)

    def __repr__(self):
        return f'PrintNode(expr={self.expr})'


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
    listitems : listitems COMMA listitem
              | listitem
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + p[3]

def p_listitem(p):
    'listitem : expr'
    p[0] = [p[1]]

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
    p[0] = [p[1]]
    
def p_tupindex(p):
    '''
    tupindex : HASH INTEGER LPAREN tupindex RPAREN
             | HASH INTEGER tup
    '''
    if len(p) == 6:
        p[0] = ListAccessNode(p[4], ExprNode(p[2] - 1), 'tupindex')
    else:
        p[0] = ListAccessNode(p[3], ExprNode(p[2] - 1), 'tupindex')

def _valid_types(arguments, types):
    """ Check if all the arguments are in the types list """
    for arg in arguments:
        if type(arg) not in types:
            return False
    return True

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