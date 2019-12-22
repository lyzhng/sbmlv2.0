from exceptions import SyntaxError, SemanticError

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
        if isinstance(self.expr, (VariableNode, ListNode, TupleNode, ListAccessNode, ListAssignNode)):
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
                valid_operand2 = _valid_types([self.operand2.evaluate()], [int])
                if self.operator == 'listindex':
                    valid_operand1 = _valid_types([assignments[self.operand1]], [str, list])
                if self.operator == 'tupindex':
                    valid_operand1 = _valid_types([assignments[self.operand1]], [tuple])
                if valid_operand1 and valid_operand2:
                    index = self.operand2.evaluate()
                    structure = assignments[self.operand1]
                    if index < 0 or index > len(structure):
                        raise SemanticError
                    return structure[self.operand2.evaluate()]
        if isinstance(self.operand1, ExprNode):
            valid_operand2 = _valid_types([self.operand2.evaluate()], [int])
            if self.operator == 'tupindex':
                valid_operand1 = _valid_types([self.operand1.evaluate()], [tuple])
            if self.operator == 'listindex':
                valid_operand1 = _valid_types([self.operand1.evaluate()], [str, list])
            if valid_operand1 and valid_operand2:
                index = self.operand2.evaluate()
                structure = self.operand1.evaluate()
                if index < 0 or index >= len(structure):
                    raise SemanticError
                return structure[self.operand2.evaluate()]
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
    
def _valid_types(arguments, types):
    """ Check if all the arguments are in the types list """
    for arg in arguments:
        if type(arg) not in types:
            return False
    return True