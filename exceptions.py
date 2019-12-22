class SemanticError(RuntimeError):
    def __init__(self):
        super(SemanticError, self).__init__('SEMANTIC ERROR')


class SyntaxError(Exception):
    def __init__(self):
        super(SyntaxError, self).__init__('SYNTAX ERROR')