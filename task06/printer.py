import model


class ExpressionPrinter(model.ASTNodeVisitor):
    def __init__(self):
        pass

    def visit_number(self, number):
        return str(number.value)

    def visit_function_definition(self, function_definition):
        raise NotImplementedError(
            'Function definition are not allowed in statement')

    def visit_conditional(self, condition):
        raise NotImplementedError(
            'Conditional are not allowed in statement')

    def visit_print(self, print_object):
        raise NotImplementedError(
            'Print are not allowed in statement')

    def visit_read(self, read_object):
        raise NotImplementedError(
            'Read are not allowed in statement')

    def visit_function_call(self, function_call):
        result = function_call.fun_expr.accept(self) + '('
        result += ', '.join(arg.accept(self) for arg in function_call.args)
        return result + ')'

    def visit_reference(self, reference):
        return reference.var_name

    def visit_binary_operation(self, binary_operation):
        return '(' + binary_operation.left.accept(self) + \
            ') ' + binary_operation.op + \
            ' (' + binary_operation.right.accept(self) + ')'

    def visit_unary_operation(self, unary_operation):
        return unary_operation.op + \
            '(' + unary_operation.expr.accept(self) + ')'


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.expression_printer = ExpressionPrinter()

    def visit_number(self, number):
        return str(number.value) + ';'

    def visit_function_definition(self, function_definition):
        result = 'def ' + function_definition.name + '('
        result += ', '.join(function_definition.func.args) + ') {\n'
        for statement in function_definition.func.body:
            lines = statement.accept(self).split('\n')
            result += '\n'.join(' ' * 4 + line for line in lines) + '\n'
        return result + '}'

    def visit_conditional(self, condition):
        result = 'if (' + condition.accept(self.expression_printer) + ') {\n'
        for statement in condition.if_true:
            lines = statement.accept(self).split('\n')
            result += '\n'.join(' ' * 4 + line for line in lines) + '\n'
        result += '}'
        if condition.if_false:
            result += ' else {\n'
            for statement in condition.if_false:
                lines = statement.accept(self).split('\n')
                result += '\n'.join(' ' * 4 + line for line in lines) + '\n'
            result += '}'
        return result

    def visit_print(self, print_object):
        return 'print ' + \
            print_object.expr.accept(self.expression_printer) + ';'

    def visit_read(self, read_object):
        return 'read ' + read_object.var_name + ';'

    def visit_function_call(self, function_call):
        return function_call.accept(self.expression_printer) + ';'

    def visit_reference(self, reference):
        return reference.var_name + ';'

    def visit_binary_operation(self, binary_operation):
        return binary_operation.accept(self.expression_printer) + ';'

    def visit_unary_operation(self, unary_operation):
        return unary_operation.accept(self.expression_printer) + ';'