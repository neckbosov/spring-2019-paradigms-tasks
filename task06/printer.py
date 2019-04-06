import model


class ExpressionPrinter(model.ASTNodeVisitor):
    def visit_number(self, number):
        return str(number.value)

    def visit_function(self, function):
        raise TypeError(
            'Functions are not allowed in expression')

    def visit_function_definition(self, function_definition):
        raise TypeError(
            'Function definition are not allowed in expression')

    def visit_conditional(self, condition):
        raise TypeError(
            'Conditional are not allowed in expression')

    def visit_print(self, print_object):
        raise TypeError(
            'Print are not allowed in expression')

    def visit_read(self, read_object):
        raise TypeError(
            'Read are not allowed in expression')

    def visit_function_call(self, function_call):
        result = function_call.fun_expr.accept(self) + '('
        result += ', '.join(arg.accept(self) for arg in function_call.args)
        return result + ')'

    def visit_reference(self, reference):
        return reference.var_name

    def visit_binary_operation(self, binary_operation):
        return ('(' + binary_operation.left.accept(self) +
                ') ' + binary_operation.op +
                ' (' + binary_operation.right.accept(self) + ')')

    def visit_unary_operation(self, unary_operation):
        return (unary_operation.op +
                '(' + unary_operation.expr.accept(self) + ')')


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.expression_printer = ExpressionPrinter()

    def indent_statements(self, statements):
        from textwrap import indent
        result = ''
        for statement in statements or []:
            result += indent(statement.accept(self), '    ') + '\n'
        return result

    def visit_number(self, number):
        return str(number.value) + ';'

    def visit_function(self, function):
        raise TypeError("Functions not allowed in printer")

    def visit_function_definition(self, function_definition):
        result = 'def ' + function_definition.name + '('
        result += ', '.join(function_definition.func.arg_names) + ') {\n'
        result += self.indent_statements(function_definition.func.body)
        return result + '}'

    def visit_conditional(self, condition):
        result = 'if (' + \
            condition.condition.accept(self.expression_printer) + ') {\n'
        result += self.indent_statements(condition.if_true)
        result += '}'
        if condition.if_false:
            result += ' else {\n'
            result += self.indent_statements(condition.if_false)
            result += '}'
        return result

    def visit_print(self, print_object):
        return ('print ' +
                print_object.expr.accept(self.expression_printer) + ';')

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


def pretty_print(program):
    printer = PrettyPrinter()
    print(program.accept(printer))
