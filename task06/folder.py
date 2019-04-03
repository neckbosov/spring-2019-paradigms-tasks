import model


class ConstantFolder(model.ASTNodeVisitor):
    def __init__(self):
        pass

    def visit_conditional(self, condition):
        return model.Conditional(
            condition.condition.accept(self),
            [statement.accept(self) for statement in condition.if_true],
            [statement.accept(self) for statement in condition.if_false],
        )

    def visit_function_definition(self, function_definition):
        return model.FunctionDefinition(
            function_definition.name,
            model.Function(
                function_definition.func.arg_names,
                [statement.accept(self)
                 for statement in function_definition.func.body]
            )
        )

    def visit_print(self, print_object):
        return model.Print(print_object.expr.accept(self))

    def visit_read(self, read_object):
        return read_object

    def visit_function_call(self, function_call):
        return model.FunctionCall(
            function_call.fun_expr.accept(self),
            [arg.accept(self) for arg in function_call.args]
        )

    def visit_reference(self, reference):
        return reference

    def visit_binary_operation(self, binary_operation):
        lhs = binary_operation.left.accept(self)
        rhs = binary_operation.right.accept(self)
        op = binary_operation.op
        if isinstance(model.Number, lhs) and isinstance(model.Number, rhs):
            return binary_operation.operations[op](lhs, rhs)
        elif isinstance(model.Reference, lhs)\
            and isinstance(model.Reference(rhs))\
                and op == '-' and lhs.var_name == rhs.var_name:
            return model.Number(0)
        elif op == '*' and (
                isinstance(model.Number, lhs) and lhs.value == 0 or
                isinstance(model.Number, lhs) and lhs.value == 0):
            return model.Number(0)
        else:
            return model.BinaryOperation(op, lhs, rhs)

    def visit_number(self, number):
        return number

    def visit_unary_operation(self, unary_operation):
        val = unary_operation.expr.accept(self)
        if isinstance(model.Number, val):
            return unary_operation.operations[unary_operation.op](val)
        else:
            return model.UnaryOperation(unary_operation.op, val)


def fold_constants(program):
    return program.accept(ConstantFolder())
