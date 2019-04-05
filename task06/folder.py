import model


class ConstantFolder(model.ASTNodeVisitor):
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
                function_definition.func.accept(self)
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
        if isinstance(lhs, model.Number) and isinstance(rhs, model.Number):
            return model.Number(
                binary_operation.operations[op](lhs.value, rhs.value))
        elif isinstance(lhs, model.Reference)\
            and isinstance(rhs, model.Reference)\
                and op == '-' and lhs.var_name == rhs.var_name:
            return model.Number(0)
        elif op == '*' and (
                isinstance(lhs, model.Number) and lhs.value == 0 or
                isinstance(rhs, model.Number) and rhs.value == 0):
            return model.Number(0)
        else:
            return model.BinaryOperation(op, lhs, rhs)

    def visit_number(self, number):
        return number

    def visit_unary_operation(self, unary_operation):
        val = unary_operation.expr.accept(self)
        if isinstance(val, model.Number):
            return model.Number(
                unary_operation.operations[unary_operation.op](val.value))
        else:
            return model.UnaryOperation(unary_operation.op, val)

    def visit_function(self, function):
        return model.Function(
            function.arg_names,
            [statement.accept(self) for statement in function.body]
        )


def fold_constants(program):
    return program.accept(ConstantFolder())
