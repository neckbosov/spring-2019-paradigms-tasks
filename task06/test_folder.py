import folder
import model
import printer


def syntax_tree_equals(tree1, tree2):
    if not isinstance(tree1, type(tree2)):
        return False
    if isinstance(tree1, model.Number):
        return tree1.value == tree2.value
    elif isinstance(tree1, model.Reference):
        return tree1.var_name == tree2.var_name
    elif isinstance(tree1, model.UnaryOperation):
        return tree1.op == tree2.op and syntax_tree_equals(
            tree1.expr, tree2.expr)
    elif isinstance(tree1, model.BinaryOperation):
        return tree1.op == tree2.op and\
            syntax_tree_equals(tree1.left, tree2.left) and\
            syntax_tree_equals(tree1.right, tree2.right)
    elif isinstance(tree1, model.Function):
        return tree1.arg_names == tree2.arg_names and\
            len(tree1.body) == len(tree2.body) and\
            all(syntax_tree_equals(st1, st2)
                for st1, st2 in zip(tree1.body, tree2.body))
    elif isinstance(tree1, model.FunctionDefinition):
        return tree1.name == tree2.name and syntax_tree_equals(
            tree1.func, tree2.func)
    elif isinstance(tree1, model.Conditional):
        return syntax_tree_equals(tree1.condition, tree2.condition) and\
            all(syntax_tree_equals(st1, st2)
                for st1, st2 in zip(tree1.if_true, tree2.if_true)) and\
            all(syntax_tree_equals(st1, st2)
                for st1, st2 in zip(tree1.if_false, tree2.if_false))
    elif isinstance(tree1, model.Print):
        return syntax_tree_equals(tree1.expr, tree2.expr)
    elif isinstance(tree1, model.Read):
        return tree1.var_name == tree2.var_name
    elif isinstance(tree1, model.FunctionCall):
        return syntax_tree_equals(tree1.fun_expr, tree2.fun_expr) and\
            all(syntax_tree_equals(st1, st2)
                for st1, st2 in zip(tree1.args, tree2.args))


def test_number_binary_operation_folder():
    operation = model.BinaryOperation(
        model.BinaryOperation(
            model.Number(12),
            '%',
            model.Number(5)
        ),
        '<',
        model.Number(3)
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(1)


def test_number_binary_operation_folder_nothing():
    operation = model.BinaryOperation(
        model.Reference('a'),
        '+',
        model.Reference('b')
    )
    assert syntax_tree_equals(
        operation.accept(
            folder.ConstantFolder()),
        operation)


def test_multiplication_by_zero_folder():
    operation = model.BinaryOperation(
        model.Reference('kek'),
        '*',
        model.Number(0)
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(0)


def test_itself_substraction_folder():
    operation = model.BinaryOperation(
        model.Reference('kek'),
        '-',
        model.Reference('kek')
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(0)


def test_number_unary_operation_folder():
    operation = model.UnaryOperation(
        '-', model.Number(42)
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(-42)


def test_combined(capsys):
    printer.pretty_print(folder.fold_constants(
        model.BinaryOperation(
            model.Number(10),
            '-',
            model.UnaryOperation(
                '-',
                model.BinaryOperation(
                    model.Number(3),
                    '+',
                    model.BinaryOperation(
                        model.Reference('x'),
                        '-',
                        model.Reference('x')
                    )
                )
            )
        )
    ))
    out, err = capsys.readouterr()
    assert not err
    assert out == '13;\n'
