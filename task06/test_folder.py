import folder
import model
import printer


def test_number_binary_operation():
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


def test_number_binary_operation_nothing():
    operation = model.BinaryOperation(
        model.Reference('a'),
        '+',
        model.Reference('b')
    )
    new_op = operation.accept(folder.ConstantFolder())
    assert (isinstance(new_op, model.BinaryOperation) and
            new_op.op == operation.op and
            isinstance(new_op.left, model.Reference) and
            new_op.left.var_name == 'a' and
            isinstance(new_op.right, model.Reference) and
            new_op.right.var_name == 'b')


def test_multiplication_by_zero():
    operation = model.BinaryOperation(
        model.Reference('kek'),
        '*',
        model.Number(0)
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(0)
    operation = model.BinaryOperation(
        model.Number(0),
        '*',
        model.Reference('kek')
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(0)


def test_itself_substraction():
    operation = model.BinaryOperation(
        model.Reference('kek'),
        '-',
        model.Reference('kek')
    ).accept(folder.ConstantFolder())
    assert operation == model.Number(0)


def test_number_unary_operation():
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
