import printer
import model


def test_conditional_printer_empty():
    pretty_printer = printer.PrettyPrinter()
    result = '''if (42) {
}'''
    assert model.Conditional(model.Number(42),
                             [], []).accept(pretty_printer) == result


def test_conditional_printer():
    pretty_printer = printer.PrettyPrinter()
    condition = model.Conditional(
        model.Number(42),
        [model.Print(model.Number(1))],
        [model.Print(model.Number(0))]
    )
    result = 'if (42) {\n    print 1;\n} else {\n    print 0;\n}'
    assert condition.accept(pretty_printer) == result


def test_conditional_if_true_none():
    pretty_printer = printer.PrettyPrinter()
    condition = model.Conditional(
        model.Number(42),
        None,
        [model.Print(model.Number(0))]
    )
    result = 'if (42) {\n} else {\n    print 0;\n}'
    assert condition.accept(pretty_printer) == result


def test_function_definition_printer_empty():
    pretty_printer = printer.PrettyPrinter()
    result = 'def foo() {\n}'
    function = model.FunctionDefinition("foo", model.Function([], []))
    assert function.accept(pretty_printer) == result


def test_function_definition_printer():
    pretty_printer = printer.PrettyPrinter()
    function = model.Function(
        ['a', 'b'],
        [
            model.Print(model.Reference('a')),
            model.Print(model.Reference('b'))
        ]
    )
    func_def = model.FunctionDefinition('foo', function)
    result = 'def foo(a, b) {\n    print a;\n    print b;\n}'
    assert func_def.accept(pretty_printer) == result


def test_print_printer():
    pretty_printer = printer.PrettyPrinter()
    assert model.Print(model.Number(42)).accept(pretty_printer) == 'print 42;'


def test_read_printer():
    pretty_printer = printer.PrettyPrinter()
    assert model.Read('x').accept(pretty_printer) == 'read x;'


def test_number_printer():
    pretty_printer = printer.PrettyPrinter()
    assert model.Number(10).accept(pretty_printer) == '10;'


def test_reference_printer():
    pretty_printer = printer.PrettyPrinter()
    assert model.Reference('x').accept(pretty_printer) == 'x;'


def test_binary_operation_printer():
    pretty_printer = printer.PrettyPrinter()
    add = model.BinaryOperation(model.Number(2), '+', model.Number(3))
    mul = model.BinaryOperation(model.Number(1), '*', add)
    assert mul.accept(pretty_printer) == '(1) * ((2) + (3));'


def test_unary_operation_printer():
    pretty_printer = printer.PrettyPrinter()
    assert model.UnaryOperation(
        '-', model.Number(42)).accept(pretty_printer) == '-(42);'


def test_function_call_printer():
    pretty_printer = printer.PrettyPrinter()
    func_call = model.FunctionCall(
        model.Reference('foo'), [
            model.Number(1),
            model.Number(2),
            model.Number(3)
        ]
    )
    assert func_call.accept(pretty_printer) == 'foo(1, 2, 3);'


def test_pretty_print(capsys):
    function = model.Function(['arg1'], [
        model.Read('x'),
        model.Print(model.Reference('x')),
        model.Conditional(
            model.BinaryOperation(
                model.Number(2), '==', model.Number(3)),
            [
                model.Conditional(
                    model.Number(1), [], [])
            ],
            [
                model.FunctionCall(model.Reference('exit'), [
                    model.UnaryOperation('-', model.Reference('arg1'))
                ]
                )
            ],
        ),
    ]
    )
    func_def = model.FunctionDefinition('main', function)
    printer.pretty_print(func_def)
    out, err = capsys.readouterr()
    assert not err
    assert out == '''def main(arg1) {
    read x;
    print x;
    if ((2) == (3)) {
        if (1) {
        }
    } else {
        exit(-(arg1));
    }
}
'''
