#!/usr/bin/env python3
import pytest
import model


def test_scope():
    gparent = model.Scope()
    parent = model.Scope(parent=gparent)
    child = model.Scope(parent=parent)
    parent['a'] = model.Number(0)
    parent['b'] = model.Number(1)
    child['a'] = model.Number(2)
    child['s'] = model.Number(4)
    gparent['x'] = model.Number(3)
    assert child['s'] == model.Number(4)
    assert child['a'] == model.Number(2)
    assert child['b'] == model.Number(1)
    assert child['x'] == model.Number(3)


def test_scope_error():
    scope = model.Scope()
    with pytest.raises(KeyError, match='bar'):
        scope['bar']


def test_function_definition():
    scope = model.Scope()
    foo = model.Function(
        [
            'val'
        ],
        [
            model.Number(1)
        ]
    )
    bar = model.FunctionDefinition('foo', foo).evaluate(scope)
    assert bar is foo
    assert scope['foo']


def test_conditional_true():
    scope = model.Scope()
    cond = model.Conditional(
        model.Number(0),
        [model.Number(1)],
        [model.Number(2)]
    )
    assert cond.evaluate(scope) == model.Number(2)


def test_conditional_false():
    scope = model.Scope()
    cond = model.Conditional(
        model.Number(1),
        [model.Number(1)],
        [model.Number(2)]
    )
    assert cond.evaluate(scope) == model.Number(1)


def test_print(capsys):
    scope = model.Scope()
    result = model.Print(model.Number(346)).evaluate(scope)
    assert result == model.Number(346)
    out, err = capsys.readouterr()
    assert out == '346\n'
    assert not err


def test_read(monkeypatch):
    monkeypatch.setattr('builtins.input', lambda: '228')
    scope = model.Scope()
    result = model.Read('a').evaluate(scope)
    assert result == model.Number(228)
    assert scope['a'] == model.Number(228)


def test_reference():
    scope = model.Scope()
    scope['var'] = model.Number(19)
    assert model.Reference('var').evaluate(scope) == model.Number(19)


def test_function_call():
    scope = model.Scope()
    foo = model.Function(
        [
            'val'
        ],
        [
            model.Reference('val')
        ]
    )
    result = model.FunctionCall(foo, [model.Number(0)]).evaluate(scope)
    assert result == model.Number(0)


def test_binary_operation():
    a = model.Number(2)
    b = model.Number(3)
    scope = model.Scope()
    assert model.BinaryOperation(a, '+', b).evaluate(scope) == model.Number(5)
    assert model.BinaryOperation(b, '/', a).evaluate(scope) == model.Number(1)
    assert model.BinaryOperation(b, '==', a).evaluate(scope) == model.Number(0)
    assert model.BinaryOperation(b, '!=', a).evaluate(scope) != model.Number(0)
    assert model.BinaryOperation(b, '>', a).evaluate(scope) != model.Number(0)
    assert model.BinaryOperation(b, '&&', a).evaluate(scope) != model.Number(0)
    with pytest.raises(NotImplementedError):
        model.BinaryOperation(a, '^^', b).evaluate(scope)


def test_unary_operation():
    a = model.Number(2)
    scope = model.Scope()
    assert model.UnaryOperation('-', a).evaluate(scope) == model.Number(-2)
    assert model.UnaryOperation('!', a).evaluate(scope) == model.Number(0)
    with pytest.raises(NotImplementedError):
        model.UnaryOperation('~', a).evaluate(scope)


def test_factorial():
    scope = model.Scope()
    fact = model.Function(
        ['n'],
        [
            model.Conditional(
                model.BinaryOperation(
                    model.Reference('n'),
                    '<=',
                    model.Number(1)
                ),
                [
                    model.Number(1)
                ],
                [
                    model.BinaryOperation(
                        model.FunctionCall(
                            model.Reference('factorial'),
                            [
                                model.BinaryOperation(
                                    model.Reference('n'),
                                    '-',
                                    model.Number(1)
                                )
                            ]
                        ),
                        '*',
                        model.Reference('n')
                    )

                ]
            )
        ]
    ).evaluate(scope)
    model.FunctionDefinition('factorial', fact).evaluate(scope)
    result = model.FunctionCall(
        model.Reference('factorial'),
        [model.Number(5)]
    ).evaluate(scope)
    assert result == model.Number(120)


if __name__ == '__main__':
    pytest.main()
