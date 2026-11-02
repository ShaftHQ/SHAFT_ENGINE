#!/usr/bin/env python3
"""Stdlib estimate of PMD's NPathComplexity for Java methods (Codacy's NPath gate).

Follows PMD 7 ``NpathBaseVisitor``: statements in a block multiply; ``if`` adds
its branches plus the ``&&``/``||``/ternary count of its condition; loops add one;
``switch`` sums its branches; ``try`` sums resources, body, catches and finally;
a ternary adds its parts minus one; lambdas and anonymous classes count inside
the enclosing method. Reported per named-class method or constructor.
"""

from __future__ import annotations

import re
from math import prod

_TOKEN = re.compile(
    r'"""(?:\\.|[^\\])*?"""|"(?:\\.|[^"\\\n])*"|\'(?:\\.|[^\'\\\n])*\''
    r"|//[^\n]*|/\*.*?\*/"
    r"|[A-Za-z_$][\w$]*|\d[\w.]*"
    r"|->|::|&&|\|\||[=!<>+\-*/%&|^]=|\+\+|--|\S",
    re.S,
)
_OPEN = {"(": ")", "[": "]", "{": "}"}
_TYPES = {"class", "interface", "enum", "record"}
_ASSIGN = {"=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^="}
_WILDCARD_NEXT = {">", ",", "extends", "&"}


def tokenize(source: str) -> list[tuple[str, int]]:
    """Code tokens with their 1-based line; comments dropped, literals kept as one token."""
    tokens, line, last = [], 1, 0
    for match in _TOKEN.finditer(source):
        line += source.count("\n", last, match.start())
        last = match.start()
        text = match.group()
        if not text.startswith(("//", "/*")):
            tokens.append((text, line))
    return tokens


def _close(toks: list[str], start: int) -> int:
    """Index of the bracket closing ``toks[start]``."""
    depth = 0
    for index in range(start, len(toks)):
        if toks[index] in _OPEN:
            depth += 1
        elif toks[index] in (")", "]", "}"):
            depth -= 1
            if depth == 0:
                return index
    return len(toks) - 1


def _top_level(toks: list[str], wanted) -> list[int]:
    """Indexes of tokens matching ``wanted`` outside any bracket."""
    depth, hits = 0, []
    for index, tok in enumerate(toks):
        if tok in (")", "]", "}"):
            depth -= 1
        elif depth == 0 and wanted(index, tok):
            hits.append(index)
        if tok in _OPEN:
            depth += 1
    return hits


def _split(toks: list[str], sep: str) -> list[list[str]]:
    parts, start = [], 0
    for index in _top_level(toks, lambda _i, tok: tok == sep):
        parts.append(toks[start:index])
        start = index + 1
    parts.append(toks[start:])
    return [part for part in parts if part]


def _strip_parens(toks: list[str]) -> list[str]:
    while len(toks) > 1 and toks[0] == "(" and _close(toks, 0) == len(toks) - 1:
        toks = toks[1:-1]
    return toks


def _is_ternary(toks: list[str], index: int) -> bool:
    nxt = toks[index + 1] if index + 1 < len(toks) else ">"
    after = toks[index + 2] if index + 2 < len(toks) else ""
    return toks[index] == "?" and nxt not in _WILDCARD_NEXT and not (nxt == "super" and after != ".")


def _ternary_parts(toks: list[str]):
    """Split ``c ? a : b`` at its top-level operator, or return None."""
    marks = _top_level(toks, lambda i, tok: tok == ":" or _is_ternary(toks, i))
    if not marks or toks[marks[0]] != "?":
        return None
    depth = 0
    for index in marks:
        depth += 1 if toks[index] == "?" else -1
        if depth == 0:
            return toks[:marks[0]], toks[marks[0] + 1:index], toks[index + 1:]
    return None


def bool_complexity(toks: list[str]) -> int:
    """PMD ``booleanExpressionComplexity``: ``&&`` and ``||``; a ternary root adds two plus its parts."""
    toks = _strip_parens(toks)
    parts = _ternary_parts(toks)
    if parts:
        return sum(bool_complexity(part) for part in parts) + 2
    return _conditionals(toks)


def _conditionals(toks: list[str]) -> int:
    """Count ``&&`` and ``||``, not looking inside lambdas or anonymous classes."""
    if _top_level(toks, lambda _i, tok: tok == "->"):
        return 0
    count, index = 0, 0
    while index < len(toks):
        tok = toks[index]
        if tok in _OPEN:
            end = _close(toks, index)
            if not (tok == "{" and index and toks[index - 1] == ")"):
                count += sum(_conditionals(part) for part in _split(toks[index + 1:end], ","))
            index = end
        elif tok in ("&&", "||"):
            count += 1
        index += 1
    return count


def _group_npath(toks: list[str], start: int, end: int) -> int:
    inner = toks[start + 1:end]
    if toks[start] == "{" and start and toks[start - 1] == ")":
        return _class_body_npath(inner)  # anonymous class body
    separator = "," if toks[start] != "[" else ";"
    return prod(expr_npath(part) for part in _split(inner, separator))


def expr_npath(toks: list[str]) -> int:
    """NPath of one expression: children multiply, ternaries add, lambdas count their bodies."""
    toks = _strip_parens(toks)
    if not toks:
        return 1
    assign = _top_level(toks, lambda _i, tok: tok in _ASSIGN)
    if assign:
        return expr_npath(toks[:assign[0]]) * expr_npath(toks[assign[0] + 1:])
    arrow = _top_level(toks, lambda _i, tok: tok == "->")
    if arrow:
        body = toks[arrow[0] + 1:]
        return block_npath(body[1:-1]) if body[0] == "{" else expr_npath(body)
    parts = _ternary_parts(toks)
    if parts:
        cond, then, other = parts
        return expr_npath(cond) + expr_npath(then) + expr_npath(other) + bool_complexity(cond) - 1
    result, index = 1, 0
    while index < len(toks):
        if toks[index] == "switch" and index + 1 < len(toks) and toks[index + 1] == "(":
            index, value = _switch(toks, index)
            result *= value
            continue
        if toks[index] in _OPEN:
            end = _close(toks, index)
            result *= _group_npath(toks, index, end)
            index = end
        index += 1
    return result


def _statement_end(toks: list[str], start: int) -> int:
    """Index just past the simple statement starting at ``start`` (ends at a top-level ``;``)."""
    index = start
    while index < len(toks) and toks[index] != ";":
        index = _close(toks, index) + 1 if toks[index] in _OPEN else index + 1
    return index + 1


def _paren(toks: list[str], index: int) -> tuple[list[str], int]:
    """Tokens inside the parentheses at ``index`` and the index after them."""
    end = _close(toks, index)
    return toks[index + 1:end], end + 1


def _switch(toks: list[str], index: int, branch_product: bool = False) -> tuple[int, int]:
    """NPath of the switch at ``index``: branches sum; as a returned expression PMD multiplies them."""
    tested, body_start = _paren(toks, index + 1)
    body_end = _close(toks, body_start)
    body = toks[body_start + 1:body_end]
    total, pending, cursor = 0, 0, 0
    while cursor < len(body):
        if body[cursor] not in ("case", "default"):
            cursor += 1
            continue
        label_end = cursor + 1
        while label_end < len(body) and body[label_end] not in (":", "->"):
            label_end = _close(body, label_end) + 1 if body[label_end] in _OPEN else label_end + 1
        alts = 1 if body[cursor] == "default" else len(_split(body[cursor + 1:label_end], ","))
        stop = label_end + 1
        while stop < len(body) and body[stop] not in ("case", "default"):
            stop = _close(body, stop) + 1 if body[stop] in _OPEN else stop + 1
        rhs = body[label_end + 1:stop]
        if branch_product and body[label_end] == "->":
            total = (total or 1) * _arrow_npath(rhs) * alts
        elif body[label_end] == "->":
            total += _arrow_npath(rhs) * alts
        else:
            pending += alts
            if rhs:
                total += block_npath(rhs) * pending
                pending = 0
        cursor = stop
    if branch_product:
        return body_end + 1, total * expr_npath(tested)
    return body_end + 1, total + bool_complexity(tested)


def _arrow_npath(rhs: list[str]) -> int:
    if rhs and rhs[0] == "{":
        return block_npath(rhs[1:-1])
    if rhs and rhs[0] == "throw":
        return expr_npath(rhs[1:-1])
    return expr_npath(rhs[:-1] if rhs and rhs[-1] == ";" else rhs)


def _if(toks: list[str], index: int) -> tuple[int, int]:
    cond, after = _paren(toks, index + 1)
    after, then = statement(toks, after)
    other = 1
    if after < len(toks) and toks[after] == "else":
        after, other = statement(toks, after + 1)
    return after, then + other + bool_complexity(cond)


def _loop(toks: list[str], index: int) -> tuple[int, int]:
    header, after = _paren(toks, index + 1)
    after, body = statement(toks, after)
    semis = _top_level(header, lambda _i, tok: tok == ";")
    if toks[index] == "while":
        cond = header
    elif len(semis) == 2:
        cond = header[semis[0] + 1:semis[1]]
    else:
        cond = []  # enhanced for: body + 1
    return after, body + bool_complexity(cond) + 1


def _do(toks: list[str], index: int) -> tuple[int, int]:
    after, body = statement(toks, index + 1)
    cond, after = _paren(toks, after + 1)
    return after + 1, body + bool_complexity(cond) + 1


def _try(toks: list[str], index: int) -> tuple[int, int]:
    total, cursor = 0, index + 1
    if toks[cursor] == "(":
        resources, cursor = _paren(toks, cursor)
        total += prod(expr_npath(part) for part in _split(resources, ";"))
    while cursor < len(toks) and toks[cursor] in ("{", "catch", "finally"):
        if toks[cursor] == "catch":
            _param, cursor = _paren(toks, cursor + 1)
        elif toks[cursor] == "finally":
            cursor += 1
        end = _close(toks, cursor)
        total += block_npath(toks[cursor + 1:end])
        cursor = end + 1
    return cursor, total


def _return(toks: list[str], index: int) -> tuple[int, int]:
    end = _statement_end(toks, index)
    expr = _strip_parens(toks[index + 1:end - 1])
    if not expr:
        return end, 1
    parts = _ternary_parts(expr)
    if parts:
        children = prod(expr_npath(part) for part in parts)
    elif expr[0] == "switch":
        children = _switch(expr, 0, branch_product=True)[1]
    else:
        children = expr_npath(expr)
    return end, children + bool_complexity(expr)


_STATEMENTS = {"if": _if, "while": _loop, "for": _loop, "do": _do, "try": _try, "return": _return}


def statement(toks: list[str], index: int) -> tuple[int, int]:
    """Parse one statement at ``index``; return (next index, NPath)."""
    if index >= len(toks):
        return index, 1
    tok = toks[index]
    if tok == "{":
        end = _close(toks, index)
        return end + 1, block_npath(toks[index + 1:end])
    if tok in _STATEMENTS:
        return _STATEMENTS[tok](toks, index)
    if tok == "switch":
        return _switch(toks, index)
    if tok in ("synchronized",):
        _lock, after = _paren(toks, index + 1)
        return statement(toks, after)
    if tok in _TYPES and index + 2 < len(toks) and toks[index + 1].isidentifier() \
            and toks[index + 2] in ("{", "<", "(", "extends", "implements"):
        start = toks.index("{", index)
        end = _close(toks, start)
        return end + 1, _class_body_npath(toks[start + 1:end])
    if index + 1 < len(toks) and toks[index + 1] == ":" and tok.isidentifier():
        return statement(toks, index + 2)
    end = _statement_end(toks, index)
    return end, expr_npath(toks[index:end - 1])


def block_npath(toks: list[str]) -> int:
    """NPath of a statement list: the product of its statements."""
    result, index = 1, 0
    while index < len(toks):
        if toks[index] == ";":
            index += 1
            continue
        index, value = statement(toks, index)
        result *= value
    return result


def _class_body_npath(toks: list[str]) -> int:
    return prod(npath for _name, _first, _last, npath in _members(toks, [0] * len(toks)))


def _skip_annotation(toks: list[str], index: int) -> int:
    index += 2
    while index + 1 < len(toks) and toks[index] == ".":
        index += 2
    return _close(toks, index) + 1 if index < len(toks) and toks[index] == "(" else index


def _method(toks: list[str], lines: list[int], start: int, index: int):
    """(name, first line, last line, NPath) when ``toks[start:index]`` heads a method body at ``index``."""
    header = toks[start:index]
    opens = _top_level(header, lambda _i, tok: tok == "(")
    if not opens or opens[0] == 0:
        return None  # initializer block
    end = _close(toks, index)
    return header[opens[0] - 1], lines[start + opens[0] - 1], lines[end], block_npath(toks[index + 1:end])


def _members(toks: list[str], lines: list[int]):
    """Yield (name, first line, last line, NPath) for each method or constructor in a class body."""
    index, start = 0, 0
    while index < len(toks):
        tok = toks[index]
        if tok == "@" and index + 1 < len(toks) and toks[index + 1] != "interface":
            after = _skip_annotation(toks, index)
            start = after if start == index else start
            index = after
            continue
        if tok in _TYPES and index + 1 < len(toks) and toks[index + 1].isidentifier():
            body = toks.index("{", index)
            end = _close(toks, body)
            yield from _members(toks[body + 1:end], lines[body + 1:end])
            index = start = end + 1
        elif tok in ("=", ";"):
            index = start = _statement_end(toks, index) if tok == "=" else index + 1
        elif tok == "{":
            found = _method(toks, lines, start, index)
            if found:
                yield found
            index = start = _close(toks, index) + 1
        else:
            index = _close(toks, index) + 1 if tok in ("(", "[") else index + 1


def methods(source: str):
    """Yield (name, first line, last line, NPath) for every method of every top-level type in ``source``."""
    pairs = tokenize(source)
    toks = [tok for tok, _line in pairs]
    lines = [line for _tok, line in pairs]
    index = 0
    while index < len(toks):
        if toks[index] in _TYPES and index + 1 < len(toks) and toks[index + 1].isidentifier():
            body = toks.index("{", index)
            end = _close(toks, body)
            yield from _members(toks[body + 1:end], lines[body + 1:end])
            index = end + 1
        else:
            index += 1
