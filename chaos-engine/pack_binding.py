"""
Bind a pack module's API into a ChaosEngine controller namespace.

Packs keep their code in their own module (for example the java pack's
``packs/java/maven_tools.py``). A controller such as ``hosts.py`` binds the
pack's ``__all__`` into its own globals, so pack functions resolve shared core
helpers, each other, and test patches through the controller namespace exactly
as if they were defined there.
"""

from __future__ import annotations

import contextlib
import functools
import importlib.util
import sys
import types
from pathlib import Path


# Every ``contextlib.contextmanager`` helper shares one code object.
_CONTEXTMANAGER_CODE = contextlib.contextmanager(lambda: iter(())).__code__


def _rebind_function(function: types.FunctionType, namespace: dict[str, object]) -> types.FunctionType:
    rebound = types.FunctionType(
        function.__code__,
        namespace,
        function.__name__,
        function.__defaults__,
        function.__closure__,
    )
    rebound.__kwdefaults__ = function.__kwdefaults__
    rebound.__doc__ = function.__doc__
    rebound.__qualname__ = function.__qualname__
    rebound.__annotations__ = dict(function.__annotations__)
    rebound.__module__ = str(namespace.get("__name__", function.__module__))
    return rebound


def _rebind(value: object, namespace: dict[str, object]) -> object:
    wrapped = getattr(value, "__wrapped__", None)
    if isinstance(value, types.FunctionType) and isinstance(wrapped, types.FunctionType):
        inner = _rebind_function(wrapped, namespace)
        if value.__code__ is _CONTEXTMANAGER_CODE:
            return functools.wraps(inner)(contextlib.contextmanager(inner))
        raise ValueError(f"unsupported decorated pack function: {value.__name__}")
    if isinstance(value, types.FunctionType):
        return _rebind_function(value, namespace)
    return value


def bind_pack(namespace: dict[str, object], path: Path) -> dict[str, object]:
    """Load ``path`` and bind its ``__all__`` into ``namespace``; return the exports."""
    if not path.is_file():
        raise FileNotFoundError(f"ChaosEngine pack module is missing: {path}")
    spec = importlib.util.spec_from_file_location(
        f"chaos_engine_pack_{path.parent.name}_{path.stem}", path
    )
    if spec is None or spec.loader is None:
        raise ImportError(f"ChaosEngine pack module cannot load: {path}")
    module = importlib.util.module_from_spec(spec)
    previous = sys.dont_write_bytecode
    sys.dont_write_bytecode = True
    try:
        spec.loader.exec_module(module)
    finally:
        sys.dont_write_bytecode = previous
    for name, value in vars(module).items():
        if isinstance(value, types.ModuleType) and name not in namespace:
            namespace[name] = value
    exports: dict[str, object] = {}
    for name in module.__all__:
        exports[name] = _rebind(getattr(module, name), namespace)
        namespace[name] = exports[name]
    return exports
