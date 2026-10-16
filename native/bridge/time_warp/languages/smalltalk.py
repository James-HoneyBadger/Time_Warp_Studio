"""Smalltalk language executor for Time Warp Studio.

Smalltalk-80 is the language that made "everything is an object" a working
proposal rather than a slogan: there is no syntax for control flow at all, and
``ifTrue:``, ``whileTrue:`` and ``to:do:`` are messages sent to objects. It fills
the one paradigm gap the lineup had — nothing else here is object-oriented.

Executed whole-program, like Scheme and CHIP-8. A file defines classes and
methods and then runs some code, and a method body is more than one line, so the
whole source is read before anything runs.

    Object subclass: #Counter
      instanceVariableNames: 'count'

    Counter >> initialize
      count := 0

    Counter >> increment
      count := count + 1

    Counter >> value
      ^count

    | c |
    c := Counter new.
    c increment.
    c value

-------------------------------------------------------------------------------
The specification
-------------------------------------------------------------------------------

Written down before the implementation, because the C++ port that follows will
need a specification rather than a surprise.

**Objects and classes.** Every value is an object. A class has a superclass, a
name, an instance-variable template, and *two* method dictionaries: one for its
instances and one for the class itself. ``Object subclass: #Counter`` is not
special syntax — it is the keyword message ``subclass:instanceVariableNames:``
sent to the class object ``Object``, exactly as in a real file-in. Anything after
``Object`` in that message is accepted and ignored.

**Method lookup.** Send a message by walking the receiver's class chain looking
for the selector, then stop. A selector that reaches the top is a
``doesNotUnderstand:`` error, reported with the selector and the receiver's class.

**Precedence.** Unary, then binary, then keyword, left to right within a level.
Parentheses group. ``2 + 3 * 4`` is 20 (all of those are binary, so left to
right); ``1 max: 2 + 3`` is 5 because binary binds tighter than keyword.

**Variables**, in widening scope: method arguments and temporaries, then instance
variables, then globals (the class names a definition introduced). Assignment to a
name that is not one of those creates a global, which is how the workspace bar
behaves.

**Blocks are closures.** A block captures the activation it was created in, so a
block held in an instance variable and run later still reads and writes the
variables where it was written.

**Non-local return.** ``^`` inside a block returns from the method that created the
block, not from the block. This is the part of Smalltalk that surprises people, and
faking it — returning from the block instead — gives a wrong answer that looks
right in a simple test.

**Cascades.** ``aStream show: 'x'; cr`` sends ``cr`` to the object the *first*
message went to, not to the result of ``show:``. That distinction is the entire
reason ``;`` exists.

**Source layout.** A definition is a header — ``ClassName >> selector``, or
``X subclass: #Y`` — followed by *indented* lines, and it ends at the next line
that begins in column 0. Lines outside a definition are top-level code, and
consecutive ones form a single block whatever their indentation. There is no ``!``
chunk terminator, so indentation carries the job it does in a file-in.

-------------------------------------------------------------------------------
Deliberate simplifications
-------------------------------------------------------------------------------

Each of these is a place where Smalltalk is bigger than this, and the difference
could otherwise be discovered the hard way:

  * **No metaclass tower.** A class object holds two method dictionaries rather
    than being an instance of a metaclass that is an instance of a metaclass.
    ``Counter class`` answers the class object itself.
  * **Immediate values are represented natively** — an integer is a Python
    integer, a string is a Python string — and a send dispatches on the receiver's
    *class*, which is the standard way to get Smalltalk semantics without
    allocating a wrapper per addition.
  * **No exceptions, processes, ``thisContext``, ``perform:`` or compiler.** There
    is no ``ensure:``, ``on:do:`` or semaphore.
  * **A chunk's last value is displayed unless it is nil**, which is the
    workspace's "print it" applied once per block of top-level code. Echoing every
    statement would print an object for each assignment and each ``do:``, and the
    object you were talking to is not the answer you asked for. Real output belongs
    in ``Transcript show:``, as it does in Smalltalk.

Turtle graphics are a **Time Warp extension**, as they are for COMAL, REXX, FOCAL
and APL: ``| t | t := Turtle new. t forward: 100``.
"""

from __future__ import annotations

import math
import re
from typing import Any, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from ..core.interpreter import Interpreter
    from ..graphics.turtle_state import TurtleState


class StError(Exception):
    """Raised while parsing or evaluating Smalltalk; reported with ❌."""


#: Selectors this build implements, for the command inventory.
SMALLTALK_COMMANDS = {
    "subclass:instanceVariableNames:",
    "new",
    "initialize",
    "show:",
    "cr",
    "forward:",
    "back:",
    "right:",
    "left:",
    "penUp",
    "penDown",
    "home",
    "clear",
}


# ---------------------------------------------------------------------------
# Per-run state
# ---------------------------------------------------------------------------


class Runtime:
    """Everything one run owns.

    Per run rather than module-level, so two interpreters in one process cannot
    see each other's globals or each other's Transcript.
    """

    def __init__(self, turtle: Any) -> None:
        self.turtle = turtle
        self.output: list[str] = []
        self.globals: dict[str, Any] = {}
        self.classes: dict[str, StClass] = {}
        self.transcript_buffer: list[str] = []
        self._workspace: "Frame | None" = None

    def emit(self, text: str) -> None:
        self.output.append(text)

    def workspace_frame(self) -> "Frame":
        """The one frame top-level code runs in.

        Shared across chunks rather than made per chunk, so a temporary declared
        before a class definition is still there for the code after it.
        """
        if self._workspace is None:
            self._workspace = Frame(Activation(None, _OBJECT, self), {})
        return self._workspace


# ---------------------------------------------------------------------------
# Tokens
# ---------------------------------------------------------------------------

# `|` is deliberately absent: it delimits a method's or block's temporaries, and
# treating it as a selector swallowed the `|` in `[:x | x]` and in `| c |`.
_BINARY_CHARS = set("+-*/\\~<>=&@%,?!")
_IDENT_START = set("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")
_IDENT_BODY = _IDENT_START | set("0123456789")


class Token:
    __slots__ = ("kind", "text")

    def __init__(self, kind: str, text: str) -> None:
        self.kind = kind
        self.text = text

    def __repr__(self) -> str:  # pragma: no cover - diagnostics only
        return f"Token({self.kind!r}, {self.text!r})"


def tokenize(source: str) -> list[Token]:
    """Split *source* into tokens, dropping comments.

    A comment is ``"..."`` and does not nest, which is the language's own rule and
    why a double quote cannot appear inside one.
    """
    tokens: list[Token] = []
    index = 0
    length = len(source)
    while index < length:
        character = source[index]
        if character.isspace():
            index += 1
            continue
        if character == '"':
            end = source.find('"', index + 1)
            if end == -1:
                raise StError("unterminated comment")
            index = end + 1
            continue
        if character == "'":
            index += 1
            parts: list[str] = []
            while True:
                if index >= length:
                    raise StError("unterminated string")
                if source[index] == "'":
                    if index + 1 < length and source[index + 1] == "'":
                        parts.append("'")
                        index += 2
                        continue
                    index += 1
                    break
                parts.append(source[index])
                index += 1
            tokens.append(Token("string", "".join(parts)))
            continue
        if character == "$":
            if index + 1 >= length:
                raise StError("a $ must be followed by the character it names")
            tokens.append(Token("char", source[index + 1]))
            index += 2
            continue
        if character == "#":
            if index + 1 < length and source[index + 1] == "(":
                tokens.append(Token("hash-array", "#("))
                index += 2
                continue
            if index + 1 < length and source[index + 1] == "'":
                end = source.find("'", index + 2)
                if end == -1:
                    raise StError("unterminated symbol")
                tokens.append(Token("symbol", source[index + 2 : end]))
                index = end + 1
                continue
            if index + 1 < length and source[index + 1] in _IDENT_START:
                start = index + 1
                index = start
                while index < length and source[index] in _IDENT_BODY:
                    index += 1
                tokens.append(Token("symbol", source[start:index]))
                continue
            if index + 1 < length and source[index + 1] in _BINARY_CHARS:
                start = index + 1
                index = start
                while index < length and source[index] in _BINARY_CHARS:
                    index += 1
                tokens.append(Token("symbol", source[start:index]))
                continue
            raise StError("a # must name a symbol or start an array")
        if character.isdigit():
            start = index
            while index < length and source[index].isdigit():
                index += 1
            if index < length and source[index] == "r":
                index += 1
                while index < length and source[index].isalnum():
                    index += 1
                tokens.append(Token("number", source[start:index]))
                continue
            if index < length and source[index] == "." and index + 1 < length and source[index + 1].isdigit():
                index += 1
                while index < length and source[index].isdigit():
                    index += 1
            if index < length and source[index] in "eE":
                probe = index + 1
                if probe < length and source[probe] in "+-":
                    probe += 1
                if probe < length and source[probe].isdigit():
                    index = probe
                    while index < length and source[index].isdigit():
                        index += 1
            tokens.append(Token("number", source[start:index]))
            continue
        if character in _IDENT_START:
            start = index
            while index < length and source[index] in _IDENT_BODY:
                index += 1
            if index < length and source[index] == ":":
                # A colon that is not part of := makes this a keyword.
                if not source.startswith(":=", index):
                    tokens.append(Token("keyword", source[start : index + 1]))
                    index += 1
                    continue
            tokens.append(Token("ident", source[start:index]))
            continue
        if source.startswith(":=", index):
            tokens.append(Token("assign", ":="))
            index += 2
            continue
        if source.startswith(">>", index):
            tokens.append(Token(">>", ">>"))
            index += 2
            continue
        if character in _BINARY_CHARS:
            start = index
            while index < length and source[index] in _BINARY_CHARS:
                index += 1
            tokens.append(Token("binary", source[start:index]))
            continue
        if character in "[]().;|^:":
            tokens.append(Token(character, character))
            index += 1
            continue
        raise StError(f"cannot read {character!r}")
    return tokens


# ---------------------------------------------------------------------------
# Syntax nodes
# ---------------------------------------------------------------------------


class Node:
    def evaluate(self, frame: "Frame") -> Any:  # pragma: no cover - interface
        raise NotImplementedError


class Literal(Node):
    def __init__(self, value: Any) -> None:
        self.value = value

    def evaluate(self, frame: "Frame") -> Any:
        return self.value


class ArrayLiteral(Node):
    def __init__(self, items: list[Node]) -> None:
        self.items = items

    def evaluate(self, frame: "Frame") -> Any:
        return [item.evaluate(frame) for item in self.items]


class Variable(Node):
    def __init__(self, name: str) -> None:
        self.name = name

    def evaluate(self, frame: "Frame") -> Any:
        return frame.lookup(self.name)


class Assignment(Node):
    def __init__(self, name: str, value: Node) -> None:
        self.name = name
        self.value = value

    def evaluate(self, frame: "Frame") -> Any:
        result = self.value.evaluate(frame)
        frame.assign(self.name, result)
        return result


class SelfNode(Node):
    def evaluate(self, frame: "Frame") -> Any:
        return frame.self_object


class SuperNode(Node):
    def evaluate(self, frame: "Frame") -> Any:
        return SuperProxy(frame.self_object, frame.activation.klass)


class BlockLiteral(Node):
    def __init__(self, parameters: list[str], body: list[Node]) -> None:
        self.parameters = parameters
        self.body = body

    def evaluate(self, frame: "Frame") -> Any:
        return StBlock(self.parameters, self.body, frame)


class MessageSend(Node):
    """One message send, plus any cascaded sends written after it."""

    def __init__(self, receiver: Node, selector: str, arguments: list[Node]) -> None:
        self.receiver = receiver
        self.selector = selector
        self.arguments = arguments
        self.cascades: list[tuple[str, list[Node]]] = []

    def evaluate(self, frame: "Frame") -> Any:
        receiver = self.receiver.evaluate(frame)
        result = send(receiver, self.selector, [a.evaluate(frame) for a in self.arguments], frame)
        for selector, arguments in self.cascades:
            # Every cascaded send goes to the *original* receiver; that is what
            # distinguishes `;` from another message on the result.
            result = send(receiver, selector, [a.evaluate(frame) for a in arguments], frame)
        return result


class Return(Node):
    def __init__(self, value: Node) -> None:
        self.value = value

    def evaluate(self, frame: "Frame") -> Any:
        raise NonLocalReturn(frame.activation, self.value.evaluate(frame))


class Sequence(Node):
    def __init__(self, statements: list[Node]) -> None:
        self.statements = statements

    def evaluate(self, frame: "Frame") -> Any:
        result: Any = None
        for statement in self.statements:
            result = statement.evaluate(frame)
        return result


class NonLocalReturn(Exception):
    """``^``, including from inside a block: unwind to the owning activation."""

    def __init__(self, activation: "Activation", value: Any) -> None:
        super().__init__("non-local return")
        self.activation = activation
        self.value = value


# ---------------------------------------------------------------------------
# The object model
# ---------------------------------------------------------------------------


class Method:
    """A method: parsed from the source, or a primitive written in Python."""

    def __init__(
        self,
        selector: str,
        parameters: list[str],
        body: list[Node] | None = None,
        primitive: Callable[[Any, list[Any], "Frame"], Any] | None = None,
    ) -> None:
        self.selector = selector
        self.parameters = parameters
        self.body = body
        self.primitive = primitive

    def invoke(self, receiver: Any, arguments: list[Any], frame: "Frame", klass: "StClass") -> Any:
        if self.primitive is not None:
            return self.primitive(receiver, arguments, frame)
        if len(arguments) != len(self.parameters):
            raise StError(f"{self.selector} takes {len(self.parameters)} argument(s)")
        assert self.body is not None
        activation = Activation(receiver, klass, frame.runtime)
        for name, value in zip(self.parameters, arguments):
            activation.variables[name] = value
        try:
            return Sequence(self.body).evaluate(Frame(activation, {}))
        except NonLocalReturn as unwind:
            if unwind.activation is activation:
                return unwind.value
            raise


class Activation:
    """One running method: its receiver, the class it was found in, its runtime."""

    def __init__(self, receiver: Any, klass: "StClass", runtime: "Runtime") -> None:
        self.receiver = receiver
        self.klass = klass
        self.runtime = runtime
        self.variables: dict[str, Any] = {}


class StClass:
    """A class object.

    Two method dictionaries rather than a metaclass tower; see the docstring.
    ``superclass`` may be None only for Object.
    """

    def __init__(
        self,
        name: str,
        superclass: "StClass | None" = None,
        instance_variables: tuple[str, ...] = (),
    ) -> None:
        self.name = name
        self.superclass = superclass
        self.instance_variables = list(instance_variables)
        self.methods: dict[str, Method] = {}
        self.class_methods: dict[str, Method] = {}

    def __repr__(self) -> str:  # pragma: no cover - diagnostics only
        return f"<class {self.name}>"

    def lookup(self, selector: str, class_side: bool = False) -> "Method | None":
        node: StClass | None = self
        while node is not None:
            table = node.class_methods if class_side else node.methods
            if selector in table:
                return table[selector]
            node = node.superclass
        if class_side:
            # A class object also answers the universal class-side messages:
            # `new`, `subclass:instanceVariableNames:` and friends live in Class.
            node = _CLASS
            while node is not None:
                if selector in node.class_methods:
                    return node.class_methods[selector]
                node = node.superclass
        return None

    def lookup_above(self, selector: str, start: "StClass") -> "Method | None":
        """Lookup starting *above* `start`, which is what ``super`` means."""
        node = start.superclass
        while node is not None:
            if selector in node.methods:
                return node.methods[selector]
            node = node.superclass
        return None

    def all_instance_variables(self) -> list[str]:
        names: list[str] = []
        if self.superclass is not None:
            names.extend(self.superclass.all_instance_variables())
        names.extend(self.instance_variables)
        return names


class StInstance:
    """An instance of a class defined in the source."""

    def __init__(self, klass: StClass) -> None:
        self.st_class = klass
        self.ivars: dict[str, Any] = {name: None for name in klass.all_instance_variables()}

    def __repr__(self) -> str:  # pragma: no cover - diagnostics only
        return f"a {self.st_class.name}"


class StSymbol(str):
    """A symbol: prints with a # and compares by name."""


class StOrdered(list):
    """A growable collection.

    Its own list subclass because dispatch is by class: an OrderedCollection and
    an Array holding the same elements are different objects, and with both as
    plain lists `OrderedCollection new add: 1` looked for `add:` in Array.
    """


class StCharacter(str):
    """A character, which is its own class and prints with a $."""


class StBlock:
    """A block: code, plus the frame it captured."""

    def __init__(self, parameters: list[str], body: list[Node], frame: "Frame") -> None:
        self.parameters = parameters
        self.body = body
        self.frame = frame

    def value(self, arguments: list[Any]) -> Any:
        if len(arguments) != len(self.parameters):
            raise StError(
                f"a block taking {len(self.parameters)} argument(s) was given "
                f"{len(arguments)}"
            )
        nested = Frame(self.frame.activation, {}, parent=self.frame)
        for name, value in zip(self.parameters, arguments):
            nested.temporaries[name] = value
        # A return inside the block belongs to the method, so it is deliberately
        # not caught here.
        return Sequence(self.body).evaluate(nested)


class SuperProxy:
    """What ``super`` evaluates to: the receiver, with lookup starting higher."""

    def __init__(self, receiver: Any, defining_class: StClass) -> None:
        self.receiver = receiver
        self.defining_class = defining_class


class Frame:
    """A scope: variables, plus the activation that owns ``self`` and ``^``."""

    def __init__(
        self,
        activation: Activation,
        temporaries: dict[str, Any] | None = None,
        parent: "Frame | None" = None,
    ) -> None:
        self.activation = activation
        self.temporaries: dict[str, Any] = {} if temporaries is None else temporaries
        # A block's frame chains to the one it was written in, which is what makes
        # a block a closure over the enclosing method's arguments and temporaries
        # rather than only over `self`.
        self.parent = parent

    @property
    def runtime(self) -> "Runtime":
        return self.activation.runtime

    @property
    def self_object(self) -> Any:
        return self.activation.receiver

    def lookup(self, name: str) -> Any:
        node: Frame | None = self
        while node is not None:
            if name in node.temporaries:
                return node.temporaries[name]
            node = node.parent
        if name in self.activation.variables:
            return self.activation.variables[name]
        klass: StClass | None = self.activation.klass
        receiver = self.activation.receiver
        while klass is not None:
            if name in klass.instance_variables:
                return receiver.ivars.get(name) if isinstance(receiver, StInstance) else None
            klass = klass.superclass
        return global_value(self.runtime, name)

    def assign(self, name: str, value: Any) -> None:
        node: Frame | None = self
        while node is not None:
            if name in node.temporaries:
                node.temporaries[name] = value
                return
            node = node.parent
        if name in self.activation.variables:
            self.activation.variables[name] = value
            return
        klass: StClass | None = self.activation.klass
        receiver = self.activation.receiver
        while klass is not None:
            if name in klass.instance_variables:
                if isinstance(receiver, StInstance):
                    receiver.ivars[name] = value
                return
            klass = klass.superclass
        # Not a variable anywhere: top-level assignment makes it a global.
        self.runtime.globals[name] = value


def global_value(runtime: "Runtime", name: str) -> Any:
    if name in runtime.globals:
        return runtime.globals[name]
    if name in runtime.classes:
        return runtime.classes[name]
    if name in _LIBRARY_GLOBALS:
        return _LIBRARY_GLOBALS[name]
    raise StError(f"unknown variable or global {name}")


# ---------------------------------------------------------------------------
# Dispatch
# ---------------------------------------------------------------------------


def class_of(value: Any) -> StClass:
    """The class a send starts looking in."""
    if value is None:
        return nil_class()
    if value is True:
        return true_class()
    if value is False:
        return false_class()
    if isinstance(value, SuperProxy):
        return class_of(value.receiver)
    if isinstance(value, StClass):
        return _CLASS
    if isinstance(value, StInstance):
        return value.st_class
    if isinstance(value, StSymbol):
        return _SYMBOL
    if isinstance(value, StCharacter):
        return _CHARACTER
    if isinstance(value, str):
        return _STRING
    if isinstance(value, bool):
        return _BOOLEAN
    if isinstance(value, int):
        return _INTEGER
    if isinstance(value, float):
        return _FLOAT
    if isinstance(value, StBlock):
        return _BLOCK_CLOSURE
    if isinstance(value, StOrdered):
        return _ORDERED
    if isinstance(value, list):
        return _ARRAY
    return _OBJECT


def send(receiver: Any, selector: str, arguments: list[Any], frame: "Frame") -> Any:
    """Send *selector* with *arguments* to *receiver*."""
    if isinstance(receiver, SuperProxy):
        start = receiver.defining_class
        method = start.lookup_above(selector, start)
        if method is None:
            raise StError(
                f"super sent {selector}, but no superclass of {start.name} implements it"
            )
        return method.invoke(receiver.receiver, arguments, frame, start)

    if isinstance(receiver, StClass):
        # A class object answers class-side messages, and then the universal
        # class-side ones (new, subclass:...).
        method = receiver.lookup(selector, class_side=True)
        if method is None:
            raise StError(f"{receiver.name} class does not understand {selector}")
        return method.invoke(receiver, arguments, frame, receiver)

    klass = class_of(receiver)
    method = klass.lookup(selector)
    if method is None:
        described = describe(receiver)
        raise StError(f"{described} does not understand {selector}")
    return method.invoke(receiver, arguments, frame, klass)


# ---------------------------------------------------------------------------
# Printing
# ---------------------------------------------------------------------------


def number_string(value: Any) -> str:
    if isinstance(value, float):
        if value.is_integer() and abs(value) < 1e16:
            return str(int(value))
        return f"{value:g}"
    return str(value)


def print_string(value: Any) -> str:
    """``printString``: what the workspace echoes."""
    if value is None:
        return "nil"
    if value is True:
        return "true"
    if value is False:
        return "false"
    if isinstance(value, StCharacter):
        return f"${value}"
    if isinstance(value, StSymbol):
        return f"#{value}"
    if isinstance(value, str):
        return "'" + value.replace("'", "''") + "'"
    if isinstance(value, (int, float)) and not isinstance(value, bool):
        return number_string(value)
    if isinstance(value, list):
        return "(" + " ".join(print_string(item) for item in value) + ")"
    if isinstance(value, StInstance):
        return f"a {value.st_class.name}"
    if isinstance(value, StClass):
        return value.name
    if isinstance(value, StBlock):
        return "a BlockClosure"
    return str(value)


def describe(value: Any) -> str:
    """How a receiver is named in an error message."""
    if isinstance(value, StInstance):
        return f"a {value.st_class.name}"
    return print_string(value)


# ---------------------------------------------------------------------------
# The class library
#
# `add` keeps this section readable: one line per method, and the alternative is
# thirty near-identical Method constructions.
# ---------------------------------------------------------------------------

_OBJECT: StClass
_CLASS: StClass
_BOOLEAN: StClass
_TRUE: StClass
_FALSE: StClass
_UNDEFINED: StClass
_MAGNITUDE: StClass
_NUMBER: StClass
_INTEGER: StClass
_FLOAT: StClass
_CHARACTER: StClass
_COLLECTION: StClass
_SEQUENCEABLE: StClass
_ARRAY: StClass
_ORDERED: StClass
_STRING: StClass
_SYMBOL: StClass
_BLOCK_CLOSURE: StClass
_TRANSCRIPT: StClass
_TURTLE: StClass

_LIBRARY_GLOBALS: dict[str, Any] = {}


def nil_class() -> StClass:
    return _UNDEFINED


def true_class() -> StClass:
    return _TRUE


def false_class() -> StClass:
    return _FALSE


def _add(
    klass: StClass,
    selector: str,
    parameters: list[str],
    function: Callable[[Any, list[Any], "Frame"], Any],
    class_side: bool = False,
) -> None:
    table = klass.class_methods if class_side else klass.methods
    table[selector] = Method(selector, parameters, primitive=function)


def _items(value: Any) -> list[Any]:
    if isinstance(value, str):
        return list(value)
    if isinstance(value, list):
        return list(value)
    raise StError(f"{describe(value)} is not a collection")


def _build_library() -> None:
    global _OBJECT, _CLASS, _BOOLEAN, _TRUE, _FALSE, _UNDEFINED
    global _MAGNITUDE, _NUMBER, _INTEGER, _FLOAT, _CHARACTER
    global _COLLECTION, _SEQUENCEABLE, _ARRAY, _ORDERED, _STRING, _SYMBOL
    global _BLOCK_CLOSURE, _TRANSCRIPT, _TURTLE

    _OBJECT = StClass("Object")
    _CLASS = StClass("Class", _OBJECT)
    _BOOLEAN = StClass("Boolean", _OBJECT)
    _TRUE = StClass("True", _BOOLEAN)
    _FALSE = StClass("False", _BOOLEAN)
    _UNDEFINED = StClass("UndefinedObject", _OBJECT)
    _MAGNITUDE = StClass("Magnitude", _OBJECT)
    _NUMBER = StClass("Number", _MAGNITUDE)
    _INTEGER = StClass("Integer", _NUMBER)
    _FLOAT = StClass("Float", _NUMBER)
    _CHARACTER = StClass("Character", _MAGNITUDE)
    _COLLECTION = StClass("Collection", _OBJECT)
    _SEQUENCEABLE = StClass("SequenceableCollection", _COLLECTION)
    _ARRAY = StClass("Array", _SEQUENCEABLE)
    _ORDERED = StClass("OrderedCollection", _SEQUENCEABLE)
    _STRING = StClass("String", _SEQUENCEABLE)
    _SYMBOL = StClass("Symbol", _STRING)
    _BLOCK_CLOSURE = StClass("BlockClosure", _OBJECT)
    _TRANSCRIPT = StClass("Transcript", _OBJECT)
    _TURTLE = StClass("Turtle", _OBJECT)

    # -- Object ----------------------------------------------------------
    def not_understood(receiver: Any, args: list[Any], frame: "Frame") -> Any:
        raise StError(f"{describe(receiver)} does not understand {args[0]}")

    _add(_OBJECT, "doesNotUnderstand:", ["aSelector"], not_understood)
    _add(_OBJECT, "class", [], lambda r, a, f: class_of(r))
    _add(_OBJECT, "isNil", [], lambda r, a, f: False)
    _add(_OBJECT, "notNil", [], lambda r, a, f: True)
    _add(_OBJECT, "isKindOf:", ["aClass"], lambda r, a, f: _is_kind_of(r, a[0]))
    _add(_OBJECT, "==", ["anObject"], lambda r, a, f: r is a[0])
    _add(_OBJECT, "~~", ["anObject"], lambda r, a, f: r is not a[0])
    _add(_OBJECT, "=", ["anObject"], lambda r, a, f: _equals(r, a[0]))
    _add(_OBJECT, "~=", ["anObject"], lambda r, a, f: not _equals(r, a[0]))
    _add(_OBJECT, "yourself", [], lambda r, a, f: r)
    _add(_OBJECT, "printString", [], lambda r, a, f: print_string(r))
    _add(_OBJECT, "displayString", [], lambda r, a, f: display_string(r))
    _add(_OBJECT, "ifNil:", ["aBlock"], lambda r, a, f: r)
    _add(_OBJECT, "ifNotNil:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_OBJECT, "error:", ["aString"], _error)

    # -- Class side ------------------------------------------------------
    _add(_CLASS, "new", [], _class_new, class_side=True)
    _add(_CLASS, "name", [], lambda r, a, f: StSymbol(r.name), class_side=True)
    _add(_CLASS, "superclass", [], lambda r, a, f: r.superclass, class_side=True)
    _add(
        _CLASS,
        "subclass:instanceVariableNames:",
        ["name", "variables"],
        _subclass,
        class_side=True,
    )
    _add(
        _CLASS,
        "instanceVariableNames:",
        ["variables"],
        _ignored_definition_tail,
        class_side=True,
    )
    _add(
        _CLASS,
        "classVariableNames:",
        ["variables"],
        _ignored_definition_tail,
        class_side=True,
    )
    _add(_CLASS, "package:", ["aString"], _ignored_definition_tail, class_side=True)

    # -- nil -------------------------------------------------------------
    _add(_UNDEFINED, "isNil", [], lambda r, a, f: True)
    _add(_UNDEFINED, "notNil", [], lambda r, a, f: False)
    _add(_UNDEFINED, "ifNil:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_UNDEFINED, "ifNotNil:", ["aBlock"], lambda r, a, f: r)
    _add(_UNDEFINED, "ifNil:ifNotNil:", ["nilBlock", "otherBlock"],
         lambda r, a, f: a[0].value([]))
    _add(_UNDEFINED, "printString", [], lambda r, a, f: "nil")
    _add(_UNDEFINED, "yourself", [], lambda r, a, f: r)

    # -- Boolean ---------------------------------------------------------
    _add(_TRUE, "ifTrue:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_TRUE, "ifFalse:", ["aBlock"], lambda r, a, f: r)
    _add(_TRUE, "ifTrue:ifFalse:", ["t", "f"], lambda r, a, f: a[0].value([]))
    _add(_TRUE, "ifFalse:ifTrue:", ["f", "t"], lambda r, a, f: a[1].value([]))
    _add(_TRUE, "and:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_TRUE, "or:", ["aBlock"], lambda r, a, f: True)
    _add(_TRUE, "not", [], lambda r, a, f: False)
    _add(_FALSE, "ifTrue:", ["aBlock"], lambda r, a, f: r)
    _add(_FALSE, "ifFalse:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_FALSE, "ifTrue:ifFalse:", ["t", "f"], lambda r, a, f: a[1].value([]))
    _add(_FALSE, "ifFalse:ifTrue:", ["f", "t"], lambda r, a, f: a[0].value([]))
    _add(_FALSE, "and:", ["aBlock"], lambda r, a, f: False)
    _add(_FALSE, "or:", ["aBlock"], lambda r, a, f: a[0].value([]))
    _add(_FALSE, "not", [], lambda r, a, f: True)
    for klass in (_TRUE, _FALSE):
        _add(klass, "printString", [], lambda r, a, f: print_string(r))
        _add(klass, "=", ["other"], lambda r, a, f: r is a[0])

    # -- Number ----------------------------------------------------------
    def guarded(function: Callable[[float, float], Any]) -> Callable[[Any, list[Any], "Frame"], Any]:
        def call(receiver: Any, args: list[Any], frame: "Frame") -> Any:
            right = args[0]
            if not isinstance(right, (int, float)) or isinstance(right, bool):
                raise StError(f"{describe(right)} is not a number")
            try:
                return function(receiver, right)
            except ZeroDivisionError as exc:
                raise StError("division by zero") from exc

        return call

    _add(_NUMBER, "+", ["aNumber"], guarded(lambda left, right: left + right))
    _add(_NUMBER, "-", ["aNumber"], guarded(lambda left, right: left - right))
    _add(_NUMBER, "*", ["aNumber"], guarded(lambda left, right: left * right))
    _add(_NUMBER, "/", ["aNumber"], guarded(lambda left, right: left / right))
    _add(_NUMBER, "//", ["aNumber"], guarded(lambda left, right: math.floor(left / right)))
    _add(_NUMBER, "\\\\", ["aNumber"], guarded(lambda left, right: left - right * math.floor(left / right)))
    _add(_NUMBER, "quo:", ["aNumber"], guarded(lambda left, right: int(left / right)))
    _add(_NUMBER, "rem:", ["aNumber"], guarded(lambda left, right: math.fmod(left, right)))
    _add(_NUMBER, "<", ["aNumber"], lambda r, a, f: r < a[0])
    _add(_NUMBER, "<=", ["aNumber"], lambda r, a, f: r <= a[0])
    _add(_NUMBER, ">", ["aNumber"], lambda r, a, f: r > a[0])
    _add(_NUMBER, ">=", ["aNumber"], lambda r, a, f: r >= a[0])
    _add(_NUMBER, "max:", ["aNumber"], lambda r, a, f: max(r, a[0]))
    _add(_NUMBER, "min:", ["aNumber"], lambda r, a, f: min(r, a[0]))
    _add(_NUMBER, "negated", [], lambda r, a, f: -r)
    _add(_NUMBER, "abs", [], lambda r, a, f: abs(r))
    _add(_NUMBER, "squared", [], lambda r, a, f: r * r)
    _add(_NUMBER, "sqrt", [], lambda r, a, f: math.sqrt(r))
    _add(_NUMBER, "truncated", [], lambda r, a, f: math.trunc(r))
    _add(_NUMBER, "rounded", [], lambda r, a, f: int(round(r)))
    _add(_NUMBER, "asFloat", [], lambda r, a, f: float(r))
    _add(_NUMBER, "asInteger", [], lambda r, a, f: int(r))
    _add(_NUMBER, "isZero", [], lambda r, a, f: r == 0)
    _add(_NUMBER, "isNegative", [], lambda r, a, f: r < 0)
    _add(_NUMBER, "even", [], lambda r, a, f: int(r) % 2 == 0)
    _add(_NUMBER, "odd", [], lambda r, a, f: int(r) % 2 != 0)
    _add(_NUMBER, "printString", [], lambda r, a, f: number_string(r))
    _add(_NUMBER, "displayString", [], lambda r, a, f: number_string(r))
    _add(_NUMBER, "timesRepeat:", ["aBlock"], _times_repeat)
    _add(_NUMBER, "to:do:", ["stop", "aBlock"], lambda r, a, f: _to_do(r, a[0], 1, a[1]))
    _add(_NUMBER, "to:", ["stop"], lambda r, a, f: list(range(int(r), int(a[0]) + 1)))
    _add(_NUMBER, "to:by:", ["stop", "step"], _to_by)
    _add(_NUMBER, "to:by:do:", ["stop", "step", "aBlock"],
         lambda r, a, f: _to_do(r, a[0], a[1], a[2]))

    # -- Character -------------------------------------------------------
    _add(_CHARACTER, "asString", [], lambda r, a, f: str(r))
    _add(_CHARACTER, "asInteger", [], lambda r, a, f: ord(r))
    _add(_CHARACTER, "codePoint", [], lambda r, a, f: ord(r))
    _add(_CHARACTER, "printString", [], lambda r, a, f: f"${r}")
    _add(_CHARACTER, "isDigit", [], lambda r, a, f: r.isdigit())
    _add(_CHARACTER, "isLetter", [], lambda r, a, f: r.isalpha())

    # -- Collections -----------------------------------------------------
    _add(_COLLECTION, "size", [], lambda r, a, f: len(_items(r)))
    _add(_COLLECTION, "isEmpty", [], lambda r, a, f: len(_items(r)) == 0)
    _add(_COLLECTION, "notEmpty", [], lambda r, a, f: len(_items(r)) > 0)
    _add(_COLLECTION, "do:", ["aBlock"], _do)
    _add(_COLLECTION, "collect:", ["aBlock"], lambda r, a, f: [a[0].value([i]) for i in _items(r)])
    _add(_COLLECTION, "select:", ["aBlock"], lambda r, a, f: [i for i in _items(r) if a[0].value([i])])
    _add(_COLLECTION, "reject:", ["aBlock"], lambda r, a, f: [i for i in _items(r) if not a[0].value([i])])
    _add(_COLLECTION, "detect:", ["aBlock"], _detect)
    _add(_COLLECTION, "includes:", ["anObject"], lambda r, a, f: any(_equals(i, a[0]) for i in _items(r)))
    _add(_COLLECTION, "inject:into:", ["initial", "aBlock"], _inject)
    _add(_COLLECTION, "first", [], lambda r, a, f: _items(r)[0])
    _add(_COLLECTION, "last", [], lambda r, a, f: _items(r)[-1])
    _add(_COLLECTION, "asArray", [], lambda r, a, f: _items(r))
    _add(_COLLECTION, "printString", [], lambda r, a, f: print_string(r))
    _add(_COLLECTION, "displayString", [], lambda r, a, f: print_string(r))
    _add(_COLLECTION, ",", ["other"], lambda r, a, f: _concat(r, a[0]))
    _add(_COLLECTION, "=", ["other"], lambda r, a, f: _equals(r, a[0]))
    _add(_ARRAY, "at:", ["index"], _at)
    _add(_ARRAY, "at:put:", ["index", "value"], _at_put)
    _add(_ARRAY, "copyFrom:to:", ["start", "stop"],
         lambda r, a, f: _items(r)[int(a[0]) - 1 : int(a[1])])
    _add(_ARRAY, "new:", ["size"], _array_new, class_side=True)
    _add(_ARRAY, "withAll:", ["aCollection"],
         lambda r, a, f: _items(a[0]), class_side=True)
    _add(_ORDERED, "new", [], lambda r, a, f: StOrdered(), class_side=True)
    _add(_ORDERED, "withAll:", ["aCollection"],
         lambda r, a, f: StOrdered(_items(a[0])), class_side=True)
    _add(_ORDERED, "add:", ["anObject"], _ordered_add)

    # -- String and Symbol ----------------------------------------------
    _add(_STRING, "size", [], lambda r, a, f: len(r))
    _add(_STRING, "at:", ["index"], lambda r, a, f: StCharacter(_at(r, a, f)))
    _add(_STRING, ",", ["other"], lambda r, a, f: str(r) + str(a[0]))
    _add(_STRING, "asUppercase", [], lambda r, a, f: r.upper())
    _add(_STRING, "asLowercase", [], lambda r, a, f: r.lower())
    _add(_STRING, "reverse", [], lambda r, a, f: r[::-1])
    _add(_STRING, "asSymbol", [], lambda r, a, f: StSymbol(r))
    _add(_STRING, "asString", [], lambda r, a, f: str(r))
    _add(_STRING, "asNumber", [], lambda r, a, f: _as_number(r))
    _add(_STRING, "do:", ["aBlock"], _do)
    _add(_STRING, "isEmpty", [], lambda r, a, f: len(r) == 0)
    _add(_STRING, "printString", [], lambda r, a, f: print_string(str(r)))
    _add(_STRING, "displayString", [], lambda r, a, f: str(r))
    _add(_STRING, "=", ["other"], lambda r, a, f: _equals(r, a[0]))
    _add(_SYMBOL, "printString", [], lambda r, a, f: f"#{r}")
    _add(_SYMBOL, "displayString", [], lambda r, a, f: str(r))
    _add(_SYMBOL, "asString", [], lambda r, a, f: str(r))
    # #foo = 'foo' is false: a Symbol and a String are different things, and
    # making them equal hides a mistake rather than catching one.
    _add(_SYMBOL, "=", ["other"], lambda r, a, f: isinstance(a[0], StSymbol) and str(a[0]) == str(r))
    _add(_SYMBOL, "value:", ["aString"], lambda r, a, f: StSymbol(str(r) + str(a[0])))

    # -- BlockClosure ----------------------------------------------------
    _add(_BLOCK_CLOSURE, "value", [], lambda r, a, f: r.value([]))
    _add(_BLOCK_CLOSURE, "value:", ["a"], lambda r, a, f: r.value([a[0]]))
    _add(_BLOCK_CLOSURE, "value:value:", ["a", "b"], lambda r, a, f: r.value([a[0], a[1]]))
    _add(_BLOCK_CLOSURE, "whileTrue:", ["aBlock"], lambda r, a, f: _loop(r, a[0], True))
    _add(_BLOCK_CLOSURE, "whileFalse:", ["aBlock"], lambda r, a, f: _loop(r, a[0], False))
    _add(_BLOCK_CLOSURE, "whileTrue", [], lambda r, a, f: _loop(r, None, True))
    _add(_BLOCK_CLOSURE, "whileFalse", [], lambda r, a, f: _loop(r, None, False))
    _add(_BLOCK_CLOSURE, "printString", [], lambda r, a, f: "a BlockClosure")

    # -- Transcript ------------------------------------------------------
    # These answer nil rather than self, and so do the Turtle methods. A chunk
    # echoes its final value, so returning self would end a chunk with "a
    # Transcript" instead of with the answer. A cascade does not care: it sends to
    # the original receiver, not to the result.
    def transcript_show(receiver: Any, args: list[Any], frame: "Frame") -> Any:
        frame.runtime.transcript_buffer.append(display_string(args[0]))
        return None

    def transcript_cr(receiver: Any, args: list[Any], frame: "Frame") -> Any:
        frame.runtime.emit("".join(frame.runtime.transcript_buffer))
        frame.runtime.transcript_buffer.clear()
        return None

    def transcript_show_cr(receiver: Any, args: list[Any], frame: "Frame") -> Any:
        frame.runtime.emit(display_string(args[0]))
        return None

    _add(_TRANSCRIPT, "show:", ["anObject"], transcript_show)
    _add(_TRANSCRIPT, "cr", [], transcript_cr)
    _add(_TRANSCRIPT, "show:cr:", ["anObject", "aString"], transcript_show_cr)
    _add(_TRANSCRIPT, "nl", [], transcript_cr)
    _add(_TRANSCRIPT, "new", [], _transcript_new, class_side=True)

    # -- Turtle (a Time Warp extension) ----------------------------------
    def turtle_move(direction: str) -> Callable[[Any, list[Any], "Frame"], Any]:
        def call(receiver: Any, args: list[Any], frame: "Frame") -> Any:
            turtle = frame.runtime.turtle
            amount = args[0]
            if direction == "forward":
                turtle.forward(amount)
            elif direction == "back":
                turtle.back(amount)
            elif direction == "right":
                turtle.right(amount)
            else:
                turtle.left(amount)
            return None

        return call

    def turtle_pen(down: bool) -> Callable[[Any, list[Any], "Frame"], Any]:
        def call(receiver: Any, args: list[Any], frame: "Frame") -> Any:
            if down:
                frame.runtime.turtle.pendown()
            else:
                frame.runtime.turtle.penup()
            return None

        return call

    _add(_TURTLE, "forward:", ["aNumber"], turtle_move("forward"))
    _add(_TURTLE, "back:", ["aNumber"], turtle_move("back"))
    _add(_TURTLE, "right:", ["aNumber"], turtle_move("right"))
    _add(_TURTLE, "left:", ["aNumber"], turtle_move("left"))
    _add(_TURTLE, "penUp", [], turtle_pen(False))
    _add(_TURTLE, "penDown", [], turtle_pen(True))
    _add(_TURTLE, "home", [], lambda r, a, f: (f.runtime.turtle.home(), None)[1])
    _add(_TURTLE, "clear", [], lambda r, a, f: (f.runtime.turtle.clear(), None)[1])
    _add(_TURTLE, "x", [], lambda r, a, f: round(f.runtime.turtle.x, 6))
    _add(_TURTLE, "y", [], lambda r, a, f: round(f.runtime.turtle.y, 6))
    _add(_TURTLE, "heading", [], lambda r, a, f: round(f.runtime.turtle.heading, 6))
    # A Turtle instance stands for the canvas, so `new` is enough to make one.
    _add(_TURTLE, "new", [], lambda r, a, f: StInstance(r), class_side=True)

    _LIBRARY_GLOBALS.clear()
    _LIBRARY_GLOBALS.update(
        {
            "Object": _OBJECT,
            "Class": _CLASS,
            "Boolean": _BOOLEAN,
            "True": True,
            "False": False,
            "nil": None,
            "Magnitude": _MAGNITUDE,
            "Number": _NUMBER,
            "Integer": _INTEGER,
            "Float": _FLOAT,
            "Character": _CHARACTER,
            "Collection": _COLLECTION,
            "SequenceableCollection": _SEQUENCEABLE,
            "Array": _ARRAY,
            "OrderedCollection": _ORDERED,
            "String": _STRING,
            "Symbol": _SYMBOL,
            "BlockClosure": _BLOCK_CLOSURE,
            "Transcript": StInstance(_TRANSCRIPT),
            "Turtle": _TURTLE,
        }
    )


# -- primitive implementations shared by several classes ---------------------


def _equals(left: Any, right: Any) -> bool:
    if isinstance(left, StSymbol) or isinstance(right, StSymbol):
        return (
            isinstance(left, StSymbol)
            and isinstance(right, StSymbol)
            and str(left) == str(right)
        )
    if isinstance(left, (int, float)) and not isinstance(left, bool):
        if isinstance(right, (int, float)) and not isinstance(right, bool):
            return left == right
        return False
    if isinstance(left, bool) or isinstance(right, bool):
        return left is right
    if isinstance(left, list):
        return (
            isinstance(right, list)
            and len(left) == len(right)
            and all(_equals(a, b) for a, b in zip(left, right))
        )
    if isinstance(left, str) and isinstance(right, str):
        return str(left) == str(right)
    if isinstance(left, StInstance) and isinstance(right, StInstance):
        return left is right
    return left is right


def _is_kind_of(value: Any, klass: Any) -> bool:
    if not isinstance(klass, StClass):
        raise StError("isKindOf: needs a class")
    node: StClass | None = class_of(value)
    while node is not None:
        if node is klass:
            return True
        node = node.superclass
    return False


def display_string(value: Any) -> str:
    if isinstance(value, str) and not isinstance(value, StSymbol):
        return str(value)
    return print_string(value)


def _error(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    raise StError(display_string(args[0]))


def _class_new(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    if not isinstance(receiver, StClass):
        raise StError("new needs a class")
    instance = StInstance(receiver)
    initializer = receiver.lookup("initialize")
    if initializer is not None:
        initializer.invoke(instance, [], frame, receiver)
    return instance


def _name_of(value: Any) -> str:
    """A name written either as a symbol or as a string."""
    if isinstance(value, StSymbol):
        # str(), not print_string(): the class is called Counter, not #Counter.
        return str(value)
    return display_string(value)


def _subclass(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    """``Object subclass: #Name instanceVariableNames: 'a b'``."""
    name = _name_of(args[0])
    variables = tuple(
        word for word in display_string(args[1]).replace(",", " ").split() if word
    )
    if name in frame.runtime.classes or name in _LIBRARY_GLOBALS:
        raise StError(f"{name} is already defined")
    klass = StClass(name, receiver, variables)
    frame.runtime.classes[name] = klass
    frame.runtime.globals[name] = klass
    return klass


def _ignored_definition_tail(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    """The rest of a class definition: `classVariableNames:`, `package:`.

    Accepted so that a definition copied from a real file-in parses, and ignored
    because this build has neither class variables nor packages.
    """
    return receiver


def _as_number(text: Any) -> Any:
    stripped = str(text).strip()
    try:
        return int(stripped)
    except ValueError:
        pass
    try:
        return float(stripped)
    except ValueError as exc:
        raise StError(f"{stripped!r} is not a number") from exc


def _concat(left: Any, right: Any) -> Any:
    if isinstance(left, str) and not isinstance(left, StSymbol):
        return str(left) + display_string(right)
    return _items(left) + _items(right)


def _at(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    """One element, with a message that says which index was wrong."""
    items = _items(receiver)
    index = int(args[0])
    if index < 1 or index > len(items):
        raise StError(
            f"index {index} is out of range for a collection of {len(items)}"
        )
    return items[index - 1]


def _at_put(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    items = _items(receiver)
    index = int(args[0]) - 1
    if index < 0 or index >= len(items):
        raise StError(f"index {args[0]} is out of range for a collection of {len(items)}")
    items[index] = args[1]
    if isinstance(receiver, list):
        receiver[:] = items
        return args[1]
    return args[1]


def _array_new(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    size = int(args[0])
    if size < 0:
        raise StError("an array cannot have a negative size")
    return [None] * size


def _ordered_add(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    receiver.append(args[0])
    return args[0]


def _do(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    block = args[0]
    if not isinstance(block, StBlock):
        raise StError("do: needs a block")
    for item in _items(receiver):
        block.value([item])
    return receiver


def _detect(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    block = args[0]
    for item in _items(receiver):
        if block.value([item]):
            return item
    raise StError("detect: found nothing")


def _inject(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    block = args[1]
    total = args[0]
    for item in _items(receiver):
        total = block.value([total, item])
    return total


def _times_repeat(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    block = args[0]
    for _ in range(int(receiver)):
        block.value([])
    return receiver


def _to_by(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    """`to:by:` — the same range as `to:by:do:` without running a block."""
    stop, step = args[0], args[1]
    if step == 0:
        raise StError("to:by: cannot step by zero")
    if step > 0:
        return list(range(int(receiver), int(stop) + 1, int(step)))
    return list(range(int(receiver), int(stop) - 1, int(step)))


def _to_do(start: Any, stop: Any, step: Any, block: Any) -> Any:
    if step == 0:
        raise StError("to:by:do: cannot step by zero")
    value = start
    if step > 0:
        while value <= stop:
            block.value([value])
            value += step
    else:
        while value >= stop:
            block.value([value])
            value += step
    return start


#: A loop with no bound in the source still needs one here: the engine runs a
#: whole-program language inside one call, so an unintended infinite loop has to
#: report itself rather than hang the IDE.
_MAX_LOOP_ITERATIONS = 5_000_000


def _loop(condition: Any, body: Any, wanted: Any) -> Any:
    if not isinstance(condition, StBlock):
        raise StError("whileTrue:/whileFalse: needs a block as its receiver")
    if body is None:
        raise StError(
            "whileTrue and whileFalse need a body: write [cond] whileTrue: [body]"
        )
    guard = 0
    # Only `true` is true, and only `false` is false: a block answering nil stops
    # the loop rather than being treated as a falsey value.
    while condition.value([]) is wanted:
        body.value([])
        guard += 1
        if guard > _MAX_LOOP_ITERATIONS:
            raise StError("the loop ran for too long")
    return None


def _transcript_new(receiver: Any, args: list[Any], frame: "Frame") -> Any:
    return StInstance(_TRANSCRIPT)


_build_library()


# ---------------------------------------------------------------------------
# The parser
# ---------------------------------------------------------------------------

class Parser:
    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens
        self.index = 0

    # -- helpers ---------------------------------------------------------
    def peek(self) -> Token | None:
        return self.tokens[self.index] if self.index < len(self.tokens) else None

    def at_end(self) -> bool:
        return self.index >= len(self.tokens)

    def advance(self) -> Token:
        token = self.peek()
        if token is None:
            raise StError("the input ended in the middle of an expression")
        self.index += 1
        return token

    def accept(self, kind: str, text: str | None = None) -> bool:
        token = self.peek()
        if token is not None and token.kind == kind and (text is None or token.text == text):
            self.index += 1
            return True
        return False

    def expect(self, kind: str, what: str) -> Token:
        token = self.peek()
        if token is None or token.kind != kind:
            found = "the end of the input" if token is None else repr(token.text)
            raise StError(f"expected {what}, found {found}")
        self.index += 1
        return token

    # -- statements ------------------------------------------------------
    def parse_body(self, stop_kinds: set[str]) -> tuple[list[str], list[Node]]:
        """Temporaries and statements, as a method or block body.

        ``| a b |`` is read only at the start, which is where Smalltalk allows it
        and what leaves a later ``|`` free to be a binary selector.
        """
        temporaries: list[str] = []
        if self.accept("|"):
            while not self.accept("|"):
                token = self.expect("ident", "a temporary variable name")
                temporaries.append(token.text)
        statements: list[Node] = []
        while True:
            while self.accept("."):
                pass
            token = self.peek()
            if token is None or token.kind in stop_kinds:
                break
            statements.append(self.parse_statement())
            if self.peek() is not None and self.peek().kind == ".":
                continue
            break
        return temporaries, statements

    def parse_statement(self) -> Node:
        if self.accept("^"):
            return Return(self.parse_expression())
        return self.parse_expression()

    # -- expressions -----------------------------------------------------
    def parse_expression(self) -> Node:
        return self.parse_keyword_message()

    def parse_keyword_message(self) -> Node:
        receiver = self.parse_binary_message()
        while True:
            token = self.peek()
            if token is None or token.kind != "keyword":
                return self._with_cascades(receiver)
            selector = ""
            arguments: list[Node] = []
            while True:
                keyword = self.peek()
                if keyword is None or keyword.kind != "keyword":
                    break
                self.index += 1
                selector += keyword.text
                arguments.append(self.parse_binary_message())
            receiver = MessageSend(receiver, selector, arguments)

    def parse_binary_message(self) -> Node:
        receiver = self.parse_unary_message()
        while True:
            token = self.peek()
            if token is None or token.kind != "binary":
                return receiver
            self.index += 1
            argument = self.parse_unary_message()
            receiver = MessageSend(receiver, token.text, [argument])

    def parse_unary_message(self) -> Node:
        receiver = self.parse_primary()
        while True:
            token = self.peek()
            if token is None or token.kind != "ident":
                return receiver
            if token.text in ("self", "super", "true", "false", "nil"):
                return receiver
            self.index += 1
            receiver = MessageSend(receiver, token.text, [])

    def _with_cascades(self, node: Node) -> Node:
        if not isinstance(node, MessageSend):
            return node
        while self.accept(";"):
            token = self.peek()
            if token is None:
                raise StError("a cascade ended without a message")
            if token.kind == "keyword":
                selector = ""
                arguments: list[Node] = []
                while True:
                    keyword = self.peek()
                    if keyword is None or keyword.kind != "keyword":
                        break
                    self.index += 1
                    selector += keyword.text
                    arguments.append(self.parse_binary_message())
            elif token.kind == "binary":
                self.index += 1
                selector = token.text
                arguments = [self.parse_unary_message()]
            elif token.kind == "ident":
                self.index += 1
                selector = token.text
                arguments = []
            else:
                raise StError(f"{token.text!r} cannot start a cascaded message")
            node.cascades.append((selector, arguments))
        return node

    def parse_primary(self) -> Node:
        token = self.peek()
        if token is None:
            raise StError("the input ended where a value belongs")
        if token.kind == "number":
            self.index += 1
            return Literal(_number_of(token.text))
        if token.kind == "string":
            self.index += 1
            return Literal(token.text)
        if token.kind == "char":
            self.index += 1
            return Literal(StCharacter(token.text))
        if token.kind == "symbol":
            self.index += 1
            return Literal(StSymbol(token.text))
        if token.kind == "hash-array":
            self.index += 1
            return self.parse_array_literal()
        if token.kind == "ident":
            self.index += 1
            if token.text == "self":
                return SelfNode()
            if token.text == "super":
                return SuperNode()
            if token.text == "true":
                return Literal(True)
            if token.text == "false":
                return Literal(False)
            if token.text == "nil":
                return Literal(None)
            if self.accept("assign"):
                return Assignment(token.text, self.parse_expression())
            return Variable(token.text)
        if token.kind == "(":
            self.index += 1
            inner = self.parse_expression()
            self.expect(")", "a closing parenthesis")
            return inner
        if token.kind == "[":
            self.index += 1
            parameters: list[str] = []
            while self.accept(":"):
                parameters.append(self.expect("ident", "a block argument name").text)
            if parameters:
                self.expect("|", "a | after a block's arguments")
            temporaries, body = self.parse_body({"]"})
            self.expect("]", "a closing bracket")
            node = BlockLiteral(parameters, body)
            if temporaries:
                node.body.insert(0, _TemporaryDeclaration(temporaries))
            return node
        if token.kind == "|":
            raise StError(
                "a | here declares temporaries, and Smalltalk allows them only "
                "once, at the start of a method or block"
            )
        raise StError(f"{token.text!r} is not the start of a value")

    def parse_array_literal(self) -> Node:
        items: list[Node] = []
        while True:
            token = self.peek()
            if token is None:
                raise StError("an array literal was not closed")
            if token.kind == ")":
                self.index += 1
                return ArrayLiteral(items)
            items.append(self.parse_primary())


class _TemporaryDeclaration(Node):
    """Declares names in the frame; a block's temporaries happen to be simpler."""

    def __init__(self, names: list[str]) -> None:
        self.names = names

    def evaluate(self, frame: "Frame") -> Any:
        for name in self.names:
            frame.temporaries.setdefault(name, None)
        return None


def _number_of(text: str) -> Any:
    if "r" in text:
        radix, _, digits = text.partition("r")
        return int(digits, int(radix))
    if "." in text or "e" in text or "E" in text:
        return float(text)
    return int(text)


# ---------------------------------------------------------------------------
# Reading a file: definitions and top-level code
# ---------------------------------------------------------------------------

#: `ClassName >> selector` or `ClassName class >> selector`.
_METHOD_HEADER = re.compile(r"^\s*([A-Za-z_]\w*)\s*(class)?\s*>>\s*(.*)$")

#: `Object subclass: #Name`, which is how a class definition starts.
_CLASS_HEADER = re.compile(r"^\s*[A-Z]\w*\s+subclass:")


def split_chunks(source: str) -> list[str]:
    """Group the source into definitions and blocks of top-level code.

    A definition — a class or a method — is a header line followed by *indented*
    lines, and it ends at the next line that begins in column 0. Lines that are
    not inside a definition are top-level code, and consecutive ones form a single
    block however they are indented.

    That distinction is what stands in for a file-in's ``!`` terminator. Splitting
    on column 0 alone was wrong in a way that looked fine in one example: code
    written one statement per line became one chunk per statement, so each line was
    echoed separately instead of the block answering once.
    """
    chunks: list[str] = []
    current: list[str] = []
    inside_definition = False

    def flush() -> None:
        if current:
            chunks.append("\n".join(current))
            current.clear()

    for line in source.splitlines():
        if _METHOD_HEADER.match(line) or _CLASS_HEADER.match(line):
            flush()
            current.append(line)
            inside_definition = True
            continue
        if not current:
            current.append(line)
            inside_definition = False
            continue
        if inside_definition and line.strip() and not line[0].isspace():
            flush()
            current.append(line)
            inside_definition = False
            continue
        current.append(line)
    flush()
    return [chunk for chunk in chunks if chunk.strip()]


def _install_method(chunk: str, runtime: "Runtime") -> str:
    """Define one method. Returns the text to display."""
    lines = chunk.splitlines()
    match = _METHOD_HEADER.match(lines[0])
    assert match is not None
    class_name, class_side_marker, pattern = match.group(1), match.group(2), match.group(3)
    body_source = "\n".join(lines[1:])

    klass = runtime.classes.get(class_name)
    if klass is None:
        library = _LIBRARY_GLOBALS.get(class_name)
        if not isinstance(library, StClass):
            raise StError(f"{class_name} is not a class this build knows")
        klass = library

    selector, parameters = _method_pattern(pattern)
    try:
        tokens = tokenize(body_source)
    except StError as exc:
        raise StError(f"in {class_name} >> {selector}: {exc}") from exc
    parser = Parser(tokens)
    temporaries, statements = parser.parse_body(set())
    if not statements:
        raise StError(f"{class_name} >> {selector} has an empty body")
    if temporaries:
        statements.insert(0, _TemporaryDeclaration(temporaries))
    method = Method(selector, parameters, body=statements)
    table = klass.class_methods if class_side_marker else klass.methods
    if selector in table:
        raise StError(f"{class_name}{' class' if class_side_marker else ''} already "
                      f"implements {selector}")
    table[selector] = method
    where = f"{class_name} class" if class_side_marker else class_name
    return f"{where} >> {selector}\n"


def _method_pattern(pattern: str) -> tuple[str, list[str]]:
    """The selector and argument names written after ``>>``."""
    text = pattern.split('"')[0].strip()
    if not text:
        raise StError("a method definition needs a selector after >>")
    tokens = tokenize(text)
    if not tokens:
        raise StError("a method definition needs a selector after >>")
    if tokens[0].kind == "ident" and len(tokens) == 1:
        return tokens[0].text, []
    if tokens[0].kind == "binary":
        if len(tokens) != 2 or tokens[1].kind != "ident":
            raise StError(f"{text!r} is not a binary method pattern")
        return tokens[0].text, [tokens[1].text]
    if tokens[0].kind == "keyword":
        selector = ""
        parameters: list[str] = []
        index = 0
        while index < len(tokens):
            if tokens[index].kind != "keyword":
                raise StError(f"{text!r} is not a keyword method pattern")
            selector += tokens[index].text
            index += 1
            if index >= len(tokens) or tokens[index].kind != "ident":
                raise StError(f"{text!r} is missing the argument after {tokens[index - 1].text}")
            parameters.append(tokens[index].text)
            index += 1
        return selector, parameters
    raise StError(f"{text!r} is not a method pattern")


def _run_code(chunk: str, runtime: "Runtime") -> list[str]:
    """Evaluate top-level statements, echoing the last value if it is not nil.

    Only the last one: echoing every statement would print an object for each
    assignment and each ``do:``, and the object you were talking to is not the
    answer you asked for.
    """
    lines: list[str] = []
    parser = Parser(tokenize(chunk))
    temporaries, statements = parser.parse_body(set())
    if not statements:
        return lines
    frame = runtime.workspace_frame()
    for name in temporaries:
        frame.temporaries.setdefault(name, None)
    value: Any = None
    for statement in statements:
        try:
            value = statement.evaluate(frame)
        except NonLocalReturn as unwind:
            value = unwind.value
    if value is not None:
        lines.append(print_string(value))
    return lines


def execute_smalltalk(
    interpreter: "Interpreter",
    source: str,
    turtle: "TurtleState",
) -> str:
    """Read a Smalltalk file: definitions first, then run the code in it."""
    runtime = Runtime(turtle)
    try:
        chunks = split_chunks(source)
    except StError as exc:
        return f"❌ Smalltalk: {exc}\n"

    for chunk in chunks:
        header = _METHOD_HEADER.match(chunk.splitlines()[0])
        try:
            if header is not None:
                runtime.emit(_install_method(chunk, runtime).rstrip("\n"))
                continue
            for line in _run_code(chunk, runtime):
                runtime.emit(line)
        except StError as exc:
            return "".join(_with_newlines(runtime.output)) + f"❌ Smalltalk: {exc}\n"
        except NonLocalReturn as unwind:
            return (
                "".join(_with_newlines(runtime.output))
                + f"❌ Smalltalk: ^ outside a method returned {print_string(unwind.value)}\n"
            )
        except RecursionError:
            return (
                "".join(_with_newlines(runtime.output))
                + "❌ Smalltalk: the expression is nested too deeply\n"
            )
        except (IndexError, TypeError, ValueError, ArithmeticError) as exc:
            # A wrong argument type should read as a Smalltalk error, not as a
            # Python traceback the user cannot act on.
            return "".join(_with_newlines(runtime.output)) + f"❌ Smalltalk: {exc}\n"

    if runtime.transcript_buffer:
        runtime.emit("".join(runtime.transcript_buffer))
        runtime.transcript_buffer.clear()
    return "".join(_with_newlines(runtime.output))


def _with_newlines(lines: list[str]) -> list[str]:
    """Every emitted item becomes one output line.

    The engine splits whatever is returned into lines and logs them one at a
    time, so a Transcript's `show:` without a `cr` cannot be joined to the next
    thing printed. A run therefore emits complete lines.
    """
    return [line if line.endswith("\n") else line + "\n" for line in lines]
