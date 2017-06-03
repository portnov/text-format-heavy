text-format-heavy README
========================

This is Haskell string formatting library, which prefers functionality and
extendability over light weight and (probably, in some cases) performance.
This library is more or less analog of Python's string.format function, and
has similar syntax. It also exposes all required interfaces to extend and
customize it.

Most notable features are:

 * Automatically numbered variable placeholders (`{}`);
 * Positional variable placeholders (`{1}`);
 * Named variable placeholders (`{name}`);
 * Placeholders can be used in any order; one variable can be used several
   times or not used at all.
 * Specific format can be used for each variable substitution (`{0:+8.4}`).

Formatting strings are present by `Format` type. Values of this type can be
parsed from lazy Text, or can be entered as string literals, since `Format`
implements `IsString`.

The `format` function takes a `Format` specification and a container with
variables. Container types are generalized by `VarContainer` type class.
Standard container implementations include:

 * `Single` type for case when you need to pass only one variable.
 * Tuples and lists. These contain numbered variables, i.e. `{0}`, `{1}`, etc.
 * `[(Text, a)]` and `Map Text a`. These contain named variables, i.e.
   `{name}`.

One can implement custom variable containers, for example some record types.

Types of variables that can be used for subsitiution are generalized by
`Formatable` type class. Each implementation defines default value formatting
rules, and a syntax of variable format specification. For example, for
integers, floats and strings, python-like syntax is used. Standard set of
variable types includes:

 * Integers (`Int` and `Integer`, others can be easily added);
 * Floats (`Float` and `Double`);
 * Strings (`String`, lazy and strict `Text`);
 * Booleans;
 * Time/date values from Data.Time.
 * Any instance of `Show` type class can be used by packing it into `Shown`
   constructor.

One can implement custom variable types.

For examples, please refer to [GitHub
wiki](https://github.com/portnov/text-format-heavy/wiki) and `examples/`
directory in this repo. There are also some examples in haddock documentation.

License: BSD3.
