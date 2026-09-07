---
title: Conversions
layout: default
parent: Language
nav_order: 11
permalink: /language/conversions/
description: "The To/TryTo conversion family: every conversion is either proven lossless at compile time or returns nullable. Spek has no cast operator, and no conversion ever throws."
---

# Conversions

C# spells every type conversion the same way. `(byte)total`, `(Circle)shape`,
and `(Severity)wire` look identical at the call site, but they carry three
different risk profiles: the numeric cast wraps silently on overflow (cast
an `int` holding 300 to `byte` and you get `44`, no error, no warning), the
reference downcast throws `InvalidCastException` at runtime, and the
floating-point cast truncates by design. One syntax hides all three, so a
reader can't tell a safe conversion from a gamble without looking up both
types.

Spek doesn't import that operator. Conversions are two ordinary-looking
methods with a one-sentence contract: **`To` can't fail, `TryTo` can't lie.**
`To<T>()` compiles only when the conversion is provably lossless, so it has
no failure case to handle. `TryTo<T>()` takes every fallible direction and
returns nullable, so a value that doesn't convert exactly comes back as
`null`. No member of the family throws, wraps, or truncates. This chapter
covers both methods, the rounding strategies for floating-point values, and
the three diagnostics that police the boundary.

## Why there is no cast operator

Write a cast and the compiler stops you with a diagnostic that names the
replacement:

<!-- spek-test: ignore -->
```spek
var narrowed = (byte)total;
// error[CE0129]: '(byte)' — Spek has no cast operator. Use x.To<byte>()
//   when the conversion is lossless, or x.TryTo<byte>() (byte?) when it
//   can lose information.
```

The hint tracks the target. Cast to a declared message or class and
[CE0129](/reference/errors/#ce0129) suggests a type test instead ("Test the
type instead: 'x is Circle v' or 'x as Circle'."); cast to an enum and it
suggests `TryTo<Severity>()`. In each case the diagnostic is teaching the
spelling whose semantics match what the cast would have silently gambled on.

The deeper rule at work: when one familiar C# syntax bundles several
behaviors with different risk profiles, Spek retires the syntax and gives
each behavior its own checked spelling, even at the cost of some C# muscle
memory.

## To: proven lossless at compile time

Two kinds of conversion can never lose information: numeric widening
(`int → long`, `float → double`) and reference upcasts (`Circle → Shape`,
where every `Circle` already is a `Shape`). `To<T>()` covers exactly those:

<!-- spek-test: compile -->
```spek
namespace Telemetry;

abstract message Reading();
message Temperature(double celsius) : Reading;

module Scaling
{
    public long TotalBytes(int packets, int packetSize)
    {
        return packets.To<long>() * packetSize.To<long>();
    }

    public Reading AsReading(Temperature t)
    {
        return t.To<Reading>();   // upcast: every Temperature is a Reading
    }
}
```

Ask `To` for a direction that could lose data and the build fails:

<!-- spek-test: ignore -->
```spek
public byte Compress(long total)
{
    return total.To<byte>();
    // error CS1503: Argument 1: cannot convert from 'long' to 'byte'
}
```

That error is Roslyn's own, which reflects how the guarantee works:
`To<T>()` accepts only arguments C# can convert to `T` *implicitly*, and the
proof comes from the C# type system. Both `spekc compile --check` and
`dotnet build` surface the failure, and the fix is always the same: if the
loss is real, say so with `TryTo`.

C#'s judgment of "implicit" is not quite the same as "lossless", and Spek
closes the gap. C# treats the integer-to-floating widenings as implicit even
though a `float` cannot represent every `int` (its mantissa is narrower than
the source's integer range, so `16777217` rounds to `16777216`). Spek rejects
those pairs itself: `To<float>()` from any integer type, and `To<double>()`
from `long` or `ulong`, fail with
[CE0138](/reference/errors/#ce0138) and steer to `TryTo`, which returns null
when the value is not exactly representable. Every other implicit conversion
passes through on C#'s proof alone.

One direction is absent: enum to integral (`severity.To<int>()`) is not in
the family and fails the same way, because C# has no implicit enum-to-int
conversion for the proof to lean on. Perform that conversion in a C# interop
file.

## TryTo: exact or null

`TryTo<T>()` performs the fallible directions and returns `T?`. Its contract
is *exactly representable*, which is stricter than "fits in range": if
`TryTo` hands you a value, converting it back gives the value you started
with.

<!-- spek-test: compile -->
```spek
namespace Telemetry;

module Fit
{
    public byte Compress(long total)
    {
        var b = total.TryTo<byte>();       // byte? — null unless exact
        if (b != null) { return b.Value; }
        return 255;
    }
}
```

So `(300L).TryTo<byte>()` is `null` because 300 doesn't fit,
`(2.9).TryTo<int>()` is `null`, never a truncated `2`, and
`(0.1d).TryTo<float>()` is `null` because the nearest `float` to `0.1`
drifts from the `double`. A fraction never silently disappears. If you want
it gone, the next section is how you say so.

{: .note }
> **Where this comes from.** These are the semantics of Swift's
> `Int8(exactly:)` initializers: conversion succeeds only when the value
> round-trips, and failure is a typed absence rather than an exception.
> Nothing in the family throws; `null` is the only failure channel, so a
> `TryTo` in a hot message handler can never take down the actor.

### Rounding: part of the request

When you *do* want a fractional value coerced into an integer, the rounding
strategy is an argument to the conversion, not a preprocessing step you
perform first. The floating-to-integral overloads accept the BCL's
`MidpointRounding`, the same enum `Math.Round` uses:

<!-- spek-test: compile -->
```spek
namespace Telemetry;

module Rounding
{
    public int Floor(double raw)
    {
        return raw.TryTo<int>(MidpointRounding.ToZero) ?? 0;              // 2.9 → 2
    }

    public int Ceiling(double raw)
    {
        return raw.TryTo<int>(MidpointRounding.ToPositiveInfinity) ?? 0;  // 2.1 → 3
    }
}
```

All five strategies work: `ToEven`, `AwayFromZero`, `ToZero`,
`ToNegativeInfinity`, and `ToPositiveInfinity`. The precise names matter for
negative values, where "round down" is ambiguous. `ToZero` and `ToNegativeInfinity` disagree about `-2.5`. The strategy refines the
contract without bending it: the result is `null` only when the *rounded*
value doesn't fit the target, so `(1e300).TryTo<int>(MidpointRounding.ToZero)`
and `double.NaN` are still `null`. The parameter exists only on the
floating-to-integral overloads. There is nothing to round anywhere else,
and overload resolution polices that.

### The known-fits idiom

Sometimes you know the value fits: a `long` row count that is definitely
small, an index already bounds-checked. The family deliberately has no
throwing member, so the claim of knowledge is yours to write:

<!-- spek-test: compile -->
```spek
namespace Telemetry;

module Budget
{
    public int Definite(long total)
    {
        return total.TryTo<int>() ?? throw new System.OverflowException();
    }
}
```

The `?? throw` makes the assumption visible at the call site and reviewable
in a diff, which is exactly where a "this can't overflow" claim should live.

### Integers into enums

An integer arriving off the wire may or may not name a real enum variant, so
the integral-to-enum direction is `TryTo`, returning the variant or `null`:

<!-- spek-test: compile -->
```spek
namespace Wire;

enum Severity { Low, Medium, High }

flags enum Access { Read, Write, Execute }

module Decode
{
    public Severity ParseSeverity(int raw)
    {
        return raw.TryTo<Severity>() ?? Severity.Low;
    }

    public bool IsValidAccess(int raw)
    {
        return raw.TryTo<Access>() != null;   // 3 = Read|Write: valid
    }
}
```

For a plain [enum](/language/enums/) the rule is membership: the value must
be a defined variant. For a
[`flags enum`](/language/enums/#flags-enums) that rule would be wrong, because
`Access.Read | Access.Write` (`3`) is a perfectly valid value that no single
variant defines. The flags-aware rule is that every set bit must correspond
to a defined flag, so `3` parses while `8` (an undefined bit) is `null`.
This is a check `Enum.IsDefined` gets wrong in C#, where valid combinations
are rejected and casting undefined values succeeds silently.

### Reference downcasts

Going down the hierarchy is fallible, so it too is `TryTo`:

<!-- spek-test: compile -->
```spek
namespace Geometry;

abstract message Shape();
message Circle(double radius) : Shape;
message Square(double side) : Shape;

module Area
{
    public double Of(Shape s)
    {
        var c = s.TryTo<Circle>();   // Circle? — null when s is a Square
        if (c != null) { return 3.14159 * c.radius * c.radius; }
        return 0.0;
    }
}
```

A reference-target `TryTo` compiles to C#'s `as`, which keeps
Roslyn's relatedness check alive: `someDouble.TryTo<Circle>()` is a compile
error (`CS0039`, the types are provably unrelated), never a call that
compiles and always returns `null`. The nullable result chains cleanly with
the rest of the null tooling, so `s.TryTo<Circle>()?.radius` reads as one
expression.

## Object sources: is-patterns are the path

No overload of `To` or `TryTo` accepts `object`. Write `o.TryTo<int>()` and
the generated C# fails with `error CS1503: Argument 1: cannot convert from
'object' to 'int'`, by construction: a type-erased source is an untrusted
source, and the tool for interrogating one is a pattern, which puts the
failure branch in front of you:

<!-- spek-test: compile -->
```spek
namespace Inspect;

message Circle(double radius);

module Untrusted
{
    public double AreaOf(object payload)
    {
        if (payload is Circle c) { return 3.14159 * c.radius * c.radius; }
        return 0.0;
    }
}
```

## External types: the C# interop escape hatch

The compiler routes each conversion by its target, so the target must be
something it can reason about: a numeric primitive, or a declared enum,
class, message, or interface. Anything else is
[CE0127](/reference/errors/#ce0127):

<!-- spek-test: ignore -->
```spek
var url = raw.TryTo<Uri>();
// error[CE0127]: 'TryTo<Uri>' — conversion target 'Uri' must be a numeric
//   primitive or a declared enum, class, message, or interface. Convert
//   external types in a C# interop file.
```

The escape hatch is the one the message names: put the conversion in a
plain `.cs` file in the same project, where the full C# conversion toolbox
applies, and call it from Spek like any other method. The boundary is
deliberate. Inside `.spek` files every conversion carries the family's
guarantees; conversions the compiler can't check live where their language
can check them.

## What the emitter generates

`To` and `TryTo` are compiler-known methods. The emitter rewrites each call
to a static call on the runtime's conversion classes, or to `as` for
reference targets:

```csharp
packets.To<long>()       // → global::Spek.Conversions.To<long>(packets)
raw.TryTo<Severity>()    // → global::Spek.EnumConversions.TryTo<Severity>(raw)
shape.TryTo<Circle>()    // → (shape as Circle)
```

The static form is load-bearing: C# extension receivers accept only
identity, reference, and boxing conversions, so an extension-method `To`
would never see the numeric widening it exists to prove. Because the
compiler rewrites these calls, the two names are reserved
([CE0128](/reference/errors/#ce0128)) so the rewrite can never hijack a
method you wrote:

<!-- spek-test: ignore -->
```spek
module Parser
{
    public int To(int x) { return x; }
    // error[CE0128]: 'To' is reserved for the Spek conversion family
    //   (x.To<T>() / x.TryTo<T>()), whose calls the compiler rewrites.
    //   Rename the method.
}
```

The conversion classes live in `Spek.Runtime`, so C# code in the same
solution can call `Spek.Conversions.TryTo<byte>(total)` directly and get the
same exact-or-null semantics.

## Next

Conversions complete the value-handling story: data changes type only along
proven-lossless paths or through a visible `null`. Next,
[Modules](/language/modules/) are where stateless conversion-heavy helper
code like the examples above actually lives.

## Related reading

- [Enums](/language/enums/): declaring enums and flags enums, and the
  exhaustive matching that pairs with `TryTo`'s defined-or-null parsing.
- [Messages](/language/messages/): the immutable types whose fields wire
  values are usually converted into.
- [CE0127](/reference/errors/#ce0127), [CE0128](/reference/errors/#ce0128),
  [CE0129](/reference/errors/#ce0129): the conversion-family diagnostics.
