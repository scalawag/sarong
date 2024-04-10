# sarong

A tool for building indented string blocks blocks using string interpolation.
I created this to make a code generator more readable, but it can be used 
in any situation where you want formatted, multi-line strings. The difference
between this and Scala's stock interpolation is that it preserves whitespace 
and other formatting around the strings when they contain newlines.

## Getting Started

```sbt
libraryDependencies += "org.scalawag.sarong" %% "sarong" % "1.0.1"
```

## Usage

### Basics

Use similarly to Scala's stock string interpolation. The main difference is 
that, when multi-line strings are inserted, indentation is preserved. So, 
this code:

```scala
val insertMe = "all\nthat\njazz"

sarong"""
  Where there's a nightly brawl and...
    $insertMe
"""
```

yields this string:

```
Where there's a nightly brawl and...
  all
  that
  jazz
```

Note that all lines of the inserted string are indented to match where the 
first is located. It's as if the newlines in the inserted string are really 
just newlines (without carriage returns). The lines are padded with spaces, 
so this code:

```scala
val insertMe = "all\nthat\njazz"

sarong"""
  Where there's a nightly brawl and $insertMe!
"""
```

yields this string:

```
Where there's a nightly brawl and all
                                  that
                                  jazz!
```

Also note here that the thing following the interpolation (here an exclamation 
point) is only included after the last line of the inserted string.

### Bounding Boxes

You may also have noticed that, in the examples above, the resulting string 
is not indented as far as the template. This is because the string resulting 
from a `sarong` literal is always trimmed down to a bounding box. That means 
that all leading and trailing spaces are deleted and every line the longest 
common amount of whitespace removed. This effectively removes whatever 
indentation is most readable in your code while preserving the lines' 
relative indentation.

Hopefully, this ends up being intuitive. It's particularly handy when you're 
creating code, such as this.

```scala
val xdef = sarong"""
  val x = 8
"""

val ydef = sarong"""
  val y = {
    x * 2
  }
"""

sarong"""
  object Mine {
    $xdef
    $ydef
  }
"""
```

This yields the following string, which hopefully looks like you'd expect.

```
object Mine {
  val x = 8
  val y = {
    x * 2
  }
}
```

### Unfolding

Generally, insertions are formatted using `.toString`. This is similar to 
the built-in string interpolation. Something that comes up quite a bit, 
though, is the need to iterate through a collection of items and format them 
all the same. This is subtly different than regular insertion in that the 
leading a trailing strings for the insertion are repeated for each item in 
the collection.

You request this using the extension method `.unfold`, which should be 
available on `Traversable`, `Option` and `Iterator`. You have to import the 
entire package to get these extension methods.

```scala
val letters = Iterator("a", "b", "c")

sarong"""
  * ${letters.unfold}!
"""
```

```
* a!
* b!
* c!
```

You can also combine this with a collection of multi-line insertions.

```scala
val args = List(
  sarong"""
    List(2, 3, 5, 7).map { len =>
      "x" * len
    }
  """,
  sarong"""
    "just a string"
  """,
  sarong"""
    {
      // This a kind of a weird way to structure code, but
      // hopefully, it gives you the right idea.
      17
    }
  """
)

sarong"""
  List(
    ${args.unfold},
  )
"""
```

gives you:

```
List(
  List(2, 3, 5, 7).map { len =>
    "x" * len
  },
  "just a string",
  {
    // This a kind of a weird way to structure code, but
    // hopefully, it gives you the right idea.
    17
  },
)
```

Everything's still relatively aligned and the commas are where you'd hope.
If your unfoldable is empty, you don't get any lines (or commas) at all.

```scala
sarong"""
  List(
    ${Nil.unfold},
  )
"""
```

leads to:

```
List(
)
```

### Mutiple Insertions on One Line

sarong supports multiple insertions on one line and tries to do what you'd 
expect, but things get really non-intuitive really quickly. I recommend that 
you don't do it.

```scala
val x = "foo".mkString("\n")
val y = "bar".mkString("\n")

sarong"""$x $y"""
```

gives you this:

```
f
o
o b
  a
  r
```

Multiple unfolds on a single line is not supported. That's because I'm not 
even sure what I would expect it to do! If you have a suggestion, I'm all ears!
