# Operational Transformation

This is my implementation of Operational Transformation in Scala.

NB: This is a pretty naive implementation and much of it doesn't look like idiomatic Scala. My first goal is to understand the algorithms and get them working, and do optimization later.

It operates strictly on plaintext documents, and implements operations composed using the following basic components:

* `Retain(n: Int)` - advance the cursor in the document, skipping over `n` characters
* `Insert(chars: String)` - insert `chars` at the current position of the cursor
* `Delete(n: Int)` - delete `n` characters from the cursor's current position

I have implemented the two crucial functions on operations: compose, and transform.

* `val composedOp: Operation = (op1 ++ op2)` - used between two operations that should be applied in sequence from the same starting document, this method composes them together into a single operation. This is useful for simplifying operations before they are to be transformed... rather than transforming 5 x 5 operations you can compose each side and transform 1 x 1 operation.
* `val (xfServerOp, xfClientOp) = transform(serverOp, clientOp)` - given two operations that should be applied in parallel on the same starting document, this will transform both operations returning them in a tuple: the server's operation will be transformed to be applied to the client's document, and the client's operation will be transformed to be applied on the server. Since all operations must still be ordered, we assume that the server's changes are applied "first" and in any conflict the server "wins".

## Usage

The library can be imported into your project with SBT. First, add my repository to your resolvers:

    resolvers ++= Seq(
      // ...other resolvers...
      "kahn.ws" at "https://repo.kahn.ws/maven/snapshots"
    )
    
And then add the dependency:

    libraryDependencies ++= Seq(
      // ...your other dependencies...
      "ws.kahn" %% "ot" % "1.0-SNAPSHOT"
    )
    
Currently only the 1.0-SNAPSHOT release has been published, and it's compiled for both Scala 2.10 and 2.11. As this is a little personal experiment / work in progress, may not work as intended, or at all.
