package it.unibo.pps.tasks.adts

//import it.unibo.pps.u03.extensionmethods.Sequences.Sequence, Sequence.*
//import it.unibo.pps.u03.extensionmethods.Optionals.Optional, Optional.*

/*  Exercise 3: 
 *  Implement a Stack ADT
 *  Suggestion: 
 *  - push adds an element and returns the new stack
 *  - pop returns:
 *  -- empty optional is stack is empty
 *  -- a pair of top of the stack and the new stack after removal if not empty
 */
object _Ex3Stacks:
  import it.unibo.pps.u03.Sequences.Sequence, Sequence.*
  import it.unibo.pps.u03.Optionals.Optional, Optional.*

  trait StackADT:
    type Stack[A]
    def empty[A]: Stack[A] // factory
    extension [A](stack: Stack[A])
      def push(a: A): Stack[A]
      def pop(): Optional[(A, Stack[A])]
      def asSequence(): Sequence[A]

  object StackImpl extends StackADT:
    type Stack[A] = Sequence[A]
    def empty[A]: Stack[A] = Nil()

    extension [A](stack: Stack[A])

      def push(a: A): Stack[A] =
        Cons(a, stack)

      def pop(): Optional[(A, Stack[A])] = stack match
        case Cons(h, t) => Just((h, t))
        case _ => Empty()

      def asSequence(): Sequence[A] = stack match
        case Cons(h, t) => Cons(h, t)
        case _ => Nil()