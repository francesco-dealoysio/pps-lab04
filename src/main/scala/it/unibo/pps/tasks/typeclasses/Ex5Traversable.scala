package it.unibo.pps.tasks.typeclasses

import it.unibo.pps.u03.Sequences.Sequence
import Sequence.*
import it.unibo.pps.u03.Optionals.Optional
import it.unibo.pps.u03.Optionals.Optional.*

/*  Exercise 5: 
 *  - Generalise by ad-hoc polymorphism logAll, such that:
 *  -- it can be called on Sequences but also on Optional, or others... 
 *  -- it does not necessarily call log, but any function with analogous type
 *  - Hint: introduce a type class Traversable[T[_]]], capturing the ability of calling a
 *    "consumer function" on all elements (with type A) of a datastructure T[A] 
 *    Note Traversable is a 2-kinded trait (similar to Filterable, or Monad)
 *  - Write givens for Traversable[Optional] and Traversable[Sequence]
 *  - Show you can use the generalisation of logAll to:
 *  -- log all elements of an Optional, or of a Traversable
 *  -- println(_) all elements of an Optional, or of a Traversable
 */

object Ex5Traversable:

  def log[A](a: A): Unit = println("The next element is: " + a)

  trait Traversable[T[_]]:
    def foreach[A](container: T[A])(consumer: A => Unit): Unit

  def consumeAll[T[_]: Traversable, A](container: T[A])(consumer: A => Unit): Unit =
    val traversable = summon[Traversable[T]]
    traversable.foreach(container)(consumer)

  def logAll[T[_]: Traversable, A](container: T[A]): Unit =
    consumeAll(container)(log)

  given Traversable[Sequence] with
    def foreach[A](container: Sequence[A])(consumer: A => Unit): Unit =
      container match
        case Cons(h, t) =>
          consumer(h)
          foreach(t)(consumer)
        case _ => ()

  given Traversable[Optional] with
    def foreach[A](container: Optional[A])(consumer: A => Unit): Unit =
      container match
        case Just(a) => consumer(a)
        case Empty() => ()

 /* Per Sequence : se c'è una testa h, applica consumer (h) poi continua ricorsivamente sulla coda t
    se è vuota, finisce
  Per Optional : Just(a) → applica consumer (a)
                 Empty() → non fa nulla. */

  @main def tryTraversable =
    val seq = Cons(1, Cons(2, Cons(3, Nil())))
    logAll(seq)
    consumeAll(seq)(println)

    val o1 = Just(42)
    logAll(o1)
    consumeAll(o1)(println)

    val o2 = Empty()
    logAll(o2)
    consumeAll(o2)(println)