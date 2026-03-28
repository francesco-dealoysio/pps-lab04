package it.unibo.pps.tasks.adts

import org.junit.*
import org.junit.Assert.*

class _Stacktest:
  import Ex3Stacks.StackImpl
  import it.unibo.pps.u03.Sequences.Sequence
  import it.unibo.pps.u03.Optionals.Optional

  val stack = StackImpl

  import stack.*
  @Test def testEmptyStackHasNoElements() =
    assertEquals(Sequence.Nil(), empty[Int].asSequence())
  
  @Test def testPushAddsElementToStack() =
    assertEquals(Sequence.Cons(10, Sequence.Nil()), empty[Int].push(10).asSequence())
  
  @Test def testPopOnEmptyStackReturnsEmpty() =
    assertEquals(Optional.Empty(), empty[Int].pop())
  
  @Test def testPopOnStackWithOneElementReturnsElementAndEmptyStack() =
    assertEquals(Optional.Just((10, empty[Int])), empty[Int].push(10).pop())
  
  @Test def testPushMultipleElementsAndVerifyOrder() =
    val stack = empty[Int].push(10).push(20).push(30)
    assertEquals(Sequence.Cons(30, Sequence.Cons(20, Sequence.Cons(10, Sequence.Nil()))), stack.asSequence())

  @Test def testPopMultipleElementsMaintainsOrder() =
    val stack = empty[Int].push(10).push(20)
    val popResult = stack.pop()
    assertEquals(Optional.Just((20, empty[Int].push(10))), popResult)
