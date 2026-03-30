package it.unibo.pps.tasks.adts

/*  Exercise 1: 
 *  Complete the implementation of ComplexADT trait below, so that it passes
 *  the test in ComplexTest.
 */

object _Ex1ComplexNumbers:

  trait ComplexADT:
    type Complex

    def complex(re: Double, im: Double): Complex
    extension (complex: Complex)
      def re(): Double
      def im(): Double
      def sum(other: Complex): Complex
      def subtract(other: Complex): Complex
      def asString(): String

  object BasicComplexADT extends ComplexADT:

    // Change assignment below: should probably define a case class and use it?
    type Complex = (Double, Double)
    def complex(re: Double, im: Double): Complex = (re, im)

    extension (complex: Complex)
      
      def re(): Double = complex match
        case (re, im) => re

      def im(): Double = complex match
        case (re, im) => im

      def sum(other: Complex): Complex =
        (complex.re() + other.re(), complex.im() + other.im())

      def subtract(other: Complex): Complex =
        (complex.re() - other.re(), complex.im() - other.im())

      def asString(): String = complex match
        case (0, 0)   => "0.0"
        case (0, im)  => im + "i"
        case (re, 0)  => re + ""
        case (re, im) => re + (if im < 0 then " - " + im * (-1) else " + " + im) + "i"