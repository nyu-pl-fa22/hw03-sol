package pl.hw03

import scala.collection.mutable

object hw03 extends App:

  // Problem 1: Numbers to Words
  def numToWords(num: Int): String =
    require(num >= 0)
    val sb = new mutable.StringBuilder
  
    var rem = num
    while rem > 0 do
      val next = rem % 10 match
        case 0 => "zero"
        case 1 => "one"
        case 2 => "two"
        case 3 => "three"
        case 4 => "four"
        case 5 => "five"
        case 6 => "six"
        case 7 => "seven"
        case 8 => "eight"
        case 9 => "nine"

      if sb.nonEmpty then
        sb.insert(0, '-')

      sb.insert(0, next)
      rem = rem / 10
    end while
    
    if sb.nonEmpty then sb.result() else "zero"
  
  // Problem 2: Newton's Method
  
  def squareRoot(c: Double, epsilon: Double): Double = 
    require (c >= 0) // makes sure that we don't call squareRoot with negative values
    require (epsilon >= 0) // the error bound should also be positive

    var xn = 1.0
    
    // Replace the ??? with your implementation of Newton's Method
    
    while Math.abs(xn * xn - c) >= epsilon do
      xn = xn - (xn * xn - c) / (2 * xn)
    end while
    
    xn
  
  // Problem 3: Binary Search

  def isStrictlySorted(a: Array[Int]): Boolean = 
    require (a != null) // make sure that 'a' is non-null

    var i = 0

    while i < a.length && (i == 0 || a(i - 1) < a(i)) do
      i = i + 1
    end while

    a.length == i

  def binarySearch(x: Int, a: Array[Int]): Int = 
    require (a != null && isStrictlySorted(a)) // make sure that 'a' is non-null and sorted
    
    var l: Int = 0
    var r: Int = a.length
    var found: Boolean = false
    var i: Int = 0

    // the (inductive) loop invariant
    def invariant(): Unit =
      assert (0 <= l && l <= r && r <= a.length)
      assert (r == a.length || x < a(r))
      assert (l <= 0 || a(l - 1) < x)
      assert (!found || 0 <= i && i < a.length && x == a(i))
      
    while l < r && !found do
      invariant()
      val m = l + (r - l) / 2

      if a(m) < x then
        l = m + 1
      else if a(m) > x then
        r = m
      else
        i = m
        found = true
    end while
    invariant()
    
    if !found then i = r

    i
  
  // Add testing code here, or better yet, add them as additional unit tests to test/scala/pl/hw03/hw03Spec.scala
  
  // Examples:
  // println(squareRoot(9, 0.0001))
  // println(isStrictlySorted(Array(1, 2, 3)))
  // println(binarySearch(2, Array(1, 3, 4)))

