package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

import PerformanceUtils.*

def runBasicBenchmarks(): Unit =
  val SIZE = 50000
  val INDEX_TO_READ = SIZE / 2
  val immListBench = (1 to SIZE).toList
  val immVecBench = (1 to SIZE).toVector
  val mutArrBufBench = ArrayBuffer.from(1 to SIZE)

  println("\nRead")
  measure("List read") {
    immListBench(INDEX_TO_READ)
  }
  measure("Vector read") {
    immVecBench(INDEX_TO_READ)
  }
  measure("ArrayBuffer read") {
    mutArrBufBench(INDEX_TO_READ)
  }

  println("\nPrepend")
  measure("List prepend") {
    0 :: immListBench
  }
  measure("Vector prepend") {
    0 +: immVecBench
  }
  measure("ArrayBuffer prepend") {
    0 +=: mutArrBufBench.clone()
  }

  println("\nAppend")
  measure("List append") {
    immListBench :+ 0
  }
  measure("Vector append") {
    immVecBench :+ 0
  }
  measure("ArrayBuffer append") {
    mutArrBufBench.clone() += 0
  }

@main def checkPerformance: Unit =

  /* Linear sequences: List, ListBuffer */
  val immList = List(1, 2, 3)
  println(immList)
  val readImmL = immList(1)
  println(readImmL)
  val updateImmL = immList.updated(1, 4)
  val deleteImmL = immList.filterNot(_ == 1)
  println(updateImmL)
  println(deleteImmL)
  println(immList)

  val mutList = ListBuffer(1, 2, 3)
  println(mutList)
  val readMutL = mutList(1)
  println(readMutL)
  mutList(1) = 4
  mutList -= 1
  println(mutList)

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val immVec = Vector(1, 2, 3)
  println(immVec)
  val readImmV = immVec(1)
  println(readImmV)
  val updateImmV = immVec.updated(1, 4)
  val deleteImmV = immVec.filterNot(_ == 1)
  println(updateImmV)
  println(deleteImmV)
  println(immVec)

  val arr = Array(1, 2, 3)
  println(arr.mkString("Array(", ", ", ")"))
  val readArr = arr(1)
  println(readArr)
  arr(1) = 4
  println(arr.mkString("Array(", ", ", ")"))
  val deleteArr = arr.filterNot(_ == 1)
  println(deleteArr.mkString("Array(", ", ", ")"))

  val mutArr = ArrayBuffer(1, 2, 3)
  println(mutArr)
  val readMutA = mutArr(1)
  println(readMutA)
  mutArr(1) = 4
  mutArr -= 1
  println(mutArr)

  /* Sets */
  val immSet = Set(1, 2, 3)
  println(immSet)
  val readImmS = immSet(1)
  println(readImmS)
  val updateImmS = immSet + 4
  val deleteImmS = immSet - 1
  println(updateImmS)
  println(deleteImmS)
  println(immSet)

  val mutSet = mutable.Set(1, 2, 3)
  println(mutSet)
  val readMut = mutSet(1)
  println(readMut)
  mutSet += 4
  mutSet -= 1
  println(mutSet)

  /* Maps */
  val immMap = Map(1 -> "A", 2 -> "B", 3 -> "C", 4 -> "D", 5 -> "E")
  println(immMap)
  val readImmM = immMap(1)
  println(readImmM)
  val updateImmM = immMap + (6 -> "F")
  val deleteImmM = immMap - 1
  println(updateImmM)
  println(deleteImmM)
  println(immMap)

  val mutMap = mutable.Map(1 -> "A", 2 -> "B", 3 -> "C", 4 -> "D", 5 -> "E")
  println(mutMap)
  val readMutM = mutMap(1)
  println(readMutM)
  mutMap(6) = "F"
  mutMap -= 1
  println(mutMap)

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))

  runBasicBenchmarks()
