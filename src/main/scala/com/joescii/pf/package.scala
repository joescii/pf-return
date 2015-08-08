package com.joescii

import scala.util.Try

package object pf {
  def parse(file:String):List[FileEntry] = file.split("(?s)\r?\n").map(_.trim).filterNot(_.isEmpty)
    .foldLeft((List.empty[FileEntry], setExtractor)){
    case ((acc, e), line) =>
      (e or wtf)(line) match {
        case (wtf @ Wtf(_), _) => (acc :+ wtf, e)
        case (entry, next)  => (acc :+ entry, next)
      }
  }._1

  trait Extractor extends PartialFunction[String, (FileEntry, Extractor)] {
    def or(that:Extractor):Extractor = Extractor(this orElse that)
  }
  object Extractor {
    def apply(f: PartialFunction[String, (FileEntry, Extractor)]):Extractor = new Extractor {
      override def isDefinedAt(x: String): Boolean = f isDefinedAt x
      override def apply(v: String): (FileEntry, Extractor) = f(v)
      override def applyOrElse[A1 <: String, B1 >: (FileEntry, Extractor)](x: A1, default: (A1) ⇒ B1): B1 = f.applyOrElse(x, default)
    }
  }

  val setExtractor:Extractor = Extractor({
    case line if line startsWith "AttributeSet " => (AttributeSet(line.substring("AttributeSet ".length)), attributeExtractor)
    case line if line startsWith "ComplexSet "   => (ComplexSet  (line.substring("ComplexSet ".length)),   complexExtractor)
  })
  val Split = """(\S*)\s+(\S*)""".r
  val attributeExtractor:Extractor = Extractor({
    case Split(key, value) => (Attribute(key, value), attributeExtractor)
  }) or setExtractor
  val complexExtractor:Extractor = Extractor({
    case Split(d(real), d(imaginary)) => (Complex(real, imaginary), complexExtractor)
  }) or setExtractor
  val wtf:Extractor = Extractor({
    case line => (Wtf(line), Extractor(PartialFunction.empty))
  })

  object d {
    def unapply(s: String): Option[Double] = Try(s.toDouble).toOption
  }

  sealed trait FileEntry
  case class AttributeSet(name:String) extends FileEntry
  case class Attribute(key:String, value:String) extends FileEntry
  case class ComplexSet(name:String) extends FileEntry
  case class Complex(real:Double, imaginary:Double) extends FileEntry
  case class Wtf(line:String) extends FileEntry
}
