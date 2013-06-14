package org.goobs.sim.viz

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

import edu.stanford.nlp.util.logging.Redwood.Util._

class Trie {
  private val children:HashMap[Char,Trie] = new HashMap[Char,Trie]()
  private var is_contained:Boolean = false
  
  def +=(value:List[Char]):Unit = {
    if (value.isEmpty) {
      is_contained = true
    } else {
      val head = value.head.toLower
      children.get(head) match {
        case Some(child) => child += value.tail
        case None =>
          children(head) = new Trie()
          children(head) += value.tail
      }
    }
  }
  def +=(value:String):Unit = this += value.toCharArray.toList

  def apply(value:List[Char]):Boolean = {
    if (value.isEmpty) {
      is_contained
    } else {
      val head = value.head.toLower
      children.get(head) match {
        case Some(child) => child(value.tail)
        case None => false
      }
    }
  }
  def apply(value:String):Boolean = apply(value.toCharArray.toList)
  
  def prefix(value:List[Char]):Boolean = {
    if (value.isEmpty) {
      true
    } else {
      val head = value.head.toLower
      children.get(head) match {
        case Some(child) => child.prefix(value.tail)
        case None => false
      }
    }
  }
  def prefix(value:String):Boolean = prefix(value.toCharArray.toList)
}
