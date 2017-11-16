package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer


class Area(val name: String, val description: String,val callName: String) {
  
  
  private val items = Map[String, Item]()
  
  def addItem(item: Item): Unit = {
    items += item.name -> item
  }
  
  
  def contains(itemName: String): Boolean = {
    items.contains(itemName)
  }
  
  def removeItem(itemName: String): Option[Item] = {
    items.remove(itemName)
  }
 
  def fullDescription = {
    val doorText = {
      if (hasAccesTo.size == 1) "a door"
      else "doors"
    }
    val paasy = {
      if (hasAccesTo.size <= 1) hasAccesTo.map(_.name).mkString
    else hasAccesTo.map(_.name).take(hasAccesTo.size - 1).mkString(", ") + " and " + hasAccesTo(hasAccesTo.size - 1).name
    }
    val item = {
      if (items.size > 0) "There is a " + this.items.map(_._1).mkString(" and a") + " on the ground."
      else "There are no items to steal."
    }
    this.name.toUpperCase + "\nYou see " + doorText + " leading to " + paasy + ".\n" + item
  }

  def hasAccesTo: Vector[Area] = {
    Adventure.doorList.map(_.goesFrom(this)).flatten
  }
  def doorIsOpenTo(x: Area): Boolean = {
    Adventure.doorList.filter( _.goesFrom(this) == Some(x) ).forall(_.isOpen)
  }
}

class Door(private val from: Area, private val to: Area, var isOpen: Boolean) {
  def connects = (from, to)
  def goesFrom(x: Area) = {
    if (x == from) Some(to)
    else if (x == to) Some(from)
    else None
  }
  
}