package o1.adventure

/** The class `Item` represents items in a text adventure game. Each item has a name 
  * and a  *  longer description. (In later versions of the adventure game, items may 
  * have other features as well.)
  *
  * N.B. It is assumed, but not enforced by this class, that items have unique names. 
  * That is, no two items in a game world have the same name.
  *
  * @param name         the item's name
  * @param description  the item's description */
class Item(val name: String, var description: String, var usability: Boolean) {
  
  def use(): String = {
    "Nothing to use this for."
  }
  /** Returns a short textual representation of the item (its name, that is). */
  override def toString = this.name
  
  
}

class Wig(name: String, description: String, usability: Boolean) extends Item(name, description, usability) {
  override def use() = {
    if (!Adventure.player.isDisguised) {
      Adventure.player.isDisguised = true
    }
      else Adventure.player.isDisguised = false
      
      if (!Vladimir.isHere) {
        if (Vladimir.isComing)  {
          Vladimir.hide()
          Vladimir.alarmVladimir
        "You disguise yourself!\n" + Vladimir.vladiChat(None)
        }  
        else {
          "You change your apparel."
        }
      }
        else {
        Vladimir.provoke(true)
        "A guard saw you doing that!\nVladimir: I FOUND A SPY!"
      }
  }
}

class Key(name: String, descript: String, isUsable: Boolean,private val door: Door) extends Item(name, descript, isUsable) {
  override def use(): String = {
    if (Adventure.player.location == Adventure.office && this.usability) {
      door.isOpen = true
      if (Vladimir.isHere) {
        Vladimir.provoke(true)
        "A guard saw you doing that.\nVladimir: Ostanovit'!"
      } 
      else "You opened the door to the hidden room!"
    }
    else if (!usability) "The key is broken. :-("
    else "Can't see any locks here."
  }
}
class Glue(name: String, descri: String, someLeft: Boolean, thing: Item) extends Item(name, descri, someLeft) {
  override def use() = {
    if (usability) {
      if (Adventure.player.Inventory.contains(thing.name)) {
        thing.usability = true
        this.usability = false
        this.description = "Empty tube of glue."
        thing.description = "Almost as good as new."
        "You glue the " + thing.name + " back together!"
      }
      else "Nothing to glue."
    }
    else "There is no glue left."
  }
}
class Laptop extends Item("laptop", "Seems to contain a fun game!", true) {
  override def use() = {
    o1.tetris.Game.startup(Array())
    "You start a game."
  }
}