package o1.adventure

import scala.collection.mutable.Map

  

class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  val Inventory = Map[String, Item]()
  var isDisguised = false
  def quit(X:Any) = {
    quitCommandGiven = true
    "... but the world needed you!"
  }
  def hasQuit = {
    quitCommandGiven
  }

  def location = currentLocation
  
  def steal(thing: String) = {
    val thingFix = thing.split(" ")
    if (currentLocation.contains(thing)) {
      Inventory += thing -> currentLocation.removeItem(thing).get
      if (thing == "files") {
        Adventure.enemy.provoke(true) 
        Adventure.enemy.filesTaken
        "Quick! They are onto you. Run and disguise yourself!\nVladimir: DAVAI"
      }
      else if (Adventure.enemy.isHere) {
        Adventure.enemy.provoke(true) 
        "A guard saw that!"
      }
      else "You steal the " + thing + "."
    }
    else "What is that anyway?"
  }
  def examine(thing:String) = {
    if (Inventory.contains(thing)) Inventory(thing).description
    else "What is that anyway?"
  }
  
  def use(thing: String) = {
  if (Inventory.contains(thing)) Inventory(thing).use()
  else "No such item to use."
  }
  
  def inventory = {
    val teksti = Inventory.map(_._1).mkString(", ")
    if (Inventory.isEmpty) "You have nothing but your charming personality."
    else "You have: " + teksti + "."
  }
  /* Walk to a place with a chance to face a guard there. Can't be used to escape once a guard has been provoked. */
  def walk(place: String) = {
    var response = ""
    val placeFix = place.split(" ")
    var toGoTo = currentLocation
    for (area <- currentLocation.hasAccesTo) {
      if (placeFix.contains(area.callName)) toGoTo = area
    }
    if (toGoTo == currentLocation) {
     response = "Try some other place!\n"
    }
    else if (currentLocation.doorIsOpenTo(toGoTo)){
      
     response = "You walk to " + toGoTo.name + ".\n"
     if (Adventure.enemy.isHere && !Adventure.enemy.isComing) {
       response += " A guard didn't like your manners!\nVladimir: Come here you little..."
       currentLocation = toGoTo
       Adventure.enemy.provoke(true)
     } else if (Adventure.enemy.isHere) {
       currentLocation = toGoTo
       response += " That's a silly thing to do, when the guards are chasing you."
       Adventure.enemy.provoke(true)
     }
     else {
       currentLocation = toGoTo
       if (Adventure.enemy.callVladimir) response +=  Adventure.enemy.vladiChat(None)
     }
    }
    else {
      response = "Hmm. The door is locked\n"
      if (Adventure.enemy.callVladimir) response += Adventure.enemy.vladiChat(None)
    }
    response
  }
  /*By running the player can get to another room faster than a guard allowing him to use wig to disguise himself. If a guard sees the player running, he will get provoked.
   * If a guard hears the player running, but isn't in the same room, he will find the player normally. */
  def run(place: String) = {
    if (Adventure.enemy.isHere) {
      if (Adventure.enemy.isComing) Adventure.enemy.provoke(false)
      else {
        Adventure.enemy.provoke(false)
        Adventure.enemy.provoke(false)
      }
    } else if (Adventure.enemy.isComing) Adventure.enemy.provoke(true) 
    var response = ""
    val placeFix = place.split(" ")
    var toGoTo = currentLocation
    for (area <- currentLocation.hasAccesTo) {
      if (placeFix.contains(area.callName)) toGoTo = area
    }
    if (toGoTo == currentLocation) {
     response = "You ran into a wall.\nVladimir: Who on earth are you??"
     Adventure.enemy.provoke(true)
    }
    else if (currentLocation.doorIsOpenTo(toGoTo) && !Adventure.enemy.isComing){
      currentLocation = toGoTo
     response = "Your steps echo through the building as you run to " + toGoTo.name + ".\n"
      Adventure.enemy.alarmVladimir 
      response +=  Adventure.enemy.vladiChat(None)
    }
    else if (!currentLocation.doorIsOpenTo(toGoTo)) {
      response = "Aargh! The door is locked.\n"
      Adventure.enemy.alarmVladimir
    }
    else {
      currentLocation = toGoTo
      response = "He's on your tail!"
    }
    response    
  }
  def observe = this.location.description
  
  def answer(a: String) = {
    if (Adventure.enemy.isHere) {
    if (!a.contains('a') && a.contains('b')) {
      Adventure.enemy.vladiChat(Some('b'))
    }
    else if (!a.contains('b') && a.contains('a')) {
      Adventure.enemy.vladiChat(Some('a'))
    }
    else "Your mind of a secrent agent forces you to choose between A and B."
    }
    else "You see nobody to talk to."
  }
  
  
  
}


