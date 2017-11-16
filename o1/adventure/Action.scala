package o1.adventure


/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect, 
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim

  private val commandList = "WALK (to) " + '"' + "place" + '"' + " - You walk into the place.\nRUN (to) " + '"' + "place" + '"' +
  " - If the guards are chasing you, you need to run to the next room to disguise yourself.\nSTEAL " + '"' + "item" + '"' + " - You steal something for higher good. Use TAKE if it makes you feel better." + 
                            "\nINVENTORY - Check what you have with you. \nEXAMINE " + '"' + "item" + '"' + " - You examine the item more closely.\nUSE " + '"' + "item" + '"' +
                            " - You try to use the item\nOBSERVE - You check your surrounding area.\nQUIT - You don't need this one."
  
  
  def introCommand(): String = {
    if (this.verb == "ok") {
      Storyline.intro("next")
    } else if (this.verb == "previous") {
      Storyline.intro("previous")
    } else if (verb == "skip") {
      Storyline.intro("skip")
    } else "What?"
  }
  
  
  /** Causes the given player to take the action represented by this object, assuming 
    * that the command was understood. Returns a description of what happened as a result 
    * of the action (such as "You go west."). The description is returned in an `Option` 
    * wrapper; if the command was not recognized, `None` is returned. */
  
  
  def execute(actor: Player): Option[String] = {                             

    if (this.verb == "walk") {
      Some(actor.walk(this.modifiers))
    } else if (this.verb == "quit") {
      Some(actor.quit())
    } else if (this.verb == "steal" || this.verb == "take") {
      Some(actor.steal(this.modifiers))
    } else if (this.verb == "inventory") {
      Some(actor.inventory) 
    } else if (this.verb == "examine") {
      Some(actor.examine(this.modifiers))
    } else if (this.verb == "use") {
      Some(actor.use(this.modifiers))
    } else if (this.verb == "glue" && modifiers.contains("key")) {
      Some(actor.use("glue"))
    } else if (this.verb == "observe") Some(actor.observe)
    else if (this.verb == "help") Some(commandList)
    else if(this.verb == "answer") {
      Some(actor.answer(this.modifiers))
    } else if (this.verb == "run") {
      Some(actor.run(this.modifiers))
    }
    
    else {
      None
    }
    
  }


  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"  

  
}

