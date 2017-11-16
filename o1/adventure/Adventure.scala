package o1.adventure


/** The class `Adventure` represents text adventure games. An adventure consists of a player and 
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of "hard-coded" information which pertain to a very 
  * specific adventure game that involves a small trip through a twisted forest. All newly created 
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure 
  * games, you will need to modify or replace the source code of this class. */
object Adventure {

  /** The title of the adventure game. */
  val title = "The nuclear agent hazard game"
    
   val entrace = new Area("the entrace", "It looks like something out of a Bond movie. You hear guards walking about.", "entrace")
   val mainHall = new Area("a hall", """ "Hmm. Where should I go." """, "hall")
   val office = new Area("an office of some sort", "Can't see any launch codes here.", "office")
   val restroom = new Area("a restroom", "It's a restroom, alright?", "restroom")
   val court = new Area("an inner court", "A lot of guards here!", "court") 
   val hiddenRoom = new Area("a hidden room hidden behind a bookshelf", "Looks like just the place to hide things in!", "room")
   val gallery = new Area("a gallery with paintings and statues", "Someone has clearly tried to glue that arm back to the statue!", "gallery")
   val monitoringRoom = new Area("a monitoring room filled with guards", "How did I get here!?", "monitoring")
   
  
  
  val doorList: Vector[Door] = Vector(new Door(entrace, mainHall, true), new Door(mainHall, office, true), new Door(mainHall, restroom, true), new Door(mainHall, court, true),
      new Door(office, hiddenRoom, false), new Door(court, gallery, true), new Door(court, monitoringRoom, false))
  
  
  entrace.addItem(new Laptop)   
  office.addItem(new Wig("wig", "I can use this as a disguise, if the guards are looking for me.", true))
  hiddenRoom.addItem(new Item("files", "Taking these set of an alarm! I need to get out!!", false))
  
  private def itemRelations(locked: Door) = {
  
    val key = new Key("key", "It's broken! Better get some glue!", false, locked)
    restroom.addItem(key)
    gallery.addItem(new Glue("glue", "What couldn't one do with some glue!", true, key))
  }
  itemRelations(doorList(4))
 
  /** The character that the player controls in the game. */
  val player = new Player(entrace)

  /** The number of turns that have passed since the start of the game. */
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 250


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = {
    this.player.location == entrace && this.player.Inventory.contains("files")
  }

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */ 
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || this.isLosted
  
  //Vladimirs hammer
  def isLosted = Vladimir.gotYou

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = Storyline.alkuTeksti
    
  /** Returns a message that is to be displayed to the player at the end of the game. The message 
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Tremendous! You did it! America is saved!"
    else if (this.turnCount == this.timeLimit || this.isLosted)
      "Oh no! Vladimir took you down!"
    else  // game over due to player quitting
      "God bless us all now!" 
  }

  
  /** Plays a turn by executing the given in-game command, such as "go west". Returns a textual 
    * report of what happened, or an error message if the command was unknown. In the latter 
    * case, no turns elapse. */
  def playTurn(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.execute(this.player)
    if (outcomeReport.isDefined) { 
      this.turnCount += 1 
    }
    outcomeReport.getOrElse("Unknown command: \"" + command + "\".")
  }
  def playIntro(command: String) = {
    val action = new Action(command)
    val outcomeReport = action.introCommand()
    outcomeReport
  }
  
}

