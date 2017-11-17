package o1.adventure

object Storyline {
  private var gamePoint = 0
  val alkuTeksti = {"Welcome, agent. America is in a dire need for a man like you. Our President has lost our nuclear launch codes, and it has come to my " +
  "knowledge, that the Russians might have them."
  }
  private val teksti2 = {
    "Your mission, should you choose to accept it, is to infiltrate the Winter Palace and get back what is rightfully ours. We suspect they have hidden the codes " +
    "somewhere safe and set up an alarm to to alert the guards when you take them." 
  }
  private val teksti3 = {
    "You will face guards, but we don't want this thing to blow up, so don't hurt anyone. Instead you need to blend in. We hope for you to find something to disguise " + 
    "yourself with, should they spot you."
  }
  private val teksti4 = {
    "So, good luck! And come back in one piece. . . if you get the codes!"
  }
  private var story = Vector[String](teksti2, teksti3, teksti4, "A day later in the Winter Palace\n")
  
  
  
  
  val introSize = story.size - 1
  def intro(command: String) = {
    
    if (command == "next") {
      gamePoint += 1
    }
    else if (command == "skip") {
      gamePoint = introSize + 1
    }
    story(gamePoint - 1)
  }
  def introText = "SOMEWHERE IN PENTAGON\nCommands: ok, skip"

  def isIntro = {
    gamePoint <= introSize
  }
  
   


}
