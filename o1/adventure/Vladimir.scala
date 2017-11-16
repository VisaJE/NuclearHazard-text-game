package o1.adventure

import scala.util.Random
/* Guards the building. Appears randomly into areas and communicates to the player. WILL ATTACK IF SUSPECTS
 * SOMETHING!!
 */
object Vladimir {
  private val random = new Random()
  private def number = random.nextInt(100)
  private var madness = false
  private var location: Area = Adventure.office
  private var distressIndex = 0
  private var isHunting = false
  
  def hide() = {
   madness = false
   distressIndex = 0
 }
 def isComing = madness
 def gotYou = distressIndex >= 3
 
 def alarmVladimir() = {
   Vladimir.location = Adventure.player.location
 }
 
 def callVladimir(): Boolean = {
   if (!madness) {
     if (number % 3 == 0) location = Adventure.player.location
     else location = Adventure.monitoringRoom
     location == Adventure.player.location
     }
   else {
    distressIndex = 2
    true  
    }
   }
 def provoke(andAlarm: Boolean) = {
   madness = true
   distressIndex += 1
   if (andAlarm) location = Adventure.player.location
 }
 
 
 
 def filesTaken() = {
   isHunting = true
 }
 
 def vladiChat(answer: Option[Char]): String = {
   if (!Adventure.isComplete) {
   if (answer == None) {
    if (!madness) {
     if (!isHunting) "A wild guard appears!\nVladimir: 'Privet, tovarish!'\nAnswer A: Privet!\nAnswer B: What?" 
     else "Vladimir: Kuda on poshel!?\nAnswer A: What?\nAnswer B: He went that way!"
     }
    else {
      distressIndex = 3
      "Vladimir: D'yavol'skiy!!"
    }
   }
   else if (answer == Some('a')) {
     if (!isHunting && !madness) {
     location = Adventure.monitoringRoom
     "Vladimir walks away."
     }
     else if(isHunting && !madness) {
       this.provoke(true)
       "Vladimir: It's him! Davai!"
     }
     else {
       this.provoke(true)
       "Vladimir: I got you now!"
     }
   }
   else if (answer == Some('b')) {
     if (!isHunting && !madness) {
       this.provoke(true)
       "Vladimir: A Spy!! Get him!"
     }
     else if (isHunting) {
       if (!madness) {    
         location = Adventure.monitoringRoom
         "Vladimir runs away."
       } 
       else "Vladimir: I got you now!"
     }
     else "Vladimir: Find him. He's got our nuclear launch codes!"
   }
   else "...Wtf"
 }
   else "Vladimir: !!!!"
 }
 def isHere = location == Adventure.player.location
}
