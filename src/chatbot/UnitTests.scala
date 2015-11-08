package chatbot

import org.junit.Test
import org.junit.Assert._

class UnitTests {
  
  val chatBot = new Bot
  
  chatBot.updateUserInput("you are really uncertain if i should hide")

  @Test def testTranspose() {
    assertEquals("doesn't work", "YOU WANT TO KNOW IF I AM COOL", chatBot.transpose(" I WANT TO KNOW IF YOU ARE COOL "))
  }
  
  @Test def testUnderscoreConditionsTrue() {
    assertEquals("doesn't work", true, chatBot.underscoreConditions("K_YOU ARE"))
  }
  
  @Test def testUnderscoreConditionsFalse() {
    assertEquals("doesn't work", false, chatBot.underscoreConditions("K_I SHOULD"))
  }
  
  @Test def excludeMidWordKeyWordFalse() {
    assertEquals("doesn't work", false, chatBot.excludeMidWordKeyWord("KHI"))
  }
  
  @Test def excludeMidWordKeyWordTrue() {
    assertEquals("doesn't work", true, chatBot.excludeMidWordKeyWord("KYOU"))
  }
  
}