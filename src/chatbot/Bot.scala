package chatbot

import java.io._
import scala.util.Random

class Bot {

  private var userInput = ""
  private var previousUserInput = ""
  private var secondPreviousUserInput = ""
  private var botResponse = ""
  private var previousBotResponse = ""
  private val generator = new Random
  private var allResponses = scala.collection.mutable.Buffer[String]()
  private var lastKeyWord = ""
  private val transposeList = scala.collection.mutable.Map[String, String](" MYSELF " -> " YOURSELF_ ")
  private var correctSubject = false
  private var contextKeyWord = false
  private val fillerList = scala.collection.mutable.Buffer[String]("REALLY")
  private var noFillerUserInput = ""
  transposeList += (" YOURSELF " -> " MYSELF_ ",
    " YOU WEREN'T " -> " I_ WASN'T_ ",
    " YOU AREN'T " -> " I_ AM_ NOT_ ",
    " YOU ARE " -> " I_ AM_ ",
    " YOU WERE " -> " I_ WAS_ ",
    " YOU'RE " -> " I'M_ ",
    " YOU'VE " -> " I'VE_ ",
    " I'VE " -> " YOU'VE_ ",
    " I WASN'T " -> " YOU_ WEREN'T_ ",
    " I WAS NOT " -> " YOU_ WEREN'T_ ",
    " I AM " -> " YOU_ ARE_ ",
    " I'M " -> " YOU'RE_ ",
    " I'D " -> " YOU'D_ ",
    " MINE " -> " YOURS_ ",
    " MY " -> " YOUR_ ",
    " AM " -> " ARE_ ",
    " I " -> " YOU_ ",
    " ME " -> " YOU_ ",
    " YOU " -> " I_ ")

  fillerList += ("ONLY", "JUST", "VERY MUCH", "ALSO", "KIND OF", "DEFINITELY")

  // Botin "pää"funktio. Etsii databasesta sisältääkö käyttäjän antama input avainsanaa, 
  // ja jos sisältää, valitsee siihen liittyvän sopivan vastauksen. Pitää myös kirjaa
  // aiemmista vastauksista.
  def findResponse: String = {

    handleEmptyAndRepetitionInput

    val database = this.getClass().getClassLoader().getResourceAsStream("database.txt")
    val reader = new InputStreamReader(database, "UTF-8")
    val lineReader = new BufferedReader(reader)

    val responses = scala.collection.mutable.Buffer[String]()

    try {
      var currentLine = lineReader.readLine()
      var keyWord = ""
      while (currentLine != null) {
        if (currentLine.head == 'K') {
          if (noFillerUserInput.contains(removeUnderscores(currentLine).drop(1)) && removeUnderscores(currentLine).drop(1).length > keyWord.length && excludeMidWordKeyWord(currentLine)) {
            if (underscoreConditions(currentLine) || !currentLine.contains("_")) {
              responses.clear()
              keyWord = removeUnderscores(currentLine).drop(1)
              lastKeyWord = removeUnderscores(currentLine).drop(1)
              while (currentLine.head != 'R') {
                // Tarkistaa onko keyword kontekstin vaativa, ja onko oikea aihe vielä löytynyt botin vastauksista.
                if (currentLine.head == 'C' && correctSubject == false) {
                  contextKeyWord = true
                  // Jos botin parissa viime vastauksessa on avainsanaan liittyvä konteksti, merkataan että
                  // konteksti on oikea.
                  if ((previousBotResponse.contains(currentLine.drop(1)) || botResponse.contains(currentLine.drop(1)))) {
                    correctSubject = true
                  }
                }
                currentLine = lineReader.readLine()
              }
              while (currentLine.head == 'R') {
                responses += currentLine.drop(1)
                currentLine = lineReader.readLine()
              }
              var number = generator.nextInt(responses.size)

              // Tarkistetaan, että annettavaa vastausta ei ole annettu vähään aikaan ellei kaikki
              // mahdolliset vastausvaihtoehot ole jo annettu.
              if (responses.size > 1) {
                while (allResponses.contains(responses(number)) && !responses.forall(x => allResponses.contains(x))) {
                  val lastNumber = number
                  number = generator.nextInt(responses.size)
                  while (number == lastNumber) {
                    number = generator.nextInt(responses.size)
                  }
                }
              }
              previousBotResponse = botResponse
              botResponse = responses(number)
            }
          }
        }
        currentLine = lineReader.readLine()
      }
    } finally {
      lineReader.close()
      reader.close()
    }

    // Jos käyttäjä antoi kontekstin vaativan inputin ilman, että siitä puhuttiin, etsitään sopiva vastaus
    // " WRONG SUBJECT ":lle.
    if (contextKeyWord && !correctSubject) {
      updateUserInput(" WRONG SUBJECT ")
      contextKeyWord = false
      findResponse
    }

    // Resetoi context tilanteen
    contextKeyWord = false
    correctSubject = false

    // Jos ei löydy vastauksia, katsotaan sisältääkö pari aiempaa inputtia nykyisen inputin.
    // Jos sisältää, vaihetaan input REPETITION:ksi ja etsitään sillä sopiva vastaus. Ainoastaan
    // muuten laitetaan BOT DOESN'T UNDERSTAND. Tämä sen takia, että käyttäjä on saattanut aiemmin sanoa
    // esimerkiksi "well gagga" ja sanoi nyt "gagga". Nyt bot osaa tunnistaa toiston.
    // Tässä on riskinsä, mutta se on tietoinen valinta, ja tämä tilanne syntyy vain silloin, jos
    // botilla ei ole muuta vastausta.
    if (responses.size == 0) {
      if ((secondPreviousUserInput.contains(userInput) || previousUserInput.contains(userInput)) && userInput != " BOT DOESN'T UNDERSTAND ") {
        updateUserInput(" REPETITION ")
      } else {
        updateUserInput(" BOT DOESN'T UNDERSTAND ")
      }
      findResponse
    }

    // Pitää kirjaa vain 30:stä viimeisestä vastauksesta
    if (allResponses.size == 30) {
      allResponses = allResponses.drop(1)
    }
    allResponses += botResponse

    if (botResponse.contains("*")) {
      fillBotResponse
    }

    formatResponse
  }

  // Päivittää uuden käyttäjän antaman inputin ja samalla päivittää aiemmat inputit.
  def updateUserInput(line: String) = {
    secondPreviousUserInput = previousUserInput
    previousUserInput = userInput
    userInput = line
    processInput
  }

  // Jos botin antama vastaus sisältää * merkin, se pitää täydentää käyttäjän antamalla lauseen osalla.
  private def fillBotResponse = {
    val responseArray = userInput.split(lastKeyWord)
    val botResponseArray = botResponse.split('*')
    if (botResponseArray.size > 1) {
      botResponse = botResponseArray(0).trim + " " + transpose(responseArray(1)) + botResponseArray(1)
    } else {
      botResponse = botResponseArray(0).trim + " " + transpose(responseArray(1)) + "."
    }
  }

  // Vaihtaa keskenään tiettyjä sanoja ja fraaseja, kuten "you" -> "I".
  // Laittaa vaihdetun sanan perään alaviivan, jotta metodi ei ala uudestaan muuttamaan jo 
  // muutettuja sanoja. Lopuksi poistaa alaviivat.
  def transpose(stringToTranspose: String) = {
    var result = stringToTranspose
    // Laittaa avaimet suurimmasta pienimpään, jotta suurimmat otetaan huomioon ekana.
    val keys = transposeList.keys.toList.sortBy(_.length).reverse
    if (keys.exists(x => result.contains(x))) {
      for (key <- keys) {
        if (result.contains(key)) {
          result = result.replace(key, transposeList(key))
        }
      }
    }
    removeUnderscores(result).trim
  }

  // Seuraa onko käyttäjän antama input tyhjä tai sama kuin edellisellä tai sitä edellisellä kerralla.
  // Jos on niin vaihtaa inputin repetitioniksi, jolloin bot osaa antaa oikeanlaisen vastauksen.
  private def handleEmptyAndRepetitionInput = {
    if (userInput.split(" ").isEmpty) {
      updateUserInput(" NULL INPUT ")
    } else if (noFillerUserInput == noFillerWords(previousUserInput) && userInput != " BOT DOESN'T UNDERSTAND ") {
      updateUserInput(" REPETITION ")
    } else if (noFillerUserInput == noFillerWords(secondPreviousUserInput) && userInput != " BOT DOESN'T UNDERSTAND ") {
      updateUserInput(" REPETITION ")
    } else if ((userInput == " BOT DOESN'T UNDERSTAND " || userInput == " BOT DOESN'T UNDERSTAND REPETITION ") &&
      (previousUserInput == " BOT DOESN'T UNDERSTAND " || previousUserInput == " BOT DOESN'T UNDERSTAND REPETITION ") &&
      (secondPreviousUserInput == " BOT DOESN'T UNDERSTAND " || secondPreviousUserInput == " BOT DOESN'T UNDERSTAND REPETITION ")) {
      updateUserInput(" BOT DOESN'T UNDERSTAND REPETITION ")
    }
  }

  // Formatoi botin lopullisen vastauksen oikeaan muotoon. Toisin sanoen laittaa isot alkukirjaimet lauseille ja "I":sanoille.
  private def formatResponse = {
    var response = ""
    for (index <- 0 until botResponse.size) {
      if (index >= 2) {
        if (botResponse(index - 2) == '.' || botResponse(index - 2) == '!' || botResponse(index - 2) == '?') {
          response += botResponse(index).toString.toUpperCase
        } else if (index < botResponse.size - 2 && botResponse(index).toString.toLowerCase == "i" &&
          botResponse(index - 1) == ' ' && (botResponse(index + 1) == ' ' || botResponse(index + 1) == ''')) {
          response += botResponse(index).toString.toUpperCase
        } else {
          response += botResponse(index).toString.toLowerCase
        }
      } else {
        response += botResponse(index).toString.toLowerCase
      }
    }
    response.capitalize
  }

  // Tarkastaa, että käyttäjän inputissa avainsanan/-fraasin molemmilla puolilla on välilyönti (ellei ole viiminen
  // tai ensimmäinen sana).
  // Näin estetään tilanne, jossa findResponse ottaa avainsanaksi sanan/kirjaimet, jotka ovat keskellä/osana
  // toista sanaa. False jos on osana toista sanaa, true muuten.
  def excludeMidWordKeyWord(databaseLine: String): Boolean = {
    val databaseIndex = noFillerUserInput.indexOf(removeUnderscores(databaseLine).drop(1))
    if (databaseIndex == 0) {
      noFillerUserInput(removeUnderscores(databaseLine).drop(1).length) == ' '
    } else {
      (noFillerUserInput(removeUnderscores(databaseLine).drop(1).length + databaseIndex) == ' ') && (noFillerUserInput(databaseIndex - 1) == ' ')
    }
  }

  /* 
   * Databasessa osassa avainsanoista on alaviivoja. Tämä metodi tarkastaa onko, ja jos on niin ensin 
     tarkastetaan, missä kohtaa. Tämän jälkeen metodi tarkastaa vaatimukset sille, onko avainsana
     oikeassa kohdassa, jotta sitä voidaan käyttää.
   * 1. Avainsana on muotoa _sana_. Tämä tarkoittaa että avainsana on ainoa sana inputissa.
   * 2. Muotoa _sana. Tällöin avainsanan kuuluu olla ennen inputin puoltaväliä avainsanan pituutta lukuunottamatta
   * 3. Muotoa sana_. Päinvastainen tilanne kakkoskohdalle.
 */
  def underscoreConditions(databaseLine: String): Boolean = {
    if (databaseLine.contains("_")) {
      val splittedInput = noFillerUserInput.split(removeUnderscores(databaseLine).drop(1))
      ((databaseLine.drop(1).head == '_' && databaseLine.last == '_' && noFillerUserInput == " " + removeUnderscores(databaseLine).drop(1) + " ") ||
        (databaseLine.last != '_' && databaseLine.drop(1).head == '_' && splittedInput(splittedInput.size - 1).length >= ((noFillerUserInput.length / 2) - removeUnderscores(databaseLine).drop(1).length) && noFillerUserInput != " " + removeUnderscores(databaseLine).drop(1) + " ") ||
        (databaseLine.drop(1).head != '_' && noFillerUserInput != " " + removeUnderscores(databaseLine).drop(1) + " " && databaseLine.takeRight(1) == "_" && splittedInput(0).length > ((noFillerUserInput.length / 2 - removeUnderscores(databaseLine).drop(1).length))))
    } else false
  }

  // Laittaa käyttäjän inputin aina samaan muotoon. Poistaa pisteet ja huutomerkit ja laittaa muotoon " INPUT ".
  def processInput = {
    userInput = removeComma(userInput)
    if (userInput.trim.takeRight(1) == "." || userInput.trim.takeRight(1) == "!" || userInput.trim.takeRight(1) == "?") {
      userInput = " " + userInput.trim.dropRight(1).trim.toUpperCase() + " "
    } else {
      userInput = " " + userInput.trim.toUpperCase() + " "
    }
    noFillerUserInput = noFillerWords(userInput)
  }

  // Poistaa annetusta stringistä alaviivat.
  private def removeUnderscores(input: String) = {
    var noUnderscores = input
    if (noUnderscores.contains("_")) {
      val inputArray = noUnderscores.split("_")
      noUnderscores = ""
      for (item <- inputArray) {
        noUnderscores += item
      }
    }
    noUnderscores
  }
  
  // Poistaa annetusta stringistä pilkut.
  private def removeComma(input: String) = {
    var noComma = input
    if (noComma.contains(",")) {
      val inputArray = noComma.split(",")
      noComma = ""
      for (item <- inputArray) {
        noComma += item
      }
    }
    noComma
  }

  // Poistaa annetusta inputista fillerList:n sisältämät "täyte"sanat ja parantaa avainsanan hakemista.
  private def noFillerWords(input: String) = {
    var result = input
    // fillerList:n sisältämät sanat ovat täytesanoja vain, jos inputissa on yli 2 sanaa
    if (input.trim.split(" ").size > 2) {
      for (word <- fillerList) {
        if (input.contains(" " + word + " ")) {
          val inputArray = input.split(" " + word)
          result = ""
          for (item <- inputArray) {
            result += item
          }
        }
      }
    }
    result
  }

  // Jos input alkaa sanalla well, tämä metodi poistaa "well" sanan.
  private def noWellStart(input: String) = {
    var result = input
    // Poistetaan well vain, jos inputissa on vähintään 2 sanaa
    if (input.trim.split(" ").size >= 2) {
      if (input.split(" ")(0) == "well") {
        val inputArray = input.split(" " + "well")
        result = ""
        for (item <- inputArray) {
          result += item
        }
      }
    }
    result
  }

}