package at.tugraz.ist.qs2024

import at.tugraz.ist.qs2024.actorsystem.SimulatedActor
import at.tugraz.ist.qs2024.messageboard.{UnknownMessageException, UserMessage}
import at.tugraz.ist.qs2024.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2024.messageboard.clientmessages.RemoveLikeOrDislike.Type
import at.tugraz.ist.qs2024.messageboard.clientmessages._
import org.junit.runner.RunWith
import org.scalacheck.Prop.{False, classify, forAll}
import org.scalacheck.{Gen, Properties}

@RunWith(classOf[ScalaCheckJUnitPropertiesRunner])
class MessageBoardProperties extends Properties("MessageBoardProperties") {

  // Generator for valid messages
  val validMessageGen: Gen[String] = Gen.asciiPrintableStr.map(s =>
    if (s.length <= MAX_MESSAGE_LENGTH) s else s.substring(0, MAX_MESSAGE_LENGTH)
  )

  property("message length: Publish + Ack [R1]") = forAll { (author: String, message: String) =>
    // arrange-  initialize the message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // act - send and receive the messages
    worker.tell(new Publish(new UserMessage(author, message), sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reply = sut.getClient.receivedMessages.remove()

    worker.tell(new FinishCommunication(sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove.asInstanceOf[FinishAck]

    // assert - define your property and check against it
    // The following classify is optional, it prints stats on the generated values.
    // But the check inside is required.
    classify(message.length <= MAX_MESSAGE_LENGTH, "valid message length", "invalid message length") {
      // if operationAck is received, the message length should be smaller or equal to 10
      reply.isInstanceOf[OperationAck] == message.length <= MAX_MESSAGE_LENGTH
    }
  }
  // TODO: add another properties for requirements R1-R17

  property("R2") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop
      worker.tell(new Publish(new UserMessage(author, messages.head), sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_publish = sut.getClient.receivedMessages.remove()

      for (i <- 1 to 5) {
        worker.tell(new Report("Reporter", sut.getCommId, author))
        while (sut.getClient.receivedMessages.isEmpty)
          sut.getSystem.runFor(1)
        val reply_report = sut.getClient.receivedMessages.remove()
        // if operationAck is received, the message length should be smaller or equal to 10
        reply_report.isInstanceOf[OperationAck]
      }

      // Test Publish after blocked
      worker.tell(new Publish(new UserMessage(author, messages.head), sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_publish_banned = sut.getClient.receivedMessages.remove()
      reply_publish_banned.isInstanceOf[UserBanned]

      // Test Like after blocked
      worker.tell(new Like(author, sut.getCommId, 1))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_like_banned = sut.getClient.receivedMessages.remove()
      reply_like_banned.isInstanceOf[UserBanned]

      // Test Dislike after blocked
      worker.tell(new Dislike(author, sut.getCommId, 1))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_dislike_banned = sut.getClient.receivedMessages.remove()
      reply_dislike_banned.isInstanceOf[UserBanned]

      // Test RemoveLikeOrDislike after blocked
      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, 1, RemoveLikeOrDislike.Type.LIKE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_removeLikeDislike_banned = sut.getClient.receivedMessages.remove()
      reply_removeLikeDislike_banned.isInstanceOf[UserBanned]

      // Test Reaction after blocked
      worker.tell(new Reaction(author, sut.getCommId, 1, Reaction.Emoji.LAUGHING))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_banned = sut.getClient.receivedMessages.remove()
      reply_reaction_banned.isInstanceOf[UserBanned]

      // Test Edit after blocked
      worker.tell(new Edit(1, author, "new message", sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_edit_banned = sut.getClient.receivedMessages.remove()
      reply_edit_banned.isInstanceOf[UserBanned]

      // Test Delete after blocked
      worker.tell(new Delete(1, author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_delete_banned = sut.getClient.receivedMessages.remove()
      reply_delete_banned.isInstanceOf[UserBanned]

      // Test Delete after blocked
      worker.tell(new Report("Reporter", sut.getCommId, author))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_report_banned = sut.getClient.receivedMessages.remove()
      reply_report_banned.isInstanceOf[UserBanned]

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      // here would be a check
      classify(messages.head.length <= MAX_MESSAGE_LENGTH, "valid message length", "invalid message length") {
        // if operationAck is received, the message length should be smaller or equal to 10
        reply_publish.isInstanceOf[OperationAck] == messages.head.length <= MAX_MESSAGE_LENGTH
      }
    }

  /* property("example property with generators") =
     forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
       val sut = new SUTMessageBoard
       sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
       while (sut.getClient.receivedMessages.isEmpty)
         sut.getSystem.runFor(1)
       val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
       val worker: SimulatedActor = initAck.worker

       // here would be a worker.tell, e.g. in a loop

       worker.tell(new FinishCommunication(sut.getCommId))
       while (sut.getClient.receivedMessages.isEmpty)
         sut.getSystem.runFor(1)
       sut.getClient.receivedMessages.remove()

       // here would be a check
       false
     }*/

  val genAuthorAndMessage: Gen[(String, String)] = for {
    author <- validMessageGen
    text <- validMessageGen suchThat (msg => msg != author)
  } yield (author, text)

  property("[R3] Same Message published") =
    forAll(genAuthorAndMessage) { case (author, text) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      val message = new UserMessage(author, text)

      // Publishing same message which fails
      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val firstResponse = sut.getClient.receivedMessages.remove()

      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val secondResponse = sut.getClient.receivedMessages.remove()

      firstResponse.isInstanceOf[OperationAck] && secondResponse.isInstanceOf[OperationFailed]
    }

  property("[R3] Different Message published") =
    forAll(genAuthorAndMessage) { case (author, text) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      val message1 = new UserMessage(author, text)
      val message2 = new UserMessage(text, author)

      // Publishing 2 different messages which succeeds
      worker.tell(new Publish(message1, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val firstResponse = sut.getClient.receivedMessages.remove()

      worker.tell(new Publish(message2, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val secondResponse = sut.getClient.receivedMessages.remove()

      firstResponse.isInstanceOf[OperationAck] && secondResponse.isInstanceOf[OperationAck]
    }

  property("R4") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      val userMessage = new UserMessage(author, messages.head)
      // here would be a worker.tell, e.g. in a loop
      worker.tell(new Publish(userMessage, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_publish = sut.getClient.receivedMessages.remove()


      // LIKE

      worker.tell(new Like(author, sut.getCommId, userMessage.getMessageId + 1))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_like_failed = sut.getClient.receivedMessages.remove()


      // DISLIKE
      worker.tell(new Dislike(author, sut.getCommId, userMessage.getMessageId + 1))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_dislike_failed = sut.getClient.receivedMessages.remove()


      // Remove LIKE
      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, userMessage.getMessageId + 1, Type.LIKE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_remove_like_failed = sut.getClient.receivedMessages.remove()


      // EDIT
      worker.tell(new Edit(userMessage.getMessageId + 1, author, "new message", sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_edit = sut.getClient.receivedMessages.remove()


      // REACTION
      worker.tell(new Reaction(author, sut.getCommId, userMessage.getMessageId + 1, Reaction.Emoji.LAUGHING))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_react = sut.getClient.receivedMessages.remove()

      // DELETE
      worker.tell(new Delete(userMessage.getMessageId + 1, author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_delete = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_like_failed.isInstanceOf[OperationFailed] &&
        reply_dislike_failed.isInstanceOf[OperationFailed] &&
        reply_remove_like_failed.isInstanceOf[OperationFailed] &&
        reply_edit.isInstanceOf[OperationFailed] &&
        reply_react.isInstanceOf[OperationFailed] &&
        reply_delete.isInstanceOf[OperationFailed]
    }

  // Property-based test for R5 requirement
  property("[R5] Like and Dislike Management") = forAll(genAuthorAndMessage) { case (author, text) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // Publish a message to test against
    val message = new UserMessage(author, text)
    worker.tell(new Publish(message, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val publishResponse = sut.getClient.receivedMessages.remove().asInstanceOf[OperationAck]


    // Add a like to the message
    worker.tell(new Like(author, sut.getCommId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]

    // Attempt to add another like fails
    worker.tell(new Like(author, sut.getCommId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val secondLikeResponse = sut.getClient.receivedMessages.remove()
    val likeDuplicateFailed = secondLikeResponse.isInstanceOf[OperationFailed]

    // Add a dislike reverting like
    worker.tell(new Dislike(author, sut.getCommId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]

    // Attempt to add another dislike fails
    worker.tell(new Dislike(author, sut.getCommId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val secondDislikeResponse = sut.getClient.receivedMessages.remove()
    val dislikeDuplicateFailed = secondDislikeResponse.isInstanceOf[OperationFailed]

    publishResponse.isInstanceOf[OperationAck] &&
      likeResponse.isInstanceOf[ReactionResponse] &&
      likeDuplicateFailed &&
      dislikeResponse.isInstanceOf[ReactionResponse] &&
      dislikeDuplicateFailed
  }

  property("R6") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      val userMessage1 = new UserMessage(author, messages.head)
      // here would be a worker.tell, e.g. in a loop
      worker.tell(new Publish(userMessage1, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_publish1 = sut.getClient.receivedMessages.remove()

      val userMessage2 = new UserMessage(author, "Joulian")
      // here would be a worker.tell, e.g. in a loop
      worker.tell(new Publish(userMessage2, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_publish2 = sut.getClient.receivedMessages.remove()

      worker.tell(new RetrieveMessages(author, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val retrieveMessages = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_publish1.isInstanceOf[OperationAck] &&
        reply_publish2.isInstanceOf[OperationAck] &&
        retrieveMessages.isInstanceOf[FoundMessages]
    }

  val genAuthorAndMessages: Gen[(String, List[String])] = for {
    author <- validMessageGen
    textList <- Gen.listOf(validMessageGen) suchThat (_.nonEmpty)
  } yield (author, textList)

  property("[R7] Search Messages by Text") = forAll(genAuthorAndMessages) { case (author, textList) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker

    // Publish all generated messages for this author
    val publishedMessages = textList.map { text =>
      val message = new UserMessage(author, text)
      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()
      message
    }

    // Search for messages containing specific text
    val searchText = textList.head.take(3) // the message we are searching
    worker.tell(new SearchMessages(searchText, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val foundResponse = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

    var allMatch = true
    // Check that all found messages contain the search text in their content or author
    for (i <- 1 to foundResponse.messages.size()) {
      val message = foundResponse.messages.get(i - 1)
      val containsText = message.getAuthor.toLowerCase.contains(searchText.toLowerCase) ||
        message.getMessage.toLowerCase.contains(searchText.toLowerCase)
      if (!containsText) {
        allMatch = false
      }
    }

    allMatch && !foundResponse.messages.isEmpty
  }

  property("R8") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop
      worker.tell(new Report(author, sut.getCommId, "Joulian"))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_report = sut.getClient.receivedMessages.remove()

      worker.tell(new Report(author, sut.getCommId, "Joulian"))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_report2 = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_report.isInstanceOf[OperationAck] &&
        reply_report2.isInstanceOf[OperationFailed]
    }

  property("[R9] Confirm Successful Requests with OperationAck or ReactionResponse") = forAll(genAuthorAndMessage) { case (author, messageContent) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker
    val commId = sut.getCommId

    var allSuccessful = true

    // Publish a message
    val message = new UserMessage(author, messageContent)
    worker.tell(new Publish(message, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val publishResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]
    allSuccessful &= publishResponse

    // Like the message
    worker.tell(new Like(author, commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeResponse = sut.getClient.receivedMessages.remove().isInstanceOf[ReactionResponse]
    allSuccessful &= likeResponse

    // Dislike the message
    worker.tell(new Dislike(author, commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeResponse = sut.getClient.receivedMessages.remove().isInstanceOf[ReactionResponse]
    allSuccessful &= dislikeResponse

    // Edit the message
    worker.tell(new Edit(message.getMessageId, author, "Updated", commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val editResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]
    allSuccessful &= editResponse

    // Delete the message
    worker.tell(new Delete(message.getMessageId, author, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]
    allSuccessful &= deleteResponse

    // Add a report
    worker.tell(new Report("Reporter", commId, author))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val reportResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]
    allSuccessful &= reportResponse

    allSuccessful
  }

  property("[R11] Points Counter for Like and Dislike") = forAll(genAuthorAndMessage) { case (author, messageContent) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker
    val commId = sut.getCommId

    // Publish a message
    val message = new UserMessage(author, messageContent)
    worker.tell(new Publish(message, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    // Add a like to increase points by one
    worker.tell(new Like("Saimir", commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]
    val pointsAfterLike = likeResponse.points == 1

    // Add a dislike to decrease points back to zero
    worker.tell(new Dislike("Not Saimir", commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]
    val pointsAfterDislike = dislikeResponse.points == 0

    pointsAfterLike && pointsAfterDislike
  }

  property("R12") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop
      val message = new UserMessage(author, messages.head)
      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_message = sut.getClient.receivedMessages.remove()


      //Add LIKE
      worker.tell(new Like(author, sut.getCommId, message.getMessageId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_add_like = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]

      //Remove LIKE
      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, message.getMessageId, Type.LIKE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_remove_like = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]

      //Add DISLIKE
      worker.tell(new Dislike(author, sut.getCommId, message.getMessageId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_add_dislike = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]

      //Remove DISLIKE
      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, message.getMessageId, Type.DISLIKE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_remove_dislike = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]


      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_message.isInstanceOf[OperationAck] &&
        reply_add_like.isInstanceOf[ReactionResponse] && reply_add_like.points == 1 &&
        reply_remove_like.isInstanceOf[ReactionResponse] && reply_remove_like.points == 0 &&
        reply_add_dislike.isInstanceOf[ReactionResponse] && reply_add_dislike.points == -1  &&
        reply_remove_dislike.isInstanceOf[ReactionResponse] && reply_remove_dislike.points == 0

    }

  property("[R13] Overwriting Like and Dislike") = forAll(genAuthorAndMessage) { case (author, messageContent) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker
    val commId = sut.getCommId

    // Publish a message
    val message = new UserMessage(author, messageContent)
    worker.tell(new Publish(message, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    // Add a like to increase points by one
    worker.tell(new Like("Saimir", commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val likeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]
    val pointsAfterLike = likeResponse.points == 1

    // Add a dislike to overwrite the like
    worker.tell(new Dislike("Saimir", commId, message.getMessageId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val dislikeResponse = sut.getClient.receivedMessages.remove().asInstanceOf[ReactionResponse]
    val pointsAfterDislike = dislikeResponse.points == -1

    pointsAfterLike && pointsAfterDislike
  }

  property("R14") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop
      val message = new UserMessage(author, messages.head)
      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_message = sut.getClient.receivedMessages.remove()

      // LAUGHING SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.LAUGHING))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_laughing = sut.getClient.receivedMessages.remove()

      // FROWN SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.FROWN))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_frown = sut.getClient.receivedMessages.remove()

      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.FROWN))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_frown_failed = sut.getClient.receivedMessages.remove()

      // COOL SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.COOL))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_cool = sut.getClient.receivedMessages.remove()

      // CRYING SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.CRYING))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_crying = sut.getClient.receivedMessages.remove()

      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.CRYING))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_crying_failed = sut.getClient.receivedMessages.remove()

      // HORROR SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.HORROR))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_horror = sut.getClient.receivedMessages.remove()

      // SKEPTICAL SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.SKEPTICAL))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_skeptical = sut.getClient.receivedMessages.remove()

      // SMILEY SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.SMILEY))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_smiley = sut.getClient.receivedMessages.remove()

      // SURPRISE SUCCESS
      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.SURPRISE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_surprise = sut.getClient.receivedMessages.remove()

      worker.tell(new Reaction("Joulian", sut.getCommId,
        message.getMessageId, Reaction.Emoji.SURPRISE))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_reaction_surprise_failed = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_message.isInstanceOf[OperationAck] &&
        reply_reaction_laughing.isInstanceOf[ReactionResponse] &&
        reply_reaction_frown.isInstanceOf[ReactionResponse] &&
        reply_reaction_frown_failed.isInstanceOf[OperationFailed] &&
        reply_reaction_cool.isInstanceOf[ReactionResponse] &&
        reply_reaction_crying.isInstanceOf[ReactionResponse] &&
        reply_reaction_crying_failed.isInstanceOf[OperationFailed] &&
        reply_reaction_horror.isInstanceOf[ReactionResponse] &&
        reply_reaction_skeptical.isInstanceOf[ReactionResponse] &&
        reply_reaction_smiley.isInstanceOf[ReactionResponse] &&
        reply_reaction_surprise.isInstanceOf[ReactionResponse] &&
        reply_reaction_surprise_failed.isInstanceOf[OperationFailed]
    }

  val genAuthorAndTwoMessages: Gen[(String, String, String)] = for {
    author <- validMessageGen suchThat (_.nonEmpty)
    firstMessage <- validMessageGen suchThat (_.nonEmpty)
    secondMessage <- validMessageGen suchThat (msg => msg.nonEmpty && msg != firstMessage)
  } yield (author, firstMessage, secondMessage)

  property("[R15] Edit to Duplicate Existing Message Fails") = forAll(genAuthorAndTwoMessages) { case (author, message1, message2) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker
    val commId = sut.getCommId

    // Publish the first message
    val firstMessage = new UserMessage(author, message1)
    worker.tell(new Publish(firstMessage, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val firstPublishResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    // Publish the second message
    val secondMessage = new UserMessage(author, message2)
    worker.tell(new Publish(secondMessage, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val secondPublishResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    // Edit message with same content
    worker.tell(new Edit(secondMessage.getMessageId, author, message1, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val editResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationFailed]

    firstPublishResponse && secondPublishResponse && editResponse
  }

  property("R16") =
    forAll(Gen.alphaStr, Gen.nonEmptyListOf(validMessageGen)) { (author: String, messages: List[String]) =>
      val sut = new SUTMessageBoard
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      // here would be a worker.tell, e.g. in a loop
      val message = new UserMessage(author, messages.head)
      worker.tell(new Publish(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_message = sut.getClient.receivedMessages.remove()

      worker.tell(new Edit(message.getMessageId, "Joulian", "new message",
        sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val reply_edit = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      reply_message.isInstanceOf[OperationAck] &&
        reply_edit.isInstanceOf[OperationFailed]
    }

  val genAuthorOtherUserAndMessage: Gen[(String, String, String)] = for {
    author <- validMessageGen suchThat (_.nonEmpty)
    otherUser <- validMessageGen suchThat (user => user.nonEmpty && user != author)
    message <- validMessageGen suchThat (_.nonEmpty)
  } yield (author, otherUser, message)

  property("[R17] Delete Message Only by Author") = forAll(genAuthorOtherUserAndMessage) { case (author, otherUser, messageContent) =>
    // Initialize the simulated system and the SUT message board
    val sut = new SUTMessageBoard
    sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
    val worker: SimulatedActor = initAck.worker
    val commId = sut.getCommId

    // Publish a message with author
    val message = new UserMessage(author, messageContent)
    worker.tell(new Publish(message, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val publishResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    // Try to delete the message using another user
    worker.tell(new Delete(message.getMessageId, otherUser, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteFailResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationFailed]

    // Try to delete the message using the correct author
    worker.tell(new Delete(message.getMessageId, author, commId))
    while (sut.getClient.receivedMessages.isEmpty)
      sut.getSystem.runFor(1)
    val deleteSuccessResponse = sut.getClient.receivedMessages.remove().isInstanceOf[OperationAck]

    publishResponse && deleteSuccessResponse && deleteFailResponse
  }
}
