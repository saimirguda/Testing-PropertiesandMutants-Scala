package at.tugraz.ist.qs2024

import at.tugraz.ist.qs2024.actorsystem.{Message, SimulatedActor}
import at.tugraz.ist.qs2024.messageboard.MessageStore.USER_BLOCKED_AT_COUNT
import at.tugraz.ist.qs2024.messageboard.UserMessage
import at.tugraz.ist.qs2024.messageboard.Worker.MAX_MESSAGE_LENGTH
import at.tugraz.ist.qs2024.messageboard.clientmessages._
import org.scalacheck.commands.Commands
import org.scalacheck.{Gen, Prop}

import scala.jdk.CollectionConverters._
import scala.util.Try

// Documentation: https://github.com/typelevel/scalacheck/blob/master/doc/UserGuide.md#stateful-testing
object MessageBoardSpecification extends Commands {
  override type State = ModelMessageBoard
  override type Sut = SUTMessageBoard

  override def canCreateNewSut(newState: State, initSuts: Iterable[State], runningSuts: Iterable[Sut]): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def newSut(state: State): Sut = new SUTMessageBoard

  override def destroySut(sut: Sut): Unit = ()

  override def initialPreCondition(state: State): Boolean = state.messages.isEmpty && state.reports.isEmpty

  override def genInitialState: Gen[State] = ModelMessageBoard(Nil, Nil, lastCommandSuccessful = false, userBanned = false)

  override def genCommand(state: State): Gen[Command] = Gen.oneOf(genPublish, genLike, genDislike, genReport, genReaction, genRetrieve, genSearch, genEdit, genRemoveLikeOrDislike, genDelete)

  val genAuthor: Gen[String] = Gen.oneOf("Alice", "Bob")
  val genReporter: Gen[String] = Gen.oneOf("Alice", "Bob", "Lena", "Lukas", "Simone", "Charles", "Gracie", "Patrick", "Laura", "Leon")
  val genMessage: Gen[String] = Gen.oneOf("msg_w_9ch", "msg_w_10ch", "msg_w_11ch_")
  val genEmoji: Gen[Reaction.Emoji] = Gen.oneOf(Reaction.Emoji.COOL, Reaction.Emoji.CRYING, Reaction.Emoji.LAUGHING, Reaction.Emoji.SMILEY, Reaction.Emoji.SURPRISE, Reaction.Emoji.SKEPTICAL)

  def genPublish: Gen[PublishCommand] = for {
    author <- genAuthor
    message <- genMessage
  } yield PublishCommand(author, message)

  case class PublishCommand(author: String, message: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      val userMessage = new UserMessage(author, message)
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Publish(userMessage, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val messageExists = state.messages.exists(m => m.author == author && m.message == message)
      if (!messageExists) {
        // create new message
        val newMessage = ModelUserMessage(
          author = author,
          message = message,
          likes = Nil,
          dislikes = Nil,
          reactions = collection.mutable.Map.empty,
          points = 0
        )

        // success and upload message
        state.copy(
          messages = newMessage :: state.messages,
          lastCommandSuccessful = true
        )
      } else {
        // fail if message exists
        state.copy(
          lastCommandSuccessful = false
        )
      }
    }

    override def preCondition(state: State): Boolean = {
      !state.messages.exists(m => m.author == author && m.message == message)
    }
    override def postCondition(state: State, result: Try[Result]): Prop = {
      if(result.isSuccess) {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      }
      else {
        Prop(false)
      }
    }

    override def toString: String = s"Publish($author, $message)"
  }

  def genReaction: Gen[ReactionCommand] = for {
    author <- genAuthor
    message <- genMessage
    rName <- genAuthor
    reactionType <- genEmoji
  } yield ReactionCommand(author, message, rName, reactionType)

  case class ReactionCommand(author: String, message: String, rName: String, reactionType: Reaction.Emoji) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker
      val userMessage = new UserMessage(author, message)

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == message)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))


      // Send a reaction to the message
      worker.tell(new Reaction(rName, sut.getCommId, messageID, reactionType)) // Assuming message ID is 1
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.map {
        case m if m.author == author && m.message == message =>
          val userReactions = m.reactions.getOrElse(rName, collection.mutable.Set())
          userReactions.add(reactionType)
          m.copy(reactions = m.reactions + (rName -> userReactions))
        case m => m
      }
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = {
      state.messages.exists(m => m.author == author && m.message == message) && !state.userBanned
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if(result.isSuccess)
      {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      }
      else {
        !state.lastCommandSuccessful
      }
    }

    override def toString: String = s"Reaction($author, $message, $rName, $reactionType)"
  }

  def genLike: Gen[LikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    likeName <- genAuthor
  } yield LikeCommand(author, message, likeName)

  case class LikeCommand(author: String, message: String, likeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker
      val userMessage = new UserMessage(author, message)

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == message)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))


      // Send a reaction to the message
      worker.tell(new Like(likeName, sut.getCommId, messageID)) // Assuming message ID is 1
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.map {
        case m if m.author == author && m.message == message && !m.likes.contains(likeName) =>
          m.copy(
            likes = likeName :: m.likes,
            dislikes = m.dislikes.filterNot(_ == likeName),
            points = m.points + 1
          )
        case m => m
      }
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = {
      state.messages.exists { m =>
        m.author == author && m.message == message && !m.likes.contains(likeName)
      }
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if(result.isSuccess)
      {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      }
      else {
        Prop(false)
      }
    }

    override def toString: String = s"Like($author, $message, $likeName)"
  }

  def genDislike: Gen[DislikeCommand] = for {
    author <- genAuthor
    message <- genMessage
    dislikeName <- genAuthor
  } yield DislikeCommand(author, message, dislikeName)

  case class DislikeCommand(author: String, message: String, dislikeName: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker
      val userMessage = new UserMessage(author, message)

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == message)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))

      // Send a reaction to the message
      worker.tell(new Dislike(dislikeName, sut.getCommId, messageID)) // Assuming message ID is 1
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.map {
        case m if m.author == author && m.message == message && !m.dislikes.contains(dislikeName) =>
          m.copy(
            dislikes = dislikeName :: m.dislikes,
            likes = m.likes.filterNot(_ == dislikeName),
            points = m.points - 1
          )
        case m => m
      }
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean =  {
      state.messages.exists { m =>
        m.author == author && m.message == message && !m.dislikes.contains(dislikeName)
      }
    }

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if(result.isSuccess)
      {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      }
      else {
        Prop(false)
      }
    }

    override def toString: String = s"Dislike($author, $message, $dislikeName)"
  }

  def genReport: Gen[ReportCommand] = for {
    reporter <- genReporter
    reported <- genAuthor
  } yield ReportCommand(reporter, reported)

  case class ReportCommand(reporter: String, reported: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker: SimulatedActor = initAck.worker

      worker.tell(new Report(reporter, sut.getCommId, reported))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty)
        sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      // R2 If a user has been reported at least USER BLOCKED AT COUNT (= 6) times.
      if (state.reports.count(r => r.reportedClientName == reporter) > USER_BLOCKED_AT_COUNT) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = true
        )
      }

      // R8 A user may report another user only if he has not previously reported the user in question.
      if (state.reports.exists(report => report.clientName == reporter && report.reportedClientName == reported)) {
        return state.copy(
          lastCommandSuccessful = false,
          userBanned = false
        )
      }

      state.copy(
        lastCommandSuccessful = true,
        userBanned = false,
        reports = ModelReport(reporter, reported) :: state.reports
      )
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val reply: Message = result.get
        val newState: State = nextState(state)
        (reply.isInstanceOf[UserBanned] == newState.userBanned) && (reply.isInstanceOf[OperationAck] == newState.lastCommandSuccessful)
      } else {
        false
      }
    }

    override def toString: String = s"Report($reporter, $reported)"
  }

  def genRetrieve: Gen[RetrieveCommand] = for {
    author <- genAuthor
  } yield RetrieveCommand(author)

  // just a suggestion, change it according to your needs.
  case class RetrieveCommandResult(success: Boolean, messages: List[String])

  case class RetrieveCommand(author: String) extends Command {
    type Result = RetrieveCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker

      // Retrieve all messages by the author
      worker.tell(new RetrieveMessages(author, sut.getCommId ))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      // Result
      val messageTexts = result.messages.asScala.toList.map { userMessage =>
        userMessage.getMessage
      }
      RetrieveCommandResult(success = true, messages = messageTexts)
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = {
      state.messages.exists(_.author == author)
    }

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val expectedMessages = state.messages.filter(_.author == author).map(_.message).sorted
        result.get.messages == expectedMessages
      } else {
        false
      }
    }

    override def toString: String = s"Retrieve($author)"
  }

  def genSearch: Gen[SearchCommand] = for {
    searchText <- genMessage
  } yield SearchCommand(searchText)

  case class SearchCommandResult(success: Boolean, messages: List[String])

  case class SearchCommand(searchText: String) extends Command {
    type Result = SearchCommandResult

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker

      // Send a search command to the message board
      worker.tell(new SearchMessages(searchText, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      // Result
      val messageTexts = result.messages.asScala.toList.map { userMessage =>
        userMessage.getMessage
      }
      SearchCommandResult(success = true, messages = messageTexts)
    }

    def nextState(state: State): State = {
      // TODO
      state
    }

    override def preCondition(state: State): Boolean = true

    override def postCondition(state: State, result: Try[Result]): Prop = {
      if (result.isSuccess) {
        val expectedMessages = state.messages
          .filter(m => m.message.contains(searchText) || m.author.contains(searchText))
          .map(_.message).sorted

        // Compare retrieved messages to the expected ones
        result.get.messages.sorted == expectedMessages
      } else {
        false
      }
    }

    override def toString: String = s"Search($searchText)"
  }

  def genEdit: Gen[EditCommand] = for {
    author <- genAuthor
    oldMessage <- genMessage
    newMessage <- genMessage
  } yield EditCommand(author, oldMessage, newMessage)

  case class EditCommand(author: String, oldMessage: String, newMessage: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker

      worker.tell(new SearchMessages(oldMessage, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == oldMessage)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))

      // Assume the message has ID 1 for simplicity; adjust as needed
      worker.tell(new Edit(messageID, author, newMessage, sut.getCommId)) // Adjust the message ID
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.map {
        case m if m.author == author && m.message == oldMessage => m.copy(message = newMessage)
        case m => m
      }
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = {
      // Ensure the author owns the old message and hasn't already published the new message
      state.messages.exists(m => m.author == author && m.message == oldMessage) &&
        !state.messages.exists(m => m.author == author && m.message == newMessage)
    }

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      } else {
        false
      }
    }

    override def toString: String = s"Edit($author, $oldMessage, $newMessage)"
  }

  def genRemoveLikeOrDislike: Gen[RemoveLikeOrDislikeCommand] = for {
    author <- genAuthor
    remover <- genAuthor
    message <- genMessage
    removeType <- Gen.choose(0, 1)
  } yield RemoveLikeOrDislikeCommand(author, remover, message, removeType)

  case class RemoveLikeOrDislikeCommand(author: String, remover: String, message: String, removeType: Int) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == message)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))


      val removeLikeOrDislikeType = if (removeType == 0) RemoveLikeOrDislike.Type.LIKE else RemoveLikeOrDislike.Type.DISLIKE
      worker.tell(new RemoveLikeOrDislike(author, sut.getCommId, messageID, removeLikeOrDislikeType))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.map {
        case m if m.author == author && m.message == message =>
          removeType match {
            case 0 => m.copy(likes = m.likes.filterNot(_ == author), points = m.points - 1)
            case 1 => m.copy(dislikes = m.dislikes.filterNot(_ == author), points = m.points + 1)
          }
        case m => m
      }
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = {
      // Check that the message exists and the user has the corresponding like/dislike
      state.messages.exists { m =>
        m.author == author && m.message == message &&
          (removeType == 0 && m.likes.contains(author)) || (removeType == 1 && m.dislikes.contains(author))
      }
    }

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      } else {
        false
      }
    }

    override def toString: String = s"RemoveLikeOrDislike($author, $message, $removeType)"
  }

  def genDelete: Gen[DeleteCommand] = for {
    author <- genAuthor
    message <- genMessage
    deletingUser <- genAuthor
  } yield DeleteCommand(author, message, deletingUser)

  case class DeleteCommand(author: String, message: String, deletingUser: String) extends Command {
    type Result = Message

    def run(sut: Sut): Result = {
      // Initialize communication with the system
      sut.getDispatcher.tell(new InitCommunication(sut.getClient, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val initAck = sut.getClient.receivedMessages.remove.asInstanceOf[InitAck]
      val worker = initAck.worker

      worker.tell(new SearchMessages(message, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val resultSearch = sut.getClient.receivedMessages.remove().asInstanceOf[FoundMessages]
      val messageID = resultSearch.messages.asScala.toList
        .find(m => m.getAuthor == author && m.getMessage == message)
        .map(_.getMessageId)
        .getOrElse(throw new NoSuchElementException("Message not found"))


      worker.tell(new Delete(messageID, deletingUser, sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      val result = sut.getClient.receivedMessages.remove()

      // End communication
      worker.tell(new FinishCommunication(sut.getCommId))
      while (sut.getClient.receivedMessages.isEmpty) sut.getSystem.runFor(1)
      sut.getClient.receivedMessages.remove()

      result
    }

    def nextState(state: State): State = {
      val updatedMessages = state.messages.filterNot(m => m.author == author && m.message == message)
      state.copy(messages = updatedMessages, lastCommandSuccessful = true)
    }

    override def preCondition(state: State): Boolean = {
      state.messages.exists(m => m.author == author && m.message == message) && deletingUser == author
    }

    override def postCondition(state: State, result: Try[Message]): Prop = {
      if (result.isSuccess) {
        val expectedState = nextState(state)
        expectedState.lastCommandSuccessful
      } else {
        false
      }
    }

    override def toString: String = s"Delete($author, $message, $deletingUser)"
  }
}
