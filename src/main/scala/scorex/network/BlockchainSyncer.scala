package scorex.network

import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef}
import scorex.app.LagonakiApplication
import scorex.app.settings.Constants
import scorex.block.Block
import scorex.network.message.{BlockMessage, GetSignaturesMessage}
import scorex.utils.ScorexLogging

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._


case class Synchronize(peer: InetSocketAddress)

case class NewBlock(block: Block, sender: Option[InetSocketAddress])

case class BlocksDownload(signatures: List[Array[Byte]], peer: InetSocketAddress)

case class BlockchainSyncer(application: LagonakiApplication) extends Actor with ScorexLogging {

  import BlockchainSyncer._

  private lazy val networkController = application.networkController

  private var status = Status.Offline

  override def preStart() = {
    context.system.scheduler.schedule(1.second, 2.seconds)(self ! CheckState)
    context.system.scheduler.schedule(500.millis, 1.second)(networkController ! GetMaxChainScore)
  }

  override def receive = {
    case CheckState =>
      status match {
        case Status.Offline =>

        case Status.Syncing =>
          val sigs = application.blockchainStorage.lastSignatures(application.settings.MaxBlocksChunks)
          val msg = GetSignaturesMessage(sigs)
          networkController ! NetworkController.SendMessageToBestPeer(msg)

        case Status.Generating =>
          log.info("Trying to generate a new block")
          application.wallet.privateKeyAccounts().find{privKeyAcc =>
            val state = application.storedState
            val history = application.blockchainStorage
            implicit val transactionModule = application.transactionModule
            application.consensusModule.generateNextBlock(privKeyAcc, state, history) match {
              case Some(block) =>
                self ! NewBlock(block, None)
                true
              case None => false
            }
          }
      }

    case MaxChainScore(scoreOpt) => scoreOpt match {
      case Some(maxScore) =>
        if (maxScore > application.blockchainStorage.score) status = Status.Syncing
        else status = Status.Generating

      case None => status = Status.Offline
    }

    case NewBlock(block, remoteOpt) =>
      if (block.isValid) {
        log.info(s"New block: $block")
        application.storedState.processBlock(block)
        application.blockchainStorage.appendBlock(block)
        val height = application.blockchainStorage.height()
        val exceptOf = remoteOpt.toList
        networkController ! NetworkController.BroadcastMessage(BlockMessage(height, block), exceptOf)
      } else {
        log.warn(s"Non-valid block: $block from ${remoteOpt.map(_.toString).getOrElse("local")}")
      }

    case GetStatus => sender() ! status

    case nonsense: Any => log.warn(s"BlockchainSyncer: got something strange $nonsense")
  }
}

object BlockchainSyncer {

  object Status extends Enumeration {
    val Offline = Value(0)
    val Syncing = Value(1)
    val Generating = Value(2)
  }

  case object CheckState

  case object GetMaxChainScore

  case class MaxChainScore(scoreOpt: Option[BigInt])

  case object GetStatus
}