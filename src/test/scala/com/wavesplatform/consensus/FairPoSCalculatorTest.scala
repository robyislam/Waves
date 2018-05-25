package com.wavesplatform.consensus

import cats.data.NonEmptyList
import cats.implicits._
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scorex.account.PrivateKeyAccount

import scala.util.Random

class FairPoSCalculatorTest extends PropSpec with PropertyChecks with Matchers {

  import PoSCalculator._

  val pos: PoSCalculator = NxtPoSCalculator //FairPoSCalculator

  case class Block(height: Int, gs: Array[Byte], baseTarget: Long, miner: PrivateKeyAccount, timestamp: Long, delay: Long)

  def generationSignature: Array[Byte] = {
    val arr = new Array[Byte](32)
    Random.nextBytes(arr)
    arr
  }

  val balance           = 50000000L * 100000000L
  val blockDelaySeconds = 60
  val defaultBaseTarget = 100L

  def target(prevBlockTimestamp: Long, prevBaseTarget: Long, timestamp: Long, balance: Long): BigInt = {
    val blockDelaySeconds = (timestamp - prevBlockTimestamp) / 1000
    BigInt(prevBaseTarget) * blockDelaySeconds * balance
  }

  property("///") {

    val minerGen = Gen.oneOf(mkMiners.toList)
    val baseTargetGen = Gen.chooseNum(20, 200)

    forAll(minerGen, baseTargetGen) { (minerWithBalance, baseTarget) =>
      val (miner, balance) = minerWithBalance
      val gs               = generatorSignature(generationSignature, miner.publicKey)
      val h                = hit(gs)
      val delay            = round(pos.calculateDelay(h, baseTarget, balance))
      val target           = BigInt(baseTarget) * (delay / 1000) * balance
      (target > h) shouldBe true
    }
  }
  def round(timestamp: Long): Long = (Math.ceil(timestamp / 1000.0) * 1000).toLong

  property("Correct consensus parameters of blocks generated with FairPoS") {

    val miners = mkMiners
    val first  = Block(0, generationSignature, defaultBaseTarget, miners.head._1, System.currentTimeMillis(), 0)

    val chain = (1 to 100000 foldLeft NonEmptyList.of(first))((acc, _) => {
      val gg     = acc.tail.lift(1)
      val lastBlock = acc.head

      val blocks = miners.map(mineBlock(lastBlock, gg, _))

      val next = blocks.minBy(_.delay)

      next :: acc
    }).reverse.tail

    chain.sliding(2)
      .forall({
        case a::b::Nil =>
          val h = hit(b.gs)
          val t = target(a.timestamp, a.baseTarget, b.timestamp, miners(b.miner))
          println(h, t, b.miner)
          h < t
      }) should be(true)

    val maxBT = chain.maxBy(_.baseTarget).baseTarget
    val avgBT = chain.map(_.baseTarget).sum / chain.length
    val minBT = chain.minBy(_.baseTarget).baseTarget

    val maxDelay = chain.tail.maxBy(_.delay).delay
    val avgDelay = chain.tail.map(_.delay).sum / (chain.length - 1)
    val minDelay = chain.tail.minBy(_.delay).delay

    println(
      s"""
        |BT: $minBT $avgBT $maxBT
        |Delay: $minDelay $avgDelay $maxDelay
      """.stripMargin
    )

    val minersPerfomance = calcPerfomance(chain, miners)

//    println(minersPerfomance)
//    assert(minersPerfomance.forall(p => p._2 < 1.1 && p._2 > 0.9))
    assert(avgDelay < 80000 && avgDelay > 40000)
    assert(avgBT < 200 && avgBT > 20)
  }

  def mineBlock(prev: Block, grand: Option[Block], minerWithBalance: (PrivateKeyAccount, Long)): Block = {
    val (miner, balance) = minerWithBalance
    val gs               = generatorSignature(generationSignature, miner.publicKey)
    val h                = hit(gs)
    val delay            = pos.calculateDelay(h, prev.baseTarget, balance)

    val bt = pos.calculateBaseTarget(
      blockDelaySeconds,
      prev.height + 1,
      prev.baseTarget,
      prev.timestamp,
      grand.map(_.timestamp),
      prev.timestamp + delay
    )

    Block(
      prev.height + 1,
      gs,
      bt,
      miner,
      prev.timestamp + delay,
      delay
    )
  }

  def calcPerfomance(chain: List[Block], miners: Map[PrivateKeyAccount, Long]): Map[Long, Double] = {
    val balanceSum  = miners.values.sum
    val blocksCount = chain.length

    chain
      .groupBy(_.miner)
      .map(mbs => {
        val (miner, blocks) = mbs

        val minerBalance   = miners(miner)
        val expectedBlocks = ((minerBalance.toDouble / balanceSum) * blocksCount).toLong
        val perfomance     = blocks.length.toDouble / expectedBlocks

        minerBalance -> perfomance
      })
  }

  def mkMiners: Map[PrivateKeyAccount, Long] =
    List(
      PrivateKeyAccount(generationSignature) -> 200000000000000L,
      PrivateKeyAccount(generationSignature) -> 500000000000000L,
      PrivateKeyAccount(generationSignature) -> 1000000000000000L,
      PrivateKeyAccount(generationSignature) -> 1500000000000000L,
      PrivateKeyAccount(generationSignature) -> 2000000000000000L,
      PrivateKeyAccount(generationSignature) -> 2500000000000000L
    ).toMap
}
