package scorex.transaction

import com.wavesplatform.TransactionGen
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import scorex.transaction.assets.{BurnTransaction, BurnTransactionV1, BurnTransactionV2}
import scorex.account.PublicKeyAccount
import com.wavesplatform.state.ByteStr

class BurnTransactionSpecification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Burn serialization roundtrip") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = issue.builder.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("Burn serialization from TypedTransaction") {
    forAll(burnGen) { issue: BurnTransaction =>
      val recovered = TransactionParsers.parseBytes(issue.bytes()).get
      recovered.bytes() shouldEqual issue.bytes()
    }
  }

  property("JSON format validation for BurnTransactionV1") {
    val js = Json.parse("""{
                       "type": 6,
                       "id": "Ci1q7y7Qq2C2GDH7YVXsQ8w5vRRKYeoYTp9J76AXw8TZ",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "signature": "uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE",
                       "chainId": null,
                       "version": 1,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "amount": 10000000000
                    }
    """) //TODO: change to "quantity" after NODE-765 fix

    val tx = BurnTransactionV1
      .create(
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        10000000000L,
        100000000L,
        1526287561757L,
        ByteStr.decodeBase58("uapJcAJQryBhWThU43rYgMNmvdT7kY747vx5BBgxr2KvaeTRx8Vsuh4yu1JxBymU9LnAoo1zjQcPrWSuhi6dVPE").get
      )
      .right
      .get
    js shouldEqual tx.json()
  }

  property("JSON format validation for BurnTransactionV2") {
    val js = Json.parse("""{
                       "type": 6,
                       "id": "6QA1sLV53euVCX5fFemNuEyRVdQ5JYo5dWDsCmtKADRc",
                       "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                       "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                       "fee": 100000000,
                       "timestamp": 1526287561757,
                       "proofs": [
                       "3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H"
                       ],
                       "chainId": 84,
                       "version": 2,
                       "assetId": "9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz",
                       "amount": 10000000000
                    }
    """) //TODO: change to "quantity" after NODE-765 fix

    val tx = BurnTransactionV2
      .create(
        2,
        'T',
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").right.get,
        ByteStr.decodeBase58("9ekQuYn92natMnMq8KqeGK3Nn7cpKd3BvPEGgD6fFyyz").get,
        10000000000L,
        100000000L,
        1526287561757L,
        Proofs(Seq(ByteStr.decodeBase58("3NcEv6tcVMuXkTJwiqW4J3GMCTe8iSLY7neEfNZonp59eTQEZXYPQWs565CRUctDrvcbtmsRgWvnN7BnFZ1AVZ1H").get))
      )
      .right
      .get

    js shouldEqual tx.json()
  }

}
