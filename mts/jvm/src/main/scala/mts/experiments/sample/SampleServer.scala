package mts.experiments.sample

import mts.tasks.TaskConfig

import java.io.InputStream
import java.security.{ SecureRandom, KeyStore }
import javax.net.ssl.{ SSLContext, TrustManagerFactory, KeyManagerFactory }

import akka.actor.ActorSystem
import akka.http.scaladsl.server.{ RouteResult, Route, Directives }
import akka.http.scaladsl.{ ConnectionContext, HttpsConnectionContext, Http }
import akka.stream.Materializer
import akka.stream.ActorMaterializer
import com.typesafe.sslconfig.akka.AkkaSSLConfig

import scala.util.{ Success, Failure }

class SampleServer(implicit config: TaskConfig) {
  import config._
  implicit val system: ActorSystem = actorSystem
  implicit val materializer: Materializer = ActorMaterializer()
  import system.dispatcher

  // Manual HTTPS configuration

  val password: Array[Char] = new java.util.Scanner(
    getClass.getClassLoader.getResourceAsStream(s"$serverDomain-keystore-password")
  ).next.toCharArray

  val ks: KeyStore = KeyStore.getInstance("JKS")
  val keystore: InputStream = getClass.getClassLoader.getResourceAsStream(s"$serverDomain.jks")

  require(keystore != null, "Keystore required!")
  ks.load(keystore, password)

  val keyManagerFactory: KeyManagerFactory = KeyManagerFactory.getInstance("SunX509")
  keyManagerFactory.init(ks, password)

  val tmf: TrustManagerFactory = TrustManagerFactory.getInstance("SunX509")
  tmf.init(ks)

  val sslContext = SSLContext.getInstance("TLS")
  sslContext.init(keyManagerFactory.getKeyManagers, tmf.getTrustManagers, new SecureRandom)
  val https = ConnectionContext.https(sslContext)

  val service: SampleWebservice = new SampleWebservice
  val httpBinding = Http().bindAndHandle(service.route, interface, httpPort)
  val httpsBinding = Http().bindAndHandle(service.route, interface, httpsPort, connectionContext = https)
  httpBinding.onComplete {
    case Success(binding) ⇒
      val localAddress = binding.localAddress
      println(s"Server is listening on http://${localAddress.getHostName}:${localAddress.getPort}")
    case Failure(e) ⇒
      println(s"Binding failed with ${e.getMessage}")
      system.terminate()
  }
  httpsBinding.onComplete {
    case Success(binding) ⇒
      val localAddress = binding.localAddress
      println(s"Server is listening on https://${localAddress.getHostName}:${localAddress.getPort}")
    case Failure(e) ⇒
      println(s"Binding failed with ${e.getMessage}")
      system.terminate()
  }
}
