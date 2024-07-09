package io.rolandvarga.http

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.ZoneId
import java.net.{InetSocketAddress, SocketException}
import scala.util.{Failure, Success, Try}
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import io.rolandvarga.logger.Logger

case class Request(methodWithPath: String)
case class Response(status: String, body: String)

trait Handler {
  def apply(request: Request): Response
}

class Server(
  socket: ServerSocketChannel,
  selector: Selector,
  var root: RoutingNode,
  var isRunning: Boolean = false,
)(implicit ec: ExecutionContext) extends SignalHandler {
  def run() = {
    socket.configureBlocking(false)
    socket.register(selector, SelectionKey.OP_ACCEPT)

    isRunning = true

    Logger.info("ready to accept new connections")

    while (isRunning) {
      try {
        selector.select()
        val selectedKeys = selector.selectedKeys()
        val iterator = selectedKeys.iterator()

        while(iterator.hasNext) {
          val key = iterator.next()
          iterator.remove()

          if (key.isAcceptable) {
            val client = socket.accept()
            client.configureBlocking(false)
            client.register(selector, SelectionKey.OP_READ)
          }

          if (key.isReadable) {
            handleRequest(key.channel().asInstanceOf[SocketChannel])
          }
        }
      } catch {
        case e: java.io.IOException => {
          Logger.error(s"IO exception while running server: ${e.printStackTrace()}")
        }
        case e: Exception => {
          Logger.error(s"unexpected error: ${e.printStackTrace()}")
        }
      }
    }
  }

  override def handle(sig: Signal): Unit = {
    Logger.warn("received an interrupt; time to shutdown...")
    isRunning = false
    selector.wakeup() // NOTE: we send a wakeup otherwise the selector will block the main thread
                      //  until a new request comes in, and completely ignore our signal up until that point
    socket.close()
  }

  private def handleRequest(channel: SocketChannel) = {
    Logger.info(s"received a connection from ${channel.getRemoteAddress}")

    try {
      val buffer = ByteBuffer.allocate(256)

      val readBytes = channel.read(buffer)
      if (readBytes == -1) {
        channel.close()
      } else {
        buffer.flip()

        val request = parseRequest(buffer)
        val resp = Server.buildResponse(root, request)

        buffer.clear()
        buffer.put(resp)
        buffer.flip()

        channel.write(buffer)
        buffer.clear()
        channel.close()
      }
    } catch {
      case e: SocketException => {
        Logger.error(s"communicating with socket: ${e.printStackTrace()}")
      }
      case e: Exception => {
        Logger.error(s"handling connection: ${e.printStackTrace()}")
      }
    }
  }

  private def parseRequest(input: ByteBuffer): Request = {
    val headChunk = input.array().takeWhile(_ != '\r')
    val headerLine = new String(headChunk, "UTF-8")

    val methodWithPath = headerLine.split(" ").take(2).mkString(" ")
    Request(methodWithPath)
  }
}

object Server {
  def indexHandler: Handler = new Handler {
    override def apply(request: Request): Response = {
      Response("200 OK", "HELLO FROM INDEX")
    }
  }

  def fooHandler: Handler = new Handler {
    override def apply(request: Request): Response = {
      Response("200 OK", "HELLO FROM FOO")
    }
  }

  def barHandler: Handler = new Handler {
    override def apply(request: Request): Response = {
      Response("200 OK", "HELLO FROM BAR")
    }
  }

  val PROTOCOL = "HTTP/1.1"
  val CRLF = "\r\n"
  val CONTENT_TYPE = "Content-Type: text/html; charset=utf-8"
  val SERVER_HEADER = "Server: my-http 1.0"
  val ALLOW_ORIGIN = "Access-Control-Allow-Origin: *"
  val ALLOW_CREDENTIALS = "Access-Control-Allow-Credentials: true"

  val EMPTY_BODY = ""

  def buildResponse(root: RoutingNode, request: Request): Array[Byte] = {
    val response = (Routes.search(root, request.methodWithPath)) match {
      case Some(node) => {
        node.handler(request)
      }
      case _ => {
        Response("404 NOT FOUND", EMPTY_BODY)
      }
    }

    val now = ZonedDateTime.now(ZoneId.of("GMT"))
    val formatter = DateTimeFormatter.ofPattern("EEE, dd MMM yyyy HH:mm:ss z")
    val nowFormatted = now.format(formatter)

    val contents = (
      s"$PROTOCOL ${response.status}" + CRLF
        + s"Date: $nowFormatted" + CRLF
        + CONTENT_TYPE + CRLF
        + s"Content-Length: ${response.body.length}" + CRLF
        + SERVER_HEADER + CRLF
        + ALLOW_ORIGIN + CRLF
        + ALLOW_CREDENTIALS + CRLF
        + "Connection: close" + CRLF
        + CRLF
        + response.body
      )
    contents.getBytes("ISO-8859-1")
  }

  def create: Server = {
    val listenerTry = Try(ServerSocketChannel.open().bind(new InetSocketAddress(80)))

    listenerTry match {
      case Success(listener) => {
        implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

        var root = RoutingNode("", indexHandler, collection.mutable.Map.empty)
        Routes.insert(root, "GET /", indexHandler)
        Routes.insert(root, "GET /foo", fooHandler)
        Routes.insert(root, "GET /foo/bar", barHandler)

        val selector = Selector.open()
        val server = new Server(listener, selector, root)

        Signal.handle(new Signal("INT"), server.handle )
        Signal.handle(new Signal("TERM"), server.handle )

        server
      }
      case Failure(e: java.io.IOException) => {
        Logger.error(s"opening socket: $e")
        scala.sys.exit(1)
      }
    }
  }
}
