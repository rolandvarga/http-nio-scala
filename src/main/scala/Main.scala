import io.rolandvarga.http.Server

object Main extends App {
  val server = Server.create
  server.run()
}