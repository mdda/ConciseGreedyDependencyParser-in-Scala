package ConciseGreedyDependencyServer

import org.zeromq.ZMQ

// See : https://www.playframework.com/documentation/2.0/ScalaJson
// And : https://www.playframework.com/documentation/2.3.x/ScalaJson
import play.api.libs.json._


object rrserver {
  def serve(args : Array[String]) {
    println("HELLO server - Started")
    
    // Prepare our context and socket
    val context = ZMQ.context(1)
    val receiver = context.socket(ZMQ.REP)
    
    receiver.connect("tcp://localhost:5560") // So the client (or broker) must 'bind' to the socket
    println("HELLO server - Connected")

    while (true) {
      // Wait for next request from client
      val request = receiver.recv (0)  // This are no flags, nothing to do with null termination
      
      println ("Received request: [" + 
                new String(request) // Creates a String from request
                + "]")

/*      
      // Do some 'work'
      try {
        Thread.sleep (1000)
      } catch {
        case e: InterruptedException => e.printStackTrace()
      }
*/
      // Parse method and path : 
      val json = Json.parse(request)
      
      (json \ "path").validate[String] match {
        case s: JsSuccess[String] => //println("Path: " + s.get)
          s.get match {
            case "/redcatlabs/handshakes/api/v1.0/parse" => {
                println("Doing a parse")
              }
          }
        case e: JsError => println("Errors: " + JsError.toFlatJson(e).toString()) 
      }
     
      // Send reply back to client
      val reply = "World".getBytes
      //NOOOO ! :: reply(reply.length-1)=0 //Sets the last byte of the reply to 0 
      receiver.send(reply, 0)
    }
  }
}
