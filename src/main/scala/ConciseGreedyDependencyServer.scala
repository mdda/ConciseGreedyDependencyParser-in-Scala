package ConciseGreedyDependencyServer

import org.zeromq.ZMQ

// See : https://www.playframework.com/documentation/2.0/ScalaJson
// And : https://www.playframework.com/documentation/2.3.x/ScalaJson
import play.api.libs.json._

import ConciseGreedyDependencyParser.{CGDP, Tagger, DependencyMaker}

case class ZMQserver(utils : CGDP, tagger : Tagger, dm : DependencyMaker) {
  def serve(args : Array[String]) {
    println("HELLO server - Started")
    
    // Prepare our context and socket
    val context = ZMQ.context(1)
    val receiver = context.socket(ZMQ.REP)
    
    receiver.connect("tcp://localhost:5560") // So the client (or broker) must 'bind' to the socket
    println("HELLO server - Connected")

    while (true) {
      // Wait for next request from client
      val request = receiver.recv (0)  // This has flags of zero, nothing to do with null termination
      println ("Received request: >" + new String(request) + "<") // Creates a String from request

      val json = Json.parse(request)

      val response = List(json \ "method", json \ "path").map(_.validate[String]) match {
        case List(method:JsSuccess[String], path:JsSuccess[String]) => //println("Path: " + s.get)
          (method.get, path.get) match {
            case ("POST", "/redcatlabs/handshakes/api/v1.0/parse") =>
                parse_sentences(json \ "body")
                
            case (m,p) => 
              Json.obj(
                "status" -> 404,
                "body" -> s"Path '$p' not found for method '$m'"
              )
          }
        case List(method, path:JsError) =>  
          Json.obj(
            "status" -> 500,
            "body" -> "Need both 'method' and 'path' to be defined"
          )
      }
     
      // Send reply back to client
      val reply_bytes = Json.stringify(response).getBytes
      receiver.send(reply_bytes, 0)
    }
  }
  
  def parse_sentences(body: JsValue):JsValue = {
    println("Doing a parse")
    
    val results = for( txt <- (body \ "sentences").as[List[String]] ) yield {
      val sentence = utils.sentence(txt)
      val tags = tagger.tag(sentence)
      //println(s"tagged = ${sentence.map{_.norm}.zip(tags)}")
      val structure = dm.parse(sentence)  // This actual re-tags the sentence...  wasteful
      
      Json.obj(
        "words" -> sentence.map{_.norm},
        "tags" -> tags,
        "structure" -> structure
      )
    }
    
    Json.obj(
      "status" -> 200,
      "body" -> Json.arr(results)
    )    
  }
}
