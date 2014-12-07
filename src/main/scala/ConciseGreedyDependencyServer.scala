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

      // Parse method and path : 
      val json = Json.parse(request)
      
      val response = (json \ "path").validate[String] match {
        case s: JsSuccess[String] => //println("Path: " + s.get)
          s.get match {
            case "/redcatlabs/handshakes/api/v1.0/parse" => {
                parse_sentences(json \ "body")
              }
            case _ =>       Json.obj(
                              "status" -> 404,
                              "body" -> "Path not found"
                            )
          }
        case e: JsError =>  Json.obj(
                              "status" -> 500,
                              "body" -> JsError.toFlatJson(e).toString()
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
