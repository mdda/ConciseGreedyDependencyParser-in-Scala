// To see it working within sbt, do a : 
// run-main ConciseGreedyDependencyParser.Main XYZ
// or (if this is the only package with a main() in it) :
// run XYZ  

package ConciseGreedyDependencyParser

import scala.collection.mutable

/*  SOMEONE needs to make 'scala-pickling' (a) better documented, and (b) use fewer implicits, so compile time doesn't x4...
// https://github.com/scala/pickling
import scala.pickling._
import json._
//import binary._
*/

import java.io.{PrintWriter, FileOutputStream, File}

package object ConciseGreedyDependencyParserObj {
  type ClassNum  = Int
  type ClassName = String
  
  type DependencyIndex = Int
  type Move      = Int
  
  type FeatureName = String
  type FeatureData = String
  type Score = Float
  
  type Word = String
  type Sentence = List[WordData]
  
  case class WordData(raw:Word, pos:ClassName="", dep:DependencyIndex=(-1)) {
    lazy val norm:Word = {
      if      (raw.contains('-') && raw(0) != '-')        "!HYPHEN"
      else if (raw.length == 4 && raw.forall(_.isDigit))  "!YEAR"
      else if (raw(0).isDigit)                            "!DIGITS"
      else                                                raw.toLowerCase
    }
  }
}

import ConciseGreedyDependencyParserObj._

case class Feature(name:FeatureName, data:FeatureData)

class Perceptron(n_classes:Int) {
  // These need not be visible outside the class
  type TimeStamp = Int
  
  case class WeightLearner(current:Int, total:Int, ts:TimeStamp) {
    def add_change(change:Int) = {
      WeightLearner(current + change, total + current*(seen-ts), seen)
    }
  }
  
  type ClassToWeightLearner = mutable.Map[ ClassNum,  WeightLearner ]  // tells us the stats of each class (if present) 
  
  // The following are keyed on feature (to keep tally of total numbers into each, and when)(for the TRAINING phase)
  val learning =  mutable.Map.empty[
                    String,    // Corresponds to Feature.name
                    mutable.Map[
                      String,  // Corresponds to Feature.data 
                      ClassToWeightLearner
                    ]
                  ] // This is hairy and mutable...
  
  // Number of instances seen - used to measure how 'old' each total is
  var seen:TimeStamp = 0
  
  type ClassVector = Vector[Score]

  def predict(classnum_vector : ClassVector) : ClassNum = { // Return best class guess for this vector of weights
    classnum_vector.zipWithIndex.maxBy(_._1)._2   // in vector order (stabilizes) ///NOT : (and alphabetically too)
  }

  def current(w : WeightLearner):Float =  w.current
  def average(w : WeightLearner):Float = (w.current*(seen-w.ts) + w.total) / seen // This is dynamically calculated
  // No need for average_weights() function - it's all done dynamically
  
  def score(features: Map[Feature, Score], score_method: WeightLearner => Float): ClassVector = { // Return 'dot-product' score for all classes
    if(false) { // This is the functional version : 3023ms for 1 train_all, and 0.57ms for a sentence
      features
        .filter( pair => pair._2 != 0 )  // if the 'score' multiplier is zero, skip
        .foldLeft( Vector.fill(n_classes)(0:Float) ){ case (acc, (Feature(name,data), score)) => {  // Start with a zero classnum->score vector
          learning
            .getOrElse(name, Map[String,ClassToWeightLearner]())   // This is first level of feature access
              .getOrElse(data, Map[ ClassNum,  WeightLearner ]())       // This is second level of feature access and is a Map of ClassNums to Weights (or NOOP if not there)
                .foldLeft( acc ){ (acc_for_feature, cn_wl) => { // Add each of the class->weights onto our score vector
                  val classnum:ClassNum = cn_wl._1
                  val weight_learner:WeightLearner = cn_wl._2
                  acc_for_feature.updated(classnum, acc_for_feature(classnum) + score * score_method(weight_learner))
                  //acc_for_feature(classnum) += score * score_method(weight_learner)
                }}
        }}
    }
    else { //  This is the mutable version : 2493ms for 1 train_all, and 0.45ms for a sentence
      val scores = (new Array[Score](n_classes)) // All 0?
      
      features
        .filter( pair => pair._2 != 0 )  // if the 'score' multiplier is zero, skip
        .map{ case (Feature(name,data), score) => {  // Ok, so given a particular feature, and score to weight it by
          if(learning.contains(name) && learning(name).contains(data)) {
            learning(name)(data).foreach { case (classnum, weight_learner) => {
              //println(s"classnum = ${classnum} n_classes=${n_classes}")
              scores(classnum) += score * score_method(weight_learner)
            }}
          }
        }}
      scores.toVector
    }  
  }
  
  def update(truth:ClassNum, guess:ClassNum, features:Iterable[Feature]): Unit = { // Hmmm ..Unit..
    seen += 1
    if(truth != guess) {
      for {
        feature <- features
      } {
        learning.getOrElseUpdate(feature.name, mutable.Map[FeatureData, ClassToWeightLearner]() )
        var this_map = learning(feature.name).getOrElseUpdate(feature.data, mutable.Map[ClassNum, WeightLearner]() )
                
        //println(s"  update [${feature.name},${feature.data}] : ${learning(feature.name)(feature.data)}")
        if(this_map.contains(guess)) {
          this_map.update(guess, this_map(guess).add_change(-1))
        }
        this_map.update(truth, this_map.getOrElse( truth, WeightLearner(0,0,seen) ).add_change(+1))
        
        learning(feature.name)(feature.data) = this_map
      }
    }
  }
  
  override def toString():String = {
    s"perceptron.seen=[$seen]\n" + 
    learning.map({ case (feature_name, m1) => {
      m1.map({ case (feature_data, cn_feature) => {
        cn_feature.map({ case (cn, feature) => {
          s"$cn:${feature.current},${feature.total},${feature.ts}"
        }}).mkString(s"${feature_data}[","|","]\n")
      }}).mkString(s"${feature_name}{\n","","}\n")
    }}).mkString("perceptron.learning={\n","","}\n")
  }
  
  def load(lines:Iterator[String]):Unit = {
    val perceptron_seen     = """perceptron.seen=\[(.*)\]""".r
    val perceptron_feat_n   = """(.*)\{""".r
    val perceptron_feat_d   = """(.*)\[(.*)\]""".r
    def parse(lines: Iterator[String]):Unit = if(lines.hasNext) lines.next match {
      case perceptron_seen(data) => {
        seen = data.toInt
        parse(lines)
      }
      case "perceptron.learning={" => {
        parse_featurename(lines)
        parse(lines)
      }
      case _ => () // line not understood : Finished with perceptron
    }
    def parse_featurename(lines: Iterator[String]):Unit = if(lines.hasNext) lines.next match {
      case perceptron_feat_n(feature_name) => {
        //println(s"Reading FeatureName[$feature_name]")
        learning.getOrElseUpdate(feature_name, mutable.Map[FeatureData, ClassToWeightLearner]() )
        parse_featuredata(feature_name, lines)
        parse_featurename(lines) // Go back for more featurename sections
      }
      case _ => () // line not understood : Finished with featurename
    }
    def parse_featuredata(feature_name:String, lines: Iterator[String]):Unit = if(lines.hasNext) lines.next match {
      case perceptron_feat_d(feature_data, classnum_weight) => {
        //println(s"Reading FeatureData[$feature_data]")
        learning(feature_name).getOrElseUpdate(feature_data, mutable.Map[ClassNum, WeightLearner]() )
        classnum_weight.split('|').map( cw => { 
          val cn_wt = cw.split(':').map(_.split(',').map(_.toInt))
          // println(s"Tagger node : $cn_wt"); 
          learning(feature_name)(feature_data) += (( cn_wt(0)(0), WeightLearner(cn_wt(1)(0), cn_wt(1)(1), cn_wt(1)(2)) ))
        }) 
        parse_featuredata(feature_name, lines)  // Go back for more featuredata lines
      }
      case _ => () // line not understood : Finished with featuredata
    }
    parse(lines)
  }
  
/* 
  // http://stackoverflow.com/questions/23182577/even-trivial-serialization-examples-in-scala-dont-work-why
  // http://stackoverflow.com/questions/23072118/scala-pickling-how
  def save(path: String):Unit = {
    print(s"Saving model to ${path}")
    
    val pw = new PrintWriter(new File(path))
    // Uncommenting this line increases compile time from 2s to 10s...
    //pw.write(learning.pickle.value)
    pw.close
  }

  def load(path: String):Unit = {
    print(s"Loading Perceptron model to ${path}")
    
    val buffered_source = scala.io.Source.fromFile(path)
    val unpickled = buffered_source.toString.unpickle[learning.type]
    //val unpickled = learning
    
    // Now copy the unpickled version into learning
    learning.clear
    learning ++= unpickled
  }
*/

}

object Tagger {  // Here, tag == Part-of-Speech
  
  // Make a tag dictionary for single-tag words : So that they can be 'resolved' immediately, as well as the class list
  def classes_and_tagdict(training_sentences: List[Sentence]): (Vector[ClassName], Map[Word, ClassNum])  = {
    
    def functional_approach(): (Set[ClassName], Map[ Word, Map[ClassName, Int] ]) = {
      // takes 120ms on full training data
      training_sentences.foldLeft( ( Set[ClassName](), Map[ Word, Map[ClassName, Int] ]() ) ) { case ( (classes, map), sentence ) => {
        sentence.foldLeft( (classes, map) ) { case ( (classes, map), word_data) => {
          val count = map.getOrElse(word_data.norm, Map[ClassName, Int]()).getOrElse(word_data.pos, 0:Int)
          ( 
            classes + word_data.pos, 
            map + (( word_data.norm, 
                     map.getOrElse(word_data.norm, Map[ClassName, Int]()) 
                     + ((word_data.pos, count+1)) 
                  )) 
          )
        }}
      }}
    }
    
    def mutational_approach(): (mutable.Set[ClassName], mutable.Map[ Word, mutable.Map[ClassName, Int] ]) = {
      // takes 60ms on full training data !
      val class_set = mutable.Set[ClassName]()
      val full_map  = mutable.Map[ Word, mutable.Map[ClassName, Int] ]()
                      //.withDefault( k => mutable.Map[ClassName, Int]().withDefaultValue(0) )       // FAIL - reuse
                      //.withDefaultValue( new mutable.Map[ClassName, Int]().withDefaultValue(0) )   // FAIL - types
                      
      for {
        sentence <- training_sentences
        word_data <- sentence
      } {
        class_set += word_data.pos
        full_map.getOrElseUpdate(word_data.norm, mutable.Map[ClassName, Int]().withDefaultValue(0))(word_data.pos) += 1
        //println(s"making (${word_data.norm})(${word_data.pos})=>(${full_map(word_data.norm)(word_data.pos)})")
      }
      
      (class_set, full_map)
    }
    
    // First, get the set of classnames, and the counts for all the words and tags
    val (class_set, full_map) = if(false) functional_approach() else mutational_approach()
    
    // Convert the set of classes into a nice map, with indexer
    val classes = class_set.toVector.sorted  // This is alphabetical
    val class_map = classes.zipWithIndex.toMap
    println(s"Classes = [${classes.mkString(",")}]")

    val freq_thresh = 20
    val ambiguity_thresh = 0.97
    
    // Now, go through the full_map, and work out which are worth 'resolving' immediately - and return a suitable tagdict
    val tag_dict = mutable.Map[Word, ClassNum]().withDefaultValue(0)
    for {
      (norm, classes) <- full_map
      if(classes.values.sum >= freq_thresh)  // ignore if not enough samples
      (cl, v) <- classes
      if(v >= classes.values.sum * ambiguity_thresh) // Must be concentrated (in fact, cl must be unique... since >50%)
    } {
      tag_dict(norm) = class_map(cl)
      //println(s"${norm}=${cl}")
    }
    (classes, tag_dict.toMap)
  }
 
  def load(lines:Iterator[String]):Tagger = {
    var (classes, tag_dict)=(Array[ClassName](), mutable.Map[Word, ClassNum]())
    
    val tagger_classes = """tagger.classes=\[(.*)\]""".r
    val tagger_tag_dict = """tagger.tag_dict=\[(.*)\]""".r
    def parse(lines: Iterator[String]):Unit = lines.next match {
      case tagger_classes(data) => {
        classes = data.split('|')
        parse(lines)
      }
      case tagger_tag_dict(data) => {
        tag_dict ++= data.split('|').map( nc => { val arr = nc.split('='); (arr(0),arr(1).toInt) })  // println(s"Tagger pair : $nc"); 
        parse(lines)
      }
      case _ => () // line not understood : Finish
    }
    parse(lines)
    
    val t = new Tagger(classes.toVector, tag_dict.toMap)
    t.perceptron.load(lines)
    t
  }

}

class Tagger(classes:Vector[ClassName], tag_dict:Map[Word, ClassNum]) {
  println(s"Tagger.Classes = [${classes.mkString(",")}]")
  val getClassNum = classes.zipWithIndex.toMap.withDefaultValue(-1) // -1 => "CLASS-NOT-FOUND"
  //def getClassNum(class_name: ClassName): ClassNum = classes.indexOf(class_name) // -1 => "CLASS-NOT-FOUND"
  
  val perceptron = new Perceptron(classes.length)

  def get_features(word:List[Word], pos:List[ClassName], i:Int):Map[Feature,Score] = {
    val feature_set = mutable.Set[Feature]() 
    feature_set += Feature("bias",       "")  // It's useful to have a constant feature, which acts sort of like a prior
    
    feature_set += Feature("word",       word(i))  
    feature_set += Feature("w suffix",   word(i).takeRight(3))  
    feature_set += Feature("w pref1",    word(i).take(1))  
    
    feature_set += Feature("tag-1",      pos(i-1))  
    feature_set += Feature("tag-2",      pos(i-2))  
    feature_set += Feature("tag-1-2",    s"${pos(i-1)} ${pos(i-2)}")  
    
    feature_set += Feature("w,tag-1",    s"${word(i)} ${pos(i-1)}")  
    
    feature_set += Feature("w-1",        word(i-1))  
    feature_set += Feature("w-1 suffix", word(i-1).takeRight(3))  
    
    feature_set += Feature("w-2",        word(i-2))  
    
    feature_set += Feature("w+1",        word(i+1))  
    feature_set += Feature("w+1 suffix", word(i+1).takeRight(3))  
    
    feature_set += Feature("w+2",        word(i+2))  
    
    // All weights on this set of features are ==1
    feature_set.map( f => (f, 1:Score) ).toMap
  }

  def train(sentences:List[Sentence]):Unit = sentences.foreach( train_one )
  def train_one(sentence:Sentence):Unit = { process(sentence, true); () }
    
  def tag(sentence:Sentence):List[ClassName] = process(sentence, false)
    
  def process(sentence:Sentence, train:Boolean):List[ClassName] = {
    //val context:List[Word] = ("%START%" :: "%PAD%" :: (sentence.map( _.norm ) :+ "%ROOT%" :+ "%END%"))
    val words_norm = sentence.map( _.norm )
    val words:List[Word] = (List("%START%","%PAD%") ::: words_norm ::: List("%ROOT%","%END%"))
    val truth:List[ClassNum] = if(train) (List(-1,-1) ::: sentence.map( wd => getClassNum(wd.pos) ) ::: List(-1,-1)) else Nil

    val (i_final, all_tags) =
    words_norm.foldLeft( (2:Int, List[ClassName]("%START%","%PAD%")) ) { case ( (i, tags), word_norm ) => { 
      val guess = tag_dict.getOrElse(word_norm, {   // Don't do the feature scoring if we already 'know' the right PoS
        val features = get_features(words, tags, i)
        val score = perceptron.score(features, if(train) perceptron.current else perceptron.average)
        val guessed = perceptron.predict( score )
        
        if(train) {// Update the perceptron
          //println(f"Training '${word_norm}%12s': ${classes(guessed)}%4s -> ${classes(truth(i))}%4s :: ")
          perceptron.update( truth(i), guessed, features.keys)
        }
        guessed // Use the guessed value for next prediction/learning step (rather than the truth...)
      }) 
      (i+1, tags :+ classes(guess))
    }}
    all_tags.drop(2)
  }
  
  override def toString():String = {
    classes.mkString("tagger.classes=[","|","]\n") + 
    tag_dict.map({ case (norm, classnum) => s"$norm=$classnum" }).mkString("tagger.tag_dict=[","|","]\n") +
    "\n" +
    perceptron.toString
  }

}


class DependencyMaker(tagger:Tagger) {
  val SHIFT:Move=0; val RIGHT:Move=1; val LEFT:Move=2; val INVALID:Move=(-1)
  val move_names = Vector[ClassName]("SHIFT", "RIGHT", "LEFT")
  println(s"DependencyMaker.Classes = [$move_names]")
  //val getClassNum = classes.zipWithIndex.toMap.withDefaultValue(-1) // -1 => "CLASS-NOT-FOUND"
  
  val perceptron = new Perceptron(move_names.length)
  //confusionmatrix == UNUSED  
  
  case class DefaultList(list:List[Int], default:Int=0) { 
    def apply(idx: Int): Int = if(idx>=0 || idx<list.length) list(idx) else {
      println(s"Hit Default in DefaultList idx = ${idx}")
      default
    }
    def ::(a: Int) = DefaultList(a::list, default)
  }

  case class ParseState(n:Int, heads:Vector[Int], lefts:Vector[DefaultList], rights:Vector[DefaultList]) { // NB: Insert at start, not at end...
      // This makes the word at 'child' point to head and adds the child to the appropriate left/right list of head
      def add(head:Int, child:Int) = {
        if(child<head) {
          ParseState(n, heads.updated(child, head), lefts.updated(head, child :: lefts(head)), rights)
        } 
        else {
          ParseState(n, heads.updated(child, head), lefts, rights.updated(head, child :: rights(head)))
        }
      }
  }
  def ParseStateInit(n:Int) = {
    // heads are the dependencies for each word in the sentence, except the last one (the ROOT)
    val heads = Vector[Int](n) // This is used for 'n' elsewhere
    
    // Each possible head (including ROOT) has a (lefts) and (rights) list, initially none
    // Entries (0, ..., n-1) are words, (n) is the 'ROOT'  ('to' is INCLUSIVE)
    val lefts  = (0 to n).map( i => DefaultList(Nil, 0) ).toVector
    val rights = (0 to n).map( i => DefaultList(Nil, 0) ).toVector
    ParseState(n, heads, lefts, rights)
  }

/*
    STATE STRUCTURE (According to blog post) :
     * An index, i, into the list of tokens;
     * The dependencies added so far, in Parse
     * A stack, containing word indices that occurred before i, for which we’re yet to assign a head.

    -- By construction, stack is always stored in increasing order [ a,c,f,g ] i
       So "I(head)-g" is a left dangling branch, and "F(head)-g" is right dangling one
    
*/
  
  case class CurrentState(i:Int, stack:List[Int], parse:ParseState) {
    def transition(move:Move):CurrentState = move match {
      // i either increases and lengthens the stack, 
      case SHIFT => CurrentState(i+1, i::stack, parse)
      // or stays the same, and shortens the stack, and manipulates left&right 
      case RIGHT => CurrentState(i, stack.tail, parse.add(stack.tail.head, stack.head))   // parse.add(stack[-2], stack.pop())
      case LEFT  => CurrentState(i, stack.tail, parse.add(i, stack.head))                 // parse.add(i, stack.pop())
    }
    
    def valid_moves:Set[Move] = List[Move](  // only depends on stack_depth (not parse itself)
      if(i < parse.n -1 )  SHIFT else INVALID, // i.e. not yet at the last word in sentence
      if(stack.length>=2)  RIGHT else INVALID,
      if(stack.length>=1)  LEFT  else INVALID
    ).filterNot( _ == INVALID).toSet
    
    // This may be equivalent to there being no valid_moves
    def parse_complete = !(stack.length>0 || i<(parse.n-1)) // i.e. we've at (or beyond) the last word, and have no stack left

    def get_gold_moves(gold_heads:Vector[Int]) = {
      def deps_between(target:Int, others:List[Int]) = {
        others.exists( word => (gold_heads(word)==target || gold_heads(target) == word))
      }
      
      val valid = valid_moves
      if(stack.length==0 || ( valid.contains(SHIFT) && gold_heads(i) == stack.head)) {
        Set(SHIFT)
      }
      else if( gold_heads(stack.head) == i ) {
        Set(LEFT)
      }
      else {
        /*
        // HMM  : Actually, it looks like this logic (from Python) can be flipped over : 
        //        by constructing a 'val non_gold' and return 'valid - non_gold'
        val all_moves = Set(SHIFT, RIGHT, LEFT)
        var costly = all_moves -- valid  // i.e. all invalid moves are 'costly'
        
        // If the word second in the stack is its gold head, Left is incorrect
        if( stack.length >= 2 && gold_heads(stack.head) == stack.tail.head ) {
          costly += (LEFT)
        }
        
        // If there are any dependencies between i and the stack, pushing i will lose them.
        if( deps_between(i, stack) ) {  // This is redundent / over-cautious :: !costly.contains(SHIFT) && 
          costly += (SHIFT)
        }
        
        // If there are any dependencies between the stackhead and the remainder of the buffer, popping the stack will lose them.
        if( deps_between(stack.head, (i+1 until parse.n-1).toList) ) { // UNTIL is EXCLUSIVE of top
          costly += (LEFT)
          costly += (RIGHT)
        }
        
        (all_moves -- costly)
        */
        
        // TODO : Test the two different approaches for equality
        
        // If the word second in the stack is its gold head, Left is incorrect
        val left_incorrect = (stack.length >= 2 && gold_heads(stack.head) == stack.tail.head)
        
        // If there are any dependencies between i and the stack, pushing i will lose them.
        val dont_push_i    = deps_between(i, stack) // This is redundent / over-cautious :: !costly.contains(SHIFT) && 
        
        // If there are any dependencies between the stackhead and the remainder of the buffer, popping the stack will lose them.
        val dont_pop_stack = deps_between(stack.head, (i+1 until parse.n-1).toList) // UNTIL is EXCLUSIVE of top
        
        val non_gold = List[Move](
          if( left_incorrect )  LEFT  else INVALID,
          if( dont_push_i )     SHIFT else INVALID,
          if( dont_pop_stack )  LEFT  else INVALID,
          if( dont_pop_stack )  RIGHT else INVALID
        ).toSet
        (valid -- non_gold)
      }
    }
/*    
def get_gold_moves(n0, n, stack, heads, gold):

    valid = get_valid_moves(n0, n, len(stack))
    if not stack or (SHIFT in valid and gold[n0] == stack[-1]):
        return [SHIFT]
    if gold[stack[-1]] == n0:
        return [LEFT]
        
    costly = set([m for m in MOVES if m not in valid])
    #print "Costly = ", costly
    
    # If the word behind s0 is its gold head, Left is incorrect
    if len(stack) >= 2 and gold[stack[-1]] == stack[-2]:
        costly.add(LEFT)
        
    # If there are any dependencies between n0 and the stack,
    # pushing n0 will lose them.
    if SHIFT not in costly and deps_between(n0, stack, gold):
        costly.add(SHIFT)
        
    # If there are any dependencies between s0 and the buffer, popping
    # s0 will lose them.
    if deps_between(stack[-1], range(n0+1, n-1), gold):
        costly.add(LEFT)
        costly.add(RIGHT)
        
    return [m for m in MOVES if m not in costly]
*/    
  }
    
  def train_one(sentence:Sentence):Unit = { process(sentence, true); () }
  def parse(sentence:Sentence):List[Int] = process(sentence, false)
    
  def process(sentence:Sentence, train:Boolean):List[Int] = {
    //print "train_one(%d, n=%d, %s)" % (itn, n, words, )
    //print " gold_heads = %s" % (gold_heads, )
    
    // NB: Our structure just has a 'pure' list of sentences.  The root will point to (n)
    // Previously it was assumed that the sentence has 1 entry pre-pended, and the stack starts at {1}
    val tags = tagger.tag(sentence)
  
    def move_through_sentence_from(state: CurrentState): CurrentState = {
      val valid_moves = state.valid_moves
      if(state.parse_complete) {
        if(valid_moves.size > 0) {          // Is this exactly the same as having no valid moves?
          println("Valid Moves left where parse_complete")
        }
        state // This the answer!
      }
      else {
        println(s"  i/n=$state.i/$state.parse.n stack=$state.stack")
        
        //val features = extract_features(words, tags, state)
        val features = Map[Feature, Score]()  // TODO !! 

        // This will produce scores for features that aren't valid too
        val score = perceptron.score(features, if(train) perceptron.current else perceptron.average)
        
        // Sort valid_moves scores into descending order, and pick the best move
        val guess = valid_moves.map( m => (-score(m), m) ).toList.sortBy( _._1 ).head._2 
        
        if(train) {  // Update the perceptron
          //println(f"Training '${word_norm}%12s': ${classes(guessed)}%4s -> ${classes(truth(i))}%4s :: ")
          
          //val gold_moves = get_gold_moves(i, n, stack, parse.heads, gold_heads)
          val gold_moves = Set[Move]() // TODO !! 
          val best = gold_moves.map( m => (-score(m), m) ).toList.sortBy( _._1 ).head._2 
          perceptron.update(best, guess, features.keys)
        }
        
        move_through_sentence_from( state.transition(guess) ) 
      }
    }

    // This annotates the list of words so that parse.heads is its best guess when it finishes
    val final_state = move_through_sentence_from( CurrentState(1, List(0), ParseStateInit(sentence.length)) )
   
    final_state.parse.heads.toList
  }

}

/*
*/

/*
def extract_features(words, tags, n0, n, stack, parse):
    def get_stack_context(depth, stack, data):
        if depth >= 3:
            return data[stack[-1]], data[stack[-2]], data[stack[-3]]
        elif depth >= 2:
            return data[stack[-1]], data[stack[-2]], ''
        elif depth == 1:
            return data[stack[-1]], '', ''
        else:
            return '', '', ''

    def get_buffer_context(i, n, data):
        if i + 1 >= n:
            return data[i], '', ''
        elif i + 2 >= n:
            return data[i], data[i + 1], ''
        else:
            return data[i], data[i + 1], data[i + 2]

    def get_parse_context(word, deps, data):
        if word == -1:
            return 0, '', ''
        deps = deps[word]
        valency = len(deps)
        if not valency: // i.e. valency==0
            return 0, '', ''
        elif valency == 1:
            return 1, data[deps[-1]], ''
        else:
            return valency, data[deps[-1]], data[deps[-2]]

    features = {}
    # Set up the context pieces --- the word (W) and tag (T) of:
    # S0-2: Top three words on the stack
    # N0-2: First three words of the buffer
    # n0b1, n0b2: Two leftmost children of the first word of the buffer
    # s0b1, s0b2: Two leftmost children of the top word of the stack
    # s0f1, s0f2: Two rightmost children of the top word of the stack

    depth = len(stack)
    s0 = stack[-1] if depth else -1

    Ws0, Ws1, Ws2 = get_stack_context(depth, stack, words)
    Ts0, Ts1, Ts2 = get_stack_context(depth, stack, tags)
   
    Wn0, Wn1, Wn2 = get_buffer_context(n0, n, words)
    Tn0, Tn1, Tn2 = get_buffer_context(n0, n, tags)
    
    Vn0b, Wn0b1, Wn0b2 = get_parse_context(n0, parse.lefts, words)
    Vn0b, Tn0b1, Tn0b2 = get_parse_context(n0, parse.lefts, tags)
    
    Vn0f, Wn0f1, Wn0f2 = get_parse_context(n0, parse.rights, words)
    _, Tn0f1, Tn0f2 = get_parse_context(n0, parse.rights, tags)
  
    Vs0b, Ws0b1, Ws0b2 = get_parse_context(s0, parse.lefts, words)
    _, Ts0b1, Ts0b2 = get_parse_context(s0, parse.lefts, tags)

    Vs0f, Ws0f1, Ws0f2 = get_parse_context(s0, parse.rights, words)
    _, Ts0f1, Ts0f2 = get_parse_context(s0, parse.rights, tags)
    
    # Cap numeric features at 5? 
    # String-distance
    Ds0n0 = min((n0 - s0, 5)) if s0 != 0 else 0

    features['bias'] = 1
    # Add word and tag unigrams
    for w in (Wn0, Wn1, Wn2, Ws0, Ws1, Ws2, Wn0b1, Wn0b2, Ws0b1, Ws0b2, Ws0f1, Ws0f2):
        if w:
            features['w=%s' % w] = 1
    for t in (Tn0, Tn1, Tn2, Ts0, Ts1, Ts2, Tn0b1, Tn0b2, Ts0b1, Ts0b2, Ts0f1, Ts0f2):
        if t:
            features['t=%s' % t] = 1

    # Add word/tag pairs
    for i, (w, t) in enumerate(((Wn0, Tn0), (Wn1, Tn1), (Wn2, Tn2), (Ws0, Ts0))):
        if w or t:
            features['%d w=%s, t=%s' % (i, w, t)] = 1

    # Add some bigrams
    features['s0w=%s,  n0w=%s' % (Ws0, Wn0)] = 1
    features['wn0tn0-ws0 %s/%s %s' % (Wn0, Tn0, Ws0)] = 1
    features['wn0tn0-ts0 %s/%s %s' % (Wn0, Tn0, Ts0)] = 1
    features['ws0ts0-wn0 %s/%s %s' % (Ws0, Ts0, Wn0)] = 1
    features['ws0-ts0 tn0 %s/%s %s' % (Ws0, Ts0, Tn0)] = 1
    features['wt-wt %s/%s %s/%s' % (Ws0, Ts0, Wn0, Tn0)] = 1
    features['tt s0=%s n0=%s' % (Ts0, Tn0)] = 1
    features['tt n0=%s n1=%s' % (Tn0, Tn1)] = 1

    # Add some tag trigrams
    trigrams = ((Tn0, Tn1, Tn2), (Ts0, Tn0, Tn1), (Ts0, Ts1, Tn0), 
                (Ts0, Ts0f1, Tn0), (Ts0, Ts0f1, Tn0), (Ts0, Tn0, Tn0b1),
                (Ts0, Ts0b1, Ts0b2), (Ts0, Ts0f1, Ts0f2), (Tn0, Tn0b1, Tn0b2),
                (Ts0, Ts1, Ts1))
    for i, (t1, t2, t3) in enumerate(trigrams):
        if t1 or t2 or t3:
            features['ttt-%d %s %s %s' % (i, t1, t2, t3)] = 1

    # Add some valency and distance features
    vw = ((Ws0, Vs0f), (Ws0, Vs0b), (Wn0, Vn0b))
    vt = ((Ts0, Vs0f), (Ts0, Vs0b), (Tn0, Vn0b))
    d = ((Ws0, Ds0n0), (Wn0, Ds0n0), (Ts0, Ds0n0), (Tn0, Ds0n0),
         ('t' + Tn0+Ts0, Ds0n0), ('w' + Wn0+Ws0, Ds0n0))
    for i, (w_t, v_d) in enumerate(vw + vt + d):
        if w_t or v_d:
            features['val/d-%d %s %d' % (i, w_t, v_d)] = 1
    return features

*/

/* 

def train_both(parser, sentences, nr_iter):
    parser.tagger.start_training(sentences)
    for itn in range(nr_iter):
        corr = 0; total = 0
        random.shuffle(sentences)
        #for words, gold_tags, gold_parse, gold_label in sentences:
        for words, gold_tags, gold_parse in sentences:
            corr += parser.train_one(itn, words, gold_tags, gold_parse)
            if itn < 5:
                parser.tagger.train_one(words, gold_tags)
            total += len(words)
        print itn, '%.3f' % (float(corr) / float(total))
        if itn == 4:  ## Why now?
            parser.tagger.model.average_weights()
    print 'Averaging weights'
    parser.model.average_weights()

*/  


class CGDP {
  def read_CONLL(path:String): List[Sentence] = {
    println(s"read_CONLL(${path})")
    val source = scala.io.Source.fromFile(path).mkString
    val sections = source.split("\\n\\n").toList                 //;println(s"Sections : ${sections.mkString}")
    
    val sentences = sections.map(
      s => {                                                     //;println(s"Section = ${s}")
        val lines = s.split("\\n").toList
        val body  = lines.map( l => {                            //;println(s"Line = ${l}")
          val arr = l.split("\\s+")
          val (raw, pos, dep) = (arr(0), arr(1), arr(2).toInt)
          val dep_ex = if(dep==0) (lines.length+1) else dep
          WordData(raw, pos, dep_ex)
        })
        //WordData("%START%", "%START%") :: WordData("%PAD%", "%PAD%") :: ( body :+ WordData("%ROOT%", "%ROOT%") :+ WordData("%END%", "%END%") )
        body  // Don't pretty up the sentence itself
      }
    )
    sentences
  }
  
  def sentence(s0: String): Sentence = {
    // Protect unusual cases of '.'
    val s1 = s0.replaceAllLiterally("...", " #ELIPSIS# ")
               .replaceAll( """(\d)\.""", """$1#POINT#""")
               .replaceAll( """(\d+)(#POINT#?)(\d*)\.""", """ $1$2$3 """) // Spaces either side of a (decimal) number
    
    // Space out remaining unusual characters
    val space_out = List(",", ".", ":", ";", "$", "\"", "''", "``", "'t", "'m", "'ve", "'d")
    val s2 = space_out.foldLeft(s1){ (str, spaceme) => str.replaceAllLiterally(spaceme, " "+spaceme+" ") }
    
    val s3 = s2.replaceAllLiterally( """#POINT#""", """.""").replaceAllLiterally("#ELIPSIS#", "...") // Undo dot protection 
    
    // case class WordData(raw:Word, pos:ClassName="", dep:DependencyIndex=(-1))
    s3.split("""\s+""").map( word => WordData(word) ).toList
  }
}



object Main extends App {
  def benchmark(f:Unit=>Unit, iters:Int=10, warmup:Int=5) = {
    for(i <- 0 to warmup) { f() }
    val t0 = System.nanoTime()
    for(i <- 0 to iters)  { f() }
    val t1 = System.nanoTime()
    println(f"Elapsed time = ${(t1-t0)/iters/1000/1000.0}%7.2fms, averaged over ${iters} iterations, with ${warmup} iterations warm-up")
  }
  
  override def main(args: Array[String]):Unit = {
    //args.zipWithIndex map { t => println(s"arg[${t._2}] = '${t._1}'") }
    if(args.contains("learn")) {
      val utils = new CGDP
      
      //val sentences = utils.read_CONLL("/home/andrewsm/nltk_data/corpora/dependency_treebank/wsj_0001.dp") // Single File
      val training_sentences = (for (
         (file,i) <- new File("/home/andrewsm/nltk_data/corpora/dependency_treebank/").listFiles.toList.sorted.zipWithIndex
         if( file.getName.endsWith(".dp") )
         //if(i<5)
      ) yield utils.read_CONLL(file.getPath) ).flatMap( a => a ) 
  
      val (classes, tag_dict) = Tagger.classes_and_tagdict(training_sentences)
      //benchmark( Unit=>{ val (classes, tag_dict) = Tagger.classes_and_tagdict(training_sentences) }, 30)
      
      val tagger = new Tagger(classes, tag_dict)
      //benchmark( Unit=>{ tagger.train(training_sentences) }, 10) // Overall efficiency - not dramatic
      tagger.train(training_sentences)
      
      val s = training_sentences(0)
      //benchmark( Unit=>{ tagger.train_one(s) }, 50) // Mainly 'score'
      println(s"""Text = ${s.map(_.raw).mkString(" ")}""")
      println(s"original = ${s}")
      println(s"tagged = ${s.map{_.norm}.zip(tagger.tag(s))}")
      
      if(true) {
        //val fos = new FileOutputStream("tagger-toString.txt")
        val fos = new PrintWriter("tagger-toString.txt")
        fos.write(tagger.toString)
        fos.close
      }
    }
    else if(args.contains("load")) {
      val utils = new CGDP
      
      val file_lines = scala.io.Source.fromFile("tagger-toString.txt").getLines
      val tagger = Tagger.load(file_lines)
      
      val txt="Pierre Vinken, 61 years old, will join the board as a nonexecutive director Nov. 29 ."
      val s = utils.sentence(txt)
      println(s"tagged = ${s.map{_.norm}.zip(tagger.tag(s))}")
    }
    else {
      printf("Usage :\nrun {learn}\n")
    }
    
  }
}
