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
    val gold_tags:List[ClassNum] = if(train) (List(-1,-1) ::: sentence.map( wd => getClassNum(wd.pos) ) ::: List(-1,-1)) else Nil

    val (i_final, all_tags) =
    words_norm.foldLeft( (2:Int, List[ClassName]("%START%","%PAD%")) ) { case ( (i, tags), word_norm ) => { 
      val guess = tag_dict.getOrElse(word_norm, {   // Don't do the feature scoring if we already 'know' the right PoS
        val features = get_features(words, tags, i)
        val score = perceptron.score(features, if(train) perceptron.current else perceptron.average)
        val guessed = perceptron.predict( score )
        
        if(train) {// Update the perceptron
          //println(f"Training '${word_norm}%12s': ${classes(guessed)}%4s -> ${classes(gold_tags(i))}%4s :: ")
          perceptron.update( gold_tags(i), guessed, features.keys)
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

object DependencyMaker {
 def load(lines:Iterator[String], tagger:Tagger):DependencyMaker = {
    val dm = new DependencyMaker(tagger)
    dm.perceptron.load(lines)
    dm
  }
  
}

class DependencyMaker(tagger:Tagger) {
  val SHIFT:Move=0; val RIGHT:Move=1; val LEFT:Move=2; val INVALID:Move=(-1)
  val move_names = Vector[ClassName]("SHIFT", "RIGHT", "LEFT")
  println(s"DependencyMaker.Classes = [$move_names]")
  //val getClassNum = classes.zipWithIndex.toMap.withDefaultValue(-1) // -1 => "CLASS-NOT-FOUND"
  
  val perceptron = new Perceptron(move_names.length)

  case class ParseState(n:Int, heads:Vector[Int], lefts:Vector[List[Int]], rights:Vector[List[Int]]) { // NB: Insert at start, not at end...
      // This makes the word at 'child' point to head and adds the child to the appropriate left/right list of head
      def add(head:Int, child:Int) = {
        //println(s"ParseState.add(child=$child, head=$head)")
        //println(s"ParseState :: ${this}")
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
    val heads = Vector.fill(n)(0:Int) // i.e. (0, .., n-1)
    
    // Each possible head (including ROOT) has a (lefts) and (rights) list, initially none
    // Entries (0, ..., n-1) are words, (n) is the 'ROOT'  ('to' is INCLUSIVE)
    val lefts  = (0 to n).map( i => List[Int]() ).toVector
    val rights = (0 to n).map( i => List[Int]() ).toVector
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
      println(s"valid moves = ${valid}")
      
      if(stack.length==0 || ( valid.contains(SHIFT) && gold_heads(i) == stack.head)) {
        println(" gold move shortcut : SHIFT")
        Set(SHIFT) // First condition obvious, second rather weird
      }
      else if( gold_heads(stack.head) == i ) {
        println(" gold move shortcut : LEFT")
        Set(LEFT) // This move is a must, since the gold_heads tell us to do it
      }
      else {
        val python_logic = {
          // HMM  : Actually, it looks like this logic (from Python) can be flipped over : 
          //        by constructing a 'val non_gold' and return 'valid - non_gold'
          val all_moves = Set(SHIFT, RIGHT, LEFT)
          var costly = all_moves -- valid  // i.e. all invalid moves are 'costly'
          println(s" costly moves : ${costly}")
          
          // If the word second in the stack is its gold head, Left is incorrect
          if( stack.length >= 2 && gold_heads(stack.head) == stack.tail.head ) {
            costly += (LEFT)
          }
          
          // If there are any dependencies between i and the stack, pushing i will lose them.
          if( deps_between(i, stack) ) {  // This is redundent / over-cautious :: !costly.contains(SHIFT) && 
            costly += (SHIFT)
          }
          
          // If there are any dependencies between the stackhead and the remainder of the buffer, popping the stack will lose them.
          if( deps_between(stack.head, ((i+1) until (parse.n-1)).toList) ) { // UNTIL is EXCLUSIVE of top
            costly += (LEFT)
            costly += (RIGHT)
          }
          println(s" costly moves finally : ${costly}")
          
          (all_moves -- costly)
        }
        val new_logic = {
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
        if( new_logic != python_logic ) { // Test the two different approaches for equality
          throw new Exception(s"Gold Move Logic differs : $new_logic != $python_logic")          
        }
        
        python_logic
      }
    }
    
    def extract_features(words:Vector[Word], tags:Vector[ClassName]):Map[Feature,Score] = {
      val feature_set = mutable.Set[Feature]() 
      
      def get_stack_context[T<:String](data:Vector[T]):(T,T,T) = ( // Applies to both Word and ClassName (depth is implict from stack length)
        // NB: Always expecting 3 entries back...
        if(stack.length>0) data(stack(0)) else "".asInstanceOf[T],
        if(stack.length>1) data(stack(1)) else "".asInstanceOf[T],
        if(stack.length>2) data(stack(2)) else "".asInstanceOf[T]
      )
      
      def get_buffer_context[T<:String](data:Vector[T]):(T,T,T) = ( // Applies to both Word and ClassName (depth is implict from stack length)
        // NB: Always expecting 3 entries back...
        if(i+0 < parse.n) data(i+0) else "".asInstanceOf[T],
        if(i+1 < parse.n) data(i+1) else "".asInstanceOf[T],
        if(i+2 < parse.n) data(i+2) else "".asInstanceOf[T]
      )
      
      def get_parse_context[T<:String](idx:Int, deps:Vector[List[Int]], data:Vector[T]):(Int,T,T) = { // Applies to both Word and ClassName (depth is implict from stack length)
        if(idx<0) { // For the cases of empty stack
          (0, "".asInstanceOf[T], "".asInstanceOf[T]) 
        }
        else {
          val dependencies = deps(idx) // Find the list of dependencies at this index
          val valency = dependencies.length
          // return the tuple here :
          ( valency, 
            if(valency > 0) data(dependencies(0)) else "".asInstanceOf[T],
            if(valency > 1) data(dependencies(1)) else "".asInstanceOf[T]
          )
        }
      }

      // Set up the context pieces --- the word (W) and tag (T) of:
      //   s0,1,2: Top three words on the stack
      //   n0,1,2: Next three words of the buffer (inluding this one)
      //   n0b1, n0b2: Two leftmost children of the current buffer word
      //   s0b1, s0b2: Two leftmost children of the top word of the stack
      //   s0f1, s0f2: Two rightmost children of the top word of the stack

      val n0 = i // Just for notational consistency
      val s0 = if(stack.isEmpty) -1 else stack.head

      val (ws0, ws1, ws2) = get_stack_context(words)
      val (ts0, ts1, ts2) = get_stack_context(tags)

      val (wn0, wn1, wn2) = get_buffer_context(words)
      val (tn0, tn1, tn2) = get_buffer_context(tags)
    
      val (vn0b, wn0b1, wn0b2) = get_parse_context(n0, parse.lefts, words)
      val (_   , tn0b1, tn0b2) = get_parse_context(n0, parse.lefts, tags)

      val (vn0f, wn0f1, wn0f2) = get_parse_context(n0, parse.rights, words)
      val (_,    tn0f1, tn0f2) = get_parse_context(n0, parse.rights, tags)
      
      val (vs0b, ws0b1, ws0b2) = get_parse_context(s0, parse.lefts, words)
      val (_,    ts0b1, ts0b2) = get_parse_context(s0, parse.lefts, tags)
    
      val (vs0f, ws0f1, ws0f2) = get_parse_context(s0, parse.rights, words)
      val (_,    ts0f1, ts0f2) = get_parse_context(s0, parse.rights, tags)
      
          
      //  String-distance :: Cap numeric features at 5? (NB: n0 always > s0, by construction)
      val dist = if(s0 >= 0) math.min(n0 - s0, 5) else 0  // WAS :: ds0n0
      
      feature_set += Feature("bias", "")  // It's useful to have a constant feature, which acts sort of like a prior

      // Add word and tag unigrams
      List(wn0, wn1, wn2, ws0, ws1, ws2, wn0b1, wn0b2, ws0b1, ws0b2, ws0f1, ws0f2).foreach(
        word => { if(word!=0) feature_set += Feature("w", word) }
      )
      List(tn0, tn1, tn2, ts0, ts1, ts2, tn0b1, tn0b2, ts0b1, ts0b2, ts0f1, ts0f2).foreach(
        tag  => { if(tag!=0) feature_set += Feature("t", tag) }
      )
      
      // Add word/tag pairs
      List((wn0, tn0), (wn1, tn1), (wn2, tn2), (ws0, ts0)).zipWithIndex.foreach{ 
        case ((word,tag), idx) => { if( word!=0 || tag!=0 ) feature_set += Feature(s"wt$idx", s"w=$word t=$tag") }
      }

      // Add some bigrams
      feature_set += Feature("w s0n0", s"$ws0 $wn0")
      feature_set += Feature("t s0n0", s"$ts0 $tn0")
      feature_set += Feature("t n0n1", s"$tn0 $tn1")

      // Add some trigrams
      feature_set += Feature("wtw nns", s"$wn0/$tn0 $ws0")
      feature_set += Feature("wtt nns", s"$wn0/$tn0 $ts0")
      feature_set += Feature("wtw ssn", s"$ws0/$ts0 $wn0")
      feature_set += Feature("wtt ssn", s"$ws0/$ts0 $tn0")

      // Add a quadgram
      feature_set += Feature("wtwt", s"$ws0/$ts0 $wn0/$tn0")
      
      // Add some tag trigrams
      List((tn0, tn1, tn2),     (ts0, tn0, tn1),     (ts0, ts1, tn0),    (ts0, ts1, ts1), 
           (ts0, ts0f1, tn0),   (ts0, ts0f1, tn0),   (ts0, tn0, tn0b1),
           (ts0, ts0b1, ts0b2), (ts0, ts0f1, ts0f2), 
           (tn0, tn0b1, tn0b2)
          ).zipWithIndex.foreach{
        case ((t0,t1,t2), idx) => { if( t0!=0 || t1!=0 || t2!=0 ) feature_set += Feature(s"ttt-$idx", s"$t0 $t1 $t2") }
      }
      
      // Add some valency and distance features
      List( (ws0, vs0f), (ws0, vs0b), (wn0, vn0b),
            (ts0, vs0f), (ts0, vs0b), (tn0, vn0b),
            (ws0, dist), (wn0, dist), (ts0, dist), (tn0, dist),
            ("t"+tn0+ts0, dist), ("w"+wn0+ws0, dist)
          ).zipWithIndex.foreach{
        case ((str, v), idx) => { if( str.length>0 || v!=0 ) feature_set += Feature(s"val$idx", s"$str $v") }
      } 
    
      // All weights on this set of features are ==1
      feature_set.map( f => (f, 1:Score) ).toMap
    }
/*    
*/    
  }
  
  // TODO : Shuffle sentences, based on seed :: http://stackoverflow.com/questions/11040399/scala-listbuffer-or-equivalent-shuffle
  def train(sentences:List[Sentence]):Unit = sentences.foreach( train_one )
  
  def train_one(sentence:Sentence):Unit = { process(sentence, true); () }
  def parse(sentence:Sentence):List[Int] = process(sentence, false)
    
  def process(sentence:Sentence, train:Boolean):List[Int] = {
    // NB: Our structure just has a 'pure' list of sentences.  The root will point to (n)
    // Previously it was assumed that the sentence has 1 entry pre-pended, and the stack starts at {1}
    
    // These should be Vectors, since we're going to be accessing them at random (not sequentially)
    val words      = sentence.map( _.norm ).toVector
    val tags       = tagger.tag(sentence).toVector
    val gold_heads = sentence.map( _.dep ).toVector
  
    //print "train_one(n=%d, %s)" % (n, words, )
    //print " gold_heads = %s" % (gold_heads, )
    
    def move_through_sentence_from(state: CurrentState): CurrentState = {
      val valid_moves = state.valid_moves
      if(state.parse_complete) {
        if(valid_moves.size > 0) {          // Is this exactly the same as having no valid moves?
          throw new Exception("Valid Moves left where parse_complete")          
        }
        state // This the answer!
      }
      else {
        println(s"  i/n=${state.i}/${state.parse.n} stack=${state.stack}")
        val features = state.extract_features(words, tags)

        // This will produce scores for features that aren't valid too
        val score = perceptron.score(features, if(train) perceptron.current else perceptron.average)
        
        // Sort valid_moves scores into descending order, and pick the best move
        val guess = valid_moves.map( m => (-score(m), m) ).toList.sortBy( _._1 ).head._2 
        
        if(train) {  // Update the perceptron
          //println(f"Training '${word_norm}%12s': ${classes(guessed)}%4s -> ${classes(truth(i))}%4s :: ")
          val gold_moves = state.get_gold_moves(gold_heads)
          if(gold_moves.size == 0) {
            throw new Exception("No Gold Moves!")          
          }
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

  override def toString():String = {
    perceptron.toString
  }
}

/*
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
          // CONLL dependency layout assumes [root, word1, word2, ..., wordn]  (where n == lines.length)
          // our   dependency layout assumes [word0, word1, ..., word(n-1)] { root }
          val dep_ex = if(dep==0) (lines.length+1-1) else (dep-1)    // This differs from Python version by the (-1)
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
    
    s3.split("""\s+""").map( word => WordData(word) ).toList
  }


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
    val utils = new CGDP
    
    //val sentences = utils.read_CONLL("/home/andrewsm/nltk_data/corpora/dependency_treebank/wsj_0001.dp") // Single File
    val training_sentences = (for (
       (file,i) <- new File("/home/andrewsm/nltk_data/corpora/dependency_treebank/").listFiles.toList.sorted.zipWithIndex
       if( file.getName.endsWith(".dp") )
       //if(i<5)
    ) yield utils.read_CONLL(file.getPath) ).flatMap( a => a ) 
    
    if(args.contains("train")) {
      if(args.contains("tagger") || args.contains("both")) {
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
        
        if(args.contains("save") ) {
          //val fos = new FileOutputStream("tagger-toString.txt")
          val fos = new PrintWriter("tagger-toString.txt")
          fos.write(tagger.toString)
          fos.close
        }
      }
      if(args.contains("deps") || args.contains("both")) {
        // First, load the tagger
        val file_lines = scala.io.Source.fromFile("tagger-toString.txt").getLines
        val tagger = Tagger.load(file_lines)
        
        // Now instatiate a new DependencyMaker
        val dm = new DependencyMaker(tagger)
        
        //benchmark( Unit=>{ dm.train(training_sentences) }, 10) // Overall efficiency - not dramatic
        dm.train(training_sentences)
        
        val s = training_sentences(0)
        //dm.train_one(s)
/*
        //benchmark( Unit=>{ dm.train_one(s) }, 50) // Mainly 'score'
        println(s"""Text = ${s.map(_.raw).mkString(" ")}""")
        println(s"original = ${s}")
        println(s"dependencies = ${s.map{_.norm}.zip(dm.parse(s))}")
*/
        if(args.contains("save") ) {
          val fos = new PrintWriter("dependencies-toString.txt")
          fos.write(dm.toString)
          fos.close
        }
      }
      
    }
    else if(args.contains("test")) {
      val utils = new CGDP
      
      val file_lines = scala.io.Source.fromFile("tagger-toString.txt").getLines
      val tagger = Tagger.load(file_lines)
      
      val txt="Pierre Vinken, 61 years old, will join the board as a nonexecutive director Nov. 29 ."
      val s = utils.sentence(txt)
      println(s"tagged = ${s.map{_.norm}.zip(tagger.tag(s))}")
    }
    else {
      printf("Usage :\nrun {train|test} {tagger|deps|both} {save}\n")
    }
    
  }
}
