// To see it working within sbt, do a : 
// run-main ConciseGreedyDependencyParser.Main XYZ
// or (if this is the only package with a main() in it) :
// run XYZ  

package ConciseGreedyDependencyParser

import scala.collection.mutable

/*
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

case class DefaultList(list:List[String], default:String="") {
  def apply(idx: Int): String = if(idx>=0 || idx<list.length) list(idx) else default
}

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
  
/* 
  // http://stackoverflow.com/questions/23182577/even-trivial-serialization-examples-in-scala-dont-work-why
  // http://stackoverflow.com/questions/23072118/scala-pickling-how
  def save(path: String):Unit = {
    print(s"Saving model to ${path}")
    
    //val fos = new FileOutputStream(path)
    //val pickled = learning.pickle
    //fos.write(learning.pickle)
    
    val pw = new PrintWriter(new File(path))
    // Uncommenting this line increases compile time from 2s to 10s...
    //pw.write(learning.pickle.value)
    pw.close
  }

  def load(path: String):Unit = {
    print(s"Loading Perceptron model to ${path}")
    
    //val pickled = learning.pickle
    //val unpickled = pickled.unpickle[Wrapper]
    //learning =  // Hmm : this is a val of a mutable.Map, may need to copy data in explicitly
    
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
    val (classes, tag_dict)=(Vector[ClassName](), Map[Word, ClassNum]())
    val t = new Tagger(classes, tag_dict)
    //t.perceptron.load()
    t
  }

}

class Tagger(classes:Vector[ClassName], tag_dict:Map[Word, ClassNum]) {
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
    
  def train_one(sentence:Sentence):Unit = {
    //val context:List[Word] = ("%START%" :: "%PAD%" :: (sentence.map( _.norm ) :+ "%ROOT%" :+ "%END%"))
    //val context:List[Word] = (List[Word]("%START%","%PAD%") ::: sentence.map( _.norm ) ::: List[Word]("%ROOT%","%END%"))
    
    val words_norm = sentence.map( _.norm )
    val words:List[Word] = (List("%START%","%PAD%") ::: words_norm ::: List("%ROOT%","%END%"))
    val truth:List[ClassNum] = (List(-1,-1) ::: sentence.map( wd => getClassNum(wd.pos) ) ::: List(-1,-1))

    words_norm.foldLeft( (2:Int, List[ClassName]("%START%","%PAD%")) ) { case ( (i, tags), word_norm ) => { 
      val guess = tag_dict.getOrElse(word_norm, {   // Don't do the feature scoring if we already 'know' the right PoS
        val features = get_features(words, tags, i)
        val score = perceptron.score(features, perceptron.current)
        val guessed = perceptron.predict( score )
        
        // Update the perceptron
        //println(f"Training '${word_norm}%12s': ${classes(guessed)}%4s -> ${classes(truth(i))}%4s :: ")
        perceptron.update( truth(i), guessed, features.keys)
        
        guessed // Use the guessed value for next prediction/learning step (rather than the truth...)
      }) 
      (i+1, tags :+ classes(guess))
    }}
    
  }
  
  def tag(sentence:Sentence):List[ClassName] = {
    val words_norm = sentence.map( _.norm )
    val words:List[Word] = (List("%START%","%PAD%") ::: words_norm ::: List("%ROOT%","%END%"))

    val (i_final, all_tags) =
    words_norm.foldLeft( (2:Int, List[ClassName]("%START%","%PAD%")) ) { case ( (i, tags), word_norm ) => { 
      val guess = tag_dict.getOrElse(word_norm, {   
        val score = perceptron.score(get_features(words, tags, i), perceptron.average) // use the averaging version
        perceptron.predict( score )
      }) 
      (i+1, tags :+ classes(guess))
    }}
    all_tags.drop(2)
  }
  
  override def toString():String = {
    classes.mkString("tagger.classes=[","|","]\n") + 
    tag_dict.map({ case (norm, classnum) => s"$norm:$classnum" }).mkString("tagger.tag_dict=[","|","]\n") +
    "\n" +
    perceptron.toString
  }

}



/* 
class PerceptronTagger(object):
    '''Greedy Averaged Perceptron tagger'''
    model_loc = os.path.join(os.path.dirname(__file__), 'models/tagger.pickle')
    def __init__(self, classes=None, load=True):
        self.tagdict = {}
        if classes:
            self.classes = classes
        else:
            self.classes = set()
        self.model = Perceptron(self.classes)
        if load:
            self.load(PerceptronTagger.model_loc)

    def save(self):
        # Pickle as a binary file
        pickle.dump((self.model.weights, self.tagdict, self.classes),
                    open(PerceptronTagger.model_loc, 'wb'), -1)

    def load(self, loc):
        w_td_c = pickle.load(open(loc, 'rb'))
        self.model.weights, self.tagdict, self.classes = w_td_c
        self.model.classes = self.classes


def _pc(n, d):
    return (float(n) / d) * 100

def train(parser, sentences, nr_iter):
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
      val parser = new CGDP
      
      //val sentences = parser.read_CONLL("/home/andrewsm/nltk_data/corpora/dependency_treebank/wsj_0001.dp") // Single File
      val training_sentences = (for (
         (file,i) <- new File("/home/andrewsm/nltk_data/corpora/dependency_treebank/").listFiles.toList.sorted.zipWithIndex
         if( file.getName.endsWith(".dp") )
         //if(i<5)
      ) yield parser.read_CONLL(file.getPath) ).flatMap( a => a ) 
  
      val (classes, tag_dict) = Tagger.classes_and_tagdict(training_sentences)
      //benchmark( Unit=>{ val (classes, tag_dict) = Tagger.classes_and_tagdict(training_sentences) }, 30)
      
      val tagger = new Tagger(classes, tag_dict)
      //benchmark( Unit=>{ tagger.train(training_sentences) }, 10) // Overall efficiency - not dramatic
      tagger.train(training_sentences)
      
      val s = training_sentences(0)
      //benchmark( Unit=>{ tagger.train_one(s) }, 50) // Mainly 'score'
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
      val file_lines = scala.io.Source.fromFile("tagger-toString.txt").getLines
      val tagger = Tagger.load(file_lines)
      
    }
    else {
      printf("Usage :\nrun {learn}\n")
    }
    
  }
}
