// To see it working within sbt, do a : 
// run-main ConciseGreedyDependencyParser.Main XYZ
// or (if this is the only package with a main() in it) :
// run XYZ  

package ConciseGreedyDependencyParser

case class DefaultList(list:List[String], default:String="") {
  def apply(idx: Int): String = if(idx>=0 || idx<list.length) list(idx) else default
}

import scala.collection.mutable
object Perceptron {
  type ClassName = String
}
class Perceptron(classes:Vector[Perceptron.ClassName]) {
  type Feature = String
  
  type ClassNum = Int
  
  // The following are keyed on feature (to keep tally of total numbers into each, and when)(for the TRAINING phase)
  case class WeightLearner(current:Int, total:Int, ts:TimeStamp) 
  val learning = mutable.Map.empty[Feature, mutable.Map[ClassNum, WeightLearner]] // This is hairy and mutable...
  
  // Number of instances seen - used to measure how 'old' each total is
  var ts:TimeStamp = 0
  

  // Keyed on feature, then on class# (as a map), to give us accurate weight for that class (for the USAGE phase)
  type Weight   = Float
  val weights  = mutable.Map.empty[Feature, mutable.Map[ClassNum, Weight]]  // This is hairy and mutable...
  
  
  type Score = Float
  type ClassVector = Vector[Score]
  
  def predict(features: Map[Feature, Score]): Perceptron.ClassName = { // Return best class guess for these features-with-weights
    val classnum_vector = score(features)
    // Find the best classnum for this vector, in vector order (stabilizes) ///NOT : (and alphabetically too)
    val best_classnum = classnum_vector.zipWithIndex.maxBy(_._1)._2
    classes(best_classnum)
  }
  
  def score(features: Map[Feature, Score]): ClassVector = { // This is based on the 
    features
      .filter( pair => pair._2 != 0 )  // if the 'score' multiplier is zero, skip
      .foldLeft( Vector.fill(classes.length)(0) ){ case (acc, (feature, score)) => {  // Start with a zero classnum->score vector
        weights.getOrElse(feature, Map.empty) // This is a Map of ClassNums to Weights (or NOOP if not there)
          .foldLeft( acc ){ case (acc_for_feature, (classnum, weight)) => { // Add each of the class->weights onto our score vector
            acc_for_feature.updated(classnum, acc_for_feature(classnum) + score * weight)
          }}
      }}
  }
  
  def update(truth:Perceptron.ClassName, guess:Perceptron.ClassName, features:List[Feature]): Unit = {
    ts += 1
    if(truth != guess) {
      // For each of the features, add 1 to truth, subtract 1 from guess
      // and keep track of 'totals' and 'ts'
      
      /*
       Ahh : Now, reading the original blog, I see the issue about the AveragePerceptron
       The totals/ts are to keep track of the average value of the weight over the whole training cycle
       But the current predictions are made just using the current weights (without the to-date averaging feature)
      
       Later, when an 'average_weights' is done, everything could be popped into the weights of a Read-Only structure...
       EXCEPT: That honnibal code breaks its own rule, and does an 'average_weights' on ITER=4 for some reason!
      
        So: Question is whether 
            a) to have two objects, one which is RO (and immutable) after training 
                (and the other with the tracking stuff for during training, potentially mutable)
                The load/save can be done on the post 'average_weights' call, and crunched smaller - 
                however this looses the state of the 'most current' weights
            b) have one Trainable object, which can also be used for prediction later (potentially mutable, as here)
      
        The 'thing' seems to be that :
            i)  the {Perceptron with non-averaging weights being used to guide updating} goes about the job of fixing 
                up the average case, and the follows up by targetting the remaining error cases really effectively.
            ii) But the average_weights version is better for usage in the field, since it is less training-order sensitive
            
            :: probably a good idea to do 'scoring' in 2 different ways: Using AverageWeight or CurrentWeight
               - this would mean having 2 different prediction things too (or pass the relevant scoring function)
               
        To make the stored images on disk small, should have two distinct objects, one Averaged ={total/ts}, one with {current, total, ts}
        But this is 'on the face of it' a small saving, since the pickled parser is 34Mb.
        
        Simply storing the trio (since ts==const, almost WLOG) {current, total, ts} would be more flexible code-wise, 
        at small loss of flexbility. The float 'average' would be generated on-demand from total & ts.
        
        In fact, the float 'average' can be computed on demand - no need to store it at all (hardly any time saving).
        
        More space savings available by splitting out the feature header independent of the feature values
        
      */
      
      // TODO
    }
    // else SMELL
  }
  
/*
    def update(self, truth, guess, features):       
        def upd_feat(c, f, w, v):
            param = (f, c)
            self._totals[param] += (self.i - self._tstamps[param]) * w
            self._tstamps[param] = self.i
            self.weights[f][c] = w + v

        self.i += 1
        if truth == guess:
            return None
        for f in features:
            weights = self.weights.setdefault(f, {})
            upd_feat(truth, f, weights.get(truth, 0.0), 1.0)
            upd_feat(guess, f, weights.get(guess, 0.0), -1.0)
*/  
  
}


/* class Perceptron(object):
    def __init__(self, classes=None):
    
        # Each feature gets its own weight vector, so weights is a dict-of-arrays
        self.classes = classes
        self.weights = {}
        
        # The accumulated values, for the averaging. These will be keyed by feature/class tuples
        self._totals = defaultdict(int)
        
        # The last time the feature was changed, for the averaging. Also keyed by feature/class tuples
        # (tstamps is short for timestamps)
        self._tstamps = defaultdict(int)
        
        # Number of instances seen
        self.i = 0

    def predict(self, features):
        '''Dot-product the features and current weights and return the best class.'''
        scores = self.score(features)
        
        # Do a secondary alphabetic sort, for stability
        return max(self.classes, key=lambda clas: (scores[clas], clas))

    def score(self, features):
        all_weights = self.weights
        scores = dict((clas, 0) for clas in self.classes)
        for feat, value in features.items():
            if value == 0:
                continue
            if feat not in all_weights:
                continue
            weights = all_weights[feat]
            for clas, weight in weights.items():
                scores[clas] += value * weight
        return scores
*/
/*
    def update(self, truth, guess, features):       
        def upd_feat(c, f, w, v):
            param = (f, c)
            self._totals[param] += (self.i - self._tstamps[param]) * w
            self._tstamps[param] = self.i
            self.weights[f][c] = w + v

        self.i += 1
        if truth == guess:
            return None
        for f in features:
            weights = self.weights.setdefault(f, {})
            upd_feat(truth, f, weights.get(truth, 0.0), 1.0)
            upd_feat(guess, f, weights.get(guess, 0.0), -1.0)

    def average_weights(self):
        for feat, weights in self.weights.items():
            new_feat_weights = {}
            for clas, weight in weights.items():
                param = (feat, clas)
                total = self._totals[param]
                total += (self.i - self._tstamps[param]) * weight
                averaged = round(total / float(self.i), 3)
                if averaged:
                    new_feat_weights[clas] = averaged
            self.weights[feat] = new_feat_weights

    def save(self, path):
        print "Saving model to %s" % path
        pickle.dump(self.weights, open(path, 'w'))

    def load(self, path):
        self.weights = pickle.load(open(path))


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


def read_conll_mdda(loc):
    print "read_conll_mdda(%s)" % (loc, )
    for sent_str in open(loc).read().strip().split('\n\n'):
        lines = [line.split() for line in sent_str.split('\n')]
        
        words = DefaultList('')
        tags  = DefaultList('')
        heads = [None]
        for i, (word, pos, head) in enumerate(lines):
            #print "%d = %s" % (i, word)
            words.append(intern(word))
            #words.append(intern(normalize(word)))
            tags.append(intern(pos))
            heads.append(int(head) if head != '0' else len(lines) + 1) # mdda : don't increment our file...
        pad_tokens(words)
        pad_tokens(tags)
        #print "END OF SENTENCE"
        yield words, tags, heads


def pad_tokens(tokens):
    tokens.insert(0, '<start>')
    tokens.append('ROOT')


def learn_mdda(model_dir, train_loc, load_if_exists=False):
    if not os.path.exists(model_dir):
        os.mkdir(model_dir)
    
    random.seed(04)   # Has some successes, the failure on assert(gold_moves)

    parser = Parser(load=load_if_exists)
    sentences = list()
    for f in [f for f in os.listdir(train_loc) if os.path.isfile(os.path.join(train_loc, f)) and f.endswith(".dp")]:
        sentences.extend(list(read_conll_mdda(os.path.join(train_loc, f))))
        #break # Just 1 set of sentences to start
    #print sentences
    train(parser, sentences, nr_iter=50)
    parser.save()
    
    return 
  
*/  

object Main extends App {
  override def main(args: Array[String]):Unit = {
    //args.zipWithIndex map { t => println(s"arg[${t._2}] = '${t._1}'") }
    if(args.contains("learn")) {
      // learn_mdda("models", "/home/andrewsm/nltk_data/corpora/dependency_treebank/")
    }
    
    printf("Usage :\nrun {learn}\n")
  }
}
