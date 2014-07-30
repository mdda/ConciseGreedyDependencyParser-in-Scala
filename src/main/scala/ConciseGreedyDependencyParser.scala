// To see it working within sbt, do a : 
// run-main ConciseGreedyDependencyParser.package XYZ
// or (if this is the only package with a main() in it) :
// run XYZ  

package object ConciseGreedyDependencyParser {
  case class DefaultList(list:List[String], default:String="") {
    def apply(idx: Int): String = if(idx>=0 || idx<list.length) list(idx) else default
  }

/*

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

  def main(args: Array[String]) {
    //args.zipWithIndex map { t => println(s"arg[${t._2}] = '${t._1}'") }
    if(args.contains("learn")) {
      // learn_mdda("models", "/home/andrewsm/nltk_data/corpora/dependency_treebank/")
    }
    
    printf("Usage :\nrun {learn}\n")
  }
}
