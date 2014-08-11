"""A simple implementation of a greedy transition-based parser. Released under BSD license."""
from os import path
import os
import sys

from collections import defaultdict

import random
import time
import pickle

import re # For sentence splitting only

SHIFT = 0; RIGHT = 1; LEFT = 2;
MOVES = (SHIFT, RIGHT, LEFT)
START = ['-START-', '-START2-']
END = ['-END-', '-END2-']


class DefaultList(list):
    """A list that returns a default value if index out of bounds."""
    def __init__(self, default=None):
        self.default = default
        list.__init__(self)

    def __getitem__(self, index):
        try:
            return list.__getitem__(self, index)
        except IndexError:
            return self.default


class Parse(object):
    def __init__(self, n):
        self.n = n
        self.heads = [None] * (n-1)
        #self.labels = [None] * (n-1)
        self.lefts = []
        self.rights = []
        for i in range(n+1):
            self.lefts.append(DefaultList(0))
            self.rights.append(DefaultList(0))

    def add(self, head, child, label=None):
        self.heads[child] = head
        #self.labels[child] = label
        if child < head:
            self.lefts[head].append(child)
        else:
            self.rights[head].append(child)


class Parser(object):
    def __init__(self, load=True):
        model_dir = os.path.join(os.path.dirname(__file__), 'models/')
        self.model = Perceptron(MOVES)
        if load:
            self.model.load(path.join(model_dir, 'parser.pickle'))
        self.tagger = PerceptronTagger(load=load)
        self.confusion_matrix = defaultdict(lambda: defaultdict(int))

    def save(self):
        self.model.save(path.join(os.path.dirname(__file__), 'models/parser.pickle'))
        self.tagger.save()
    
    def parse(self, words):
        n = len(words)
        i = 2; stack = [1]; parse = Parse(n)
        tags = self.tagger.tag(words)
        while stack or (i+1) < n:
            features = extract_features(words, tags, i, n, stack, parse)
            scores = self.model.score(features)
            valid_moves = get_valid_moves(i, n, len(stack))
            guess = max(valid_moves, key=lambda move: scores[move])
            i = transition(guess, i, stack, parse)
        return tags, parse.heads

    def train_one(self, itn, words, gold_tags, gold_heads):
        n = len(words)
        #print "train_one(%d, n=%d, %s)" % (itn, n, words, )
        #print " gold_heads = %s" % (gold_heads, )
        i = 2; stack = [1]; parse = Parse(n)
        tags = self.tagger.tag(words)
        while stack or (i + 1) < n:
            #print "  i/n=%d/%d stack=" % (i,n ), stack
            features = extract_features(words, tags, i, n, stack, parse)
            scores = self.model.score(features)
            
            valid_moves = get_valid_moves(i, n, len(stack))
            guess = max(valid_moves, key=lambda move: scores[move])
            
            gold_moves = get_gold_moves(i, n, stack, parse.heads, gold_heads)
            assert gold_moves
            best = max(gold_moves, key=lambda move: scores[move])
            
            self.model.update(best, guess, features)
            i = transition(guess, i, stack, parse)
            self.confusion_matrix[best][guess] += 1
        return len([i for i in range(n-1) if parse.heads[i] == gold_heads[i]])


def transition(move, i, stack, parse):
    if move == SHIFT:
        stack.append(i)
        return i + 1
    elif move == RIGHT:
        parse.add(stack[-2], stack.pop())
        return i
    elif move == LEFT:
        parse.add(i, stack.pop())
        return i
    assert move in MOVES


def get_valid_moves(i, n, stack_depth):
    moves = []
    if (i+1) < n:
        moves.append(SHIFT)
    if stack_depth >= 2:
        moves.append(RIGHT)
    if stack_depth >= 1:
        moves.append(LEFT)
    return moves


def get_gold_moves(n0, n, stack, heads, gold):
    def deps_between(target, others, gold):
        for word in others:
            if gold[word] == target or gold[target] == word:
                return True
        return False

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
        if not valency:
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


class Perceptron(object):
    def __init__(self, classes=None):
        # Each feature gets its own weight vector, so weights is a dict-of-arrays
        self.classes = classes
        self.weights = {}
        # The accumulated values, for the averaging. These will be keyed by
        # feature/clas tuples
        self._totals = defaultdict(int)
        # The last time the feature was changed, for the averaging. Also
        # keyed by feature/clas tuples
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

    def tag(self, words, tokenize=True):
        prev, prev2 = START
        tags = DefaultList('') 
        context = START + [self._normalize(w) for w in words] + END
        for i, word in enumerate(words):
            tag = self.tagdict.get(word)
            if not tag:
                features = self._get_features(i, word, context, prev, prev2)
                tag = self.model.predict(features)
            tags.append(tag)
            prev2 = prev; prev = tag
        return tags

    def start_training(self, sentences):
        self._make_tagdict(sentences)
        self.model = Perceptron(self.classes)

    """  UNUSED : THERE IS NO end_training here...
    def train(self, sentences, save_loc=None, nr_iter=5):
        '''Train a model from sentences, and save it at save_loc. nr_iter
        controls the number of Perceptron training iterations.'''
        self.start_training(sentences)
        for iter_ in range(nr_iter):
            for words, tags in sentences:
                self.train_one(words, tags)
            random.shuffle(sentences)
        self.end_training(save_loc)
    """
    
    def save(self):
        # Pickle as a binary file
        pickle.dump((self.model.weights, self.tagdict, self.classes),
                    open(PerceptronTagger.model_loc, 'wb'), -1)

    def train_one(self, words, tags):
        prev, prev2 = START
        context = START + [self._normalize(w) for w in words] + END
        for i, word in enumerate(words):
            guess = self.tagdict.get(word)
            if not guess:
                feats = self._get_features(i, word, context, prev, prev2)
                guess = self.model.predict(feats)
                self.model.update(tags[i], guess, feats)
            prev2 = prev; prev = guess

    def load(self, loc):
        w_td_c = pickle.load(open(loc, 'rb'))
        self.model.weights, self.tagdict, self.classes = w_td_c
        self.model.classes = self.classes

    def _normalize(self, word):
        if '-' in word and word[0] != '-':
            return '!HYPHEN'
        elif word.isdigit() and len(word) == 4:
            return '!YEAR'
        elif word[0].isdigit():
            return '!DIGITS'
        else:
            return word.lower()

    def _get_features(self, i, word, context, prev, prev2):
        '''Map tokens into a feature representation, implemented as a
        {hashable: float} dict. If the features change, a new model must be
        trained.'''
        def add(name, *args):
            features[' '.join((name,) + tuple(args))] += 1

        i += len(START)
        features = defaultdict(int)
        # It's useful to have a constant feature, which acts sort of like a prior
        add('bias')
        add('i suffix', word[-3:])
        add('i pref1', word[0])
        add('i-1 tag', prev)
        add('i-2 tag', prev2)
        add('i tag+i-2 tag', prev, prev2)
        add('i word', context[i])
        add('i-1 tag+i word', prev, context[i])
        add('i-1 word', context[i-1])
        add('i-1 suffix', context[i-1][-3:])
        add('i-2 word', context[i-2])
        add('i+1 word', context[i+1])
        add('i+1 suffix', context[i+1][-3:])
        add('i+2 word', context[i+2])
        return features

    def _make_tagdict(self, sentences):
        '''Make a tag dictionary for single-tag words.'''
        counts = defaultdict(lambda: defaultdict(int))
        for sent in sentences:
            #print type(sent[1])
            for word, tag in zip(sent[0], sent[1]):
                #print " %s : %s" % (word, tag, )
                counts[word][tag] += 1
                self.classes.add(tag)
        freq_thresh = 20
        ambiguity_thresh = 0.97
        for word, tag_freqs in counts.items():
            tag, mode = max(tag_freqs.items(), key=lambda item: item[1])
            n = sum(tag_freqs.values())
            # Don't add rare words to the tag dictionary
            # Only add quite unambiguous words
            if n >= freq_thresh and (float(mode) / n) >= ambiguity_thresh:
                self.tagdict[word] = tag
                print "_make_tagdict added : %10s -> %10s" % (word, tag, )

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

def read_pos(loc):
    for line in open(loc):
        if not line.strip():
            continue
        words = DefaultList('')
        tags = DefaultList('')
        for token in line.split():
            if not token:
                continue
            word, tag = token.rsplit('/', 1)
            #words.append(normalize(word))
            words.append(word)
            tags.append(tag)
        pad_tokens(words); pad_tokens(tags)
        yield words, tags


def read_conll(loc):
    print "read_conll(%s)" % (loc, )
    for sent_str in open(loc).read().strip().split('\n\n'):
        lines = [line.split() for line in sent_str.split('\n')]
        
        words = DefaultList('')
        tags  = DefaultList('')
        heads = [None]
        labels = [None]
        for i, (word, pos, head, label) in enumerate(lines):
            #print "%d = %s" % (i, word)
            words.append(intern(word))
            #words.append(intern(normalize(word)))
            tags.append(intern(pos))
            heads.append(int(head) + 1 if head != '-1' else len(lines) + 1)  
            labels.append(label)
        pad_tokens(words)
        pad_tokens(tags)
        yield words, tags, heads, labels

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


def main_orig(model_dir, train_loc, heldout_in, heldout_gold):
    if not os.path.exists(model_dir):
        os.mkdir(model_dir)

    input_sents = list(read_pos(heldout_in))
    parser = Parser(load=False)
    sentences = list(read_conll(train_loc))
    train(parser, sentences, nr_iter=15)
    parser.save()
    c = 0
    t = 0
    gold_sents = list(read_conll(heldout_gold))
    t1 = time.time()
    for (words, tags), (_, _, gold_heads, gold_labels) in zip(input_sents, gold_sents):
        _, heads = parser.parse(words)
        for i, w in list(enumerate(words))[1:-1]:
            if gold_labels[i] in ('P', 'punct'):
                continue
            if heads[i] == gold_heads[i]:
                c += 1
            t += 1
    t2 = time.time()
    print 'Parsing took %0.3f ms' % ((t2-t1)*1000.0)
    print c, t, float(c)/t

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
    
    #input_sents = list(read_pos(heldout_in))
    
    
    c = 0
    t = 0
    gold_sents = list(read_conll(heldout_gold))
    t1 = time.time()
    for (words, tags), (_, _, gold_heads, gold_labels) in zip(input_sents, gold_sents):
        _, heads = parser.parse(words)
        for i, w in list(enumerate(words))[1:-1]:
            if gold_labels[i] in ('P', 'punct'):
                continue
            if heads[i] == gold_heads[i]:
                c += 1
            t += 1
    t2 = time.time()
    print 'Parsing took %0.3f ms' % ((t2-t1)*1000.0)
    print c, t, float(c)/t

def sentence_to_words(sentence):
    s = sentence 
    s = s.replace("...", " #ELIPSIS# ")
    s = s.replace(",", " , ").replace(".", " . ").replace(":", " : ").replace(";", " ; ")
    s = s.replace("$", " $ ")
    s = re.sub(r"(\d+\.?\d*)", r" \1 ", s) 
    s = s.replace("\"", " \" ").replace("''", " '' ").replace("``", " `` ")
    s = s.replace(" #ELIPSIS# ", " ... ")
    s = s.replace("'t", " 't ").replace("'m", " 'm ").replace("'ve", " 've ").replace("'d", " 'd ")
    
    words = DefaultList('')
    for w in s.split(): # On all whitespace
        words.append(w)
    
    return words
    

"""
## Redshift run parameters (for hints)
./scripts/train.py -x zhang+stack -k 8 -p ~/data/stanford/train.conll ~/data/parsers/tmp
./scripts/parse.py ~/data/parsers/tmp ~/data/stanford/devi.txt /tmp/parse/
./scripts/evaluate.py /tmp/parse/parses ~/data/stanford/dev.conll
"""
if __name__ == '__main__':
    #main(sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4])
    
    if True :
        learn_mdda("models", "/home/andrewsm/nltk_data/corpora/dependency_treebank/")

    if True :
        sentences = [
        "Sorry for taking a little longer than just the weekend : Though I think I've now got a good handle on the problem, the given solutions, and have realistic ideas about what you can expect from an implementation...",
        "I've attached a ZIP of your originals, plus some derived entities - which I extracted using open source (non-commercial) software that was trained on 'generic' documents.  ",
        "FWIW, these appear to be Gnosis-level results.", 
        "Having dug into the area a little, I'm convinced that the greatest gains will come from a system that learns as it goes (and will therefore adapt to your specific datasets). "
        "For instance, your Singapore data is very comprehensive, with 'gold standard' entities and relationship already extracted from it. ", 
        "So for new documents, the system would have a lot of additional data to hone its 'hypotheses' on, and should be expected to do pretty well.",
        "For the new Malaysian data (for example), a generic machine learner won't already know that 'Dato'' is a significant marker for Person-hood, but would quickly be able to figure it out once a few examples had been tagged expertly.", 
        "Similarly, the date/period extraction tasks would be improved quickly, which are plainly more important for your work than for 'generic' documents.  "
        "Thus, a cooperative system could be expected to rise fairly quickly to Invoxis-level quality, and grow dynamically beyond that.",
        "Does it make sense for me to put together some notes in a quick presentation (to send to you ~tonight) - and then to follow up with a quick sit-down sometime soon?  ",
        "On one hand : I don't have anything specific to sell you.",
        "On the other hand : these systems are not a one-size-fits-all kind of thing.",
        "Would you be free to meet up and chat some time the week after next? ", 
        "Might be easier if we have a face to face informal discussion, and save you the need to put together a presentation."
        "As discussed here is a sample set of docs for you. ", 
        "The excel file named POC Sample includes a starting example that we gave to the vendor for them to understand what detail and level of structure we need in our output.",
        "Ding Zhigang (the brother of CEO - Ms Ding Zhiying) is our Non-Executive Director and Non-Executive Chairman and was appointed on 8 August 2008. ", 
        "He oversees the Group's overall business strategies for future development. ",
        "From 2005 to the present, he is the general manager of Wuxi Dadi Real Estate Co. ", 
        "He was the president of Wuxi Xin-Dingqiu Real Estate Co. from June 2002 to 2005. ", 
        "Between 1999 and 2001, he was the vice-president of Jiangsu Dingqiu Industrial Co. Ltd. ",
        "Between 1995 and 1999, he was the general manager of China Trading Resource Inc. in the USA.",
        "Ding Zhiying is our CEO and was appointed as Executive Director on 20 April 2004. ",
        "She oversees the day-to-day management and operations of the Group as well as the sales and marketing of the Group. ",
        "She is also responsible for the Group's overall business strategies and policies. ",
        "Prior to joining the Group, Ms Ding was the general manager of Jiangsu Xindingqiu between August 2001 and February 2004. ",
        "Between 1994 and 2000, Ms Ding worked in the Shenzhen office of Yixing Silk-Linen Factory and was in charge of regional sales. ",
        "Prior to that, she was in charge of finance in Yixing Taihua Silk Weaving Factory from 1990 to 1993. ",
        "She obtained an accounting certificate from the Nanjing University, Adult Educational Institute in December 1991.",
        ]
        
        parser = Parser(load=True)
        for s in sentences:
            words = sentence_to_words(s)
            pad_tokens(words)
            
            tags, structure = parser.parse(words)
            
            for (i, (w,t,s)) in enumerate(zip(words, tags, structure)):
                #print i, " : ", w, " - ", t, " - ", s
                if s>=len(words)-1: s='ROOT'
                print "%2d : %20s : %4s - %2s" % (i+0, w, t, s)
        