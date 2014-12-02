Concise Greedy Dependency Parser (in Scala)
===========================================

This Dependency Parser was inspired by the following two blog posts : 

  * http://honnibal.wordpress.com/2013/09/11/a-good-part-of-speechpos-tagger-in-about-200-lines-of-python/

  * http://honnibal.wordpress.com/2013/12/18/a-simple-fast-algorithm-for-natural-language-dependency-parsing/

With original BSD code here : 

  * https://gist.github.com/syllog1sm/10343947

This version includes the original code, but is mostly about implementing the same minimalist ideals, in Scala.

There's a much more complete system (which extends over very many more files, however) 
at Matthew Honnibal's (the original author) GitHub account : https://github.com/syllog1sm/redshift.  


Significant Files
--------------------------------------

Original Python version (as well as a works-for-me version, that's adapted to import from Python's NLTK CONLL files) :
```
./python/honnibal-original-gist.py
./python/concise-greedy-dependency-parser.py 
```

The Scala version :

```
./src/main/scala/ConciseGreedyDependencyParser.scala 
```

Installation
--------------------------------------
As usual for Scala :
```
git clone https://github.com/mdda/ConciseGreedyDependencyParser-in-Scala.git
cd ConciseGreedyDependencyParser-in-Scala
sbt
```

In order to read in a directory's worth of free 'gold tagged' data, download the Python NLTK CONLL files, either by following the instructions at : 
```
http://www.nltk.org/data.html
```

or by downloading directly from : ```http://www.nltk.org/nltk_data/```, the dataset that is particularly useful is : ```Dependency Parsed Treebank```.
At the time of writing, a direct link is : ```http://nltk.github.com/nltk_data/packages/corpora/dependency_treebank.zip```.

Once the file archive has been downloaded and the files extracted, 
search the Scala source for ```nltk_data``` and update the path to point to the appropriate directory.



Running
--------------------------------------

Within the ```sbt``` environment, the following commands will do something: 

```
run learn tagger
run learn tagger save
run learn deps
run learn deps save
run learn both save
run test tagger
run test gold
# TODO : run test deps
```

Upcoming
--------------------------------------

Adding a ZeroMQ mode, so that the trained Parser can respond to incoming requests
dynamically.  

Of course, a REST interface would also be possible, but (for reasons beyond this implementation),
there's more value in making it work a part of a ZeroMQ toolchain.  Indeed, 
[this ZeroMQ/Clojure blog post](http://augustl.com/blog/2013/zeromq_instead_of_http/) 
makes it clear that there are many desirable properties of 'HTTP' semantics over ZeroMQ.



