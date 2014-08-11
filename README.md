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

or by downloading directly from : ```http://www.nltk.org/nltk_data/```.

The dataset that is particularly useful is : ```Dependency Parsed Treebank```.


Then search the Scala file for ```nltk_data``` and update the path to point to the appropriate directory.



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
