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
create a link in the main directory to the ```nltk_data``` location.  On my machine : 
```
cd ConciseGreedyDependencyParser-in-Scala
ln -s /home/andrewsm/nltk_data .
```

If you want to explicitly link to a specific directory instead, 
search the Scala source for ```nltk_data``` and update the path to point directly to the appropriate directory.


Pre-Installation (sbt from scratch)
--------------------------------------
On my Fedora Linux server, all that is required is the ```zeromq3``` backend library (for the Server functionality), 
and an [installation of ```sbt```](http://www.scala-sbt.org/release/tutorial/Installing-sbt-on-Linux.html) 
(which automatically brings in java, etc as dependencies) : 
```
yum install zeromq3-devel
wget https://dl.bintray.com/sbt/rpm/sbt-0.13.7.rpm
yum localinstall sbt-0.13.7.rpm
```


Running the training / testing
--------------------------------------

Within the ```sbt``` environment, the following commands will do something: 

```
> run learn tagger
> run learn tagger save

> run learn deps
> run learn deps save

> run learn both save

> run test tagger
> run test gold

# TODO : run test deps

> run server <PORT>
```


Running as a ZeroMQ Server
--------------------------------------
Once the models have been trained and saved, the program can be run in server mode.
In server mode, it responds to REP/REQ messages via ZeroMQ.

Of course, an HTTP REST interface would also be possible, but (for reasons beyond this implementation),
there's additional value in making it work a part of a ZeroMQ toolchain.  
Indeed, [this ZeroMQ/Clojure blog post](http://augustl.com/blog/2013/zeromq_instead_of_http/) 
makes it clear that there are many desirable properties of 'HTTP' semantics over ZeroMQ.

If you just want to run it straight from the command line 
(for instance, if you've done a ```run learn both save``` and now you just want to 
use the Parser in server-mode) : 

```
$ sbt "run server <PORT>"
```

For convenience, there's a simple Python ZeroMQ ```client``` that sends a valid request to the server.
There's also a Python ZeroMQ ```broker```, which should be left running, since it binds() to a pair of ports, and bridges them.
These utilities are in the ```./python``` directory.

