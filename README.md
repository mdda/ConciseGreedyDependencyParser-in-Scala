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

As background material, there's also a [presentation about this project](http://redcatlabs.com/2014-12-10_ParsingEnglish.scala/) that I gave to the [Singapore Scala MeetUp Group](http://www.meetup.com/Singapore-Scala-Programmers/events/218727190/).


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

In order to ease development, the ```build.scala``` is set up to fork 
the scala code that's being run - and a bash script ``` ./kill-CGDP``` will kill
the child process without bringing down the sbt session.


Python ZeroMQ Utilities
--------------------------------------

For convenience, there are simple Python ZeroMQ ```client``` and ```broker``` 
programs that can send valid requests to the (scala) server.  
These utilities are in the ```./python``` directory.

To get them running, the ```zeromq-devel``` library should be installed (as above), and also the Python ZeroMQ library 
(this can also easily be done within a ```virtualenv```, which is nice from an isolation point-of-view): 
```
pip install pyzmq
```

The Python ZeroMQ ```broker``` (which should be left running, since it binds() to a pair of ports, and bridges them) can be started :
```
python python/concise-greedy-dependency-parser-broker-zmq.py &
```
(this runs without any output).

The client program can easily be updated for testing :
```
python python/concise-greedy-dependency-parser-client-zmq.py
```

It will produce output in the following form (newlines added for clarity) :
```
Client is connected to socket on broker frontend
Sending Request # 1
Received reply # 1 >{
 "status":200,
 "body":[
  [{
   "words":["Between","#YEAR#","and","#YEAR#",",","Ms","Ding","worked",
            "in","the","Shenzhen","office","of","Yixing","Silk-Linen","Factory",
            "and","was","in","charge","of","regional","sales","."],
   "tags": ["NNP","CD","CC","CD",",","NNP","NNP","VBD",
            "IN","DT","NNP","NN","IN","VBG","NNP","NNP",
            "CC","VBD","IN","NN","IN","JJ","NNS","."],
   "structure":[7,3,3,0,7,6,7,24,7,11,11,8,11,12,15,13,7,7,17,18,19,22,20,7]
  }]
 ]
}<
```
