Updates / Issues for Scala conversion
===========================================

Internal weight representation
------------------------------

The Python version (deliberately) has two modes of operation : Training and 'Real'.

During training, the 'weight' associated with every hash entry corresponds to the current count of plusses and minuses to that item, and 
the total/timestamp is there to record how long the weight has lingered in each state.

The reason for capturing the average weight (based on the overall training period) is that *honnibal* observes that 
the current count is too jittery : It's much more reasonable to use the overall average values during training
when it comes to 'real life' usage of the library.  

Hence the need for the ```average_weights()``` function, which effectively replaces the weights calculated during training with
an average weight figure.

However : This introduces a before/after split.  Once the ```average_weights()``` function is called, the perceptron(s) are in 
'real world' mode, and can't be trained further.

For the Scala version, the three integers (current, total, ts) are always preserved (and stored to disk as integers).  This has the following benefits :

 * The average weights, when required, are calculated on-the-fly (not a significant computation)

 * The perceptrons are always ready to do further training, since sufficient state is stored

 * Despite more data being stored, the disk space used is actually less
 
One argument for doing it this way is that it means that ```score()``` doesn't have to be written twice.  However, Scala is very comfortable with
juggling functions around, so the Scala ```score()``` method can simply be passed either the ```current``` or ```average``` function.


Internal dependency list representation
------------------------------------------------------------

Scala lists are quicker to prepend to, so instead of maintaining ```stack``` using ```.append()``` and ```stack[-1]```, 
the Scala version simply does ``` x :: stack``` and checks ```stack.head```.


Gold Move Logic
------------------------------------------------------------

The ```gold_moves``` logic is essentially ```all_moves - (all_moves - valid_moves - bad_moves)```.  This makes sense in Python, which
doesn't have a nice ```Set``` natively.  But in Scala, it can easily be flipped around.


Unused code
------------------------------------------------------------

There appears to be stale code in the Python code-base : 

 * ```confusionmatrix``` is unused  

 * ```DefaultList(list:List[Int], default:Int=0)``` apparently never gets called outside of its bounds, because of the careful guards in ```get_features()```


Bug?
------------------------------------------------------------

In the Python version, ```get_features()``` contains the following test :

```
if(stack0 != 0)
```

this  should probably be ```if(stack0>=0)``` (since ```-1``` is the invalid value for ```stack0```, ```0``` is a valid, though strange, possibility)


Misc (mainly to do with off-by-one effects)
------------------------------------------------------------

The Python version slightly pre-pads the sentence words, which is actually unnecessary : Fixing this requires
a bunch of minor updates, but it's worth pointing out for consistency purposes.

```
//if(stack.length>=1)  LEFT  else INVALID // Original version
if(stack.length>=1 && stack.head != n)  LEFT  else INVALID // See page 405 for second condition
```

```
// "range(i+1, n-1)" should be "range(i+1, n)" //  since range(i,j) is [i,j)
val dont_pop_stack = deps_between(stack.head, ((i+1) until (parse.n)).toList) // UNTIL is EXCLUSIVE of top
```


Overall performance / stats
------------------------------------------------------------

When training on the 199 file ```dependency_treebank```, we get the following results 
(performance figures are the fraction of correct tags / dependency assignments over the whole of the training set for each iteration)

* Tagger : 10 iterations : 23s.  Saved size on disc : 1.8Mb (800k zipped)

  * Tagger Performance = Vector(0.849, 0.918, 0.940, 0.952, 0.960, 0.966, 0.971, 0.975, 0.978, 0.980)

* Dependency Parser : 15 iterations : 350s.  Saved size on disc : 14Mb of data (6Mb zipped)

  * Dependency Performance = Vector(0.740, 0.817, 0.846, 0.865, 0.885, 0.898, 0.907, 0.918, 0.926, 0.934, 0.940, 0.943, 0.949, 0.952, 0.956)


Overall, the Scala version is longer than the Python one.  However, the basic code is over more concise - except for :

 * the auto-vivification features for maps in Python (needs more boilerplate in Scala)
 
 * the pickling, which is built-in for Python, and is very tedious in Scala :
   
   * There is a scala-pickle library, that is 'coming soon', but that appears to ```x4``` compile times (? because it's macro/implict heavy)
  
   * In the end, it was most expedient to just write the Scala pickling by hand - with the side-effect that the stored files are human-legible
   
 * there are a lot more comments, mainly to help decipher what should be going on
 
 * there is test code to verify that ```gold_moves``` does actually work if the moves themselves are used on a gold sentence
 
 * there's some benchmarking code to test the speed of the mutable / immutable choices
 
 * there are both mutable and immutable versions of ```score``` and ```classes_and_tagdict``` (which is required before a tagger can be instantiated)

