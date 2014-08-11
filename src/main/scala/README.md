Updates / Issues for Scala conversion
===========================================

Internal representation
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




Scala lists are quicker to prepend to, so we're checking stack.head (== stack[0]) rather than stack[-1]

gold_moves logic is essentially 'all_moves - (all_moves - valid_moves - bad_moves)', which can be flipped around

Odd : in get_features() :: "if(stack0 != 0)" should probably be "if(stack0>=0)" (since -1 is the bum value)

//confusionmatrix == UNUSED  
//DefaultList(list:List[Int], default:Int=0)  Never gets tested outside of its bounds (careful guards in get_features())

//if(stack.length>=1)  LEFT  else INVALID // Original version
if(stack.length>=1 && stack.head != n)  LEFT  else INVALID // See page 405 for second condition

// "range(i+1, n-1)" should be "range(i+1, n)" //  since range(i,j) is [i,j)
val dont_pop_stack = deps_between(stack.head, ((i+1) until (parse.n)).toList) // UNTIL is EXCLUSIVE of top

10 iterations : 23s,  1.8Mb of data (800k zipped):
Tagger Performance = Vector(0.8492446, 0.9186569, 0.9406191, 0.95281583, 0.9603189, 0.9662161, 0.971103, 0.9758865, 0.9780635, 0.97971076)

15 iterations : 350s, 14Mb of data (6Mb zipped):
Dependency Performance = Vector(0.7401748, 0.8171352, 0.8462659, 0.8658807, 0.8850597, 0.8982282, 0.90726405, 0.9184, 0.9264295, 0.9348865, 0.9400386, 0.9435337, 0.94961494, 0.9521072, 0.95588624)
