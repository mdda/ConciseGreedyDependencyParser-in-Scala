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

