# Racket-Lisp-Project
Implementing functions in Racket for Computational and Formal Systems:

Implemented all the required functions in addition to >10 extra credit functions.
I designed the factors-of function to run as efficiently as possible as well as providing an alternative simple version.

The efficient version logically removes numbers it does not need to check based on previous checks, and stores this information in a vector as it computes. The irrelevant numbers are removed until only divisors are left.

For example, finding factors of 10:

Trival method is to check divisiblity of 10 by [1,2,3,4,5,6,7,8,9,10] --> [1,2,5,10]:
1|10? True, 
2|10? True, 
3|10? False,
4|10? False,
5|10? True,
6|10? False,
7|10? False,
8|10? False,
9|10? False,
10|10? True.

However, if a number does NOT divide 10, then neither will it's multiples and they do not need to be considered...
1|10? True, 
2|10? True, 
3|10? False...Eliminate(3, 6, 9), 
4|10? False...Eliminate(4,8),
5|10? True, 
7|10? False...Eliminate(7), 
10|10? True.

This works almost like a sieve and saves what would otherwise be a great deal of recomputation in the long run.
Racket method implements this by generating a vector of the range from 1 to sqrt(n) (since 10/2 = 5 where 2|10,5|10, factors >sqrt(n) can be extrapolated via multipilcation)
If a number in the vector is not divisible, every subsequent multiple is marked then filtered out and returned.
