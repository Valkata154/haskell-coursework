# General Remarks

In this coursework you will be asked to create functions which solve specific problems. Some
of these problems will be variations of problems you have seen in the practical exercises, and
some will be derived from material we have covered in the lectures. You will be given the typesignatures, the order of the arguments, and you will be required to replace the definitions, of
the functions in the associated cwk.hs file.

# Questions

**Question 1 - Sets**

Write a function (complement) which, given a set A and a prospective universal set U, returns
the complement of A with respect to U, wrapped in the Just type constructor. Note that if the
set A is not a subset of U, then you should return Nothing. (3 marks)

Multisets are sets which allow multiple copies of the same element, but are unordered. In list
form, for example, [1,1,2,3] is equal to [1,2,3,1], but not to [1,2,3]. Multisets can be
represented as lists of pairs (a, n), which indicates that the element a appears n times in the
multiset. Write a function toMultiset which takes a list of elements, and returns a list of pairs,
representing the multiset. Note you do not need to worry about the order of the pairs in the
new list. (2 marks)

Write a function (mIntersect) that returns the multiset intersection of two multisets, using the
previous function. Note that you do not need to worry about the order of the elements in your
return value. (1 mark)

**Question 2 - Functions and Relations**

The transitive closure of a relation R is the smallest relation bigger than R which is transitive.
In other words, it is R with whatever pairs added to make R transitive. Write a function
(transClosure) which takes a list of pairs (standing for R) and returns a list of pairs which is
the transitive closure of R. You do not need to worry about the order in which pairs appear in
your return value. (3 marks)

**Question 3 - Combinatorics**

Write a function (missing2) which takes a list of n distinct integers, xs, and returns a list of
lists of (n − 2) integers which encode all possible ways of choosing subsets of size (n − 2) from
xs. Order each list so that the smallest integer appears first in that list; i.e. [1, 3, 4] not [1, 4, 3].
(3 marks)

**Question 4 - Primes**

Special instructions. For this question, the test cases are ranked in terms of size:

• a 2-digit number (1 mark)

• a 4-digit number (1 mark)

• a 5-digit number (2 marks)

• a 6-digit number (2 marks)

• a 10-digit number (4 marks)

Write a function (nextPrimes) which takes an integer n and returns the three smallest primes
(in order) bigger than n.

Write a function (primeFactorisation) which takes an integer n and returns a list representing
the prime factorisation of n. In other words, it is possible that elements in your list will appear
more than once. It should be the case that if you multiply all of the numbers in your list
together, you get n.

**Question 5 - RSA**

Write a function (eTotient) which takes an integer n, and returns the number of integers less
than n which are coprime with n. (2 marks)

Write a function (encode) which takes two numbers p and q, a message m and a number e and,
if p, q and e are suitable for use in the RSA encryption algorithm, returns the encoded message
wrapped in a Just type constructor. If they are unsuitable, return Nothing. (2 marks)
