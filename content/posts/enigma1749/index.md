+++
title = "Enigma #1749"
description = "A quick look at New Scientist Enigma number 1749, with a simple solution in Javascript."
date = 2013-06-10
[extra]
created = "2013-05-30"
+++

I thought I'd post a random _New Scientist_ enigma (no. 1749, From 18th May 2013) for you to have a go at if you like, followed by the solution and quick explanation:

# The Problem

In a new design of cellphone, each of the 10 number keys is associated with some letters of the alphabet. All letters appear once, but are not in the usual alphabetic order found on a standard phone. For example, _W_ appears on the 0 button, _S_ is on the 5 button and _N_ is on the 9 button.

It turns out that NEW = SCIENTIST/ENIGMA.

What number is GENETICS?

# The Solution

If we find values for each of the letters that satisfies the first equation, we can answer the question. Since we know the digit associated with 3 of the letters, we just need to find the digit associated with the other 7 (A, C, E, G, I, M and T). We can probably narrow our search somewhat by a process of deduction, but given that there are only 10 million possible combinations of digits to test in order to arrive at our answer, I'll forgo that and just write a simple program to do the work for me.

Thus, to find the correct solution I wrote the following snippet in Javascript:

```
//runs the test with a value for each letter:
function testActual(A, C, E, G, I, M, T)
	{
	var NEW = 900 + E * 10;
	var SCIENTIST = 500000000 + C*10000000 + I*1000000 + E*100000 + 90000 + T*1000 + I*100 + 50 + T;
	var ENIGMA = E*100000 + 90000 + I*1000 + G*100 + M*10 + A;
	return NEW == SCIENTIST/ENIGMA;
	}

//tests one number:
function testOne(val)
	{
	//create 7 digit string from val provided:
	s = val+"";
	while(s.length != 7) s = "0"+s;

	//split string into array of numbers and apply as args to function:
	var a = s.split("").map(function(e){return parseInt(e);});

	//return array if done or false if not:
	return testActual.apply(null, a)? a : false;
	}

//test every possibility until we find solution:
function solve()
	{
	var a, i = 0;
	while(!(a = testOne(i))) ++i;
	var A = a[0], C = a[1], E = a[2], G = a[3], I = a[4], M = a[5], T = a[6];
	return ""+G+E+9+E+T+I+C+5;
	}
```

Running `solve()` tests every possibility until one of them is correct, and then prints out the number that _GENETICS_ is equal to. `testOne()` takes in a number, and converts it into a 7 digit array, which is then applied to `testActual()`, so that each array item takes on a variable name. `testActual()` constructs the correct numbers given a value for each letter, and tests to see whether the equation works out with those values.

Running this, we find that the value for _GENETICS_ is _25950365_. Further, the values for _A_ and _M_ are 3 and 5 respectively, and so substituting in numbers for the original equation, we end up with:

```text
950 = 563590350/593253
```

This equation is valid, verifying the result. Nice and easy.