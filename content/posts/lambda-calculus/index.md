+++
title = "An Introduction to Lambda Calculus"
description = "A pretty in depth introduction to Lambda Calculus, the precursor to modern functional programming as well as some crazy esoteric programming languages like Iota and Jot."
date = 2013-03-17
[extra]
created = "2013-03-16"
+++

Recently, I've written about a couple of compression algorithms [here][sequitur] and [here][huffman], and a few more in my release [here][compression]. Soo, time for something different! Step in Lambda Calculus, first formulated by Alonzo Church.

# Why should I care?

Lambda calculus has been called _the smallest universal programming language in the world_. Why?  because there is only a single way to define a function, and a single scheme which dictates how functions are applied to variables (called variable substitution). There are no operators, like `+` or `-`, no types, or numbers, or strings.

But, from this simple scheme, we can represent numbers, create recursive functions, and, well, do everything we need to construct anything we want. Lambda calculus is _Turing equivalent_, which basically means you can use it to recreate any computer program which takes in some input and produces some output as a result. Which I think is really cool.

Most of what I have learned was taken from [this very helpful tutorial][tutorial], which I fully encourage you to look at for more detail and examples. I'm going to have a shot at writing my own introduction to lambda calculus below. I'll also mention Combinatory Logic, which, seen in terms of lambda calculus, can reduce this already simple programming language to just 2 symbols (and parantheses)!

# So, how does it work?

First off, it's worth abandoning many existing ideas you may have in mind regarding a programming language, so here are some basic differences between lambda calculus and mostly any programming language you'll have used:

- There are no numbers, or strings, or mathematical operators in lambda calculus.
- Applying functions to expressions (in other words, some input), is the only operation that occurs in lambda calculus.
- Accordingly, everything must be represented in terms of functions, and functions that modify other functions.

So, the only thing that _does_ anything in lambda calculus is the _function_. This is what a few different functions may look like in lambda calculus:

```txt
λx.x
λy.yz
λy.(λa.ay)
```

The lambda symbol (λ) is used to mark the beginning of a function. The symbol following the λ is the variable which is used as a placeholder in the subsequent function definition (this variable is also termed _bound_ to the function). This is then followed by a dot, which in turn is followed by the function definition.

Written in the style of a more conventional programming language, the above might look something like:

```javascript
function(x) {
	return x
}

function(y) {
	return yz
}

function(y) {
	return function(a) {
		return ay
	}
}
```
More generally, a function in lambda calculus takes the form:

```txt
λ<placeholder>.<definition>
```

Where the function definition can be any valid lambda _expression_; this can be either a function, a variable (a single letter), or a function being applied to one of these (which will eventually result in either a function or a variable anyway if evaluated). The placeholder is just a variable, which is a single letter.

What _is_ a function? Well, the only thing that a function actually does is take in some expression, and substitute that in place of every instance of its placeholder (the letter written after the λ symbol) everywhere in its definition. Much like a function in most ordinary programming languages takes some input in place of a variable name.

Let's look at these functions in more detail.

## Passing in Variables

In the following, we provide the variable `y` to the function `λx.wx`, using parentheses to separate the function from the variables passed in (otherwise, we wouldn't know when the function ended and the variables began). The result is obtained by substituting the placeholder `x` in the function definition with the input `y`, and then returning the result of this:

```txt
(λx.wx)y = wy
```

This is known as an _application_; the function `λx.wx` is applied to the thing to the right of it, in this case the variable `y`. Here are some more examples of some lambda functions being applied to expressions, and the consequent results (don't worry if you can't follow what's going on in all of these yet!):

```txt
(λx.xx)y = yy
(λx.xx)xy = xxy
(λx.xx)(xy) = xyxy
(λx.xx)(λy.y) = (λy.y)(λy.y) = (λy.y)
(λx.x(λy.y))z = z(λy.y)
```

In each case, we take the expression to the immediate right of the first function we come across, working from left to right, and pass it in to the function. How?

- First we find out what the placeholder is for that function by looking to see what follows the λ sign.
- Next, we substitute every occurrence of that placeholder in the function definition, with the expression that we have passed in.
- We then return everything after the dot, having done this substitution.
- Finally, we replace the original function and expression that we evaluated with this returned expression. In the forth example listed above, we can see that performing this step leads to something else which in turn can be evaluated in the same way.

Taking an example from above, these steps are carried out as follows:

```txt
(λx.wx)y
       ^
       this is our input variable.

(λx.wx)y
 ^^^^^
 this is the function we are applying to it.

(λx.wx)y
  ^
  we find the placeholder, which is x.

(λx.wx)y
    ^^
    this is the function definition.

(λx.wy)
     ^
     swap any instances of placeholder in it for our input.

wy
^^
return the transformed definition in place of "(λx.wx)y"
```

We always work from left to right, however anything surrounded by parentheses is grouped together and treated as one _thing_. Expressions within parentheses can be evaluated immediately by following the above, or at some later stage; it doesn't matter. Evaluation continues until we cannot do anything else (in other words, there are no functions left, or there is a function left but nothing to the right of it to apply it to).

It is important to note that variables are either _bound_ or _free_ in a given function. Bound variables are the variables that are used as placeholders in a function. In programming terminology, the placeholder for a given function is the same as an argument to a function. So, in the function definition following `λx`, all instances of `x` are bound; they are placeholders, ready to be substituted for whatever is going to be passed in to the function.

In expressions such as `(λy.y)(λx.y)`, the `y` in the first function expression is bound, but the `y` in the second function expression is free.

In addition, it is worth noting that variables are bound to the nearest instance of themselves following a λ sign. For example, in the expression `λx.x(λx.x)`, the placeholder `x` used in the inner function definition is not related to that used in the outer definition. So:

```txt
Correct:
(λx.x(λx.x))a = a(λx.x)

Wrong:
(λx.x(λx.x))a = a(λx.a)

```

One way of looking at this is in terms of variable scope. The placeholder used in the innermost function defines a separate scope inside that function for that placeholder. Any subsequent instances of that placeholder inside the innermost function definition are tied to that scope, and are not related to any placeholders used in containing functions. Thus, the following is true:

```txt
(λx.x(λx.x))a = (λx.x(λy.y))a = (λx.x(λt.t))a
```

The innermost `λx` defines a new scope, which the following occurrence of `x` is tied too. It is entirely separate from the preceding `x`, and thus can be renamed without affecting the function.

One way to avoid this potential confusion is to rename any placeholder variables if they are passed in to a function which uses the same variables. You'll see that I do this when working through some examples to avoid confusing myself!

As we have established, it does not matter which symbol is used for placeholders, since as soon as the function is applied to some expression, each placeholder is swapped for an instance of that expression anyway. So, to expand on the above, the following is also true:

```txt
Function 1:
λy.yz = λa.az = λt.tz

Applications:
(λy.yz)a = az
(λa.az)a = az
(λt.tz)a = az

Function 2:
λx.(λy.wxy) = λz.(λy.wzy) = λz.(λt.wzt)

Applications:
λx.(λy.wxy)ab = (λy.way)b = wab
λz.(λy.wzy)ab = (λy.way)b = wab
λz.(λt.wzt)ab = (λt.wat)b = wab
```

Free variables, on the other hand, are not substituted for anything else. Unlike variables in other programming languages, we do not assign them to values or anything like that. In fact, you'll find that they don't crop up very much, and in things such as combinatory logic (we'll mention that later), you'll see that they are not actually required at all in order to reproduce the result of any lambda function.

It's important to avoid confusing the symbols used as placeholders in functions (those that are going to be swapped out) with any free variables. The symbol used to represent a given placeholder is irrelevant, but the symbols used for free variables do matter. Thus, in any case whereby some confusion may be caused due to the variables used to represent placeholders, we simply rename the placeholders (bound variables). This can be summarized as:

> Where a function is applied to an expression: if the same symbols are used at any point in the function and the expression being passed in, and in one of these cases they are just placeholders, we replace the placeholders with different symbols in order to avoid confusing them with the free variables.

For example:

```txt
Correct:
(λx.(λy.xy))y = (λx.(λt.xt))y = λt.yt

Wrong:
(λx.(λy.xy))y = λy.yy
```

As we can see above, we are passing the variable `y` into a function which will substitute all occurrences of `x` for it. However, in the inner function contained within the outer function's definition, `y` is also being used as a placeholder. To avoid mixing the free variable `y` with the placeholder `y`, we rename the placeholder `y` in the inner function to something different. If we do not, we end up confusing free and bound variables, and consequently turning the free variable `y` into a placeholder, which it is not.

## Evaluation Order

As I mentioned earlier, it is important to note that the order in which things are evaluated, by convention, runs from left to right. Failing to do so will lead to errors, as in the following example:

```txt
Correct:
(λa.(λb.ba))(λx.x)(λy.y) =
(λb.b(λx.x))(λy.y) =
(λy.y)(λx.x) =
λx.x

Wrong (evaluating from the right first):
(λa.(λb.ba))(λx.x)(λy.y) =
(λa.(λb.ba))(λy.y) =
λb.b(λy.y)
```

If we wish to be explicit in the order that things must be evaluated in, we can use parentheses to group things, leading to a different result:

```txt
Correct (additional brackets group arguments into one):
(λa.(λb.ba))((λx.x)(λy.y)) =
λb.b((λx.x)(λy.y)) =
λb.b(λy.y)

Correct (you can evaluate expressions within brackets at any point):
(λa.(λb.ba))((λx.x)(λy.y)) =
(λa.(λb.ba))(λy.y) =
λb.b(λy.y)

Wrong (we're just ignoring brackets here):
(λa.(λb.ba))((λx.x)(λy.y)) =
(λb.b(λx.x))(λy.y) =
(λy.y)(λx.x) =
λx.x
```

The use of brackets groups expressions together. As such, they are treated as one _thing_ outside the brackets. Inside the brackets, you must still evaluate from left to right, but you can hold off evaluating inside the brackets, or evaluate inside them straight away; the result is the same regardless. Here is an example:

```txt
Let's evaluate this function application:
(λa.((λb.ab)a))((λb.ab)c))

Evaluating innermost parentheses first:
(λa.((λb.ab)a))((λb.ab)c)) =
(λa.aa)((λb.ab)c)) =
(λa.aa)(ac) =
(ac)(ac)

Evaluating outermost parentheses first:
(λa.((λb.ab)a))((λb.ab)c)) =
(λa.((λb.ab)a))((λt.at)c)) =
(λb.((λt.at)c))b)((λt.at)c) =
((λt.at)c))((λt.at)c)) =
((λt.at)c))(ac) =
(ac)(ac)
```

As we can see, it doesn't matter when things inside parentheses are evaluted, although some routes may take longer than others. As such, various evaluation strategies exist, but that is outside the scope of this introduction. Personally, I tend to evaluate anything inside parentheses as soon as possible in the examples I have worked through here.

## Functions with Multiple Inputs

One of the things that you may have noticed thus far is that functions can only take in one input. While strictly true, using a method known as _currying_, we can in fact emulate functions that can take in as many variables as we need. We do this by returning a function when a function is applied to something. This function can then take in another variable. Here is an example:

```txt
Function designed to take in two variables:
λx.(λy.yx)

Application involving two variables a and b:
(λx.(λy.yx))ab = (λy.ya)b = ba

Written in shorthand:
λxy.yx

Application using shorthand:
(λxy.yx)ab = ba

```

As we can see above, we can emulate functions with as many variables as we want by returning a function ready to accept the next variable each time one variable is passed in, and repeating this as much as needed. This can be represented using the above shorthand, which makes things easier to follow (but is functionally identical).

Some random examples of functions which work with multiple variables, and their shorthand abbreviations:

```txt
λw.(λx.(λy.yxw)) = λwxy.yxw
λa.(λb.a(λy.yb)) = λab.a(λy.yb)
```

So, now we (hopefully) have a grasp on how to use functions in lambda calculus, how do we put it all to use? I mean, we don't have numbers or strings or anything right?

# Right! So How do I Make Use of This?

## Representing Numbers

Well, given that we don't have things like numbers in this language, one of the most important things to do is to come up with a way to represent them. One such means to represent numbers is known as [Church encoding][church]. Under this, our numbers, known as _Church numerals_, are represented in terms of lambda functions, as follows:

```txt
0 = λab.a
1 = λab.a(b)
2 = λab.a(a(b))
3 = λab.a(a(a(b)))
...
```

Each number, as represented above, is supposed to work by taking in two arguments, a function `a`, and something to apply it to: `b`. It then applies the function `a` that number of times to a second thing we pass in, `b`. For example, applying the function `λx.xx` to `y` 3 times would double the number of occurrences of `y` 3 times:

```txt
3(λx.xx)y =
(λab.a(a(a(b))))(λx.xx)y =
(λb.(λx.xx)((λx.xx)((λx.xx)b)))y =
(λx.xx)((λx.xx)((λx.xx)y)) =
(λx.xx)((λx.xx)(yy)) =
(λx.xx)(yyyy) =
yyyyyyyy
```

So, the church numerals represent numbers in a very fundamental form; the application of some function that number of times. In the above, the church numeral 3 means simply to apply the function `λx.xx` to `y` 3 times.

## Basic Arithmetic

Next up, we can define functions which work with these numbers. For example, the successor function - which we can call `S` - simply increments any number passed into it by one:

```txt
S = λabc.b(abc)
```

Applying this to any number, for example 1, increments it as follows:

```txt
S1 =
(λabc.b(abc))(λab.a(b)) =
λbc.b((λab.a(b))bc) =
λbc.b(b(c)) =
2
```

Noting that, while the letters used to represent the placeholders in the final function differ from those used to represent the number 2 earlier (`a` and `b`), the function itself is identical, as it does exactly the same thing.

As numbers apply their fist argument to their second argument that number of times, we can use this successor function `S` for addition. For example, adding 1 and 2 results can be carried out as follows:

```txt
1S2 =
(λab.a(b))(λabc.b(abc))(λab.a(a(b))) =
(λabc.b(abc))(λab.a(a(b))) =
(λabc.b(abc))(λat.a(a(t))) =
λbc.b((λat.a(a(t)))bc) =
λbc.b(b(b(c))) =
3
```

In the same way, we can define functions for things like multiplication, which takes the form `λxyz.x(yz)`. Here, we can see this function in action multiplying 2 and 3 together:

```txt
M = λxyz.x(yz)

Now, let's try to multiply 2 and 3:

M23 =
(λxyz.x(yz))23 =
(λyz.2(yz))3 =
λz.2(3z)

let's swap 2 and 3 with the functions they represent to finish the job:

λz.2(3z) =
λz.(λab.a(a(b)))((λab.a(a(a(b))))z) =
λz.(λab.a(a(b)))(λb.z(z(z(b)))) =
λz.(λab.a(a(b)))(λt.z(z(z(t)))) =
λz.(λb.(λt.z(z(z(t))))((λt.z(z(z(t))))b)) =
λz.(λb.(λt.z(z(z(t))))(z(z(z(b))))) =
λz.(λb.z(z(z(z(z(z(b))))))) =
λzb.z(z(z(z(z(z(b)))))) =
6
```

Noting that I choose to rename the placeholder `b` to `t` to avoid later confusion, although stricly speaking it was not necessary.

## More Lambda Calculus

All in all, things can get pretty complex. From here, we can go on to define logical operators, recursion, and far more complex things as required, but for me, the beauty of lambda calculus lies in the very simple foundation it begins with, and the expressive power of this basic core.

For a more in depth tutorial, I'll refer you [here][tutorial]; it covers basically everything you need to start building more advanced expressions in lambda calculus.

# Combinatory Logic and Lambda Calculus

While [combinatory logic][combinatory] was actually invented before lambda calculus, to me it feels like lambda calculus is more foundational, and combinatory logic can be seen as an application of it. Lambda calculus is burdened by the need for free variables, which can be numerous and can cause confusion if placeholder variables are not appropriately renamed when applying functions.

Combinatory logic can be seen as a subset of lambda calculus expressions, which have no free variables within them. The clever thing is that, when combined, they enable the user to create all other lambda calculus expressions. What's more, only two of these expressions are actually necessary to pull this off. They are the following:

```txt
K = λxy.x
S = λxyz.(xz(yz))
```

Given combinations of just these two lambda functions (or, in combinatory logic terms, combinators), we can create any other lambda function, and thus any program. For example, to recreate the result of the function `λx.x` - also known as the identity function - we combine `SKK`. Applying it to some variable `a`, results in the same output, `a`, just as the lambda function `λx.x` would do:

```txt
(SKK)a =
SKKa =
(λxyz.(xz(yz)))(λxy.x)(λxy.x)a =
(λxy.x)a((λxy.x)a) =
(λxy.x)a(λy.a) =
(λy.a)(λy.a) =
a
```

In the same way, any lambda function can be translated into its equivalent representation using only `S` and `K` combinators. This has led to the development of several _esoteric_ programming languages (defined as programming languages which push the boundaries of computer programming language design in one way or another) which use very few symbols, such as [unlambda], [Iota and Jot][iota]. The latter use only 2 symbols in the entire language!

# Conclusion

Once you understand it, lambda calculus is a very simple foundation from which very complex things can be built. Given that you only have functions to work with, you must decide on a way in order to represent everything in terms of functions. Once your representation is built up, you'll find that anything is possible (although some things can get rather complicated).

One of the things I like about the idea is that it provides a way of representing things without explicit symbols, for example numbers. While numbers are an arbitrary set of symbols that we devised, representing numbers in terms of functions as has been done with church numerals feels a little bit special, and really gets to the core of what a number is.

Lambda calculus, or the idea behind it, has also spawned or influenced various languages, from those like LISP to more esoteric ones formed from combinatory logic operators. One of the reasons I got into it was by coming across some of these languages, in which code is simply strings of 2 characters, and wondering how easy it would be to evolve useful programs from them using Genetic Algorithms.

Anyway, thanks for reading! Got any questions or comments? Get in touch below.




[tutorial]: http://www.utdallas.edu/~gupta/courses/apl/lambda.pdf
[iota]: http://semarch.linguistics.fas.nyu.edu/barker/Iota/
[sequitur]: ./posts/sequitur/index.md
[huffman]: ./posts/huffman-coding/index.md
[compression]: https://jsdw.github.io/js-compression-machine/
[church]: http://en.wikipedia.org/wiki/Church_encoding
[combinatory]: http://en.wikipedia.org/wiki/Combinatory_logic
[unlambda]: http://www.madore.org/~david/programs/unlambda/