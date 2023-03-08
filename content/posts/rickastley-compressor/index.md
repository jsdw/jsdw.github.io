+++
title = "Rick Astley Inspired Self-decompressing Text Compressor"
description = "A bit of code-golf inspired fun: introducing a Javascript snippet and tool to build it that when executed, prints out the lyrics to a well known song."
date = 2013-05-15
+++

After a friend directed me to [this][golf] code golf on Stack Exchange, I thought I'd have a quick go myself at implementing a Javascript version. Here it is:

```txt
s="We'r7n6strangers@6loV3 L@h7rules>s6d6I2AJull commitment's;K@hink= of3 wTldn't?et@hisJromBny9?uy.+P4n-2AU if5Bsk m71Don'tF me58bliU t6see++PESS0QE0QP4-P.+++*PMRup/letRdown/runBrTU>desert5/makeRcry/say?oodbye/tellB lie>hurt5*,* each9Jor s6long3r heart:nBch= but38shy@6say it2Insid7w7both L;:n?o= on2W7L@he?ame>we're< play it*2I just wannaFR1Gotta makeRuUerstaU*2NH *)M, nHC2(GiV*howKJeel=2*\n*2YT*2We'V L* yT*o *e *'re@o6* other*'s bee* what*?onna*ing*BU * g* t*A* a*?iV*D*2(Ooh*@ell*G*eVr<*I* f* I'm*know*2NHC*N*O*2 *Rup)*5 *,CQE*ou*nd*ve";for(c=0;c<45;)d=s.split('*'),s=s.split(String.fromCharCode(c+43)).join(d[++c]);d[0]
```

Try running it in the developers console in your web browser to see what it does.

# What's going on?

So, the above code is 625 bytes in length. It's not the shortest possible length by a fair way, but the code to generate it is relatively fast (for a relatively small amount of text anyway), so it's a reasonable compromise. You can have a play and compress your own text to evaluatable Javascript [here][code].

With better spacing and use of Javascript, the above code looks like this:

```javascript
s="We'r7n6strangers@6loV3 L@h7rules>s6d6I2AJull commitment's;K@hink= of3 wTldn't?et@hisJromBny9?uy.+P4n-2AU if5Bsk m71Don'tF me58bliU t6see++PESS0QE0QP4-P.+++*PMRup/letRdown/runBrTU>desert5/makeRcry/say?oodbye/tellB lie>hurt5*,* each9Jor s6long3r heart:nBch= but38shy@6say it2Insid7w7both L;:n?o= on2W7L@he?ame>we're< play it*2I just wannaFR1Gotta makeRuUerstaU*2NH *)M, nHC2(GiV*howKJeel=2*\n*2YT*2We'V L* yT*o *e *'re@o6* other*'s bee* what*?onna*ing*BU * g* t*A* a*?iV*D*2(Ooh*@ell*G*eVr<*I* f* I'm*know*2NHC*N*O*2 *Rup)*5 *,CQE*ou*nd*ve";

for(c=0;c<47;c++) {
	d=s.split('*');
	s=s.split(String.fromCharCode(c+43)).join(d[c+1]);
}

d[0];
```

And works by doing the following:

```txt
while C is less than 47
	split the string S into array D using "*" as the delimiter.
	split the string S into array TMP using the charcode C+43 as the delimiter.
	join array TMP back into string S, inserting D[C+1] between each element.
	increment C
evaluate D[0]
```

which can be more simply written as:

```txt
while C is less than 47
	split the string S into array D using "*" as the delimiter.
	replace all instances of character with code C+43 in S with string d[C+1]
	increment C
evaluate D[0]
```

By spliting a string into an array using a certain delimiter, and then joining it back into a string using a given string, we are just replacing all instances of a character with a desired string, so the two pseudocodes are equivalent.

## What Does This Achieve?

Essentially, the string `s` is created by recursively looking over some input (in this case, lyrics to the Rick Astley song "Never Gonna Give You Up"), extracting substrings that repeat, and creating rules which map some new symbol onto the substring. Rules take the form `[rule_symbol] -> [substring]`. The symbols used to represent a rule is then used in place of any occurrence of the substring that it maps to. As such, when we replace all of the rule symbols with the substrings that they map onto (baring in mind that these substrings can themselves be composed of other rule symbols), we end up back with our original string. This is the basic idea behind grammar compression schemes; we find sequences of characters which repeat, and replace them with some new symbol, so that we only need to represent those characters once in a rule, rather than multiple times in the original string.

The symbols used to represent rules are themselves just regular ascii characters, starting from a predefined character code (in the above case 43). If a character code that we want to use as a rule symbol is already used in the original text as a regular character, we create a _dummy_ rule `[character] -> [character]`, which maps the character to itself. As such, when we substitute our rule symbols for whatever they map too, we don't mess up any of the original characters.

## Creating Rules: the RePair Algorithm

To create the rules, I implemented a quick and dirty version of the RePair algorithm, which finds repetition by looking at pairs of characters that repeat, and at each stage turning the most frequently occurring pair of characters into a rule, until there is no more repetition. This algorithm is very similar to Sequitur, which I have [described previously][sequitur], except it looks at blocks of characters rather than working sequentially over a stream of characters.

Some basic pseudocode for RePair might look like this:

```txt
let s be our original string
let r be our rules

while there is repetition
	find the pair of characters that repeats the most in s
	create a rule representing this pair
	replace all instances of this pair with the rule symbol

remove all rules that are only used once
```

After creating rules, I order them in terms of the amount that they save, and remove any rules that do not save me anything (taking into account the extra cost of any dummy rules we need to make to preserve the original characters). I then join them onto my string (which itself has been replaced in many parts with rule symbols). The symbols used to represent rules are then selected such that the symbol for each rule relates to its location in the string. As such, when we split the string by our delimiter (in the above case "*"), we know where to look for each rule.

Finally, given that we have a means to convert a compressed string back into the original, I just needed to find the shortest amount of Javascript code that would do this for me, eventually arriving at the code I have displayed here.

# Final Remarks

The RePair implementation does find the rules which lead to the greatest compression of the original string unfortunately, as other variants of this algorithm have compressed the string to a greater extent than I managed with RePair. Using an alternative algorithm for generating the rules would lead to greater compression.

That said, my implementation, while horribly inefficient, was still relatively fast for this short task, unlike some of the alternatives used to find the optimal rules. You can have a play with my implementation [here][code], to generate your own compressed strings.



[golf]: http://codegolf.stackexchange.com/questions/6043/were-no-strangers-to-code-golf-you-know-the-rules-and-so-do-i
[sequitur]: ./posts/sequitur/index.md
[code]: /projects/rickastley/