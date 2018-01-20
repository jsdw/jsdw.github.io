+++
title = "Compression Algorithms: Sequitur"
description = "An introduction to the Sequitur compression algorithm with an updated algorithm describing how it works, along with the common pitfalls in implementing it."
date = "2013-04-30"
[extra]
created = "2013-03-10"
+++

An algorithm I came across recently that I found particularly ingenious was Sequitur. Invented by Craig Nevill-Manning and Ian Witten in 1997, it is an algorithm that detects non-overlapping repetitions in a sequence and builds from it a hierarchical representation describing it. One of the things I particularly liked was that it does this by reading in the sequence one symbol at a time (for example, characters of the alphabet), rather than having to see the entire sequence beforehand. You can find a description by the authors [here][sequitur.info]. You can also find the results of my experimentation with compression algorithms [here][my-algorithm]

# Why is Sequitur Interesting?

As humans, we have to learn about a whole range of sequential information all of the time. It is key to our ability to associate cause and effect for example, and come to learn that one thing tends to follow another. One of the most obvious places in which we learn sequences is in music; we quickly learn the repetitive parts of a tune and upon hearing enough, can predict which notes will follow a given piece of tune. Sequitur is interesting because it too can spot repetition and learns about it in the form of creating rules which embody learned sequences, and some of the principles which make it tick could perhaps be adapted to more interesting learning problems.

# How Does it Work?

Core to how Sequitur works is that it starts with the smallest possible type of repetition - the repetition of just two symbols - and that it treats the rules formed from repeated sequences the same as it would individual characters when looking for repetition. This allows it to start small and build up increasingly large repeated sequences, all using one algorithm, and all in linear time and space.

The following is some pseudo-code describing the core steps taken by the algorithm, adapted from [here][sequitur.info]:

```text
For each new input symbol is observed, link it to the last symbol in S

Each time a new link is made between two symbols
    Check the digram index for another instance of this digram
    If another instance is found and does not overlap with this one
        If the other instance is a complete rule
            Replace this digram with the symbol representing that rule
        Otherwise
            Create a new rule
            Replace both digrams with a new symbol representing this rule
    Else if this digram does not exist
        Add this digram to the index
    Else
        Do nothing (because other digram is overlapping).

Each time a digram is replaced by a symbol representing a rule
    For each symbol in the digram
        If the symbol represents a rule
            If the rule represented only occurs once now
                Replace the symbol with the contents of the rule
                Remove the rule
```

# A Little More Detail, Please?

Basically, you must maintain two tables. First, we need a table known above in the pseudo-code as the digram index, that lists all unique digrams (pairs of two symbols) and points to their location (be it in the rule table or the sequence `S`). This is known as the digram index. Second, we need a table listing all of the constructed rules alongside the unique symbols constructed to represent them. This is known as the rule index.

Given these tables, we can start taking in input symbols (for example, characters). Each time we take in a symbol, we link it to the last symbol in our sequence called `S`. If there is nothing in `S`, then we create no links and just make the symbol the beginning of `S`. Following the pseudo-code, each time we add a link (so, we do nothing for the first symbol we add, then), we look at the new digram created (in other words, the symbol we just added, plus the one it was linked too). If this digram has never been seen before, we simply add it to the digram index, along with a pointer to its position in `S`. If it has been seen before, we check the rules index to see whether there is a rule that represents it (and does not contain any other symbols), and if there is, we replace the instance of it with the symbol representing that rule. Otherwise, we have a digram that has been seen before but does not belong to a rule, so we create a new rule to represent it, and replace both instances of it with this rule.

The important thing here is that these steps are applied whenever a line in the pseudocode leads to the creation of more links, not just whenever a new digram is added. For instance, once the line `Replace both digrams with a new symbol representing this rule` has completed, we need to look at any new digrams formed as a result of this substitution (being careful to only check each new digram once in light of potential overlaps).

Given that we do just the above, we would end up with no rules that represent more than 2 symbols. We could however end up with a host of rules that each represent other rules, which themselves represent more rules, and so on. This is where the final chunk of pseudo-code - the rule utility step - comes into play. Here, whenever we replace a digram containing a rule symbol with a new rule symbol, we check to see whether in doing so, that rule is no longer used more than once. This is because every time we replace digrams with a rule, two instances of a digram get removed from their current positions, and only one instance of the digram is added back (into the rules index). Any rule that is only used once is no longer useful, and as such should be removed in place of the original symbols it represents.

If we implement both of these steps (in other words, all of the pseudo-code), we enforce two things: digram uniqueness, and rule utility. digram uniqueness is enforced because no digram is ever seen more than once (it's replaced by a rule as soon as it is), and rule utility is enforced because every rule is used no less than twice.

Enforcing rule utility allows any given rule to encompass not just two symbols, but any length of symbols. As an example, consider the sequence of symbols `ABCABC`, where `R` is the rules index, and `D` is the digram index:

Added  | S         | R          | D             | Comments
:------|:----------|:-----------|:--------------|:--------
 A     | A         |            |               | No links made, so nothing to do
 B     | AB        |            | AB            | Link made, new digram AB stored in D
 C     | ABC       |            | AB, BC        | Link made, new digram BC stored in D
 A     | ABCA      |            | AB, BC, CA    | Link made, new digram CD stored in D
 B     | ABCAB     |            | AB, BC, CA    | Link made, digram AB already seen, make rule
       | 1C1       | 1:AB       | AB            | Rule created to represent AB
       | 1C1       | 1:AB       | AB, 1C, C1    | Links made, digrams 1C and C1 seen. Both new
 C     | 1C1C      | 1:AB       | AB, 1C, C1    | Link made, digram 1C already seen, make rule
       | 22        | 1:AB, 2:1C | AB, 1C        | Rule created to represent 1C
       | 22        | 2:ABC      | AB, BC        | Rule 1 only used once now, so remove
       | 22        | 2:ABC      | AB, BC, 22    | Link made, new digram 22 stored in D

While it may be hard to follow, the main thing of note happens when `C` is added for the second time. This causes the creation of a new rule, `2`, which initially represents `1C`. However, as the rule `1` is now only used once, it is removed and substituted for its contents AB. As a result, rule `2` now represents `ABC`. for longer repeated sequences, the same steps take place and can result in rules representing very long sequences of symbols.

Worth noticing is that if you added ABC again, you'd end up with `S = CCC`, and of course, you'd find that the new digram created (`CC`) is the same as an existing digram (the first two C's). This is why it is important to make sure that the new digram does not overlap with its prior instance before turning it into a rule. In the case that an overlap is seen, you don't want to update the digram index to point to the new digram either, otherwise if you added it again you would get overlap again.

# Summary

Sequitur is a rather beautiful algorithm (in my mind) for spotting repetition in sequences and representing any repetition in terms of new symbols. As such, a hierarchy is formed whereby rules represent both original symbols and other rules, forming a tree structure from the original input sequence from which it can be reconstructed.

In my experience, it is easy to make some mistakes during implementation as a result of improperly understanding the algorithm or simply implementing it wrong. Here is a summary of some things to look out for:

- Check any digrams that are created **after** a given line of pseudocode has finished. For instance, if we swap two digrams for a rule, only check for new digrams after both digrams have been replaced.
- When performing the above step, it is easy to accidentally check a digram twice. Don't!
- Make sure to enforce rule utility by removing rules if instances of them are removed, and there is only one instance left.
- Do not do anything if a new digram has already been seen, but overlaps with the other occurrence of it (eg if we see the sequence `AAA`, the second pair of A's overlaps with the first pair).
- If a digram has been seen before in a rule, but is __not__ the complete rule (that is, the rule contains other symbols), do not replace it with an instance of that rule. Instead, treat the digram as if it has been seen twice and substitute both occurrences with a new rule.

Baring those in mind, I hope that with the information provided above, you would be able to implement Sequitur yourself if you so wish. If you do, and you run into any issues, give me a shout! I have made use of the algorithm (and a bunch of other neat compression tricks) in my own data compression application; you can have a look by [clicking here][my-algorithm].

Thanks for reading, and take care!


[sequitur.info]: http://sequitur.info/jair/
[my-algorithm]: https://jsdw.github.io/js-compression-machine/
