# My notes: Foreword by Alan Perlis

> Every computer program is a model, hatched in the mind, of a real or
> mental process.

> ...we become convinced of program truth through argument.

## His comments on Lisp

>  The list, Lisp's native data structure, is largely responsible for
>  such growth of utility.

> In Pascal the plethora of declarable data structures induces a
> specialization within functions that inhibits and penalizes casual
> cooperation. It is better to have 100 functions operate on one data
> structure than to have 10 functions operate on 10 data structures.

This was written in 1984, when the current Algol-based typed languages
were quite impoverished. The rise of Standard ML and Haskell in 1990
completely changed the landscape, though, I believe. The list is still
very important even in ML and Haskell, but by the 1990s, it was
already clear that overuse of lists in Lisp was a problem.

## Looking back at Lisp from today

### Norvig on Lisp

Peter Norvig in his 1992 book (which I highly recommend as a deep dive
into programming in Lisp)
["Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp "](http://norvig.com/paip.html)
was already writing,
["Lisp makes it easy to rely on lists, but one must avoid the temptation to overuse lists; to use them where another data structure is more appropriate."](https://books.google.com/books?id=eH6jBQAAQBAJ&pg=PA340&lpg=PA340&dq=lisp+overuse+lists&source=bl&ots=Yiyxi4n9k7&sig=ANeroOFa0ujE4smt-QY2n_cGu0U&hl=en&sa=X&ved=0CDYQ6AEwBGoVChMIv-y42sbHyAIVQjg-Ch0F2gYn#v=onepage&q=lisp%20overuse%20lists&f=false)
(Note that Norvig himself switched to Python in around 2000 and was
instrumental to popularizing the language at Google and by extension
globally. Some links to his thoughts on Python:

- [Python for Lisp Programmers](http://norvig.com/python-lisp.html)
- [Why he went Python](https://news.ycombinator.com/item?id=1803815)
- [Some beautiful IPython demo notebooks](http://norvig.com/ipython/) (he
regularly posts new ones)

### Peter Seibel on Lisp

Peter Seibel in his excellent 2005 book (now freely available online)
["Practical Common Lisp"](http://www.gigamonkeys.com/book/) wrote:

["Historically, lists were Lisp's original composite data type, though it has been decades since they were its only such data type. These days, a Common Lisp programmer is as likely to use a vector, a hash table, or a user-defined class or structure as to use a list."](http://www.gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html)

Interestingly, he has become an active user of Haskell in recent
years. Also, last year he taught his 8-year-old daughter Haskell and
they [coded something together](https://twitter.com/peterseibel/status/579793703193051136).

### My observation of Lisp

Lisp is still very much alive, and I believe it will never die, but
the comparison to Pascal is now irrelevant 30 years after the
publication of this foreword. Most languages now have built-in data
types or allow ease in creating them. You could say that many
languages have stolen many good ideas from Lisp.
