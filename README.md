# wordle

Port of [Aditya Sengupta's wordle solver](https://aditya-sengupta.github.io/coding/2022/01/13/wordle.html) to Scala to learn some basic Scala syntax.

His algorithm uses information theory (specifically differential entropy) to choose the guess that will most evenly partition the possible answers. Intuitively, this is similar to how binary search is the most efficient algorithm for finding a randomly chosen number between 0-100 as it divides the search space into 2 even buckets at every step.