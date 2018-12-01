# [Advent of Code](https://adventofcode.com) - 2018 (Haskell Edition)

Note: Complete solutions (and a benchmark) for 2017 (in Scala, Haskell and Eta) can be found ([here](https://github.com/rolandtritsch/scala-aoc-2017); star it, if you like it :)).

To make this work you need to ...

* install [stack](https://www.haskellstack.org)
* run `stack test`
* run `stack build`
* run `stack exec haskell-aoc-exe`

Note: The code also compiles with [Eta](https://eta-lang.org). To make this work you need to ...

* install [etlas](https://eta-lang.org/docs/user-guides/eta-user-guide/installation/etlas)
* run `etlas test`
* run `etlas build`
* run `etlas run haskell-aoc-exe`

Note: You need to have stack installed and need to run `stack build --dry-run` first (to generate the `cabal` file (the dry-run will invoke hpack to generate the cabal file from the package.yaml file)).
