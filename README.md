# [PoPL](http://cgi.di.uoa.gr/~prondo/LANGUAGES/languages.html) - [Assignment 3](http://cgi.di.uoa.gr/~prondo/LANGUAGES/as3.pdf).
Final Assignment for the Principles of Programming Languages class.
All functions implemented according to the given guidelines.

## Execution
Example execution:
```sh
    ghci maze.hs
```
Now in the interpreter:
```sh
    let a = kruskal $ makeMaze 3 3
    let b = braid a
    let c = solvePerfect a (0, 0) (2, 2)
    let d = solveBraid b (0, 0) (2, 2)
    putStrLn $ showMaze a c
    putStrLn $ showMaze b d
```

## OS - Tools
- Ubuntu 15.10
- GHCi, version 7.8.4

## Authors
- Alexandros Kalimeris
- Antonis Skarlatos
