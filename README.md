ShortestPathProblem
===================

This repo is just homework for optimisation cource. The problem is find shortest path in graph from left-bottom to right-top corner. 
Graph has strongly specified structure - square with n inner vertexes and 2-4 edges on each vertex. 
Input file contains edges weights description from left-top to right-bottom.

How to execute
-------------
    >сabal update
    >cabal install cabal-install
    >cabal install diagrams diagrams-svg
    >runghc src/graph.hs example/input.txt -w 1000 -o out.svg

Example input
------------
      10   4    10   5    2    11   3
    3    7    4    6    2    6    9    9
      1    10   2    8    10   8    11    
    9    6    4    8    3    4    8    7
      10    8    7    4    9    4    5
    9    6    5    9    8    7    4    7
      4    3    8    6    5    3    5
    6    9   10   10   5    10    9    8
      3    5    1    4    1    8    9
    5    3    0    5    3    4    3    2
       4    0    0    7    3    8    3
    9    3    1    0    3    8    6    4
       5    5    3    0    0    2    9
    20   40   9    20   7    0    10   2
       0    0    0    0    0    1    8


Example output
--------------
