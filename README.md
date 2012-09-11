hStereopsis
===========

Goal: to learn Haskell and do something usefull.
Idea: stereo image matching to generate a depth map.
How: Hierarchical Belief Propagation

The Plan:
* implement image file reading, displaying, writing
* implement several simple kernels - sobel, gaussian
* prepare Markov Network to represent the belief space
* add loopy belief propagation
* optimise performance by adding several hierarchical levels
* improve performance by selective update of the nodes
* experiment with visual cues such as generated occlusion, edges maps

