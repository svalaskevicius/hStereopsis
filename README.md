hStereopsis
===========

Goal
----
To learn Haskell and do something usefull.

Idea
----
Stereo image matching to generate a depth map. 

How
---
Calculating images disparity map by using the Hierarchical Belief Propagation algorithm.

The Plan
--------
* implement image file reading, displaying, writing
* implement several simple kernels - sobel, gaussian
* prepare Markov Network to represent the belief space
* add loopy belief propagation
* optimise performance by adding several hierarchical levels
* improve performance by selective update of the nodes
* experiment with visual cues such as generated occlusion, edges maps

