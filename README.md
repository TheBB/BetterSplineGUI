BetterSplineGUI
===============

This is mostly me playing around with OpenGL in Haskell. The principal motivation is a NURBS visualization program with reasonable performance.

Big thanks to [bergey](https://github.com/bergey) for his implementation of the first five [wikibooks tutorials](https://en.wikibooks.org/wiki/OpenGL_Programming) in Haskell [here](https://github.com/bergey/haskell-OpenGL-examples). Most other tutorials are outdated either in the OpenGL sense or the Haskell sense (or both).

Currently this repository represents nothing more than a port of the fifth wikibook tutorial to

* Gtk and GtkGL rather than GFLW. A dependency on GLFW is maintained due to timing, but this will be dropped as the intended end product won't need it.
* Functional reactive programming, using the reactive banana library.
