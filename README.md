assemble
========

![](/images/logo.png)

Assemble is a library for connecting graphs!  This concept has been generalized within this library.  However, I find that I find its functionality useful when dealing with channels.  

The core concept of assemble is creating vertices to represent the functionality of some system.  For example, this system could consist of connections between streams.  

Consider the following system:  

![](/images/problematic-example.png)

Now, let's assume that these nodes are connected by channels/streams (e.g., [manifold](https://github.com/ztellman/manifold), [core.async](https://github.com/clojure/core.async))

An interesting problem can result depending on the connecting order of these graphs.  Let's assume, for the sake of my example :), that they are connected in random order! 

