assemble
========

![](/images/logo.png)

This is a library still in progress! :).  But the core functionality has definitely been achieved.  In particular, it advances the concepts described in [watershed](http://github.com/hypower-org/watershed).

Assemble is a library for connecting graphs!  This concept has been generalized within this library.  However, I find that I find its functionality useful when dealing with channels.  

The core concept of assemble is creating vertices to represent the functionality of some system.  For example, this system could consist of connections between streams.  

Consider the following system:  

![](/images/problematic-example.png)

Now, let's assume that these nodes are connected by channels/streams (e.g., [manifold](https://github.com/ztellman/manifold), [core.async](https://github.com/clojure/core.async))

An interesting problem can result depending on the connecting order of these graphs.  Let's assume, for the sake of my example :), that they are connected in random order! 

First, let's say vertex :a is connected to vertex :b 

![](/images/problematic-example-1.png)

And then some time passes...

and more time...

and yet more...

and, finally (!), vertex :a is connected to vertex :c! 

![](/images/problematic-example.png)

The problem here, which I hope is obvious, is that vertex :b could have consumed an unknown amount of information from vertex :a before vertex :a was connected to vertex :c.  

One of the major reasons I began programming in clojure was to get away from an idea very similar to this: mutable state.  In my opinion, this trivial example I just described exhibits a form of mutable state.  Just not one with which we're familiar.  

I believe that there exists a simple solution to this issue that I propose with the following diagram: 

![](/images/problematic-example-fixed.png)

As a solution to this "mutable state" problem, I introduce a vertex called :a' (a prime).  This vertex is an exact copy of vertex :a (think ```(map identity ...)```).  However, we tell verticies :b and :c that :a' is, in fact, vertex :a!  This means that we can connect the verticies as follows: 

Step one: 

![](/images/problematic-example-fixed-1.png)

Step two: 

![](/images/problematic-example-fixed-2.png)


