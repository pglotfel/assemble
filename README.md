assemble
========

<p align="center">
  <img src = "/images/logo.png">
</p>

<br></br>
<br></br>
<br></br>

This is a library still in progress! :).  But the core functionality has definitely been achieved.  In particular, it advances the concepts described in [watershed](http://github.com/hypower-org/watershed).

Assemble is a library for connecting graphs!  This concept has been generalized within this library.  However, I find that I find its functionality useful when dealing with channels.  

The core concept of assemble is creating vertices to represent the functionality of some system.  For example, this system could consist of connections between streams.  

Consider the following system:  

<p align="center">
  <img src = "/images/problematic-example.png">
</p>

Now, let's assume that these nodes are connected by channels/streams (e.g., [manifold](https://github.com/ztellman/manifold), [core.async](https://github.com/clojure/core.async))

An interesting problem can result depending on the connecting order of these graphs.  Let's assume, for the sake of my example :), that they are connected in random order! 

First, let's say vertex :a is connected to vertex :b 

<p align="center">
  <img src = "/images/problematic-example-1.png">
</p>

And then some time passes...

and more time...

and yet more...

and, finally (!), vertex :a is connected to vertex :c! 

<p align="center">
  <img src = "/images/problematic-example.png">
</p>

The problem here, which I hope is obvious, is that vertex :b could have consumed an unknown amount of information from vertex :a before vertex :a was connected to vertex :c.  This result may not be a problem in some cases, but I'll leave it as an exercise to the reader to determine the cases where it might be :). 

One of the major reasons I began programming in clojure was to get away from an idea very similar to this: mutable state.  In my opinion, this trivial example I just described exhibits a form of mutable state.  Just not one with which we're familiar.  

I believe that there exists a simple solution to this issue that I propose with the following diagram: 

<p align="center">
  <img src = "/images/problematic-example-fixed.png">
</p>

As a solution to this "mutable state" problem, I introduce a vertex called :a' (a prime).  This vertex is an exact copy of vertex :a (think ```(map identity ...)```).  However, we tell verticies :b and :c that :a' is, in fact, vertex :a!  This means that we can connect the verticies as follows: 

Step one: 

<p align="center">
  <img src = "/images/problematic-example-fixed-1.png">
</p>

Step two: 

<p align="center">
  <img src = "/images/problematic-example-fixed-2.png">
</p>

At this point, the system has essentially been fully assembled, but no data has started flowing because the "real" vertex :a has not yet been connected to the system.  Let's do that now: 

<p align="center">
  <img src = "/images/problematic-example-fixed-3.png">
</p>

Now, the data can flow freely, and we don't have to worry about early consumption of data!

Assemble actually does exactly this when dealing with systems!  However, it attempts to generalize these input systems using a couple nice graph algorithms which are implemented in the assemble.graph namespace.  

We've gone over an example pictorally, so let's now review it if implemented in watershed.  For this code example, I'll be using [manifold](http://github.com/ztellman/manifold).  

```clojure 

(ns whatever
  (:require [assemble.core :as a]
            [manifold.stream :as s]))

```
Let's add a vertex with assemble! 

```clojure

(a/vertex :a [] (fn [f] (fn [] (s/periodically 1000 f))) (fn [] 1))

```
Wow, that's a lot of anonymous functions.  I'll break down exactly what's happening: 

The first parameter to a/vertex is the title of the vertex!  Fairly straightforward.  

The following vector are the dependencies of vertex :a.  From our example, we know that vertex :a has no dependencies, so we leave this blank.  

The next function is the "generator" function.  This function takes a function as an argument and returns, essentially, how the vertex outputs its data.  In this case, it's going to use some of the manifold.stream namespace functionality and return a function that will emit the result of applying that function with no arguments every 1000 ms.  That is, ```(s/periodically 1000 f)```, where f is the function to be periodically applied.  

The final function is the main purpose of the vertex.  In this case, it is to create the value 1.  Because of the supplied generator function, this vertex will produce the value 1 every 1000 ms.  






