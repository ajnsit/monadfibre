Monad Fibre (monadfibre-0.1.2.1)
===============================

***Note: This repo is currently not being maintained. I may start working on this again if some fresh and promising ideas come up.***

This package defines Monadic functions which provide Choice and Parallelism - (&lt;||&rt;) and (&lt;&&&rt;) - that work on Monads that provide a (MonadBi m IO) instance.


Changelog
=========

0.1.2.1 : Removed dependency on STM

0.1.2 : Moved MonadBi to a separate library (monadbi). This library provides a good example of how to use MonadBi.

0.1.1 : Removed @Fibre@ monad transformer. Instead only two functions (<||>) and (<&&>) are provided that work on Monads that provide a (MonadBi m ()) instance.

0.1 : Intial release
