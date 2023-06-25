# Modularity, Objects, and State

## prelude

In previouse sections we understand how **abstract** powerfully combine primitive _procedure_ and _data_ to compound entities.

Abstract can help us to overcome the some of the complexity problems on builing a large systems. But it's not fair sufficient because by abstract we can only combine logic inside single place (file), we need a hyper view of constructing whole program.

So we need _modular_ system to help us develop program's different parts separately.

### A case: Object Oriented Modeling Physical Systems

A powerful strategy for building system to model physical systems is to build a corresponding _Computational Object_ for each original objectoriginal object. and every actions of the original object will get a `symbolic operation` (like `(put 'add add)`).

The ideal assumption of this model is:

1. if we have to add a new computational object, we only need to add this computational object on code rather than change the whole system strategically.
2. If we need to extend a new _action_, we just need to add the related symbolic actions of that.

If those can be done, we can confine our testing and checking just on our
modificated parts locally.

### Object Oriented & Streams Oriented ways

Object Oriented Model will organize whole system as the collection of distinct objects whose behavior may change over time.

Streams-based organizational strategy concenrates on the streams of information that flow in the systems, much as an electrical engineer views a signal-proceessing system.

Both ways will raise significant linguistic issues in programming.

1. For Object, cause the Object's status can be changed, we need to keep track of this object thus the original _substitution model_ of computation is no longer fitful, we need to use _environment model_ of computaion. Meanwhile, we need to take account of the influence of _time_.

2. to tackle with those issue with time, we can try using _streams_ model which can decouple _simulated time in our model_ from _the order of the events that take place in the computer during evaluation_. We will accomplish this using a technique known as delayed evaluation.
