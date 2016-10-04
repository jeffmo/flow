---
title: Exact Object Types
short-title: Exact Object Types
author: bhosmer
---

Flow makes pervasive use of *subtyping*.

Subtyping is a powerful tool that allows for type-safe code reuse in many situations where types overlap with each other. It's especially useful when dealing with object types. 

For example, it allows a single type to describe a class as well as all of its subclasses; Or to describe all objects with a certain critical set of properties, without restricting what other properties an object might contain.

But this descriptive power can sometimes result in unwanted permissiveness, making useful idioms unsound (and sometimes unsafe). For these situations, Flow introduces the concept of **exact object types**.

## A Quick Recap on Subtyping 

When we say that some type `A` is a subtype of another type `B` (we can denote this as `A <: B`) we mean that a value of type `A` can be used anywhere a value of type `B` is expected. There’s a *substitutability guarantee* from `A` to `B`.

<img src="https://fb.quip.com/-/blob/NbbAAA8EEYV/VL7916VxKT5Dl1MKrsE1Vw" style="float: right; width: 100px; height: 100px; margin-left: 15px;" /> When we talk about a “type”, we are talking about a set of values. A `number` type describes the set of all numbers, a `string` type describes the set of all strings, etc. So a "subtype relationship" between two types describes a subset inclusion between sets of values. We denote this with `A <: B` (“`A` is a subtype of `B`”) which implies that `values(A) ⊆ values(B)` (“All of the values in set `A` are also found within set `B`”).


A standard Flow object type says which properties an object must have, but it says nothing about which properties it must *not* have.  This means that the set of values of a particular object type can be pretty diverse: For example, the object type `{ name: string }` includes *any* object with a string-valued property called `name`. From this we could say that object types in Flow are “positive” (they *have* these properties) rather than “negative” (they *do not have* these properties).

A lot of power and utility flows (sorry!) from this arrangement. Most importantly, it lets us type [lvalues](https://msdn.microsoft.com/en-us/library/f90831hc.aspx) like function parameters and variables purely in terms of what we require of incoming [rvalues](https://msdn.microsoft.com/en-us/library/f90831hc.aspx), while leaving those rvalues free to vary in other ways. This promotes reuse and spares us unnecessary descriptive overhead.

## The Problem 

Sometimes we need to talk about properties an object *doesn’t* have.

For instance, here's an example of the popular JS idiom which uses a property’s existence as a kind of sentinel:

```js
// expect obj to be either an object with properties x and y,
// or an object with property z.
function foo(obj) {
  if (obj.x) {      // if x is present,
    return obj.y;   // y should also be present
  }
  return obj.z;     // otherwise, z should be present
}

```

How should we type `foo` (and in particular, its parameter `obj`) in a safe way that still reflects the intent of the code?

A first cut might use a union of object types:

```js
type MyObj = { x: string, y: number } | { z: number };

function foo(obj: MyObj) {
  if (obj.x) {
    return obj.y;
  }
  return obj.z;
}
```

At first glance this seems to encode the desired meaning, i.e. that `y` will appear whenever `x` does, and if that pair is absent `z` will be present. But if we [run this through Flow](https://flowtype.org/try/#0PQKgBAAgZgNg9gdzCYAoVAXAngBwKZgCyWA8gEYBWYAvGAN5gAeAXGORXgMYYA0YWrAHYBXALZk8AJzABfMAB96YAF5CxE6TIDc6KMMHcAlnEFgocOAAo4lVsXYBKeqjBhDUMNcoA6Rk7oAkAHAwG4ejG4AzmA4kniReIK8Lq5gcRjCkqY2FN5YWsGhWGCRABZwwjAAJmAAhjCRcGASMXEJSSkyKemZ2T7KBYVgcBilUgiGCSol5ZU1LbHxiRioXUA), we get an interesting error:

```bash
7:     return obj.y;    // y should also be present
                  ^ property `y`. Property not found in
7:     return obj.y;    // y should also be present
              ^ object type
```

What’s happening here is that subtyping gives the second member of our union *too much* descriptive power.

Remember that `{ z: number }` doesn’t mean “objects with a number property `z` and no other properties”. Rather, it means “objects with a number property `z` and *any* other properties”.  Other properties like, say, `x`.

So our `if` statement, which is intended to guarantee the presence of `y` based on the the presence of `x`, will actually admit objects without a `y`, because `{ z: number, x: <whatever> }` is most definitely a subtype of `{ z: number }`.

Subtyping defeats the *negative* intent of our union type's second member.

## The Solution 

Exact object types add this negative implication that's missing from ordinary object types in Flow.

Exact object types are expressed with the delimiters `{|` and `|}` rather than `{` and `}`. An exact object type denotes the set of objects containing *exactly *the properties listed — no more, no less. Objects with extra properties are not allowed.

For instance, the exact object type `{| z: number |}` means “objects with a number property `z` and no other properties”. If we use exact types in our example,

```
type MyObj = {| x: Object, y: number |} | {| z: number |};

function foo(obj: MyObj) {
  if (obj.x) {
    return obj.y;
  }
  return obj.z;
}
```

[Flow reports no errors](https://flowtype.org/try/#0PQKgBAAgZgNg9gdzCYAoVAXAngBwKZgCyWA8gEYBWYAvGAN5gAeAXGORXgMYYA0YWrAHYBXALZk8AJzABfMAB96igF5CxE6fJkBudFGGDuASziCwUOHAAUcSq2LsAlPVRgwRqGBuUAdI2d0AJCBwMDunozuAM5gOJJ4UXiCvK5uYPEYwpJmthQ+WNohYVhgUQAWcMIwACZgAIYwUXBgErHxicmpMqkZWTm+yoVFYHAYZVIIRolgyqUVVbWtcQlJGKjdQA), because now, as intended, the presence of `x` guarantees that we do indeed have a value from the first member of the union, and thus guarantees the presence of `y`.

(Note for the eagle-eyed: It’s true that this example will typecheck even when only the second member of the union is exact, and it's good practice to think through why this is the case. However, the bulk of real-world use cases will require that all members be made exact, and it's idiomatic to do so.)
