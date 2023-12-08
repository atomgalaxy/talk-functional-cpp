# (Fun)ctional c++

Gašper Ažman

2023-12-05

C++ London Meetup

==SLIDE==

<!-- .slide: data-background-image="image/wizard-glitch.png" data-background-size="contain" -->

==SLIDE==

## Disclaimers

==DOWN==

**This is slide code.**

==DOWN==

## Omitted

- noexcept propagation
- SFINAE
- nicer errors
- all of the metaprogramming.

==DOWN==

## Ommitted (cont)

- compile-time optimizations
- lots of niceness
- automatic inference
- and tons of other stuff.

==DOWN==

<!-- .slide: data-background-image="image/forest-dwelling.png" data-background-size="contain" -->

==DOWN==

C++ is not made for this. Yet.

I have proposals in flight that will make this better.

==DOWN==

<!-- .slide: data-background-image="image/viaduct-balthazar.png" data-background-size="contain" -->

==DOWN==

The errors are bad.

==DOWN==

The compile times are worse.


==DOWN==

The runtime performance is actually pretty great

(something something aliasing inlining constprop).

==DOWN==

<!-- .slide: data-background-image="image/viaduct.png" data-background-size="contain" -->

==DOWN==

When in doubt, do the simple thing.

Functional approaches pay off in the large.

==SLIDE==

## Optimize for correctness

==DOWN==

<!-- .slide: data-background-image="image/viaduct-balthazar.png" data-background-size="contain" -->

==DOWN==

Let the compiler prove our code.

Only plausible business logic should compile.

Tests are CRCs for our proofs.

The typesystem _is_ a proof system.

==DOWN==

Our brain does not fit a lot, and proofs are hard.

We break down problems - but do we break down solutions?

==DOWN==

Context is the greatest complectifier of proof.

==DOWN==

<!-- .slide: data-background-image="image/dragon-tower.png" data-background-size="contain" -->

<!--
But we need to prove our code too! We do it intuitively, otherwise our code
wouldn't work.

We make little programs that check the proofs in our head make sense. We call
these tests. But tests only check for obvious errors in our thinking, but they
don't check for all of them. We should use the tools at our disposal.

The most important tool we have is our brain, and it doesn't fit a whole lot.
That's why we need ways of breaking down *solutions*, not just the problems
we're working on. The less input to your proof, the easier it is.
-->

==SLIDE==

## Context of a line of code

<!--
Functional programming is about tight control of relevant context, which aids
local reasoning, which is critical for provability.

Let's take a look at the context that's possibly relevant to a procedural line
of code in a C++ program (most lines of code call at least one function).
-->

### Explicit context:

- function arguments
- *this
    - your object has state!
- local variables

==DOWN==

### Block context:

- loop condition (if in loop)
- loop postcondition (if after loop)
    - [inverse of loop condition!]
- current enclosing if-conditions
- negations of previous if-conditions (we're in the else block)
- catch (type)
- are we processing a sequence? (ranged for-loops)

==DOWN==

<!-- .slide: data-background-image="image/dragon-timeline.png" data-background-size="contain" -->

==DOWN==

### Implicit context:

- global variables
- threadlocal variables
- syscall-accessible stuff
    - environment variables
    - filesystem
    - gid/uid
    - ...

==DOWN==

### Implicit context (cont):

- current exception (this is an implicit argument if you're in a Lippincott function!)
- which exceptions *may* be thrown by subcalls
- cursors of open files
- open sockets / connections
- shared memory regions

==DOWN==

### Implicit context (cont 2):

- dispatched async operations waiting to complete
    - how do I cancel the current operation? Is it even cancellable?
- operating system limits
- global subsystems (singletons)
    - logging
    - execution resources
    - memory allocator
    - shared network connections

==DOWN==

### Implicit context (cont 3):

- state machines: which *state* you're in?
- What execution context is running the code?
    - high priority thread
    - main thread
    - low priority thread
    - top-half of a signal handler
    - GPU, NUMA node, IO thread, compute thread, which threadpool?????

==DOWN==

For every Context grows a Monad.

-- Teta Pehta (paraphrased)

<!-- .slide: data-background-image="image/teta-pehta.jpg" data-background-position: "right" -->

==DOWN==

(Monad is meant loosely - we mean a compositional context. Functor,
Applicative, Monad, and some other things are all valid here).

==SLIDE==

## Limiting context

Most context is irrelevant at any one time.

*Which* part is up to convention and discipline.

Procedural languages have problems encoding proofs about context.

==DOWN==

In functional languages, all context is explicit.

This helps proofs.

Can we be disciplined in c++?

==SLIDE==

## Tools

We will need tools.

Banish all context-accessors from business logic into named contexts.

And we will explicitly give access.

And we will be free.

<!--

Let's see how we can cut down on the amount of context we need to consider.

First, we will unfortunately have to ban a whole lot of things. We need to be
able to trust our code, and that means code we're calling, too. We'll need to
make the assumption that there is no access of the "global context" from
anywhere. Instead, we will supply the needed context explicitly.

-->

==DOWN==

Apprentice: But master, my function has 43 parameters?

Master: you were blind, but now you see.

==DOWN==

<!-- .slide: data-background-image="image/wizard-monad.png" data-background-size="contain" -->

==DOWN==

Zen of python: flat is better than nested.

Have you considered flattening out your dependencies?

==DOWN==

<!-- .slide: data-background-image="image/magma-glowhouse.png" data-background-size="contain" -->

==DOWN==

Design for laziness: Make the right things painful.

This is *good*. It allows more of the program to be context-free.

Push control flow, mutation, and I/O towards `main()`.

==SLIDE==

## Quick example

C++ has exceptions:

```cpp
try { g(); }
catch (same_as<E1, E2, E3> auto&& exc) {
    /* no you don't says C++ */
}
```

You need to know what `g` throws.

Polymorphic runtime matching. No generic matching.

==DOWN==

This is better:

```cpp
g() | match_error(
        [](std::same_as<E1, E2, E3> auto&& exc) {
            // ahhh, yiss
        });
```

==DOWN==

You don't handle every exception? Fail to compile.

You want to let some through? Do it explicitly:

```cpp
g() | match_error(
        overload{
            [](std::same_as<E1, E2, E3> auto&& exc) {/*...*/},
            left /* id-on-error */
        }
    );
```

A glimpse of the validation monad. We'll talk about it more later.


==SLIDE==

## "basic" needed language features

- templates <!-- we'll get literally nowhere without these. -->
- structs and closures
- function overloading <!-- this is basically the "Choice" compositional pattern built into the language. -->
- operator overloading <!-- we need some kind of a programmable semicolon so we can
  drag along "context". There have been workflow operators proposed, but we'll
  use `|`, because that's what ranges does anyway. -->
- deduction guides <!-- critical; things get really verbose without them, and thus unusable -->
- concepts <!--: we'll need these for pattern-matching everywhere. -->

==DOWN==

## Support library

- std::ranges - C++'s answer to the List monad
- std::execution - Async monad (later)
- std::optional - Maybe
- std::expected - Error
- Validation: not standard
- I/O: better done with std::execution
- State: what do you think OOP is?
- Environment: not standard

==DOWN==

## The final guideline

Name common patterns and control flow.

==DOWN==

<!-- .slide: data-background-image="image/viaduct-distance.png" data-background-size="contain" -->

==SLIDE==

## Maybe / std::optional

Try multiple things and return the first one that succeeded.

==DOWN==

Procedural way:

```cpp
auto is_hostname_in_args(int, char const* const*)
    -> bool;
auto get_hostname_from_args(int, char const* const*)
    -> char const*;
auto get_target_hostname(
            int argc, char const* const* argv,
            std::string default_hostname)
        -> std::string {
    // split query / getter
    if (is_hostname_in_args(argc, argv)) {
        // perhaps... might use optional here too?
        return get_hostname_from_args(argc, argv);
    }
    // <next slide>
```

==DOWN==

```cpp
    // ad-hoc Maybe
    if (char const* maybe_host = getenv("SERVICE_HOSTNAME");
        (maybe_host != nullptr) || (*maybe_host == '\0')) {
        return maybe_host;
    }
    return default_hostname;
}
```

==SLIDE==

## Functional

First, adapt to uniformity.

The "try multiple things" context is "Maybe", modeled by `optional<T>` in c++:

==DOWN==

Before: query/getter pair

```cpp
auto is_hostname_in_args(int, char const* const*)
    -> bool;
auto get_hostname_from_args(int, char const* const*)
    -> char const*;
```

After: `std::optional`-returning object.

```cpp
inline constexpr auto maybe_hostname_from_args = 
    [](int argc, char const* const* argv) 
        -> std::optional<std::string> {/*...*/}
```

==DOWN==

Before: `nullptr`-on-nothing

```cpp
auto std::getenv(char const*) -> char const*;
```

After: `std::optional`

```cpp
inline constexpr auto get_env = [](std::string const& varname)
        -> std::optional<std::string> {
    if (char const* value = std::getenv(varname.c_str());
            value != nullptr) {
        return std::optional(std::string(value));
    }
    return {std::nullopt};
};
```

==SLIDE==

## Tool: filter

For the "nonempty" bit, we'll need a `filter`:

```cpp
inline constexpr auto filter = [](auto predicate) {
    return [p = std::move(predicate)]<class T>(T&& value)  {
        using r_t = std::optional<std::decay_t<T>>;
        if (std::invoke(p, value)) {
            return r_t(std::forward<T>(value));
        }
        return r_t(std::nullopt);
    };
};
```

==DOWN==

And we can finally put it together:

```cpp
inline constexpr auto nonempty =
    [](auto const& s){return !s.empty();};

auto get_target_hostname(
            int argc, char const* const* argv,
            std::string const& default_hostname)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{
            return get_env("SERVICE_HOSTNAME")
                   .and_then(filter(nonempty));
           })
          // can add other ways
          .value_or(auto(default_hostname));
}
```

Blech.

==DOWN==

We defined filter wrong. Let's fix it.

```cpp
inline constexpr struct filter_t {
    template <typename P>
    struct closure { P pred; };

    template <typename P>
    constexpr auto operator()(P&& predicate) const {
        return closure{std::forward<P>(predicate)};
    }

    // operator|()
} filter;
```

==DOWN==

... and the operator

```cpp
template <an_optional Opt, typename P>
friend constexpr /* hidden friend */
auto operator|(Opt&& opt, closure<P> const& cl) 
        -> std::decay_t<Opt> {
    using opt_t = std::decay_t<Opt>;
    return std::forward<Opt>(opt)
           .and_then([&]<class T>(T&& v) -> opt_t {
        if (std::invoke(cl.pred, v)) {
            return opt_t(std::forward<T>(v));
        }
        return std::nullopt;
    });
}

```

The wonderful paths to `|`-based composition.

==DOWN==

We can omit the `and_then`:

```cpp
auto get_target_hostname(
            int argc, char const* const* argv,
            std::string const& default_hostname)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{ return get_env("SERVICE_HOSTNAME")
                            | filter(nonempty); })
          .value_or(auto(default_hostname));
}
```

==DOWN==

We need a concept for the above:

```cpp
// fastest-to-compile is_instantiation_of I know of
template <typename X>
inline constexpr bool _an_optional_v = false;
template <typename T>
inline constexpr bool _an_optional_v<std::optional<T>&>
    = true;
template <typename T>
inline constexpr bool _an_optional_v<std::optional<T> const&>
    = true;

template <typename X>
concept an_optional = _an_optional_v<X&>;
```

==DOWN==

Everything called `an_XXX` or `a_YYY` is a concept.

==DOWN==

<!-- .slide: data-background-image="image/viaduct.png" data-background-size="contain" -->

==SLIDE==

## Fix the defaulting.

If you don't need to make a choice, why make it?

Just let the caller do it:

```cpp
auto maybe_target_hostname_from_params(
            int argc, char const* const* argv)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{ return get_env("SERVICE_HOSTNAME")
                            | filter(nonempty); });
          // <-- no more default
}
```

==DOWN==

Usage:

```cpp
int main(int argc, char** argv) {
    auto const config_file =
        maybe_config_file_from_params(argc, argv)
       .value_or({});
    auto const target_hostname =
        maybe_target_hostname_from_params(argc, argv)
       .or_else([&]{return config_file.maybe_get_hostname();})
       .value_or("default_hostname");
}
```

This isn't always true - you might choose to do it in one or the other location
depending on the desired semantics.

==DOWN==

<!-- .slide: data-background-image="image/wizard-floor-baubles.png" data-background-size="contain" -->

==SLIDE==

## Packs

There's another thing that is very nice to do when you want to compose these --
packing.

```cpp
auto connect(config c) -> std::optional<socket_addr> {
    return
        (c.get_maybe("hostname")
        & c.get_maybe("port"))     // an optional-pack
        | transform([](std::string && hostname, int port){
             return socket_addr{std::move(hostname), port};
        });
}
```

==DOWN==

This is a common.

We name common patterns.

```cpp
template <typename T>
inline constexpr make = []<typename... Ts>(Ts&&... args) {
    return T(std::forward<Ts>(args)...);
};
auto connect(config c) -> std::optional<socket_addr> {
    return (c.get_maybe("hostname")
           & c.get_maybe("port"))
        .transform(make<socket_addr>);
        //         ^^^^^^^^^^^^^^^^^
}
```

==DOWN==

## Monadic operations on `optional`:

```
and_then  :: opt<T>, (f(T) -> opt<U>) -> opt<U>;
transform :: opt<T>, (f(T) -> U)      -> opt<U>;
or_else   :: opt<T>, (F()  -> T)      -> opt<T>;
value_or  :: opt<T>, T                -> T;
```

==DOWN==

<!-- .slide: data-background-image="image/wizard-letterballoons.png" data-background-size="contain" -->

==SLIDE==

## Error: `expected<T, E>`

Instead of `nullopt`, we get `E`.

The flavor is different. Error is error, maybe is maybe.

<!--

The way C++ chose to model this is a tad different from the usual way of doing
`Error<E, T>`, where the constructors are `left` for errors and `right` for
success.

The `Error` monad looks superficially related to `Maybe`, but the two have a
very different flavor.

-->

==DOWN==

There are as many `expected`s as there are `E`s.

<!--
First, there is only one `Maybe` - but there are as many `Error`s as there are
error types.

Observe and contrast the operation table of monadic operations of `expected<T, E>`:
-->

==DOWN==

Note the table:

```
and_then  :: exp<T, E>, (f(T) -> exp<U, E>) -> exp<U, E>;
transform :: exp<T, E>, (f(T) -> U)         -> exp<U, E>;
or_else   :: exp<T, E>, (F()  -> T)         -> exp<T, E>;
transform_error :: exp<T, E>, (f(E) -> E')  -> exp<T, E'>;
value_or  :: exp<T, E>, T                   -> T;
```

We gained `transform_error`, the rest look like `optional`'s.

`and_then` cannot change `E`.

Neither can `or_else`.

This means we actually got an `optional`-per-`E`.

==DOWN==

`expected` is a domain-based composition type.

It is *not* better exceptions.

==SLIDE==

## Good fit

Compositional parser infrastructure.

Every parser's signature is

```cpp
auto subgrammar(std::span<char const>)
     -> std::expected<
           pair<SomeGrammarNode, std::span<char const>>,
           ParseError
        >;
```

==DOWN==

Seems limited.

It is.

Validation is better (later).

==SLIDE==

## Error Packs

Still, Error is very useful.

```cpp
// transform_nothing:: opt<T>, (f() -> E) -> exp<T, E>
auto parse_response(json const& doc)
    -> std::expected<Response, ParseError>
{
    return (
        (doc.get_maybe("version") 
            | transform_nothing(
                []{return ParseError("version is required");})
            | parse_version 
            | filter(eq(version{3, 14}), make<ParseError>)
        ) // expected<Version, ParseError>
        & (doc.get_maybe("id")
            | transform_nothing(
                []{return ParseError("id is required");})
            | parse_int
            | transform(make<Id>)
        ) // expected<Id, ParseError>
        & /*...*/
        ) // expected-pack<Version, Id, ..., ParseError>
        | transform(make<Response>);
}
```

==DOWN==

- We didn't forget error checking.
- No way to make a `Response` on failure.
- We validate everything due to strong types.

==DOWN==

## Strong types

- Version is just an integer
- constructor validates
- Response takes a Version
    - We can't make a `Response` without a `Version`

==DOWN==

Only plausible business logic should compile.

Encode things in types until that is true.

==DOWN==

```
Price x, y;
x + y; // does not compile
PriceDelta d;
Price r = x + d; // compiles
```

`Price` is a point-space over the integral module of `PriceDelta`.

`std::chrono` does this too.

<!-- .slide: data-background-image="image/viaduct-pipes.png" data-background-size="contain" -->

==SLIDE==

## Dealing with multiple Es (take 1)

We're going to need a better `variant`.

==DOWN==

If `variant` is a compositional context, what are the operations?

```
visit  :: variant<T, U, ...>, (f(T|U|...) -> V) -> V;
???
```

==DOWN==

We need `transform`, at least.

```
visit     :: variant<T, U, ...>, (f(T|U|...) -> V) -> V;
transform :: 
    variant<T, U, ...>, overload{f(T) -> T', f(U) -> U', ...}
    -> variant<T', U', ...>;
```

The usual name for `visit` is `match`.

The name for `transform` is `map` or `fmap`.

==DOWN==

### Gettin' fancy

Some kind of `bind` with partial variants: `pmap`

```
pmap :: v<T, U, ...>,
        overload{f(T)->v<X, Y>, f(U)->v<Z, W>, ...}
    -> v<X, Y, Z, W, ...>;
```

(I totally made this name up)

==DOWN==

### Example: a state machine:

```cpp
using State = variant<Initialized, Connecting,
                      Connected, Disconnected, Fail>;
using Message = variant<Connect, Stop, Data,
                        UnexpectedDisconnect>;
template <typename X>
concept a_live_state = std::same_as<X, Initialized> 
                    || std::same_as<X, Connecting>
                    || std::same_as<X, Connected>;
```

==DOWN==

We will also introduce multiple dispatch using `pmap` over a `variant-pack`:

```cpp
auto transition(State s, Message m) {
    return (s & m) | pmap(overload{ // double dispatch!
        [](Initialized s, Connect c)
            -> variant<Connecting, Fail> {...},
        [](Connected auto s, Data d)
            -> variant<Connected, Fail> {...},
        [](a_live_state auto s, Stop)
            -> variant<Disconnected> {...},
        [](a_live_state auto s, UnexpectedDisconnect)
            -> variant<Connecting, Fail> {...},
        [](auto s, auto m)
            -> variant<Fail> { return variant<Fail>{}; },
    });
}
```

`pmap` concatenates all the results.


<!--

It's getting the `invoke_result`.

of every invocation possibility, concatenating all the possibilities in the
variants, and deduplicating them to arrive at the resulting type.

Once that's done, it's turning the construction of the resulting variant into
the construction of the full one, behind the scenes.


To make things a bit more efficient, we can introduce a
`partial<Type>(in-place-args)` that we can return instead, or interpret
`variant<lazily<T>, lazily<U>>` as in-place constructors for `T` and `U`.

-->


==SLIDE==
### Back to our `expected<T, variant<Es...>>`

Let's try to use our error monad with this improved `variant` on the `Error`
side:

```cpp
open(path) // expected<File, variant<DoesNotExist,
                                     PermError>>
    | and_then(
        read_line // expected<std::string, IOError>
        | transform_error(???) // oops
    )
```

Can't.

==DOWN==

Need to try this again:

```cpp
using AllErrors = variant<DoesNotExist, PermError,
                          IOError>;
open(path) // expected<File, variant<DoesNotExist,
                                     PermError>>
    | transform_error(match(make<AllErrors>))
    // blech
    | and_then(
        read_line // expected<std::string, IOError>
        | transform_error(make<AllErrors>) // ok, now
    )
    | and_then(
        parse_version // expected<Version, SyntaxError> oh, nononono.
    )
```

We need something less annoying that accumulates error types.

==SLIDE==
## Monadic mixins -- validations

<!--
Returning back to our `expected` with multiple error types - we need to make
`expected` work with that. We call that the `validation` context.

-->

Validation "mixes in" the `variant` on the `error` side.

It also *computes* the types on the Error side.

```cpp
auto version_or_error 
    = open(path)          // validation<File, DoesNotExist,
                          //            PermError>
    | and_then(read_line  // expected<std::string, IOError>
    ) // validation<std::string, DoesNotExist,
      //            PermError, IOError>
    | and_then(parse_version);
    // validation<Version, DoesNotExist, PermError,
                           IOError, SyntaxError>
```

==DOWN==

`validation` is a closed-polymorphic equivalent to c++ exceptions.

We can also handle the errors generically.

```cpp
template <typename X, typename... Ts>
concept any_of
    = (... || std::same_as<std::remove_cvref_t<X>, Ts>);

version_or_error
    | match_error( // match may return void
        overload{
        [](any_of<DoesNotExist, PermError, IOError> auto&& e) {
            std::print("could not read file {}", e.filename);
        },
        [](SyntaxError const& e) {
            std::print("syntax error near column {}", e.col);
        }
    });
```

==DOWN==

## FIN

`transform_error` of course exists, but the `match` family doesn't require me
to invent something to return.

## Recap:

We mixed in the `variant` context into the `expected` context. Are there
other mixed-in contexts?

==SLIDE==
## Conclusion

In order to name common control flow patterns, we reach for named compositional
contexts, some of which are monads.

We explored the following contexts today:

- `optional` or "Maybe"
- `expected` or "Error"
- `variant` or "Choice"
- `validation`

==DOWN==

<!-- .slide: data-background-image="image/viaduct-balthazar.png" data-background-size="contain" -->


<!--
BONUS
## The biggest context: the async context

I am, of course, talking about `p2300`, `std::execution`, or, as you might know
it, Sender/Receiver.

It's mixing in:

- variant (for the completion channels)
- Environment (a type-tagged state composition context) for scheduler,
  cancellation token, and domain
- it's biased like `expected` and `optional` (the `set_value` channel family is primary)
- and the crown jewel, of course, the async I/O context, which governs execution.
- oh, and don't forget timing.

We really don't have time to get into that in this talk, but at least I gave
you some idea on how to understand it if you start studying it.

I've seen this context drive connections between redis caches, postgresql
servers, and multiple UDP and TCP connections. It's fantastic, and made writing
correct async code a breeze.

However, we only used it for the bits that concerned execution and async
behavior. For the rest, we used the above contexts, and strong types.

-->
