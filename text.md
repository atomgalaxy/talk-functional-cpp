It's dangerous to go alone! Take this!

We want to let the compiler prove our code.

But we need to prove our code too! You do it intuitively, otherwise your code
wouldn't work. Then we make little programs that check the proofs in our head
make sense. We call these tests. But tests only check for obvious errors in our
thinking, but they don't check for all of them. We should use the tools at our
disposal.

The most important tool we have is our brain, and it doesn't fit a whole lot.
That's why we need ways of breaking down *solutions*, not just the problems
we're working on. The less input to your proof, the easier it is.

Also, in the beginning, there was chaos. [picture of cable mess]

From this chaos, core memory was born, and with it, the Von Neumann architecture.

And for a while, it was good. The while was about 3 months. [todo check number]

Then assembly came, because rememmbering ones and zeroes is difficult. Programs
writing programs.

And then came Dijkstra and structured programming, and gave us procedures (and
with them, ABI (thanks Dijkstra)). It also gave us loops and if/then/else, and
blocks.

The chaos was being ordered, slowly, but all was not yet well. Notice that all
of these constructs were about taking things away. From infinite possibilities
and few instructions, we introduce more constructs that are more limited, and
we gain order and expressivity.

This gets rid of the fear of "anything can happen". It increases locality of
reasoning, because the programmer can trust the rest of the program doesn't do
anything weird.

The next step was layered architecture, where we forbid lower layers from
knowing things about higher ones, and also try to avoid reaching-through. This
gets rid of interference between layers, further increasing locality of
reasoning.

The next step was object-oriented programming (before object-oriented
languges), where we hide representation and make it "forbidden" to access state
in certain ways. It also introduces the value of a principled approach to open
polymorphism (think unix inodes). This step was supposed to increase locality
of reasoning about state, but ended up also being counterproductive by breaking
layering in many places.

So, state was better protected, and invariants on such state were easier to
keep, and that's pretty much where we stayed.

Objects needed constructors, and exception handling added another control flow
path to our code. Still structured, but now we had hidden control flow, and
this further *decreased* locality of reasoning. The compiler could help even
less than it could before.

Along came generic programming. This duck-typed approach allowed us to
decompose solutions into self-contained pieces in a variety of ways, trying to
reclaim some of the locality of reasoning that was lost. But the true
breakthrough in generic programming came when it stopped being ad-hoc, and
started using concepts.

Concepts, when used with discipline, allow us to restrict our thinking to what
the concept promises, and thus decouple proofs of code from the actual bits and
pieces implementing the suboperations, be it state or functions. It reduces the
amount of context to the precise bits that the concepts promise.

Let's analize the actual context of every c++ function:

Explicit context:
	- arguments
	- *this
        - your object has state!

Block context:
    - loop condition
    - loop postcondition (inverse of loop condition!)
    - current enclosing if-conditions
    - negations of previous if-conditions (we're in the else block)
    - catch (type)
    - are we processing a sequence? (ranged for-loops)

Implicit context:
    - global variables
    - threadlocal variables
    - system call accessible stuff (environment variables, filesystem, gid/uid)
    - current exception (this is an implicit argument if you're in a Lippincott function!)
    - which exceptions *may* be thrown by subcalls
    - cursors of open files
    - open sockets / connections
    - shared memory regions
    - dispatched async operations waiting to complete
    - operating system limits
    - global subsystems (singletons)
        - logging
        - execution resources
        - memory allocator
        - shared network connections
    - if your object is a state machine, which *state* you're in is implicit
      context.
    - What execution context is running the code? (high priority thread, main
      thread, low priority thread, top-half of a signal handler, GPU, NUMA
      node, IO thread, compute thread, which threadpool?)
    - how do I cancel the current operation? Is it even cancellable?

Sure, most of the time we can "infer" which of this context matters to our
function or block, so we don't consider all of it all the time; but have you
read all the code you're calling? How can you ensure the closure of its
implicit preconditions?

Yes, procedural code is hell.

Let's see how we can cut down on the amount of context we need to consider.

First, we will unfortunately have to ban a whole lot of things. We need to be
able to trust our code, and that means code we're calling, too. We'll need to
make the assumption that there is no access of the "global context" from
anywhere. Instead, we will supply the needed context explicitly.

Yeah, yeah, all well and good, but doesn't that make my function calls take an
insane number of parameters?

Well, yes, it does, but have you considered making your *requirements*
structure flatter?

Zen of python: flat is better than nested.

Applying this rule has a forcing-function effect of pushing mutation (requires
context), IO (requires io context), things that can fail (data validation), and
things that bifurcate control paths (state) up the call graph, closer to main().

This is *good*. It allows more of the program to be context-free.

Design for laziness: Make the right things painful.

```cpp
try { g(); }
catch (same_as<E1, E2, E3> auto&& exc) { /* no you don't says C++ */ }
```

```cpp
g() | upon_error([](std::same_as<E1, E2, E3> auto&& exc) {
    // ahhh, yiss
});
```

This also fails to compile if you don't handle every exception. You want to let
some through? Do it explicitly:

```cpp
g() | upon_error(
        [](std::same_as<E1, E2, E3> auto&& exc) {},
        [](auto&& x){return FWD(x);} // also known as `id`
    );
```

This gracefully handles exceptions, finally. I just showed you a glimpse of an
Error+Choice monad. We'll talk about it more later.

Before we go into monads, let's run down the basic features C++ gives us that
we can use to make our functional generic programming tools:

- templates: we'll get literally nowhere without these.
- structs and closures: ditto
- function overloading: this is basically the "Choice" compositional pattern built into the language.
- operator overloading: we need some kind of a programmable semicolon so we can
  drag along "context". There have been workflow operators proposed, but we'll
  use `|`, because that's what ranges does anyway.
- deduction guides: critical; things get really verbose without them, and thus unusable
- concepts: we'll need these for pattern-matching everywhere.

We will also need a support library. I'll show you usage; unfortunately, there
are no open-source solutions for most of these, but they're not that difficult
to implement. Also, names might be familiar, but they are by no means standard,
and I welcome renaming suggestions.

The final guideline:

Name common patterns and control flow.

## Disclaimers

**This is slide code.**

I omitted at least constexpr propagation, SFINAE, nicer errors,
compiletime-speed enhancements, lots of niceness, automatic inference, and tons
of other stuff.

C++ is not made for this. Yet. I have proposals in flight that will make this
better. A bit.

The errors are bad.

The compile times are worse.

The runtime performance is actually pretty great (something something aliasing
inlining constprop).

When in doubt, do the simple thing.

The examples are kinda contrived, but I swear it pays off in the large.


## Compositional contexts and where to find them

### Try multiple things and return the first one that succeeded

Procedural way:

```cpp
auto is_hostname_in_args(int, char const* const*) -> bool;
auto get_hostname_from_args(int, char const* const*) -> char const*;
auto get_target_hostname(
            int argc, char const* const* argv,
            std::string default_hostname)
        -> std::string {
    // split query / getter
    if (is_hostname_in_args(argc, argv)) {
        // perhaps... might use optional here too?
        return get_hostname_from_args(argc, argv);
    } 
    // ad-hoc Maybe
    if (char const* maybe_host = getenv("SERVICE_HOSTNAME");
        (maybe_host != nullptr) || (*maybe_host == '\0')) {
        return maybe_host;
    }
    return default_hostname;
}
```

It's much better if things speak the same language. For "try multiple things",
the context is usually "Maybe", modeled by `optional` in c++:

First, we prepare a few general-purpose functions that all speak `optional`:

This one should always have had a decent interface:

```cpp
// from pair of query & getter to just a getter
auto is_hostname_in_args(int, char const* const*) -> bool;
auto get_hostname_from_args(int, char const* const*) -> char const*;
// to:
inline constexpr auto maybe_hostname_from_args = 
    [](int argc, char const* const* argv) 
        -> std::optional<std::string> {/*...*/}
```

We can transform the ad-hod "maybe" with nullptr return of `std::getenv` into
an optional:

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

For the nonempty bit, we'll need a `filter`:

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

And we can finally put it together:

```cpp
inline constexpr auto nonempty = [](auto const& s){return !s.empty();};
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

It's a bit of a mouthful, mostly because of the annoying way we defined filter.
Let's try again:

```cpp
inline constexpr struct filter_t {
    template <typename P>
    struct closure { P pred; };

    template <an_optional Opt, typename P>
    friend constexpr auto operator|(Opt&& opt, closure<P> const& cl) 
            -> std::decay_t<Opt> {
        using opt_t = std::decay_t<Opt>;
        return std::forward<Opt>(opt).and_then([&]<class T>(T&& v) -> opt_t {
            if (std::invoke(cl.pred, v)) {
                return opt_t(std::forward<T>(v));
            }
            return std::nullopt;
        });
    }

    constexpr auto operator()(auto&& predicate) const {
        return closure{std::forward<decltype(predicate)>(predicate)};
    }
} filter;
```

Ah, yes, the wonderful paths we must walk to enable `|`-based composition.

However, this enables us to omit the `and_then`:

```cpp
auto get_target_hostname(
            int argc, char const* const* argv,
            std::string const& default_hostname)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{ return get_env("SERVICE_HOSTNAME") | filter(nonempty); })
          .value_or(auto(default_hostname));
}
```

We need a concept for the above:

```cpp
// fastest-to-compile is_instantiation_of I know of, as of right now
template <typename X>
inline constexpr bool _an_optional_v = false;
template <typename T>
inline constexpr bool _an_optional_v<std::optional<T>&> = true;
template <typename T>
inline constexpr bool _an_optional_v<std::optional<T> const&> = true;

template <typename X>
concept an_optional = _an_optional_v<X&>;
```

You can assume everything called `an_XXX` or `a_YYY` is a concept.

Last thing: fix the defaulting. If you don't need to make a choice, why make it?

It's actually more efficient to just let the caller do it:

```cpp
auto maybe_target_hostname_from_params(int argc, char const* const* argv)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{ return get_env("SERVICE_HOSTNAME") | filter(nonempty); });
}
```


Usage:

```
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

## Packs

There's another thing that is very nice to do when you want to compose these --
packing.

```cpp
auto connect(config c) -> std::optional<socket_addr> {
    return (c.get_maybe("hostname") & c.get_maybe("port")) // an optional-pack
        .transform([](std::string && hostname, int port){
             return socket_addr{std::move(hostname), port};
        });
}
```

This is a pretty common pattern, so it's helpful to have a constructor-lift:

```cpp
template <typename T>
inline constexpr make = []<typename... Ts>(Ts&&... args) {
    return T(std::forward<Ts>(args)...);
};
auto connect(config c) -> std::optional<socket_addr> {
    return (c.get_maybe("hostname") & c.get_maybe("port")) // an optional-pack
        .transform(make<socket_addr>);
}
```

For completeness: the table of monadic operations on `optional`:

```
and_then  :: optional<T>, (f(T) -> optional<U>) -> optional<U>;
transform :: optional<T>, (f(T) -> U)           -> optional<U>;
or_else   :: optional<T>, (F()  -> T)           -> optional<T>;
value_or  :: optional<T>, T                     -> T;
```

## Error compositional pattern - `std::expected<T, E>`

The way C++ chose to model this is a tad different from the usual way of doing
`Error<E, T>`, where the constructors are `left` for errors and `right` for
success.

The `Error` monad looks superficially related to `Maybe`, but the two have a
very different flavor.

First, there is only one `Maybe` - but there are as many `Error`s as there are
error types.

Observe and contrast the operation table of monadic operations of `expected<T, E>`:

```
and_then  :: expected<T, E>, (f(T) -> expected<U, E>) -> expected<U, E>;
transform :: expected<T, E>, (f(T) -> U)              -> expected<U, E>;
or_else   :: expected<T, E>, (F()  -> T)              -> expected<T, E>;
transform_error :: expected<T, E>, (f(E) -> E')       -> expected<T, E'>;
value_or  :: expected<T, E>, T                        -> T;
```

On the face of it, we just gained `transform_error`, and the rest look like
`optional`. However, notice that `and_then` cannot change `E`, and neither can
`or_else`. This means we actually got an `optional`-per-`E`.

This makes `expected` effectively a domain-based composition type, not a
general-purpose one.

For instance, you might want to use it for a compositional parser
infrastructure, where every parser has the signature of

```cpp
auto subgrammar(std::span<char const>)
     -> std::expected<
           pair<SomeGrammarNode, std::span<char const>>,
           ParseError
        >;
```

Nevertheless, this is one of the most useful compositional tools when combined
with a variant implementation that's a bit more fully-featured than the
standard one.

For instance, when dealing with validating json responses one gets back from
web services, it's really nice to be able to do the following:

```cpp
// transform_nothing:: optional<T>, (f() -> E) -> expected<T, E>
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
        )
        & /*...*/
        ) // expected-pack<Version, Id, ..., ParseError>
        .transform(make<Response>);
}
```

The nice thing here is that it's really easy to see we didn't forget any error
checking. There's no way to make the intermediate results without going through
a parser, validator, and constructor, and there is no way to make a `Response`
without having everything go correctly.

We used another really important tool here - strong types. Yes, Version is just
an integer, but we can't make a `Response` without a `Version`, not just any
`int`. This catches a lot of bugs.

The general guideline we had was that only plausible business logic should
compile. That meant, for instance, that `price + price` didn't make any sense -
`price + price_delta` did. Technically, prices of instruments are a point space
over a vector space of `price_delta`. We modeled that. This is also modeled in
`std::chrono`.

## Dealing with multiple error types and drilling

In order to deal with multiple error types, we're going to need a better
variant with some fun autodetecting `transform` operations.

Let's pretend `variant` is a compositional context. What is its table of basis
operations?

```
visit  :: variant<T, U, ...>, (f(T|U|...) -> V) -> V;
???
```

We probably want some equivalent of `transform`, at least.

```
visit     :: variant<T, U, ...>, (f(T|U|...) -> V) -> V;
transform :: 
    variant<T, U, ...>, overload{f(T) -> T', f(U) -> U', ...}
    -> variant<T', U', ...>;
```

The usual name for `visit` is `match`, and the name for `transform` is `map` or
`fmap`.

There's another operation that is really useful to have - some kind of `bind`
where overloads can return partial variants:

```
pmap :: v<T, U, ...>, overload{f(T)->v<T',T''>, f(U)->v<U', U''>, ...}
    -> v<T, T'', U', U'', ...>;
```

As an example, let's take a look at a state machine:

```cpp
using State = variant<Initialized, Connecting, Connected, Disconnected, Fail>;
using Message = variant<Connect, Stop, Data, UnexpectedDisconnect>;
template <typename X>
concept a_live_state = std::same_as<X, Initialized> 
                    || std::same_as<X, Connecting>
                    || std::same_as<X, Connected>;
```

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

`pmap` is doing a lot of heavy lifting here. It's getting the `invoke_result`
of every invocation possibility, concatenating all the possibilities in the
variants, and deduplicating them to arrive at the resulting type.

Once that's done, it's turning the construction of the resulting variant into
the construction of the full one, behind the scenes.

To make things a bit more efficient, we can introduce a
`partial<Type>(in-place-args)` that we can return instead, or interpret
`variant<lazily<T>, lazily<U>>` as in-place constructors for `T` and `U`.


