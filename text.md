<!-- .slide: data-background-image="image/cppnow-title-card.png" data-background-position: "right" -->
Notes:

TODO: tony give me the camera slide.
- do collides in double dispatch
- do drilling

==SLIDE==

## Legal Disclaimers

Citadel Securities is my employer and enabled a lot of this research.<!-- .element: class="fragment" -->

This is not investment advice.<!-- .element: class="fragment" -->

None of this code or text is endorsed by Citadel Securities<!-- .element: class="fragment" -->

These words are my own.<!-- .element: class="fragment" -->

==SLIDE==

## This is slide code

==DOWN==

## Omitted

- `noexcept` propagation
- SFINAE-correctness                   <!-- .element: class="fragment" -->
- nicer errors                         <!-- .element: class="fragment" -->
- (almost) all of the metaprogramming. <!-- .element: class="fragment" -->

==DOWN==

## Ommitted (cont)

- compile-time optimizations
- lots of niceness                     <!-- .element: class="fragment" -->
- automatic inference                  <!-- .element: class="fragment" -->
- and tons of other stuff.             <!-- .element: class="fragment" -->

==DOWN==

C++ is not made for this. Yet.

I have proposals in flight that will make this better.

- [P2830](http://wg21.link/P2830) `constexpr` type sorting 
- [P2825](http://wg21.link/P2825) `declcall` 
- [P2826](http://wg21.link/P2826) Expression Aliases 
- [P2989](http://wg21.link/P2989) Universal Template Parameters
- [P2841](http://wg21.link/P2841) Concept and variable-template template-parameters

==DOWN==

The errors are bad.

==DOWN==

The compile times are worse.

(but getting better)

==DOWN==

The runtime performance is actually pretty great.

(something something alias analysis)

Notes:
Turns out letting compilers know more about your code helps optimization.

==DOWN==

When in doubt, do the simple thing.

Functional approaches pay off in the large.

==SLIDE==

## The Library

All the examples in this presentation actually run!

https://github.com/libfn/functional

By: **Bronek Kozicki** and **Alex Kremer**.

Theory: **Bartosz Milewski** and yours truly.

Under the ISC open-source license.

==SLIDE==

## Optimize for correctness

==DOWN==

Let the compiler prove our code.

Only plausible business logic should compile.

Tests are CRCs for our proofs.

The typesystem _is_ a proof system.           

==DOWN==

Our brain does not fit a lot, and proofs are hard.

We break down problems;

_but do we break down solutions_?

==DOWN==

Context is the greates complectifier of proof.

(Thanks Tony & Hickey).

==DOWN==

We usually prove code intuitively. It wouldn't work otherwise.

==DOWN==

Tests are CRCs for our proofs.

They detect some incorrectness. They don't prove code correct.

Notes:
But we need to prove our code too! We do it intuitively, otherwise our code
wouldn't work.

We make little programs that check the proofs in our head make sense. We call
these tests. But tests only check for obvious errors in our thinking, but they
don't check for all of them. We should use the tools at our disposal.

The most important tool we have is our brain, and it doesn't fit a whole lot.
That's why we need ways of breaking down *solutions*, not just the problems
we're working on. The less input to your proof, the easier it is.

==SLIDE==

## Context of a line of code

### Explicit context:
- local variables
- function arguments
- your object has state
- ... and it changes!

Notes:
Functional programming is about tight control of relevant context, which aids
local reasoning, which is critical for provability.

Let's take a look at the context that's possibly relevant to a procedural line
of code in a C++ program (most lines of code call at least one function).

==DOWN==

### Block context:

- loop condition (true in loop)
- loop postcondition (true after loop)
    - [inverse of loop condition!]
- current enclosing if-conditions
- negations of previous if-conditions (we're in the else block)
- catch (type)
- are we processing a sequence? (ranged for-loops)

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

<!--
## Example: procedural code

```cpp
auto dumb_depth_first_search(Node* start_node, auto&& op) {
    std::vector todo{start_node}; std::set seen{start_node};
    while (!todo.empty()) {            // post:!empty()
        auto const node = todo.back(); // pre: !empty()
        todo.pop_back();               // pre: !empty()
        if (op(node)) { return node; } // post: !op(node)
        for (auto const& child : node->children()) {
            if (seen.contains(&child)) { continue; }
            todo.push_back(&child); // pre: child not seen
            seen.insert(&child);
        }
    } // post: todo.empty(), !op(node) for reachable nodes
    return nullptr; // not found
}
```
-->

==DOWN==

For every Context grows a Monad.

-- Teta Pehta

<small>("For every disease there grows a flower")</small>

<!-- .slide: data-background-image="image/teta-pehta.jpg" data-background-position: "right" -->

==DOWN==

Monad is meant loosely - I mean a compositional context.

Composition is the key.

Functor, Applicative, Monad, and some other things are all valid here.

We also need indexed monads and graded monads.

==SLIDE==

### Limiting context

Most context is irrelevant at any one time.

*Which* part is up to convention and discipline.

Procedural languages have problems encoding proofs about context.

==DOWN==

In functional languages, all context is explicit.

This helps proofs.

Can we be disciplined in c++?

==SLIDE==

### Tools

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

Design for laziness: Make wrong things painful.

This is *good*. It allows more of the program to be context-free.

Push control flow, mutation, and I/O towards `main()`.

Also see Tony's "Complecting made Easy".

==DOWN==

Zen of python: flat is better than nested.

Have you considered flattening out your dependencies?

==SLIDE==

## Classic monads

- Maybe: `optional`
- Error: `expected`
- List: `std::ranges`
- Async: `std::execution`
- I/O: `std::execution` IO-resources
- State: what do you think OOP without PLOP is?
- Environment: `std::execution` has `read()` and `write()`

==DOWN==

## The final guideline

Name common patterns and control flow.

==SLIDE==

## `optional` - the Maybe monad

==DOWN==

## Pattern 1: short-circuit


Bind:

```
and_then :: (T -> opt<U>) -> (opt<T> -> opt<U>)
```

Pattern:

```cpp
opt ? f(*opt) : nullopt;
```

Example:

```cpp
auto parse_url(std::string) -> optional<URL>;

auto result
    = jsonfile.get_string("url") // optional<string>
    | and_then(parse_url)        // optional<URL>
    | and_then(http_get);        // optional<HTTP_RESULT>
```

==DOWN==

## Pattern 2: defaulting

Bind: 

```
or_else :: (() -> opt<T>) -> (opt<T> -> opt<T>)
```

Pattern: _Try multiple things and return the first one that succeeded_.

```
opt ? opt : f();
```

Extended example follows.

==DOWN==

Procedural way of implementing `get_hostname`:

```cpp
auto is_hostname_in_args(int, char const* const*) -> bool;
auto get_hostname_from_args(int, char const* const*) -> char const*;

auto get_hostname(int argc, char const* const* argv,
                  std::string default_hostname)
        -> std::string {
    // split query / getter
    if (is_hostname_in_args(argc, argv)) {
        // perhaps... might use optional here too?
        return get_hostname_from_args(argc, argv);
    }
    // ad-hoc Maybe
    if (char const* maybe_host = getenv("SERVICE_HOSTNAME");
        (maybe_host != nullptr) && (*maybe_host != '\0')) {
        return maybe_host;
    }
    return default_hostname;
}
```
<!-- .element class="stretch" -->

==DOWN==

## Functional rewrite

First, adapt to uniformity.

==DOWN==

### From: query/getter pair

```cpp
auto is_hostname_in_args(int, char const* const*) -> bool;
auto get_hostname_from_args(int, char const* const*) -> char const*;
```
<!-- .element: style="width:100%" -->

### To: `optional`-returning functor.

```cpp
constexpr auto maybe_hostname_from_args = 
    [](int argc, char const* const* argv) -> std::optional<std::string>
    {
        if (is_hostname_in_args(argc, argv)) {
            return get_hostname_from_args(argc, argv);
        }
        return std::nullopt;
    };
```
<!-- .element: style="width:100%" -->

==DOWN==

### From: `nullptr`-on-nothing

```cpp
auto std::getenv(char const*) -> char const*;
```
<!-- .element: style="width:100%" -->

### To: `nullopt`-on-nothing

```cpp
constexpr auto get_env = 
    [](std::string const& varname) -> optional<std::string>
    {
        if (char const* value = std::getenv(varname.c_str());
                value != nullptr) {
            return optional(std::string(value));
        }
        return {std::nullopt};
    };
```
<!-- .element: style="width:100%" -->

==DOWN==

## Tool: `filter`

For the "nonempty" bit, we'll want a `filter`:

```cpp [3,4,6|1,2]
constexpr auto filter = [](auto predicate) {
    return [=]<class T>(T&& value) -> optional<std::remove_cvref_t<T>> {
        if (predicate(value)) {
            return value;
        }
        return std::nullopt;
    };
};
```
<!-- .element style="width: 100%" -->

==DOWN==

First pass: put it together:

```cpp [1,2,10,11|8,9,14]
constexpr auto nonempty =
    [](auto const& s){return !s.empty();};

auto get_hostname(
            int argc, char const* const* argv,
            std::string const& default_hostname)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
          | or_else([]{
            return get_env("SERVICE_HOSTNAME")
                   .and_then(filter(nonempty));
           })
          // can add other ways
          | value_or(auto(default_hostname));
}
```
<!-- .element: style="width:100%" -->

Blech.

==DOWN==

We defined filter wrong. Let's fix it.

```cpp [13,14,16|2-6|11|10,1,19]
constexpr struct filter_t {
  template <typename P> struct closure { P pred; };

  template <typename P>
  constexpr auto operator()(P&& predicate) const {
      return closure{std::forward<P>(predicate)};
  }

  template <fn::some_optional Opt, typename P>
  friend constexpr auto operator|(Opt&& opt, closure<P> const& cl) {
      return std::forward<Opt>(opt).and_then(
          [&](auto&& v) -> std::remove_cvref_t<Opt> {
              if (cl.pred(v)) {
                  return v;
              }
              return std::nullopt;
          });
  }
} filter;
```
<!-- .element class="stretch" -->

==DOWN==

We can omit the `and_then`:

```cpp [6,7]
auto get_hostname(
            int argc, char const* const* argv,
            std::string const& default_hostname)
        -> std::string {
    return maybe_hostname_from_args(argc, argv)
         | or_else([]{ return get_env("SERVICE_HOSTNAME")
                            | filter(nonempty); })
         | value_or(auto(default_hostname));
}
```
<!-- .element: style="width:100%" -->

==DOWN==

### Aside: `some_optional`

We needed a concept for the above:

```cpp
// fastest-to-compile is_specialization_of I know of
template <typename X>
constexpr bool _some_optional_v = false;
template <typename T>
constexpr bool _some_optional_v<fn::optional<T>&>
    = true;
template <typename T>
constexpr bool _some_optional_v<fn::optional<T> const&>
    = true;
// same for std::optional

template <typename X>
concept some_optional = _some_optional_v<X&>;
```
<!-- .element: style="width:100%" -->

`fn::some_optional` of course already exists; but you will need this for your
own types.

==DOWN==

Everything called `some_XXX` is a concept like the above.

==SLIDE==

## Fix the defaulting.

If you don't need to make a choice, why make it?

Just let the caller do it:

```cpp
auto maybe_hostname_from_env(int argc, char const* const* argv)
        -> std::optional<std::string> {
    return maybe_hostname_from_args(argc, argv)
          .or_else([]{ return get_env("SERVICE_HOSTNAME")
                            | filter(nonempty); });
          // <-- no more default
}
```
<!-- .element style="width:100%" -->
==DOWN==

Usage:

```cpp
int main(int argc, char** argv) {
    auto const config_file =
        maybe_config_file_from_params(argc, argv);
    auto const target_hostname
       = maybe_hostname_from_env(argc, argv)
       | or_else([&]{
            return config_file.and_then(
                [](auto& c) { return c.maybe_get_hostname(); });
        })
       | value_or("default_hostname");
}
```
<!-- .element: style="width:100%" -->

This isn't always better - you might choose to do it in one or the other
location depending on the desired semantics.

==SLIDE==

## Recap: function "lifts" on `optional<T>`:

```cpp
and_then  :: (T  -> opt<U>) -> (opt<T> -> opt<U>);
transform :: (T  ->     U)  -> (opt<T> -> opt<U>);
or_else   :: (() -> opt<T>) -> (opt<T> -> opt<T>);
value_or  ::  T             -> (opt<T> -> T);
```

`value_or` is an escape operation - final default.

==SLIDE==

## `expected`: A monad for errors

Instead of `nullopt`, we get `E`.

The flavor is different. Error is error, maybe is maybe.

Notes:

The way C++ chose to model this is a tad different from the usual way of doing
`Error<E, T>`, where the constructors are `left` for errors and `right` for
success.

The `Error` monad looks superficially related to `Maybe`, but the two have a
very different flavor.

==DOWN==

There are as many `expected`s as there are `E`s.

Notes:
First, there is only one `Maybe` - but there are as many `Error`s as there are
error types.

Observe and contrast the operation table of monadic operations of `expected<T, E>`:

==DOWN==

## Function lifts on `expected<T, E>`

```cpp
and_then        :: (T -> exp<U, E>) -> (exp<T, E> -> exp<U, E >);
transform       :: (T ->     U    ) -> (exp<T, E> -> exp<U, E >);
or_else         :: (E -> exp<T, E>) -> (exp<T, E> -> exp<T, E >);
transform_error :: (E ->        E') -> (exp<T, E> -> exp<T, E'>);
value_or        ::  T               -> (exp<T, E> -> T);
```

- ++ `transform_error`
- `or_else` now takes the error type.
- `and_then` cannot change `E`.
- Neither can `or_else`.

This means we actually got an `optional`-per-`E`.

==DOWN==

`expected` is a domain-based composition type.

It is *not* better exceptions.

==DOWN==

## Pattern: short-circuit

```cpp
and_then        :: (T -> exp<U, E>) -> (exp<T, E> -> exp<U, E >);
transform       :: (T ->     U    ) -> (exp<T, E> -> exp<U, E >);
or_else         :: (E -> exp<T, E>) -> (exp<T, E> -> exp<T, E >);
transform_error :: (E ->        E') -> (exp<T, E> -> exp<T, E'>);
value_or        ::  T               -> (exp<T, E> -> T);
```

Happy monad:

- `and_then` lifts functions that can "fail".
- `transform` is a "bridge" - lifts functions that can't fail.

Sad monad:

- `or_else` is "recover" - the _other_ bind.
- `transform_error` is the error bridge.

==SLIDE==

## Parsers: a good fit for `expected`

Compositional parser infrastructure.

Every parser's signature is

```cpp
struct ParseError { std::span<char const> where; };
auto subgrammar(std::span<char const>)
     -> std::expected<
           pair<SomeGrammarNode, std::span<char const>>,
           ParseError
        >;
```

==DOWN==

For instance, `parse_int`

```cpp
template <typename T>
using parser_for = fn::expected<std::pair<T, std::span<char const>>, ParseError>;
auto parse_int(std::span<char const> src)
        -> parser_for<int> {
    if (src.empty()) { return unexpected(ParseError(src)); }
    bool negative = src[0] == '-';
    if (negative || src[0] == '+') { src = src.subspan(1); }
    if (src.empty()) { return unexpected(ParseError(src)); }
    int result = 0;
    if ('0' <= src[0] && src[0] <= '9') {
        result = src[0] - '0';
        src = src.subspan(1);
    }
    while (!src.empty() && '0' <= src[0] && src[0] <= '9') {
        // not checking for overflow
        result = result * 10 + (src[0] - '0');
        src = src.subspan(1);
    }
    return std::pair{result * (-1 * negative), src};
}
```
<!-- .element class="stretch" -->

==DOWN==

We could rework this to reduce the number of visible branches:

```cpp
auto parse_digit(std::span<char const> src) -> parser_for<int> {
  return (!src.empty() && '0' <= src[0] && src[0] <=  '9') 
         ? parser_for<int>(std::pair{src[0] - '0', src.subspan(1)});
         : unexpected(ParseError(src));
}

auto parse_positive_digit(std::span<char const> src) -> parser_for<int> {
    return parse_digit(src).and_then([&](auto r) {
        return r.first > 0 ? parser_for<int>(r) 
                           : unexpected(ParseError(src));
    });
}
```
<!-- .element style="width: 100%" -->

==DOWN==

Rework the "integral" portion...

```cpp
auto parse_unprefixed_int(std::span<char const> src)
    -> fn::expected<std::pair<int, std::span<char const>>, ParseError> {
    return parse_positive_digit(src)
           .transform([&](auto p){
        auto&& [result, rest] = p;
        while (auto d = parse_digit(rest)) {
            // not checking for overflow
            result = result * 10 + d.value().first;
            rest = d.value().second;
        }
        return std::pair{result, rest};
    });
}
```
<!-- .element style="width: 100%" -->

==DOWN==

The sign portion...

```cpp
auto parse_maybe_sign(std::span<char const> src) -> parser_for<int> {
    if (src.empty()) { return unexpected(ParseError(src)); }
    bool negative = src[0] == '-';
    if (negative || src[0] == '+') {
        return std::pair{negative:-1:1, src.subspan(1)};
    }
    return parse_digit(src) // just check it's a digit
           .transform([&](auto&&){return std::pair{1, src};});
}
```
<!-- .element style="width: 100%" -->

==DOWN==

And finally put it all together:

```cpp
auto parse_int(std::span<char const> src) -> parser_for<int> {
    return parse_maybe_sign(src)
        .and_then([](auto sign) {
            return parse_unprefixed_int(sign.second)
                .transform([&](auto p){
                    return std::pair{sign.first * p.first, p.second};
                });
        });
}
```
<!-- .element style="width: 100%" -->

==DOWN==

Seems limited.

It is.

Problem: `std::expected` is not exceptions. We can't *also* return `IOError`.

==SLIDE==

## Towards multiple errors

Given:

```cpp
template <typename T, typename... Ts>
concept any_of = 
    (... || std::same_as<std::remove_cvref_t<T>, Ts>);
```

==DOWN==

C++ has exceptions, and we want to catch any of them:

```cpp [2|3|5]
try {
    g(2); // throws... what?
} catch (any_of<E1, E2, E3> auto&& exc) {
    /* no you don't says C++ */
} // did I get all of them?
```

You need to know what `g` throws.

- Need polymorphic runtime matching.
- Need generic matching.

==DOWN==

How about:

```cpp
// auto g(int i) -> expected<int, variant<E1, E2, E3>>;
auto r = g(2).transform_error([](auto&& v) {
        return std::visit([](any_of<E1, E2, E3> auto&&) {
            return E2{};
        });
    });
// r :: expected<int, E2>
```

Alright-ish, but ...

==DOWN==

```cpp
// auto g(int i) -> expected<int, variant<E1, E2, E3>>;
auto r = g(2).transform_error([](auto&& v) {
        return std::visit(overload{
            [](any_of<E1, E2> auto&&) { return E1{}; },
            [](any_of<E3> auto&&)     { return E2{}; }
        });
    }); // ERROR: std::visit can't deal with multiple return types
```

It doesn't compose.

`variant` is not useful for errors. `visit` does not merge types.

Variant is an index-discriminated union. We need a union.


==SLIDE==

## `sum<Ts...>`

- `Ts...` are sorted and uniqued.

```cpp
transform :: {(Ti         -> sum<Uij...>) ; i,j} 
           -> (sum<Ti...> -> sum<Uij...>; i,j  )
```
<!-- .element: style="width:100%" -->

1. given an overload set that can handle any `Ti` in `Ts...`
2. wrap every return type in sum (and flatten)
3. `join` them; that's the return type.
4. Apply the overload set to the active member
5. embed into actual return type.

==DOWN==

### join for sums

```
join<sum<Ts...>, sum<Us...>> == sum<Ts..., Us...>
join<sum<Ts...>, T> == sum<Ts..., T>
join<T, sum<Ts...>> == sum<T, Ts...>
```

`join` sorts and uniques the types for sums.

==DOWN==

```cpp
sum_for<int, string>{2} 
    | transform(overload{
        [](int x) -> sum_for<NegativeInt, PositiveInt> {
            if (x < 0) {
                return NegativeInt{x};
            }
            return PositiveInt{x};
        },
        fn::identity
    }); // sum_for<string, NegativeInt, PositiveInt>
```

==DOWN==

## We also define it for `optional`

It has to look *inside*:

```
join<
    optional<T>,
    optional<U>,
> == optional<join<T, U>>
```

`join` is not defined for arbitrary types!

==DOWN==

All monadic operations need to "obey" what `transform` means for `sum`.

```cpp
optional<sum<string, >>{} 
   | or_else([]{ return optional{42}; })
// optional<sum_for<int, string>>
```

==DOWN==

## what about `expected`?

Has to look inside on both sides.

```
join<
    expected<T, E1>,
    expected<U, E2>,
> == expected<join<T, U>, join<E1, E2>>
```

==DOWN==

```cpp
parse_line(string) -> expected<Result, ParseError>;

read_line(file) // expected<string, IOError>
    | sum_error // expected<string, sum<IOError>>
    | and_then(parse_line) // expected<Result, sum<IOError, ParseError>>
```

==DOWN==

All other monads follow the same pattern.

==SLIDE==

## Back to exceptions

Just works!

```cpp
// auto g(int i) -> expected<int, sum<E1, E2, E3>>;
g(2) | transform_error(overload{
        [](any_of<E1, E3> auto&&) {
            return E1{};
        },
        [](E2 x) {print(x); return x;}
    });
```

==DOWN==

You don't handle every exception? Fail to compile.

```cpp
// auto g(int i) -> expected<int, sum<E1, E2, E3>>;
g(2) | transform_error(
        [](any_of<E1, E3> auto&&) {
            return E1{};
        }
    );
    // ERROR: not callable with E2
```

==DOWN==

You want to let some through? Do it explicitly:

```cpp
// auto g(int i) -> expected<int, sum<E1, E2, E3>>;
auto r = g(2).transform_error(
    fn::overload{
        [](E1 const&) { return E2{}; },
        fn::identity /* id-on-E2 and E3 */
    }
); // expected<int, sum<E2, E3>>
```

==SLIDE==

## Packs

Packs stand for "multiple arguments".

```cpp
pack{1, "asdf"s} 
| transform([](int x, string y) { return pack{x, y.size(), x+y}; })
| transform([](int x, size_t y, size_t z) { std::cout <<x << y << z; });
```

They have their own operation - `join`

==DOWN==

## `cat` for packs

```
pack<Ts...> & pack<Us...> == pack<Ts..., Us...>
pack<Ts...> & U           == pack<Ts..., U>
U & pack< Ts...>          == pack<U, Ts...>
pack<pack<Ts...>>         == pack<Ts...>
```

Packs autoflatten and concatenate unpacked arguments.

==DOWN==

It still distributes inside monads, same as `join`.

Multiple monadic arguments work as packs!

```cpp
struct config {
  template <typename T>
  auto get_maybe(std::string const& key) -> fn::optional<T>;
};

struct socket_addr { std::string hostname; int port; };

auto connect(config c) -> fn::optional<socket_addr> {
    return (c.get_maybe<std::string>("hostname")
            & c.get_maybe<int>("port"))
        // optional<pack<std::string, int>>
        | transform([](std::string && hostname, int port){
             return socket_addr{std::move(hostname), port};
        });
}
```
<!-- .element: style="width:100%" -->

==DOWN==

Creating objects is common.

We name common patterns.

```cpp [2,3,8]
template <typename T>
constexpr auto make = []<typename... Ts>(Ts&&... args) {
    return T(std::forward<Ts>(args)...);
};
auto connect(config c) -> std::optional<socket_addr> {
    return (c.get_maybe("hostname")
            & c.get_maybe("port"))
           | transform(make<socket_addr>);
    //                 ^^^^^^^^^^^^^^^^^
}
```
<!-- .element: style="width:100%" -->

==SLIDE==

## Error Packs

```cpp
auto parse_response(json const& doc)
    -> fn::expected<Response, ParseError>
{
    return (
        (doc.get_maybe("version") 
            | or_else([]{return ParseError("version is required");})
            | parse_version 
            | filter(eq(version{3, 14}), make<ParseError>)
        ) // expected<Version, ParseError>
        & (doc.get_maybe("id")
            | or_else([]{return ParseError("id is required");})
            | parse_int
            | transform(make<Id>)
        ) // expected<Id, ParseError>
        & /*...*/
        ) // expected-pack<Version, Id, ..., ParseError>
        | transform(make<Response>);
}
```
<!-- .element class="stretch" -->

==DOWN==

- We didn't forget error checking.
- No way to make a `Response` on failure.
- We validate everything due to strong types.

==SLIDE==

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

<!-- .element: class="stretch" -->


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

## Making returning and calling symmetric

`sum` is special:

`transform` finds the common return type (it's a sum)

```cpp
sum<A, B, C>{b} | transform(overload{
    [](B) {...} -> sum<E, F>
    [](A) {...} -> sum<pack<C, D>, E>,
    [](C) {...} -> G,
}); // sum<E, F, G, pack<C, D>>
```

==DOWN==

## `pack` unpacks into arguments

```cpp
pack{1, A{}, B{}} | transform([](int, A, B) {});
```


==DOWN==

## pack of sums does multiple dispatch

```cpp
pack<sum<A, B>, sum<X, Y>>{A{}, B{}}
| transform{overload{
    [](A, X) {...},
    [](A, Y) {...},
    [](B, X) {...},
    [](B, Y) {...}
}}:
```

... and it also finds the common type.

==DOWN==

## The algebra

For ONE `T`:

```cpp
sum<T> = pack<T> = sum<pack<T>> = pack<sum<T>>
```

==DOWN==

For multiple:

```
pack<sum<A, B>, sum<X, Y>> 
=
sum<pack<A, X>,
    pack<A, Y>,
    pack<B, X>,
    pack<B, Y>>
```

(works for more than one `sum` in a pack)

==DOWN==

## Flattening

`sum<sum<T, U>>` -> `sum<T, U>`


`pack<pack<T, U>, V>` -> `pack<T, U, V>`.

These equivalences let us define calling overload sets.

==DOWN==

An overload set is a function defined "in parts".

Overload set calls don't compose; you can call it once, but you can't call one
with a return value (you only get one type).

This makes pipelines not work.

==DOWN==

A state machine:

```cpp
using State = sum<Initialized, Connecting, Connected, Disconnected,
                  Fail>;
using Message = sum<Connect, Stop, Data, UnexpectedDisconnect>;
template <typename X>
concept some_live_state = any_of<X, Initialized, Connecting, Connected>;

auto transition(State s, Message m) {
  return (s & m) // pack<State, Message>
    | transform(fn::overload{ // double dispatch!
    [](Initialized s, Connect c)     -> sum<Connecting, Fail> {...},
    [](Connected s, Data d)          -> sum<Connected, Fail>  {...},
    [](some_live_state auto s, Stop) -> Disconnected          {...},
    [](some_live_state auto s, UnexpectedDisconnect)
                                     -> sum<Connecting, Fail> {...},
    [](auto s, auto m)               -> Fail                  {...}
  }); // -> <Connecting, Fail, Connected, Disconnected>
}
```
<!-- .element: class="stretch" -->

==SLIDE==

## Enter graded monads

<!--
Returning back to our `expected` with multiple error types - we need to make
`expected` work with that. We call that the `expected` context.
-->

Graded monads are... relaxed.

```cpp
auto version_or_error 
    = open(path)          // fn::expected<File, sum<DoesNotExist, PermError>>
    | and_then(read_line  // expected<std::string, IOError>
    ) // expected<std::string, sum<DoesNotExist, PermError, IOError>>
    | and_then(parse_version);
    // expected<Version, sum<DoesNotExist, PermError, IOError, SyntaxError>>
```
<!-- .element: style="width: 100%" -->

==DOWN==

`expected<T, sum<...>>` is a closed-polymorphic equivalent to c++ exceptions.

We can also handle the errors generically.

```cpp
version_or_error
    | on_error( // may return void
        overload{
        [](any_of<DoesNotExist, PermError, IOError> auto&& e) {
            std::print("could not read file {}", e.filename);
        },
        [](SyntaxError const& e) {
            std::print("syntax error near column {}", e.col);
        }
    });
```
<!-- .element: style="width: 100%" -->

==DOWN==

### Back to `get_maybe`...

Using `sum`, drop the type arg of `get_maybe`:

```cpp
struct config {
  auto get_maybe(std::string const& key) 
    -> fn::optional<fn::sum_for<std::string, int, float>>;
};
auto connect(config c) -> fn::optional<socket_addr> {
  return (c.get_maybe("hostname") & c.get_maybe("port"))
     //              ^ <std::string> no longer here
     // optional<pack<sum<...>, sum<...>>
    | and_then(fn::overload{
      [](std::string && hostname, int port) {
        return fn::optional(socket_addr{std::move(hostname), port});
      },
      // fail if types weren't correct
      [](auto const&...) { return std::nullopt; }
    });
}
```
<!-- .element: style="width: 100%" -->

==SLIDE==

## Drilling

What if you have a monad in a monad?

```cpp
expected<optional<int>, E>{optional{3}}
    | transform(
        transform(
            [](int x){ return format("{}", x); }
        )
    ); // expected<optional<string>>
```

==SLIDE==

<small>Congratulations on surviving this gentle introduction to the</small>

# `sum` - `pack` kaboodle

Making overload sets composable since 2023.

==SLIDE==

## Conclusion

In order to name common control flow patterns, we reach for named compositional
contexts, some of which are monads.

We explored the following contexts today:

- `optional` or "Maybe"
- `expected` or "Error"
- `sum`-`pack` kaboodle

And we touched on what it takes to make function call composition work the same
way going out as going in.

==DOWN==

Happy hacking!

==SLIDE==

Diagrams:

- regular monad-in-a-2-category diagram
- graded monad-in-a-2-category diagram

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

References: https://arxiv.org/pdf/2001.10274.pdf

TODO:
- think up questions for audience
- restructure examples to show good practive
- restructure to show strong types first
- intruduce sum and pack and how they're different from tuple and choice

-->
