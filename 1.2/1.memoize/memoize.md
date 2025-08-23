Simple memoized pure function behaviour consists of:
- Take in arguments
- Check if they are in the cache
- If they are, return the value in the cache
- If not, 
    - evaluate the function with the provided arguments
    - return the result

Assumptions for this to work well out-of-the-box:
- primitive arguments only, no references
- cache will not grow too big
    - small set of possible arguments
    - function will not be called many times

To make it more useful, let's break both assumptions and handle the repercussions.

To bound the cache
- each entry in the cache could have a TTL
- Limit the size with the LRU, remove the least used?

To check references
- instead of JSON.stringify(), use a nested map with `unknown` type
    - Alternative designs
        - Separate “result Map”: Instead of storing the result at the leaf, you could store { child: Map, result?: R } objects. That avoids a sentinel key entirely but adds an object allocation per node and complicates the structure. Using a unique Symbol is simpler and efficient.
- To optimize, “equal by value” objects should hit the same entry, derive a stable key
- use WeakMap to work with the GC
    - mention use cases
- Treat object arguments as immutable or inclue a version in the key, otherwise cache may be stable

Illustration of the nested map:
- For f(a, b, c):
	- root.get(a) -> Map1
	- Map1.get(b) -> Map2
	- Map2.get(c) -> Map3
	- Map3.set(RESULT, result)
- Subsequent calls with the same (a, b, c) traverse to the same leaf Map3 and read RESULT.
- We’re building a nested tree of Maps keyed by each argument position.
- Think of the cache as a trie (prefix tree) over the argument tuple.
	- root is the top Map.
	- For the first argument, we get or create a child Map at key args[0].
	- For the second argument, we go into that child and get or create a child at key args[1].
	- And so on until we’ve processed all arguments.
- The reducer:
	- Initial value: root (Map<unknown, unknown>)
	- Reducer function: getOrInit(map, key)
		- If map has key, return its Map child.
		- Else create a new Map child at key and return it.
	- After reducing across all args, the accumulator is the Map at the deepest level for that specific argument tuple. This is the “leaf” for that tuple in the trie.

Notes:
- Symbols are unique identities. Even two Symbols with the same description are different:
    - Symbol("memoize-result") !== Symbol("memoize-result")
    - Only this exact Symbol instance is used as the storage key. No user input can accidentally equal it, since users don’t have a reference to that specific Symbol.
- `any` disables type checking and can mask mistakes. `unknown` preserves type safety.

Why use a tuple type for args instead of “array”?
In TypeScript, tuples and arrays are different in important ways:

- Tuples preserve arity and per-position types
	- Tuple example: [number, string] means exactly two elements: first is number, second is string.
	- This matches a function’s parameter list precisely, so the memoized function’s signature stays exact.
- Arrays are homogeneous and length-agnostic
	- Array example: (number | string)[] means any-length array of numbers or strings; TypeScript can’t know how many arguments or which type belongs to which position.

Why this matters for memoize:
- A function’s parameters are inherently a tuple (fixed positions with specific types), not “some array.”
- Using Args extends readonly unknown[] lets TypeScript infer the actual tuple type:
	- For (a: number, b: string) => R, Args becomes readonly [number, string].
	- The returned memoized function will then have the exact same call signature (...args: [number, string]) => R.
- If you used unknown[] (a plain array), you’d lose:
	- The exact number of parameters (arity).
	- The specific type per position.
	- That would degrade the type safety of the returned function.
In short: tuples preserve the function’s parameter list precisely; arrays don’t.


