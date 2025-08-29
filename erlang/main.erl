%% main.erl
-module(main).
-export([main/0, is_prime/1, is_prime_list/1]).

%% is_prime for non-negative integers
is_prime(N) when is_integer(N), N < 2 -> false;
is_prime(2) -> true;
is_prime(N) when is_integer(N) ->
  Limit = trunc(math:sqrt(N)),
  is_prime_div(N, 2, Limit);
is_prime(_) -> false.

is_prime_div(_N, I, Limit) when I > Limit -> true;
is_prime_div(N, I, Limit) ->
  case N rem I of
    0 -> false;
    _ -> is_prime_div(N, I + 1, Limit)
  end.

%% Check a list of integers: true if all are prime
is_prime_list(Ns) when is_list(Ns) ->
  all_true([is_prime(N) || N <- Ns]).

all_true([]) -> true;
all_true([true | T]) -> all_true(T);
all_true([_ | _]) -> false.

%% Memoize the list-accepting function
memoize_list_fun(Fun) ->
  MemoF = memo:memoize(Fun),
  fun(ArgsList) -> MemoF(ArgsList) end.

%% Example primes and benchmark
primes() ->
  [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,
   53,59,61,67,71,73,79,83,89,97,
   101,103,107,109,113,127,131,137,139,149,
   1981201020802099,
   144403552893599,
   29100036564239,
   1012020412021,
   166666666667,
   10999999999].

main() ->
  MemoAllPrime = memoize_list_fun(fun is_prime_list/1),
  PRIMES = primes(),

  {T1Micros, R1} = timer:tc(fun() -> MemoAllPrime(PRIMES) end),
  true = R1, % assert
  FirstMs = T1Micros / 1000.0,

  {T2Micros, R2} = timer:tc(fun() -> MemoAllPrime(PRIMES) end),
  _ = R2,
  SecondMs = T2Micros / 1000.0,

  Speedup =
    case SecondMs > 0 of
      true -> FirstMs / SecondMs;
      false -> 1.0e308
    end,
  SpeedupStr =
    case SecondMs > 0 of
      true -> io_lib:format("~.1fx", [Speedup]);
      false -> "∞x"
    end,

  io:format("[erlang:all-primes] firstMs=~.3fms secondMs=~.3fms speedup=~s~n",
            [FirstMs, SecondMs, SpeedupStr]),
  ok.
