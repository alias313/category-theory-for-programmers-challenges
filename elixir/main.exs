Code.require_file("memo.ex")

defmodule Main do
  # Fast-ish and clear: handle even numbers, then check odd divisors only.
  def is_prime(n) when is_integer(n) and n < 2, do: false
  def is_prime(2), do: true
  def is_prime(n) when is_integer(n) and rem(n, 2) == 0, do: false

  def is_prime(n) when is_integer(n) do
    limit = :math.sqrt(n) |> trunc()
    is_prime_odd_div(n, 3, limit)
  end

  def is_prime(_), do: false

  defp is_prime_odd_div(_n, i, limit) when i > limit, do: true

  defp is_prime_odd_div(n, i, limit) do
    if rem(n, i) == 0 do
      false
    else
      is_prime_odd_div(n, i + 2, limit)
    end
  end

  def is_prime_list(ns) when is_list(ns) do
    Enum.all?(ns, &is_prime/1)
  end

  def primes do
    [
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47,
      53, 59, 61, 67, 71, 73, 79, 83, 89, 97,
      101, 103, 107, 109, 113, 127, 131, 137, 139, 149,
      1_981_201_020_802_099,
      144_403_552_893_599,
      29_100_036_564_239,
      1_012_020_412_021,
      166_666_666_667,
      10_999_999_999
    ]
  end

  def main do
    memo_all_prime = Memo.memoize(&is_prime_list/1)
    primes = primes()

    {t1_us, r1} = :timer.tc(fn -> memo_all_prime.(primes) end)
    true = r1
    first_ms = t1_us / 1000.0

    {t2_us, _r2} = :timer.tc(fn -> memo_all_prime.(primes) end)
    second_ms = t2_us / 1000.0

    speedup =
      if second_ms > 0.0 do
        first_ms / second_ms
      else
        :infinity
      end

    speedup_str =
      case speedup do
        :infinity -> "∞x"
        _ -> :io_lib.format("~.1f~s", [speedup, "x"]) |> to_string()
      end

    IO.puts(
      "[elixir:all-primes] firstMs=#{Float.round(first_ms, 3)}ms " <>
        "secondMs=#{Float.round(second_ms, 3)}ms speedup=#{speedup_str}"
    )

    :ok
  end
end

Main.main()
