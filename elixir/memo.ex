defmodule Memo do
  @moduledoc """
  Memoization using a trie (nested maps) keyed by a list of primitive args.
  Primitives: integers, booleans, atoms, binaries.

  Returns a closure that keeps its cache in an Agent process.
  Cache lifetime = Agent lifetime (ephemeral).
  """

  @leaf :leaf

  @doc """
  Returns a function that memoizes `fun`, which must accept a single list of args.

  Example:
      memo = Memo.memoize(&Enum.sum/1)
      memo.([1, 2, 3]) #=> 6
  """
  def memoize(fun) when is_function(fun, 1) do
    {:ok, agent} = Agent.start_link(fn -> %{} end)

    fn args_list when is_list(args_list) ->
      Agent.get_and_update(agent, fn trie ->
        case trie_lookup(args_list, trie) do
          {:hit, value} ->
            {value, trie}

          :miss ->
            value = fun.(args_list)
            {value, trie_insert(args_list, value, trie)}
        end
      end)
    end
  end

  # Trie ops
  defp trie_lookup([], trie) do
    case Map.get(trie, @leaf) do
      nil -> :miss
      v -> {:hit, v}
    end
  end

  defp trie_lookup([k | rest], trie) do
    case Map.get(trie, k) do
      nil -> :miss
      sub when is_map(sub) -> trie_lookup(rest, sub)
      _ -> :miss
    end
  end

  defp trie_insert([], value, trie), do: Map.put(trie, @leaf, value)

  defp trie_insert([k | rest], value, trie) do
    sub0 = Map.get(trie, k, %{})
    sub1 = trie_insert(rest, value, sub0)
    Map.put(trie, k, sub1)
  end
end
