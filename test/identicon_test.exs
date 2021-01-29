defmodule IdenticonTest do
  use ExUnit.Case

  setup do
    on_exit(fn ->
      File.rm("elixir.png")
      File.rm("_.png")
    end)
  end

  test "Generate an identicon from a string" do
    expected = <<97, 69, 27, 148, 24, 58, 38, 1, 33, 254, 79, 107, 69, 241, 191, 47>>
    Identicon.from_string("elixir")
    content = File.read!("elixir.png")
    got = :crypto.hash(:md5, content)

    assert expected == got
  end

  test "Generate an identicon with name _.png from an empty string" do
    Identicon.from_string("")

    assert File.exists?("_.png")
  end
end
