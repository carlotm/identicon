defmodule Identicon.CLI do
  def main(args) do
    {_, input, _} = OptionParser.parse(args, strict: [])
    Identicon.from_string(input)
  end
end
