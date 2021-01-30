defmodule Identicon.CLI do
  def main(args) do
    {_, input, _} = OptionParser.parse(args, strict: [])

    File.write(Identicon.filename(input), Identicon.from_string(input))
  end
end
