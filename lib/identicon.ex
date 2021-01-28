defmodule Identicon do
  def from_string(str) do
    hash = :crypto.hash(:md5, str) |> :binary.bin_to_list()
    generate_image(color(hash), coords(hash)) |> save("#{str}.png")
  end

  def color([r, g, b | _]) do
    {r, g, b}
  end

  def coords(hash) do
    hash
    |> Enum.chunk_every(3, 3, :discard)
    |> Enum.map(&mirror/1)
    |> List.flatten()
    |> Enum.with_index()
    |> Enum.filter(fn {v, _} = _x -> rem(v, 2) === 0 end)
    |> Enum.map(&square/1)
  end

  def mirror([one, two | _] = row) do
    row ++ [two, one]
  end

  def square({_, i}) do
    x = rem(i, 5) * 50
    y = div(i, 5) * 50
    top_left = {x, y}
    bottom_right = {x + 50, y + 50}
    {top_left, bottom_right}
  end

  def generate_image(color, coords) do
    image = :egd.create(250, 250)
    fill = :egd.color(color)

    Enum.each(coords, fn {start, stop} ->
      :egd.filledRectangle(image, start, stop, fill)
    end)

    :egd.render(image)
  end

  def save(image, filename) do
    File.write(filename, image)
  end
end
