defmodule IdenticonTest do
  use ExUnit.Case

  test "Encode the input string to a sequence of numbers" do
    expected = [116, 181, 101, 134, 90, 25, 44, 200, 105, 60, 83, 13, 72, 235, 56, 58]
    got = Identicon.hash("elixir")

    assert expected == got.hex
  end

  test "pick an RGB triplette from a sequence of numbers" do
    expected = {116, 181, 101}
    got = Identicon.hash("elixir") |> Identicon.pick_color()

    assert expected == got.color
  end

  test "Build a grid with hex code and index" do
    expected = [
      {116, 0},
      {181, 1},
      {101, 2},
      {181, 3},
      {116, 4},
      {134, 5},
      {90, 6},
      {25, 7},
      {90, 8},
      {134, 9},
      {44, 10},
      {200, 11},
      {105, 12},
      {200, 13},
      {44, 14},
      {60, 15},
      {83, 16},
      {13, 17},
      {83, 18},
      {60, 19},
      {72, 20},
      {235, 21},
      {56, 22},
      {235, 23},
      {72, 24}
    ]

    got = Identicon.hash("elixir") |> Identicon.pick_color() |> Identicon.build_grid()

    assert expected == got.grid
  end

  test "Remove odds numbers from the grid" do
    expected = [
      {116, 0},
      {116, 4},
      {134, 5},
      {90, 6},
      {90, 8},
      {134, 9},
      {44, 10},
      {200, 11},
      {200, 13},
      {44, 14},
      {60, 15},
      {60, 19},
      {72, 20},
      {56, 22},
      {72, 24}
    ]

    got =
      Identicon.hash("elixir")
      |> Identicon.pick_color()
      |> Identicon.build_grid()
      |> Identicon.filter_odds()

    assert expected == got.grid
  end

  test "Build the pixel grid, with start and end points to draw rectangles" do
    expected = [
      {{0, 0}, {50, 50}},
      {{200, 0}, {250, 50}},
      {{0, 50}, {50, 100}},
      {{50, 50}, {100, 100}},
      {{150, 50}, {200, 100}},
      {{200, 50}, {250, 100}},
      {{0, 100}, {50, 150}},
      {{50, 100}, {100, 150}},
      {{150, 100}, {200, 150}},
      {{200, 100}, {250, 150}},
      {{0, 150}, {50, 200}},
      {{200, 150}, {250, 200}},
      {{0, 200}, {50, 250}},
      {{100, 200}, {150, 250}},
      {{200, 200}, {250, 250}}
    ]

    got =
      Identicon.hash("elixir")
      |> Identicon.pick_color()
      |> Identicon.build_grid()
      |> Identicon.filter_odds()
      |> Identicon.build_pixels()

    assert expected == got.pixel_map
  end

  test "Generate the png image" do
    expected = <<97, 69, 27, 148, 24, 58, 38, 1, 33, 254, 79, 107, 69, 241, 191, 47>>

    got =
      Identicon.hash("elixir")
      |> Identicon.pick_color()
      |> Identicon.build_grid()
      |> Identicon.filter_odds()
      |> Identicon.build_pixels()
      |> Identicon.draw_image()

    assert expected == :crypto.hash(:md5, got)
  end
end
