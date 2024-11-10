-module(edenticon_core).
-export([from_string/2]).

from_string(In, Size) ->
    Hashed = crypto:hash(md5, In),
    HashedStr = binary:bin_to_list(Hashed),
    Color = color(HashedStr),
    Coords = coords(HashedStr, Size div 5),
    write_image(Color, Coords, Size).

color([R, G, B | _]) -> {R, G, B}.

coords(Str, U) ->
    Groups = group_by(Str, 3),
    Mirrored = lists:map(fun mirror/1, Groups),
    Flatten = lists:flatten(Mirrored),
    WithIndex = lists:enumerate(0, Flatten),
    Evens = lists:filter(fun({_, V}) -> V rem 2 == 0 end, WithIndex),
    Coords = lists:map(fun(T) -> square(T, U) end, Evens),
    Coords.

group_by([], _) -> [];
group_by(L, Count) when Count > length(L) ->
    [L];
group_by(L, Count) ->
    {H, T} = lists:split(Count, L),
    case length(T) >= Count of
        true -> [H | group_by(T, Count)];
        false -> [H]
    end.

mirror([One, Two | _] = Row) ->
    Row ++ [Two, One].

square({I, _}, U) ->
    X = (I rem 5) * U,
    Y = (I div 5) * U,
    TopLeft = {X, Y},
    BottomRight = {X + U, Y + U},
    {TopLeft, BottomRight}.

write_image(Color, Coords, Size) ->
    Image = egd:create(Size, Size),
    FillColor = egd:color(Color),
    lists:foreach(fun({{X1, Y1}, {X2, Y2}}) ->
      Start = {round(X1), round(Y1)},
      Stop = {round(X2), round(Y2)},
      egd:filledRectangle(Image, Start, Stop, FillColor)
    end, Coords),
    egd:render(Image).
