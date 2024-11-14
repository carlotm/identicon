-module(edenticon_core).
-export([as/3]).

as(Format, In, Size) ->
    {HashedStr, Coords} = hash(In, Size),
    Color = color(Format, HashedStr),
    generate(Format, Color, Coords, Size).

%%
%% Helpers!
%%

generate(svg, Color, Coords, Size) ->
    RectOpts = [{fill, Color}],
    SVG1 = svg:init(Size, Size),
    SVG2 = lists:foldl(fun(Curr, Acc) ->
        {{X1, Y1} = TopLeft, {X2, Y2}} = rect_round(Curr),
        RectSize = {X2 - X1, Y2 - Y1},
        Rect = svg:rect({TopLeft, RectSize}, RectOpts),
        svg:add(Rect, Acc)
    end, SVG1, Coords),
    list_to_binary(lists:flatten(svg:export(SVG2)));
generate(png, Color, Coords, Size) ->
    Image = egd:create(Size, Size),
    lists:foreach(fun(Curr) ->
      {TopLeft, BottomRight} = rect_round(Curr),
      egd:filledRectangle(Image, TopLeft, BottomRight, Color)
    end, Coords),
    ImageData = egd:render(Image),
    Encoded = base64:encode(ImageData),
    <<"<img src=\"data:image/png;base64,",Encoded/binary,"\" />">>.

hash(In, Size) ->
    Hashed = crypto:hash(md5, In),
    HashedStr = binary:bin_to_list(Hashed),
    Coords = coords(HashedStr, Size div 5),
    {HashedStr, Coords}.

color(svg, [R, G, B | _]) ->
    L = [integer_to_list(X) || X <- [R, G, B]],
    ["rgb(", lists:join(",", L), ")"];
color(png, [R, G, B | _]) -> egd:color({R, G, B}).

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

rect_round({{X1, Y1}, {X2, Y2}}) ->
    {{round(X1), round(Y1)}, {round(X2), round(Y2)}}.
