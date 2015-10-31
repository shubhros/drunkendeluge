-module(geometry).
-export([area/1, perimeter/1, test_area/0, test_perimeter/0]).

area({rectangle, Width, Height}) ->
    Width * Height;
area({square, Side}) ->
    Side * Side;
area({circle, Radius}) ->
    3.14 * Radius * Radius;
area({right_triangle, Base, Height}) ->
    0.5 * Base * Height.

perimeter({rectangle, Width, Height}) ->
    2 * (Width + Height);
perimeter({square, Side}) ->
    4 * Side;
perimeter({circle, Radius}) ->
    2 * 3.14 * Radius;
perimeter({right_triangle, Base, Height}) ->
    Base + Height + math:sqrt(Base * Base + Height * Height).





test_area() ->
    12 = area({rectangle, 6, 2}),
    144 = area({square, 12}),
    3.14 = area({circle, 1}),
    0.5 = area({right_triangle, 1, 1}),
    tests_for_area_worked.

test_perimeter() ->
    10 = perimeter({rectangle, 2,3}),
    8 = perimeter({square, 2}),
    6.28 = perimeter({circle, 1}),
    12.0 = perimeter({right_triangle, 3, 4}),
    tests_for_perimeter_worked.
