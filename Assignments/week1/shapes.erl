%%%-------------------------------------------------------------------
%%% @author alex.verkeenko@gmail.com
%%%-------------------------------------------------------------------
-module(shapes).
-author("alex").

%% API
-export([perimeter/1, area/1, enclose/1]).

% allowed shapes definitions
% {circle, {X,Y}, R}
% {rectangle, {X,Y}, H, W}
% {triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}

% calculate line length by it coordinates
line_length({X1,Y1}, {X2,Y2})->
  math:sqrt((X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1)).

% find max provided value (same values case not holded)
get_max_value(A, B, C) when A > B, A > C -> A;
get_max_value(A, B, C) when B > A, B > C -> B;
get_max_value(A, B, C) when C > A, C > B -> C.

% find max provided value (same values case not holded)
get_min_value(A, B, C) when A < B, A < C -> A;
get_min_value(A, B, C) when B < A, B < C -> B;
get_min_value(A, B, C) when C < A, C < B -> C.

% perimeter
perimeter({circle, {_X,_Y}, R}) ->
  2*math:pi()*R;

perimeter({rectangle, {_X,_Y}, H, W}) ->
  2*(H+W);

perimeter({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
  line_length({X1,Y1}, {X2,Y2}) +
    line_length({X1,Y1}, {X3,Y3}) +
    line_length({X2,Y2}, {X3,Y3}).

% area
area({circle, {_X,_Y}, R}) ->
  math:pi() * R * R;

area({rectangle, {_X,_Y}, H, W}) ->
  H * W;

area({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
  A = line_length({X1,Y1}, {X2,Y2}),
  B = line_length({X1,Y1}, {X3,Y3}),
  C = line_length({X2,Y2}, {X3,Y3}),
  P = (A + B + C)/2,
  math:sqrt(P * (P - A) * (P - B) * (P - C)).

%enclose
enclose({circle, {X,Y}, R}) ->
  {rectangle, {X-R, Y-R}, 2*R, 2*R};

enclose({rectangle, {X,Y}, H, W}) ->
  {rectangle, {X,Y}, H, W};

enclose({triangle, {X1,Y1}, {X2,Y2}, {X3,Y3}}) ->
  X_MIN=get_min_value(X1, X2, X3),
  X_MAX=get_max_value(X1, X2, X3),
  Y_MIN=get_min_value(Y1, Y2, Y3),
  Y_MAX=get_max_value(Y1, Y2, Y3),

  {rectangle, {X_MIN,Y_MIN}, Y_MAX-Y_MIN, X_MAX-X_MIN}.
