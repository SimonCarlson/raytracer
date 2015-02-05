%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 10:24
%%%-------------------------------------------------------------------
-module(objects).

-define(Color, {255,100,100}).
%-define(Color, {1.0,0.4,0.4}).

-define(Brilliance, {0.8,0.8,0.8}).

-record(sphere, {radius=2, center, color=?Color, brilliance=?Brilliance}).

-export([sphere/2, sphere/3, intersect/2, color/1, normal/2, brilliance/1]).

sphere(Radius, Center) ->
  #sphere{radius=Radius, center=Center}.

sphere(Radius, Center, Opt) ->
  Color = case lists:keyfind(color, 1, Opt) of
            {color, C} ->
              C;
            false ->
              ?Color
          end,
  Brilliance = case lists:keyfind(brilliance, 1, Opt) of
                 {brilliance, B} ->
                   B;
                 false ->
                   ?Brilliance
               end,
  #sphere{radius=Radius, center=Center, color=Color, brilliance=Brilliance}.

color(#sphere{color=Color}) ->  Color.

brilliance(#sphere{brilliance=Brilliance}) ->
  Brilliance.

normal(I, #sphere{center=C}) ->
  vector:normalize(vector:sub(I,C)).

intersect(#sphere{radius=R, center=C}, {ray, O, L}) ->
  K = vector:sub(C,O),
  A = vector:dot(L, K),
  A2 = math:pow(A, 2),
  K2 = math:pow(vector:norm(K),2),
  R2 = math:pow(R,2),
  T2 = A2 - K2 + R2,
  closest(T2, A).

closest(T2, A) ->
  if 	T2 < 0 ->
    no;
    true ->
      T = math:sqrt(T2),
      D1 = A - T,  D2 = A + T,
      if
        (D1 > 0.0) and (D2 > 0.0) ->
          {ok, min(D1,D2)};
        (D1 > 0.0)  >
          {ok, D1};
        (D2 > 0.0) ->
          {ok, D2} ;
        true ->
          no
      end
  end.


