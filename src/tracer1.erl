%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 11:30
%%%-------------------------------------------------------------------
-module(tracer1).

-export([tracer/2]).

-define(Black, {0,0,0}).
-define(White, {255,255,255}).

tracer(Camera, Objects) ->
  {W, H} = camera:size(Camera),
  Xs = lists:seq(1, W),
  Ys = lists:seq(1, H),
  [[ray(X, Y, Camera, Objects) || X <- Xs] || Y <- Ys].

ray(X, Y, Camera, Objects) ->
  Ray = camera:ray(X, Y, Camera),
  ray(Ray, Objects).

ray(Ray, Objects) ->
  case intersect(Ray, Objects) of
    {inf, _} ->
      ?Black;
    {_, Obj} ->
      objects:color(Obj)
  end.

intersect(Ray, Objs) ->
  lists:foldl(fun(Obj, Sofar) ->
    {Dist, _} = Sofar,
    case objects:intersect(Obj, Ray) of
      {ok, D} when D < Dist ->
        {D, Obj};
      _ ->
        Sofar
    end
  end,
    {inf, no},
    Objs).


