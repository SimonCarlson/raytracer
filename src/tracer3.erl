%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 11:45
%%%-------------------------------------------------------------------
-module(tracer3).

-export([tracer/2]).

-define(Delta, 0.001).

tracer(Camera, World) ->
  {W, H} = camera:size(Camera),
  Xs = lists:seq(1, W),
  Ys = lists:seq(1, H),
  [[ray(X, Y, Camera, World) || X <- Xs] || Y <- Ys].

ray(X, Y, Camera, World) ->
  Ray = camera:ray(X, Y, Camera),
  Depth = world:depth(World),
  ray(Ray, Depth, World).

ray(_Ray, 0, World) ->
  world:background(World);
ray(Ray, Depth, World) ->
  Objs = world:objects(World),
  case intersect(Ray, Objs) of
    {inf, _} ->
      world:background(World);
    {D, Obj} ->
      {ray, O, L} = Ray,

      I = vector:add(O, vector:smul(L, (D-?Delta))),

      Normal = objects:normal(I, Obj),

      Visible = visible(I, world:lights(World), Objs),

      Illumination = lights:combine(I, Normal,  Visible),

      Reflection = ray({ray, I, reflection(L, Normal)}, Depth-1, World),

      lights:illuminate(Obj, Reflection, Illumination, World)
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


visible(Point, Lights, Objs) ->
  lists:filter(fun(Light) -> clear(Point, lights:origin(Light), Objs)  end, Lights).

clear(Point, Origin, Objs) ->
  Dir = vector:normalize(vector:sub(Origin, Point)),
  lists:foldl(fun(Obj, Acc) ->
    case Acc of
      false ->
        false;
      true ->
        case objects:intersect(Obj, {ray, Point, Dir}) of
          no ->
            true;
          _ ->
            false
        end
    end
  end,
    true,
    Objs).

reflection(L, N) ->
  vector:sub(L,vector:smul(N,2*vector:dot(L,N))).
