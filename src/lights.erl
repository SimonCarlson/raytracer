%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 11:44
%%%-------------------------------------------------------------------
%%% This module will handle everythning that is related to lights and
%%% color composition. Colors are represented as {R,G,B} where the
%%% values are 0.0 to 1.0.

-module(lights).

-export([light/2, origin/1, illuminate/3, illuminate/4, combine/3,  rgb255/1]).

-record(light, {origin, color={1.0,1.0,1.0}}).


light(Origin, Color) ->
  #light{origin=Origin, color=Color}.

origin(#light{origin=Origin}) ->
  Origin.


%%% find the color of a point given color of object, combined light
%%% and ambient light

illuminate(Obj, Ill, World) ->
  Color = objects:color(Obj),
  Ambient = world:ambient(World),
  ill(Color, add(Ill, Ambient)).

%%& also handle lights from reflection

illuminate(Obj, Refl, Ill, World) ->
  Color = objects:color(Obj),
  Bril = objects:brilliance(Obj),
  Ambient = world:ambient(World),
  add(ill(Color, add(Ill, Ambient)), ill(Refl, Bril)).


%%% the combined contribution from all visible light sources

combine(Point, Normal, Lights) ->
  lists:foldl(fun(#light{origin=Src, color=Clr}, Contr) ->
    add(contribute(Point, Normal, Src, Clr), Contr)
  end,
    {0,0,0},
    Lights).


%%% the conribution of a light source give the normal vector

contribute(Point, Normal, Source, {R,G,B}) ->
  Direction = vector:normalize(vector:sub(Source, Point)),
  Cos = (vector:dot(Direction, Normal)),
  {R*Cos, G*Cos, B*Cos}.


%%% combine two light sources

add({R1,G1,B1}, {R2,G2,B2}) ->
  {(1 - ((1-R1)*(1-R2))), (1 - ((1-G1)*(1-G2))), (1 - ((1-B1)*(1-B2)))}.

%%% illuminate the surface with a colored light

ill({R1,G1,B1}, {R2,G2,B2}) ->
  {R1*R2, G1*G2, B1*B2}.


%%% convert to {255,255,255}

rgb255(Rows) ->
  lists:map(fun(R) ->
    lists:map(fun({Ri,Gi,Bi}) ->
      {trunc(Ri*255), trunc(Gi*255), trunc(Bi*255)}
    end,
      R)
  end,
    Rows).



