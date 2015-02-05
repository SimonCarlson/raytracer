%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 10:23
%%%-------------------------------------------------------------------
-module(camera).

-export([normal/2, size/1, ray/3]).

-record(camera, {origin, %  the position of the camera
  corner, % the upper left corner of the plane
  right,  % the vector in the plane from the corner going right
  down,   % the vector in the  plane from the corner going down
  size    % the {width, height}
}).

%%% A normal camera has a position and a direction; it also has a
%%% "normal" distance to the canvas. The distance is aprx as large as
%%% the diameter of the canvas size. The canvas size as the number of
%%% pixles in width and height.
%%%
%%% We make it easy and direct the camera straight forward
%%% i.e. {0,0,1}.



normal(Pos,  Size) ->
  {Width, Height} = Size,

  %% this is the deafult direction
  Direction = {0,0,1},

  Distance = trunc(math:sqrt(math:pow(Width,2) + math:pow(Height, 2))),
  Center = vector:add(Pos, vector:smul(Direction, Distance)),
  Left = -trunc(Width/2),
  Up = trunc(Height/2),

  %% this will change if we change direction
  Corner = vector:add(Center, {Left,Up,0}),

  #camera{origin=Pos,
    corner = Corner,
    right = {1, 0, 0},
    down =  {0,-1, 0},
    size= Size}.


size(#camera{size=Size}) ->
  Size.

ray(X, Y, Camera) ->
  Origin = Camera#camera.origin,
  Xpos = vector:smul(Camera#camera.right, X),
  Ypos = vector:smul(Camera#camera.down, Y),
  XYpos = vector:add(Xpos, Ypos),
  Pos = vector:add(Camera#camera.corner, XYpos),
  Dir = vector:normalize(vector:sub(Pos, Origin)),
  {ray, Origin, Dir}.


%BOMB