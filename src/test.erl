%%%-------------------------------------------------------------------
%%% @author Simon
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. feb 2015 11:08
%%%-------------------------------------------------------------------
-module(test).
-author("Simon").

%% API
-compile(export_all).


snap() ->
  Camera = camera:normal({0,0,-800}, {600,400}),

  Obj1 = objects:sphere( 140, { 0, 0, 100}, [{color, {1.0, 0.5, 0}}]),
  Obj2 = objects:sphere( 50, { 200, 0, -200}, [{color, {0, 0.8, 0.2}}]),
  Obj3 = objects:sphere(50, { -80, 0, -400}, [{color, {0.1, 0.1, 1.0}}]),

  Light1 = lights:light({-1000, 1000, -100}, {1.0, 0.3, 0.3}),
  Light2 = lights:light({ 800, 800, -800}, {0.3, 1.0, 0.3}),
  Light3 = lights:light({ 800, -800, 800}, {0.3, 0.3, 1.0}),



  Rows = tracer2:tracer(Camera, [Obj1, Obj2, Obj3]),
  ppm:write("test1.ppm", Rows).