#!/usr/bin/env escript
%% -*- erlang -*-
%% %%! -smp enable -sname day_one

-module(aoc_day_1a).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
  ?assert(distance(<<"R2, L3">>) =:= 5),
  ?assert(distance(<<"R2, R2, R2">>) =:= 2),
  ?assert(distance(<<"R5, L5, R5, R3">>) =:= 12).

distance(Input) -> 
  StartDirection = 36,
  StartPosition = {0, 0},
  Steps = re:split(Input, ", ", [{return, list}, trim]),
  EndPosition = navigate(Steps, StartPosition, StartDirection),

  taxicab_distance(StartPosition, EndPosition).
  
taxicab_distance({P1, P2}, {Q1, Q2}) -> abs(P1 - Q1) + abs(P2 - Q2).

navigate([], Position, _Direction) -> Position;

navigate(Steps, Position, Direction) ->
  [Step | T] = Steps,
  {DirectionOffset, LengthOffset} = step_offset(Step),
  NewDirection = adjust_direction(DirectionOffset, Direction),
  NewPosition = adjust_position(Position, NewDirection, LengthOffset),

  navigate(T, NewPosition, NewDirection).

adjust_position(Position, Direction, Length) ->
  {X, Y} = Position,

  Result = case Direction of
    36 -> {(X + Length), Y};
    18 -> {(X - Length), Y};
    9 -> {X, (Y + Length)};
    27 -> {X, (Y - Length)}
  end,

  Result.

adjust_direction(DirectionOffset, Direction) ->
  NewDirection = Direction + DirectionOffset,

  if NewDirection > 36 ->
       NewDirection - 36;

     NewDirection < 0 ->
       NewDirection + 36;

     NewDirection == 0 ->
       36;

     true ->
       NewDirection
  end.

step_offset(Step) ->
  [RightOrLeft | Count] = Step,

  Direction = case [RightOrLeft] of
                "R" -> 9;
                "L" -> -9
              end,

  {Direction, list_to_integer(Count)}.
