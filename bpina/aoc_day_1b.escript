#!/usr/bin/env escript
%% -*- erlang -*-
%% %%! -smp enable -sname day_one

-module(aoc_day_1a).

-include_lib("eunit/include/eunit.hrl").

main(_) -> ?assert(distance(<<"R8, R4, R4, R8">>) =:= 4).

distance(Input) -> 
  StartDirection = 36,
  StartPosition = {0, 0},
  Steps = re:split(Input, ", ", [{return, list}, trim]),
  Path = #{},
  EndPosition = navigate(Steps, StartPosition, StartDirection, Path),

  taxicab_distance(StartPosition, EndPosition).
  
taxicab_distance({P1, P2}, {Q1, Q2}) -> abs(P1 - Q1) + abs(P2 - Q2).

navigate([], Position, _Direction, _Path) -> Position;

navigate(Steps, Position, Direction, Path) ->
  [Step | T] = Steps,
  {DirectionOffset, LengthOffset} = step_offset(Step),
  NewDirection = adjust_direction(DirectionOffset, Direction),
  
  case extend_path(Path, Position, NewDirection, LengthOffset) of
    {stopped, {_NewPath, EndPosition}} -> 
      EndPosition;
    {moved, {NewPath, NewPosition}} -> 
      navigate(T, NewPosition, NewDirection, NewPath)
  end.

extend_path(Path, CurrentPosition, _Direction, 0) -> {moved, {Path, CurrentPosition}};
extend_path(Path, CurrentPosition, Direction, Length) ->
  NewPosition = increment_position(CurrentPosition, Direction),

  case maps:is_key(NewPosition, Path) of
    true -> 
      {stopped, {Path, NewPosition}};
    false -> 
      NewPath = maps:put(NewPosition, 1, Path),
      extend_path(NewPath, NewPosition, Direction, Length - 1)
  end.

increment_position(Position, Direction) ->
  {X, Y} = Position,

  Result = case Direction of
    36 -> {(X + 1), Y};
    18 -> {(X - 1), Y};
    9 -> {X, (Y + 1)};
    27 -> {X, (Y - 1)}
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
