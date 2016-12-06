Require Import ZArith.

Inductive cardinal_direction : Type :=
| north : cardinal_direction
| east : cardinal_direction
| south : cardinal_direction
| west : cardinal_direction.

Inductive turn_direction : Type :=
| left90 : turn_direction
| right90 : turn_direction.

Inductive coordinate : Type :=
| coord : Z -> Z -> coordinate.

Inductive nav_state : Type :=
| nav : cardinal_direction -> coordinate -> nav_state.

Definition start_state : nav_state :=
  nav north (coord 0 0).

Definition turn (turn_dir : turn_direction) (s : nav_state) : nav_state :=
  match s with
  | nav heading pos =>
    nav (match heading, turn_dir with
         | north, left90 => west
         | north, right90 => east
         | east, left90 => north
         | east, right90 => south
         | south, left90 => east
         | south, right90 => west
         | west, left90 => south
         | west, right90 => north
         end)
        pos
  end.

Open Scope Z_scope.

Definition walk (blocks : nat) (s : nav_state) : nav_state :=
  let b := Z.of_nat blocks in
  match s with
  | nav heading pos =>
    nav heading
        (match pos with
         | coord n e =>
           (match heading with
            | north => coord (n + b) e
            | south => coord (n - b) e
            | east => coord n (e + b)
            | west => coord n (e - b)
            end)
         end)
  end.

Require Import Coq.Lists.List.
Open Scope list_scope.
Import ListNotations.

Fixpoint follow_directions (from : nav_state) (ds : list (turn_direction * nat)) : coordinate :=
  match ds with
  | [] => match from with
         | nav _ p => p
         end
  | (t,b) :: ds => follow_directions (walk b (turn t from)) ds
  end.

Definition manhatten_dist (p1 p2 : coordinate) : nat :=
  match p1, p2 with
  | coord n1 e1, coord n2 e2 =>
    let delta_ns := Z.abs_nat (n1 - n2) in
    let delta_ew := Z.abs_nat (e1 - e2) in
    plus delta_ns delta_ew
  end.

Definition blocks_away (ds : list (turn_direction * nat)) : nat :=
  match start_state with
  | nav _ origin => manhatten_dist origin (follow_directions start_state ds)
  end.

Notation "'L' d" := (left90, d) (at level 80).
Notation "'R' d" := (right90, d) (at level 80).

Open Scope nat_scope.

Example to_n0_e1 : follow_directions start_state [R 1] = coord 0 1.
Proof. auto. Qed.
Example to_n0_w1 : follow_directions start_state [L 1] = coord 0 (-1).
Proof. auto. Qed.
Example to_n1_e0 : follow_directions start_state [R 0; L 1] = coord 1 0.
Proof. auto. Qed.
Example to_s1_e0 : follow_directions start_state [L 0; L 1] = coord (-1) 0.
Proof. auto. Qed.

Example basic_instructions : blocks_away [R 2; L 3] = 5.
Proof. auto. Qed.
Example walk_three_sides_of_2_by_2_square : blocks_away [R 2; R 2; R 2] = 2.
Proof. auto. Qed.
Example minor_backtrack : blocks_away [R 5; L 5; R 5; R 3] = 12.
Proof. auto. Qed.

Definition puzzle_input : list (turn_direction * nat) :=
  [R 4; R 1; L 2; R 1; L 1; L 1; R 1; L 5; R 1; R 5; L 2; R 3; L 3; L 4; R 4; R 4; R 3; L 5; L 1; R 5; R 3; L 4; R 1; R 5; L 1; R 3; L 2; R 3; R 1; L 4; L 1; R 1; L 1; L 5; R 1; L 2; R 2; L 3; L 5; R 1; R 5; L 1; R 188; L 3; R 2; R 52; R 5; L 3; R 79; L 1; R 5; R 186; R 2; R 1; L 3; L 5; L 2; R 2; R 4; R 5; R 5; L 5; L 4; R 5; R 3; L 4; R 4; L 4; L 4; R 5; L 4; L 3; L 1; L 4; R 1; R 2; L 5; R 3; L 4; R 3; L 3; L 5; R 1; R 1; L 3; R 2; R 1; R 2; R 2; L 4; R 5; R 1; R 3; R 2; L 2; L 2; L 1; R 2; L 1; L 3; R 5; R 1; R 4; R 5; R 2; R 2; R 4; R 4; R 1; L 3; R 4; L 2; R 2; R 1; R 3; L 5; R 5; R 2; R 5; L 1; R 2; R 4; L 1; R 5; L 3; L 3; R 1; L 4; R 2; L 2; R 1; L 1; R 4; R 3; L 2; L 3; R 3; L 2; R 1; L 4; R 5; L 1; R 5; L 2; L 1; L 5; L 2; L 5; L 2; L 4; L 2; R 3].

(* Part 1 *)
Compute blocks_away puzzle_input.





Definition position (s : nav_state) : coordinate :=
  match s with
  | nav _ p => p
  end.

Fixpoint line_of_crumbs (from : nav_state) (dist : nat) (acc : list coordinate) : list coordinate :=
  match dist with
  | 0 => acc
  | S dist' =>
    let from' := walk 1 from in
    line_of_crumbs from' dist' (position from' :: acc)
  end.

Fixpoint breadcrumbs
         (from : nav_state)
         (ds : list (turn_direction * nat))
         (acc : list coordinate)
  : list coordinate :=
  match ds with
  | [] => rev acc
  | (t,b) :: ds =>
    let after_turn := turn t from in
    breadcrumbs (walk b after_turn) ds (app (line_of_crumbs after_turn b []) acc)
  end.

Definition same_place (c1 c2 : coordinate) : bool :=
  match c1, c2 with
  | coord n1 e1, coord n2 e2 =>
    match n1 ?= n2, e1 ?= e2 with
    | Eq, Eq => true
    | _, _ => false
    end
  end.

Fixpoint first_revisited (bs : list coordinate) (seen : list coordinate) : option coordinate :=
  match bs with
  | [] => None
  | b :: bs' =>
    if existsb (same_place b) seen
    then Some b
    else first_revisited bs' (b :: seen)
  end.

Compute breadcrumbs start_state [R 8; R 4; R 4; R 8] [].

(* Part 2 *)
Compute match first_revisited (breadcrumbs start_state puzzle_input []) [] with
        | Some c => manhatten_dist (position start_state) c
        | None => 0
        end.
