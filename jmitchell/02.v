Require Import Coq.Numbers.Natural.Peano.NPeano.
Require Import Coq.Lists.List.
Import ListNotations.

Definition digit_to_pos (d : nat) :=
  let x := modulo (d-1) 3 in
  let y := (d-1) / 3 in
  (x, y).

Definition pos_to_digit (xy : (nat * nat)) :=
  match xy with
  | (x, y) => (y * 3) + x + 1
  end.

Inductive direction : Type :=
| U : direction
| R : direction
| D : direction
| L : direction.

Definition keypad_move (n : nat) (d : direction) :=
  match digit_to_pos n with
  | (x, y) =>
    pos_to_digit (match d with
                  | U => (x, y-1)
                  | R => (min (x+1) 2, y)
                  | D => (x, min (y+1) 2)
                  | L => (x-1, y)
                  end)
  end.

Example first_line : fold_left keypad_move [U;L;L] 5 = 1.
Proof. auto. Qed.
Example second_line : fold_left keypad_move [R;R;D;D;D] 1 = 9.
Proof. auto. Qed.

Definition sample :=
  [[U; L; L];
   [R; R; D; D; D];
   [L; U; R; D; L];
   [U; U; U; U; D]].

Fixpoint decode (code : list (list direction)) (start : nat) (acc : list nat) :=
  match code with
  | [] => rev acc
  | x :: xs =>
    let next := fold_left keypad_move x start in
    decode xs next (next :: acc)
  end.

Compute decode sample 5 [].

Definition puzzle_input :=
  [[U; U; L; L; U; L; L; U; U; L; L; L; U; R; D; L; D; U; U; R; R; D; R; R; L; D; U; R; D; U; L; L; R; U; R; D; U; D; U; L; L; L; U; U; L; U; R; U; R; L; R; D; R; R; R; R; U; L; D; R; U; U; L; L; L; L; U; U; D; U; R; D; U; L; D; R; R; D; R; U; D; L; R; R; L; D; L; U; D; L; D; D; R; U; R; U; R; U; U; R; R; R; D; D; D; L; L; R; U; D; U; R; D; U; L; U; U; L; L; R; R; U; L; L; R; U; L; D; U; D; R; D; R; L; D; L; U; R; U; R; U; D; D; U; D; L; U; R; U; D; U; D; U; R; L; U; R; U; R; R; U; R; L; U; D; D; R; U; R; R; D; L; U; U; R; L; L; R; U; R; R; D; U; D; L; U; L; U; L; U; D; U; L; D; L; L; R; R; R; D; L; R; D; L; D; U; D; R; D; D; D; R; R; U; U; R; R; R; R; R; U; U; R; R; D; R; R; D; L; U; R; D; R; R; U; R; D; L; L; U; U; L; U; L; L; R; U; R; D; L; D; D; D; R; R; L; L; R; R; U; U; R; U; L; U; R; U; U; D; D; L; R; R; U; D; D; R; U; R; U; U; D; L; R; L; R; D; L; R; U; R; R; R; D; U; L; L; D; L; R; U; D; D; U; U; L; R; D; U; L; U; R; U; U; R; D; U; L; U; D; L; L; R; R; L; D; D; L; R; D; L; R; U; D; R; L; D; D; R; L; R; R; R; D; U; R; D; U; L; L; R; R; R; D; R; R; L; U; U; R; U; R; D; R; R; D; R; R; L; D; L; U; D; U; R; U; R; L; D; U; U; R; D; R; U; D; R; D; D; R; L; D; R; R; L; D; L; U; R; U; R; U; L; L; U; U; R; U; D; U; U; D; L; R; L; L];
   [L; L; L; U; L; L; U; L; D; D; U; L; R; L; L; U; R; L; L; L; R; U; U; D; D; L; R; U; U; L; R; L; U; L; L; D; L; L; R; R; D; R; L; R; L; R; L; L; D; R; U; U; U; R; U; L; D; R; D; D; L; U; D; L; L; D; U; D; U; L; L; L; R; L; U; L; L; L; R; U; L; D; R; D; R; U; D; L; L; R; L; R; L; L; U; D; U; L; R; R; R; L; D; R; U; U; L; D; D; U; L; L; D; U; L; U; L; L; U; D; U; D; L; D; R; D; U; R; D; L; D; L; L; D; U; D; R; R; R; D; L; U; U; R; R; U; U; R; U; L; L; U; R; L; D; U; R; L; R; R; L; L; D; D; U; U; U; L; D; R; L; U; U; D; U; D; L; U; R; L; U; L; U; D; U; R; R; D; R; L; L; D; D; D; D; D; R; R; U; L; L; R; L; D; U; L; U; L; D; D; R; U; U; R; R; D; L; U; D; D; D; U; D; U; R; D; D; R; D; R; U; L; U; L; L; L; L; U; U; R; D; U; R; U; U; U; U; L; U; D; L; R; U; R; R; U; L; R; D; D; R; U; R; U; R; L; L; R; L; U; U; D; U; U; U; R; D; L; L; D; D; L; U; D; R; L; L; L; U; D; L; L; L; L; U; L; R; L; U; R; D; R; R; R; D; U; U; D; L; L; D; L; D; D; D; U; R; R; D; D; R; U; R; U; U; R; D; D; R; U; R; R; L; D; D; D; U; R; D; L; L; U; U; R; U; U; U; L; R; L; U; U; R; R; U; D; R; L; L; D; L; U; R; D; U; D; R; L; U; L; D; L; R; L; U; L; U; L; U; D; D; L; R; D; U; D; R; U; D; L; U; U; L; U; U; L; D; U; R; D; R; R; R; R; L; R; U; L; L; U; D; R; D; D; R; D; L; D; U; D; R; D; R; R; L; D; L; L; L; L; U; D; D; L; R; U; L; D; L; L; D; D; U; U; L; D; D; R; R; U; L; R; R; U; U; R; U; D; R; D; U; R; L; L; L; D; D; U; U; D; R; U; U; D; L; U; L; L; D; R];
   [U; D; U; U; U; L; L; D; D; D; D; L; U; D; L; D; U; L; R; L; R; D; L; U; L; L; D; D; R; U; L; D; U; R; R; L; U; R; R; U; D; L; R; R; U; D; U; R; R; D; U; D; R; R; R; U; U; L; R; L; L; R; L; U; D; L; D; R; R; D; U; U; R; D; D; R; D; R; D; U; U; U; D; U; D; L; D; L; L; R; R; L; U; U; R; L; U; U; U; D; D; D; U; U; R; L; U; L; U; R; R; L; U; R; R; R; D; R; D; U; R; U; R; U; D; R; L; R; U; U; R; U; D; R; U; D; D; D; R; D; R; D; L; D; R; D; U; R; D; L; D; R; R; D; U; U; D; L; L; U; R; L; D; D; U; R; R; R; L; U; L; D; R; D; R; L; L; R; L; L; L; R; U; R; L; D; U; R; D; R; L; D; R; U; U; R; R; L; D; L; D; R; L; D; D; D; R; L; D; L; R; L; D; U; R; U; R; L; L; L; L; D; D; R; D; U; D; L; R; U; L; U; L; L; R; D; D; L; L; U; D; R; D; R; R; L; U; U; U; L; D; R; L; D; U; R; U; R; D; U; D; U; R; L; L; D; R; R; D; U; U; L; D; U; U; D; L; L; D; D; R; U; U; U; L; R; R; U; L; D; D; U; D; R; D; R; L; R; U; L; U; U; D; U; U; R; U; L; L; D; L; L; U; R; L; R; R; L; D; D; D; L; L; D; R; R; D; D; R; L; D; D; L; U; R; R; U; D; U; R; U; L; U; D; L; L; L; D; U; D; D; L; D; L; D; L; R; U; D; U; D; R; D; U; D; D; L; D; D; L; D; U; L; U; R; D; D; U; D; R; R; U; U; U; R; L; D; U; U; R; U; L; L; R; L; U; L; U; U; R; L; L; L; L; D; U; U; D; U; R; U; U; D; U; U; L; U; L; D; R; U; L; R; L; R; D; U; L; D; L; L; U; R; D; L; R; U; U; U; D; D; U; R; L; L; L; L; D; U; D; R; L; U; U; D; U; D; R; R; U; R; U; R; R; D; R; D; D; R; U; L; D; L; R; L; U; R; D; L; L; R; R; D; R; U; U; U; U; R; L; D; R; U; R; D; U; U; D; L; D; U; R; U; D; D; L; R; D; D; D; D; U; R; R; L; R; L; U; D; R; R; D; D; U; R; D; D; R; L; D; D; L; L; R; R];
   [U; L; D; R; U; D; U; R; U; D; U; L; L; U; D; U; D; U; R; L; D; L; L; R; R; U; L; R; R; U; L; R; U; D; L; U; L; L; L; D; R; U; L; L; D; U; R; U; U; L; D; D; U; R; D; U; U; D; L; R; D; R; U; D; U; D; D; L; D; R; D; L; U; U; L; R; R; D; L; R; U; U; L; U; L; U; U; U; D; U; U; D; D; R; D; R; L; L; U; L; L; R; R; D; L; R; R; L; U; D; R; L; U; L; L; U; U; U; U; U; R; R; D; U; R; L; L; R; U; R; R; U; L; L; L; R; L; U; R; R; U; L; R; D; U; U; R; R; L; D; D; R; R; D; R; L; U; L; D; D; R; R; D; R; L; U; L; L; R; D; L; R; R; U; R; U; D; U; R; U; L; R; L; U; D; R; U; D; L; U; D; D; D; U; D; U; D; D; U; D; L; L; R; D; L; L; D; R; U; R; U; L; U; D; R; L; R; R; U; L; R; D; D; D; D; D; R; L; D; L; R; R; L; U; U; D; L; U; U; R; R; D; U; R; R; D; L; D; L; D; U; D; R; L; U; L; L; U; L; R; L; D; R; D; U; D; L; R; U; L; L; U; L; L; R; L; D; D; R; U; R; L; L; L; R; L; D; D; D; L; L; L; R; U; R; D; D; D; L; L; U; D; L; D; L; R; L; U; U; L; L; L; R; U; L; D; R; R; D; U; D; L; R; R; D; D; U; L; R; L; L; D; U; U; R; L; L; L; L; L; D; R; U; L; D; R; L; L; L; U; U; R; D; U; R; R; U; L; U; R; L; D; D; L; R; R; U; D; U; L; U; U; R; R; L; U; L; R; D; R; D; D; L; U; L; U; L; R; R; U; R; L; D; L; R; R; R; U; D; U; R; U; R; D; U; R; D; U; L; U; R; U; L; L; R; L; D; D];
   [D; U; R; L; R; R; R; D; R; U; L; D; L; U; L; U; D; U; L; U; U; R; U; R; R; L; U; L; U; D; L; U; R; U; R; D; D; U; R; U; L; L; R; R; U; U; D; L; R; U; R; L; D; L; R; U; D; U; L; D; L; L; R; R; U; L; L; L; L; R; R; L; R; U; U; L; D; L; D; L; L; R; D; U; D; L; L; R; L; U; L; R; L; L; U; U; U; L; U; L; R; D; L; D; L; R; R; U; R; L; U; D; D; R; R; L; U; U; D; D; R; R; U; D; D; R; R; U; R; L; R; R; U; L; L; D; D; U; L; L; L; U; R; R; U; L; U; D; L; R; R; R; U; R; R; U; L; R; L; L; L; R; U; L; L; R; R; U; R; D; R; L; U; R; U; L; L; D; U; L; R; L; L; L; U; L; L; R; L; R; L; L; L; D; R; R; R; R; D; D; D; D; D; D; U; L; U; U; D; U; D; U; L; R; U; R; D; R; U; D; R; L; U; U; L; U; R; D; U; R; L; U; R; R; D; R; R; R; R; D; R; R; L; L; L; L; U; D; L; R; R; D; U; R; U; R; L; L; U; L; U; D; D; L; R; L; R; L; R; R; U; U; R; L; L; U; R; L; D; U; U; L; L; R; R; D; U; R; R; U; L; R; U; L; U; R; L; L; L; R; L; U; U; R; R; U; L; L; L; U; R; D; D; D; R; U; R; D; U; D; D; U; L; L; R; U; L; U; U; U; D; D; R; U; R; U; U; D; U; U; R; U; R; R; D; R; U; R; D; U; D; R; L; L; R; R; U; L; U; R; U; D; L; D; U; R; L; D; L; R; R; R; R; L; L; U; U; R; R; L; U; L; D; D; D; U; U; U; U; R; U; U; L; D; L; D; R; L; D; U; D; U; L; D; R; R; U; L; D; R; D; U; L; U; R; R; U; U; R; D; U]].

Compute decode puzzle_input 5 [].

Require Import Coq.Strings.Ascii.
Open Scope char_scope.

Definition keypad : Set := list (list (option ascii)).
Definition diamond_keypad : keypad :=
  [[ None; None; Some "1"; None; None ];
   [ None; Some "2"; Some "3"; Some "4"; None ];
   [ Some "5"; Some "6"; Some "7"; Some "8"; Some "9" ];
   [ None; Some "A"; Some "B"; Some "C"; None ];
   [ None; None; Some "D"; None; None ]].
Check find.

Fixpoint index_of {A:Type} (p : A -> bool) (xs : list A) : option nat :=
  match xs with
  | [] => None
  | x :: xs' => if p x then Some 0 else option_map S (index_of p xs')
  end.

Definition same_char (a b : ascii) : bool :=
  EqNat.beq_nat (nat_of_ascii a) (nat_of_ascii b).

Fixpoint button_position (c : ascii) (k : keypad) : option (nat * nat) :=
  match k with
  | [] => None
  | r :: rs =>
    match index_of (fun b => match b with
                          | None => false
                          | Some c' => same_char c' c
                          end)                   
                   r with
    | Some x => Some (x, 0)
    | None => option_map (fun pos => match pos with
                                 | (x, y) => (x, S y)
                                 end)
                        (button_position c rs)
    end
  end.

Example position_of_a : button_position "A" diamond_keypad = Some (1, 3).
Proof. auto. Qed.

Definition button_at (p : nat * nat) (k : keypad) : option ascii :=
  let (x, y) := p in
  match nth_error k y with
  | Some r => match nth_error r x with
             | Some c => c
             | None => None
             end
  | None => None
  end.

Example button_at_1_3 : button_at (1, 3) diamond_keypad = Some "A".
Proof. auto. Qed.

Definition move_dir (k : keypad) (c : option ascii) (d : direction) : option ascii :=
  match c with
  | None => None
  | Some c' =>
    match button_position c' k with
    | None => None
    | Some (x, y) => let p' := match d with
                              | U => (x, y-1)
                              | R => (x+1, y)
                              | D => (x, y+1)
                              | L => (x-1, y)
                              end in
                    match button_at p' k with
                    | None => c
                    | c' => c'
                    end
    end
  end.

Fixpoint fancy_decode (code : list (list direction)) (k : keypad) (start : ascii) (acc : list ascii) :=
  match code with
  | [] => rev acc
  | x :: xs =>
    match fold_left (move_dir k) x (Some start) with
    | None => []
    | Some next => fancy_decode xs k next (next :: acc)
    end
  end.


Example sample_with_diamond : fancy_decode sample diamond_keypad "5" [] = ["5"; "D"; "B"; "3"].
Proof. auto. Qed.

Compute fancy_decode puzzle_input diamond_keypad "5" [].
