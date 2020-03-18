open Core_kernel

module Player_id = Unique_id.Int()
module Game_id = Unique_id.Int()

type error =
  | Game_finished
  | Wrong_player_id
  | Invalid_transition

exception E of error

module Round = struct
  type t =
    | Describe
    | Charades
    | One_word
  [@@deriving sexp, bin_io]

  let next = function
    | Describe -> Charades
    | Charades -> One_word
    | One_word -> raise (E Game_finished)
end

let subround_length = Time.Span.of_min 2.

module Game = struct
  module Teams = struct
    type t = Player_id.t list * Player_id.t list
  [@@deriving bin_io]
  end

  module Team_state = struct
    type t =
      { players: Player_id.t array
      ; active: int
      }
    [@@deriving sexp, bin_io]

    let create players =
      { players= Array.of_list players
      ; active=0
      }

    let advance t =
      { t with active= (t.active + 1) mod Array.length t.players }
  end

  let next_team = function
    | `A -> `B
    | `B -> `A

  module Words = struct
    type t = {completed: string list; remaining : string list }
    [@@deriving bin_io]

    let create ws =
        {completed= [] ; remaining = List.permute ws }

    let reset t =
      create
        List.(rev_append t.completed t.remaining)

    let next { remaining; completed } =
      match remaining with
      | [] -> None
      | r :: remaining ->
        Some { remaining; completed= r :: completed }
  end

  module Running = struct
    type t =
        { teams: Team_state.t * Team_state.t
        ; active_team : [ `A | `B]
        ; subround_start: Time.t option
        ; round: Round.t
        ; words: Words.t
        }
      [@@deriving bin_io]

    let active_player t =
      let team = t.active_team in
      let a, b = t.teams in
      let team =
        match team with
        | `A -> a
        | `B -> b
      in
      team.players.(team.active)

    let current_subround_over t ~now:now0 =
      List.is_empty t.words.remaining ||
      match t.subround_start with
      | None -> true
      | Some time -> 
        Time.(Span.(diff now0 time >= subround_length))

    let advance t =
      let (a, b) = t.teams in
      let (active_team, teams) =
        match t.active_team with
        | `A ->
          ( `B, (a, Team_state.advance b) )
        | `B ->
          ( `A, (Team_state.advance a, b))
      in
      { t with active_team; teams }
  end

  type t =
    | Players_joining of Player_id.Set.t
    | Collecting_words of Teams.t * string list Player_id.Map.t
    | Running of Running.t
  [@@deriving bin_io]

  let init = Players_joining Player_id.Set.empty

  module Update = struct
    (* All updates will come with the player so we check if it's valid *)
    type t =
      | Join
      | Finalize_players

      | Add_word of string
      | Finalize_words

      | Start_subround
      | Next_word
  [@@deriving bin_io]
  end

  let update_exn ~now ~player (game : t) (u : Update.t) =
    match game, u with
    | Players_joining players, Join ->
      Players_joining (Set.add players player)
    | Players_joining players, Finalize_players ->
      let n = Set.length players in
      let teams = List.split_n (Set.to_list players) n in
      Collecting_words (teams, Player_id.Map.empty)
    | Collecting_words (teams, words), Add_word w ->
      Collecting_words
        (teams, Map.update words player ~f:(function
             | None -> [ w ]
             | Some ws -> w :: ws ))
    | Collecting_words ((a, b), words), Finalize_words ->
      Running
        { teams= Team_state.(create a, create b)
        ; active_team= `A
        ; subround_start= None
        ; words=
          Words.create (
            List.concat (Map.data words)
            |> List.dedup_and_sort ~compare:String.compare )
        ; round= Describe 
        }
    | Running g, Start_subround ->
      if Running.current_subround_over ~now g then
        let g =
          let round, words =
            match g.words.remaining with
            | [] -> Round.next g.round, Words.reset g.words
            | _ -> g.round, g.words
          in
          { g with round; words }
        in
        let g =
          match g.subround_start with
          | None -> g
          | Some _ ->
            { (Running.advance g) with
              subround_start= Some now;
            }
        in
        (
        let on_deck_player = Running.active_player g in
          if not Player_id.(on_deck_player = player)
          then raise (E Wrong_player_id) );
        Running g
      else game
    | Running g, Next_word ->
      if Running.current_subround_over ~now g then game else
        begin match Words.next g.words with
        | None -> game
        | Some ws -> Running { g with words = ws }
        end
    | _ -> raise (E Invalid_transition)

  let update ~now ~player game u =
    try Ok (update_exn ~now ~player game u)
    with E e -> Error e

  module View = struct
    type state =
      | Players_joining
      | Collecting_words of Teams.t
      | Running of
          { round: Round.t
          ; teams: Team_state.t * Team_state.t
          ; subround_start: Time.t option
          ; active_team: [`A | `B]
          ; current_word: string option }
      | Finished
    [@@deriving bin_io]

    type t =
      { game: Game_id.t
      ; state: state
      }
    [@@deriving bin_io]
  end

  let view player game : View.state =
    match game with
    | Players_joining _ -> Players_joining
    | Collecting_words (ts, _) -> Collecting_words ts
    | Running ({ round; teams; active_team; subround_start; words } as r )->
      let current_word =
        if Player_id.equal (Running.active_player r) player
        then
          List.hd words.remaining
        else None
      in
      Running
        { round
        ; teams
        ; active_team
        ; subround_start
        ; current_word 
        }
end

open Async_rpc_kernel

module Rpcs = struct
  module Register = struct
    let t =
      Rpc.Rpc.create
        ~name:"register"
        ~version:0
        ~bin_query:bin_string
        ~bin_response:Player_id.bin_t
  end

  module Initiate = struct
    type query = 
      | Create_game of Player_id.t
      | Join_game of Player_id.t * Game_id.t
    [@@deriving bin_io]

    let t =
      Rpc.State_rpc.create
        ~name:"initiate"
        ~version:0
        ~bin_query
        ~bin_state:Game.View.bin_t
        ~bin_update:Game.View.bin_state
        ~bin_error:Error.bin_t
        ()
  end

  module Move = struct
    type msg = Player_id.t * Game_id.t * Game.Update.t
    [@@deriving bin_io]

    let t =
      Rpc.One_way.create
        ~name:"move"
        ~version:0
        ~bin_msg
  end
end
