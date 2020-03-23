open Core_kernel

module Player_id = Unique_id.Int()
module Game_id = Unique_id.Int()

type error =
  | Game_finished
  | Wrong_player_id
  | Word_already_present
  | Full_words
  | Invalid_transition
[@@deriving sexp]

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

module Time = struct
  include Sexpable.Of_sexpable(String)(struct
      type t = Time.t
      let to_sexpable = Time.to_string
      let of_sexpable = Time.of_string
      end)
  include Time
end

let subround_length =
(*   Time.Span.of_min 2. *)
  Time.Span.of_sec 10.

module Pair(F: sig
    type _ t [@@deriving bin_io, sexp]
    val map: 'a t -> f:('a -> 'b) -> 'b t
  end) = struct
    type 'a t = 'a F.t * 'a F.t
    [@@deriving bin_io, sexp]
    let map (a, b) ~f = (F.map ~f a, F.map ~f b)
end

module Game = struct
  module Teams = Pair(List)

  module Team_state = struct
    type 'a t =
      { players: 'a array
      ; active: int
      ; score: int
      }
    [@@deriving sexp, bin_io]

    let increase_score t =
      { t with score= t.score + 1 }

    let map t ~f =
      { t with
        players=Array.map t.players ~f
      }

    let create players =
      { players= Array.of_list players
      ; active=0
      ; score=0
      }

    let advance t =
      { t with active= (t.active + 1) mod Array.length t.players }
  end

  module Team_states = Pair(Team_state)

  let next_team = function
    | `A -> `B
    | `B -> `A

  module Words = struct
    type t = {completed: string list; remaining : string list }
    [@@deriving bin_io, sexp]

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
    module Team_states = struct
      type 'a t =
        { teams: 'a Team_states.t; active: [ `A | `B ] }
      [@@deriving bin_io, sexp]

      let map t ~f =
        { t with teams= Team_states.map ~f t.teams }

      let active_player { active; teams=(a,b) } =
        let team =
          match active with
          | `A -> a
          | `B -> b
        in
        team.players.(team.active)

      let increase_score t =
        let teams =
          let (a, b) = t.teams in
          match t.active with
          | `A -> (Team_state.increase_score a, b)
          | `B -> (a, Team_state.increase_score b)
        in
        { t with teams }

      let advance { active; teams=(a,b) } =
        let (active, teams) =
          match active with
          | `A ->
            ( `B, (a, Team_state.advance b) )
          | `B ->
            ( `A, (Team_state.advance a, b))
        in
        { active; teams }
    end

    type t =
        { teams: Player_id.t Team_states.t
        ; subround_start: Time.t option
        ; round: Round.t
        ; words: Words.t
        }
      [@@deriving bin_io, sexp]

    let active_player t =
      Team_states.active_player t.teams

    let current_subround_over t ~now:now0 =
      List.is_empty t.words.remaining ||
      match t.subround_start with
      | None -> true
      | Some time -> 
        Time.(Span.(diff now0 time >= subround_length))

    let advance t =
      { t with teams= Team_states.advance t.teams }
  end

  module Collecting_words : sig
    type t
    [@@deriving bin_io, sexp]

    val create: words_per_player:int -> t

    val add : t -> Player_id.t -> string -> [ `Already_present | `Full | `Ok of t ]

    val for_player: t -> Player_id.t -> string list option

    val to_list : t -> string list
  end = struct
    type t =
      { by_player: String.Set.t Player_id.Map.t
      ; all: String.Set.t
      ; words_per_player: int
      }
    [@@deriving bin_io, sexp]

    let create ~words_per_player =
      { by_player= Player_id.Map.empty; all= String.Set.empty 
      ; words_per_player
      }

    let for_player { by_player; _ } player =
      Option.map (Map.find by_player player)
        ~f:Set.to_list

    let to_list { all; _ } = Set.to_list all

    let add { by_player; all; words_per_player } p w =
      if Set.mem all w
      then `Already_present
      else
        with_return (fun { return } ->
        `Ok
          { all = String.Set.add all w
          ; by_player= Map.update by_player p ~f:(function
                | None -> String.Set.singleton w
                | Some ws ->
                  if Set.length ws >= words_per_player
                  then return `Full
                  else Set.add ws w )
          ; words_per_player 
          })
  end

  type t =
    | Players_joining of { players: Player_id.Set.t; admin: Player_id.t; words_per_player: int }
    | Collecting_words of { teams: Player_id.t Teams.t ; words: Collecting_words.t; admin: Player_id.t }
    | Running of Running.t
  [@@deriving bin_io, sexp]

  let init admin words_per_player =
    Players_joining { players=Player_id.Set.empty; admin; words_per_player  }

  module Update = struct
    (* All updates will come with the player so we check if it's valid *)
    type t =
      | Join
      | Finalize_players

      | Add_word of string
      | Finalize_words

      | Start_subround
      | Next_word
  [@@deriving bin_io, sexp]
  end

  let update_exn ~now ~player (game : t) (u : Update.t) =
    match game, u with
    | Players_joining r, Join ->
      Players_joining { r with players=Set.add r.players player }
    | Players_joining { players; admin; words_per_player }, Finalize_players ->
      if Player_id.equal player admin
      then
        let n = Set.length players / 2 in
        let teams = List.split_n (Set.to_list players) n in
        Collecting_words {admin; teams;words= Collecting_words.create ~words_per_player}
      else
        game
    | Collecting_words r, Add_word w ->
      Collecting_words
        { r with
          words= 
            match Collecting_words.add r.words player w with
            | `Ok ws -> ws
            | `Already_present -> raise (E Word_already_present)
            | `Full -> raise (E Full_words)
        }
    | Collecting_words { teams=(a, b); words; admin=_ }, Finalize_words ->
      Running
        { teams=
            { teams= Team_state.(create a, create b)
            ; active= `A
            }
        ; subround_start= None
        ; words=Words.create (Collecting_words.to_list words)
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
          | Some _ -> Running.advance g
        in
        let g = { g with subround_start= Some now } in
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
        | Some ws ->
          let g = { g with teams = Running.Team_states.increase_score g.teams } in
          Running { g with words = ws }
        end
    | _ ->
      raise (E Invalid_transition)

  let update ~now ~player game u =
    try Ok (update_exn ~now ~player game u)
    with E e -> Error e

  module View = struct
    type state =
      | Players_joining of { players: string list; admin: bool }
      | Collecting_words of { teams: string Teams.t; words: string list; admin: bool }
      | Running of
          { round: Round.t
          ; teams: (Player_id.t * string) Running.Team_states.t
          ; subround_start: Time.t option
          ; current_word: [`Out_of_words | `Not_your_turn | `Word of string ] }
      | Finished
    [@@deriving bin_io, sexp]

    type t =
      { game: Game_id.t
      ; state: state
      }
    [@@deriving bin_io, sexp]
  end

  let view registered_players player game : View.state =
    match game with
    | Players_joining { players=ps; admin; words_per_player=_ } ->
      Players_joining
        { players=List.map (Set.to_list ps)
              ~f:(Map.find_exn registered_players) 
        ; admin= Player_id.equal player admin
        }
    | Collecting_words { teams=ts; words=wss; admin } ->
      let ws =
        Option.value (Collecting_words.for_player wss player)
          ~default:[]
      in
      let teams = Teams.map ts ~f:(Map.find_exn registered_players) in
      Collecting_words {teams; words= ws; admin= Player_id.equal player admin }
    | Running ({ round; teams; subround_start; words } as r )->
      let current_word =
        match List.hd words.remaining with
        | None -> `Out_of_words
        | Some w ->
          if Player_id.equal (Running.active_player r) player
          then `Word w
          else `Not_your_turn
      in
      Running
        { round
        ; teams=
            Running.Team_states.map teams
              ~f:(fun p -> (p, Map.find_exn registered_players p))
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
      | Create_game of { creator: Player_id.t; words_per_player: int }
      | Join_game of Player_id.t * Game_id.t
    [@@deriving bin_io, sexp]

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
