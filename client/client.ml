open Core_kernel
open Async_kernel
open Async_js
open Salad_bowl
open Incr_dom
open Js_of_ocaml

let () = Async_js.init ()

(* Flow:

   A player connects.

   They get an ID from the server.

   They can 
   1. Create a game. <- Game ID
   2. Join a game. Game ID ->
*)

module State = struct
  type t =
    | Enter_name of string
    | Waiting_for_player_id
    | Join_or_create_game of Player_id.t * string
    | Waiting_for_game of Player_id.t
    | In_game of Player_id.t * Game.View.t
end

module Action = struct
  type t =
    | Update_name of string
    | Enter_name
    | Receive_player_id of Player_id.t
    | Update_game_id of string
    | Join_or_create_game of Rpcs.Initiate.query
    | Initial_game of Game.View.t
    | New_view of Game.View.state
    | In_game of Game.Update.t
  [@@deriving sexp]
end

let view_team name t =
  let open Vdom in
  Node.div []
    [ ksprintf Node.text "Team %s" name
    ; Node.ul []
        (List.map t ~f:(fun x ->
            Node.li [] [ Node.text x ] ))
    ]

let view ~now (s : State.t) ~inject =
 printf "Calling view\n%!";
 let open Vdom in
 let button text action =
  Node.button
      [ Attr.type_ "button" 
      ; Attr.on_click (fun _ ->
            inject action )
      ]
      [ Node.text text
      ]
  in
  match s with
  | Enter_name _curr ->
    Node.div
      []
      [ Node.text "Enter name"
      ; Node.input
          [ Attr.type_ "text"
          ; Attr.on_input (fun _ x ->
              inject (Action.Update_name x) )
          ; Attr.on_keypress (fun e ->
                Js.Optdef.case e##.which
                  (fun () -> Event.Ignore)
                  (function
                    | 13 -> inject Enter_name
                    | _ -> Event.Ignore)
              )
          ]
          []
      ]
  | Waiting_for_player_id ->
    Node.div
      []
      [ Node.text "Waiting for player ID"
      ]
  | Join_or_create_game (player, game_id)  ->
    Node.div
      []
      [ Node.button
          [ Attr.type_ "button" 
          ; Attr.on_click (fun _ ->
                inject
                  (Action.Join_or_create_game
                     (Create_game player)
                  ) )
          ]
          [ Node.text "Create game"
          ]
      ; Node.div []
        [ Node.text "Join game"
        ; Node.input
            [ Attr.type_ "text"
            ; Attr.on_input (fun _ x ->
                inject (Action.Update_game_id x) )
            ; Attr.on_keypress (fun e ->
                  Js.Optdef.case e##.which
                    (fun () -> Event.Ignore)
                    (function
                      | 13 -> inject
                                (Join_or_create_game
                        (Join_game
                            (player, Game_id.of_string game_id)) )
                      | _ -> Event.Ignore)
                )
            ]
            []
        ]
      ]
  | Waiting_for_game _player ->
    Node.div
      []
      [ Node.text "Waiting for game"
      ]
  | In_game (self, { game; state }) ->
    match state with
    | Players_joining { players; admin } ->
      Node.div
        []
        (
        [ Node.div [] [Node.text "Waiting for players to join."]
        ; Node.div [] [ksprintf Node.text "Game ID: %s" (Game_id.to_string game)]
        ; Node.div [] [Node.text "Joined:"]
        ; Node.ul []
            (List.map players ~f:(fun x ->
                 Node.li [] [ Node.text x ] ))
        ]
        @
        if admin
        then [
        Node.button
            [ Attr.type_ "button" 
            ; Attr.on_click (fun _ ->
                  inject
                    (Action.In_game
                      Finalize_players
                    ) )
            ]
            [ Node.text "Finalize players"
            ]
      ] else [] )
    | Collecting_words { teams=(a,b); words; admin } ->
      Node.div
        []
        (
        [ Node.div [] [ksprintf Node.text "Game ID: %s" (Game_id.to_string game)]
        ; Node.div [] [Node.text "Enter words"]
        ; view_team "A" a
        ; view_team "B" b
        ; Node.input
            [ Attr.type_ "text"
            ; Attr.id "word-entry"
            ; Attr.on_keypress (fun e ->
                  let elt = Js.Opt.to_option e##.currentTarget |> Option.value_exn in
                  let value = (Js.string "value") in
                  let w =
                    Js.to_string (Js.Unsafe.get elt value)
                  in
                  Js.Optdef.case e##.which
                    (fun () -> Event.Ignore)
                    (function
                      | 13 ->
                        Js.Unsafe.set elt value (Js.string "") ;
                        inject (In_game (Add_word w))
                      | _ -> Event.Ignore)
                )
            ]
            []
        ; Node.div []
            [ Node.text "Your words"
            ; Node.ul []
                (List.map words ~f:(fun x -> Node.li [] [ Node.text x ] ) )
            ]
        ]
        @
        if admin
        then [
        Node.button
            [ Attr.type_ "button" 
            ; Attr.on_click (fun _ ->
                  inject
                    (Action.In_game
                      Finalize_words
                    ) )
            ]
            [ Node.text "Finalize words"
            ]
      ] else [] )
    | Finished -> Node.text "That's all folks!"
    | Running { round; teams; subround_start; current_word  } ->
      let view_team name ({ active; players } : _ Game.Team_state.t) =
        let players =
          Array.mapi players ~f:(fun i (_, player) ->
              let attrs =
                if i = active
                then [ Attr.style (Css_gen.background_color (`Name "orange")) ]
                else []
              in
              Node.li attrs
                [ Node.text player
                ]
            )
          |> Array.to_list
        in
        Node.div []
          [ ksprintf Node.text "Team %s" name
          ; Node.ul [] players
          ]
      in
      let round =
        match round with
        | Describe ->
          Node.span []
            [ Node.text "Use any word except "
            ; Node.create "i" [] [ Node.text "the " ]
            ; Node.text "word to get your team to guess it."
            ]
        | Charades ->
          Node.span []
            [ Node.text "Do anything but speak to get your team to guess the word."
            ]
        | One_word ->
          Node.span []
            [ Node.text "Use a single word to get your team to guess the word."
            ]
      in
      (* Either
         - Out_of_words
         - Time_remaining of Time.Span.t
         - No_time_remaining
      *)
      let round_state =
        match subround_start with
        | None -> `Waiting_to_start
        | Some subround_start ->
          let elapsed = Time.diff now subround_start in
          let remaining = 
            Time.Span.(elapsed - subround_length)
          in
          let current_round_over = Time.Span.(remaining > zero) in
          if current_round_over
          then `No_time_remaining
          else 
            match current_word with
            | `Out_of_words -> `Out_of_words
            | `Not_your_turn | `Word _ -> `Time_remaining remaining
      in
      let time_remaining =
        match round_state with
        | `Waiting_to_start -> Node.text "Waiting to start round."
        | `Out_of_words | `No_time_remaining ->
              Node.text "Round over. Waiting to start new round."
        | `Time_remaining remaining ->
          let parts = Time.Span.to_parts remaining in
          ksprintf Node.text "%d:%02d remaining."
            parts.min
            parts.sec
      in
      let current_word =
        match current_word with
        | `Out_of_words -> [ Node.text "All words have been used. On to the next round!" ]
        | `Not_your_turn -> []
        | `Word w ->
          [ Node.div []
            [ Node.div [] [ Node.text "You're up!" ]
            ; Node.div [] [ ksprintf Node.text "Current word: %s" w ]
            ; button "Next word" (In_game Next_word)
            ]
          ]
      in
      let am_active_player g =
        let next, _ = Game.Running.Team_states.active_player g in
        Player_id.equal next self
      in
      let am_next =
        am_active_player (Game.Running.Team_states.advance teams)
      in
      let am_curr =
        am_active_player teams
      in
      let start_round =
        match round_state with
        | `Time_remaining _ -> []
        | `No_time_remaining
        | `Waiting_to_start ->
          if am_curr
          then
            [ button "Start the round" (In_game Start_subround) ]
          else []
        | `Out_of_words ->
          if am_next
          then
            [ button "Start next round" (In_game Start_subround) ]
          else []
      in
      let (a, b) = teams.teams in
      Node.div []
        ([ view_team "A" a
        ; view_team "B" b
        ; Node.h3 [] [ Node.text "Current round" ]
        ; Node.div [] [ round ]
        ; Node.div [] [ time_remaining ]
        ; button "do nothing" (Action.Update_name "")
        ]
        @ current_word
        @ start_round )

module App = struct
  module Model = struct
    include State
    let cutoff _ _ = false
  end
  module Action = Action
  module State = Rpc.Connection

  let on_startup ~schedule_action:_ _model =
    Rpc.Connection.client_exn
      ~uri:(Uri.of_string "ws://localhost:8001")
      ()

  let apply_action (m: Model.t) (a:Action.t) conn ~schedule_action : Model.t =
    match m, a with
    | Enter_name _, Update_name name ->
      Enter_name name
    | Enter_name name, Enter_name ->
      don't_wait_for begin
        let%map player = Rpc.Rpc.dispatch_exn Rpcs.Register.t conn name in
        schedule_action (Action.Receive_player_id player)
      end ;
      Waiting_for_player_id
    | Waiting_for_player_id, Receive_player_id player ->
      Join_or_create_game (player, "")
    | Join_or_create_game (player, _) , Update_game_id g ->
      Join_or_create_game (player, g)
    | Join_or_create_game (player, _) , Join_or_create_game q ->
      don't_wait_for begin
        match%bind Rpc.State_rpc.dispatch Rpcs.Initiate.t conn q with
        | Ok Error e | Error e -> printf !"Join error: %{sexp:Error.t}\n%!" e ; Deferred.unit
        | Ok (Ok (s0, ss, _)) ->
          schedule_action (Initial_game s0) ;
          Pipe.iter_without_pushback ss ~f:(fun s ->
            schedule_action (New_view s) )
      end ;
      Waiting_for_game player
    | Waiting_for_game player, Initial_game s -> In_game (player, s)
    | In_game (player, { game; state=_ }) , New_view v -> In_game (player, { game; state= v })
    | In_game (p, g), In_game a ->
      Rpc.One_way.dispatch_exn Rpcs.Move.t conn (p, g.game, a) ;
      m
    | _ -> m

  let create model ~old_model:_ ~inject =
    let open Incr.Let_syntax in
    let%bind now = 
      let%map () = 
        let%map () =
          Incr.Clock.at_intervals
            Incr.clock (Time_ns.Span.of_sec 0.05)
        in
        printf "firing!\n%!"
      and now =
        Incr.Clock.watch_now Incr.clock
      in
      now
    in
    let%map model = model in
    let apply_action = apply_action model in
    let view = view model ~inject ~now:(Time_ns.to_time_float_round_nearest now) in
    Component.create ~apply_action model view

(*
  let create model ~old_model:_ ~inject =
    let open Incr.Let_syntax in
    let%map model = model
    and now =
      let%map () =
        let%map () =
          Incr.Clock.at_intervals
            Incr.clock (Time_ns.Span.of_sec 0.05)
        in
        printf "firing\n%!"
      and now =
        Incr.Clock.watch_now Incr.clock
      in
      now
    in
    let apply_action = apply_action model in
    let view = view model ~inject ~now:(Time_ns.to_time_float_round_nearest now) in
    Component.create ~apply_action model view
*)
end

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app"
    ~initial_model:(State.Enter_name "")
