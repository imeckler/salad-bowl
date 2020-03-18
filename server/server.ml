open Salad_bowl
open Core
open Async

module App_state = struct
  type per_game = 
    { game      : Game.t
    ; listeners : (Player_id.t * Game.View.state Pipe.Writer.t) list
    }

  type t =
    { games : per_game Game_id.Map.t
    ; players: string Player_id.Map.t
    }

  let init =
    { games = Game_id.Map.empty
    ; players=Player_id.Map.empty 
    }
end

let initiate (state : App_state.t) (q : Rpcs.Initiate.query) =
    with_return (fun { return } ->
      let r, w = Pipe.create () in
      let state, player, game =
          match q with
          | Create_game player -> 
            let game = Game_id.create () in
            { state with games=
                Game_id.Map.set state.games
                  ~key:game
                  ~data:{
                    game=Players_joining (Player_id.Set.singleton player);
                    listeners=[(player, w)]
                  }
            }
            , player
            , game
          | Join_game (player, game) ->
            { state with games=
              Map.update state.games game ~f:(function
                | Some {game=Players_joining g; listeners } ->
                  { game= Players_joining (Set.add g player)
                  ; listeners= (player, w) :: listeners
                  }
                | _ -> return (Or_error.error_string "Game not joinable" )
              )
            } 
            , player
            , game
      in
      Ok (state, player, game, r) )

let register (state : App_state.t) (q : string) =
  let id = Player_id.create () in
  ({ state with players=Map.set state.players ~key:id ~data:q }, id)

let main () =
  let state =
    ref App_state.init
  in
  Tcp.Server.create
    (Tcp.Where_to_listen.of_port 8000)
    ~on_handler_error:`Raise
    (fun _ r w -> 
       Async.Rpc.Connection.serve_with_transport
         ~handshake_timeout:(Some (Time.Span.of_sec 10.))
         ~heartbeat_config:None
         ~description:(Info.of_string "foo")
         ~connection_state:(fun _ -> ())
         ~on_handshake_error:`Raise
         ~implementations:(
           Rpc.Implementations.create_exn
             ~on_unknown_rpc:`Raise
             ~implementations:
               [ Rpc.Rpc.implement Rpcs.Register.t
                   (fun () q -> 
                      let s, resp = register !state q in
                      state := s ;
                      Deferred.return resp )
               ; Rpc.State_rpc.implement Rpcs.Initiate.t
                   (fun () q ->
                      let open Or_error.Let_syntax in
                      Deferred.return begin
                        let%map (s, _p, g, r) = initiate !state q in
                        state := s;
                        ({ Game.View.game=g; state=Players_joining }, r ) 
                      end
                   )
               ; Rpc.One_way.implement Rpcs.Move.t 
                   (fun () (player, game_id, u) ->
                      match Map.find !state.games game_id with
                      | None -> ()
                      | Some { game; listeners } ->
                        match 
                          Game.update ~now:(Time.now()) ~player 
                            game u
                        with
                        | Error _e -> ()
                        | Ok game ->
                          state :=
                            { !state with games= Map.set !state.games ~key:game_id ~data:{ game; listeners } } ;
                          List.iter listeners ~f:(fun (p, w) ->
                              Pipe.write_without_pushback w
                                (Game.view p game)) )
               ] )
         (Rpc.Transport.of_reader_writer r w
            ~max_message_size:1_000_000) )
