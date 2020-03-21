open Salad_bowl
open Core
open Async

module App_state = struct
  type per_game = 
    { game      : Game.t
    ; listeners : (Player_id.t * Game.View.state Pipe.Writer.t sexp_opaque) list
    }
  [@@deriving sexp]

  type t =
    { games : per_game Game_id.Map.t
    ; players: string Player_id.Map.t
    }
  [@@deriving sexp]

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
                    game=
                      Players_joining
                        { players=Player_id.Set.singleton player
                        ; admin= player
                        };
                    listeners=[(player, w)]
                  }
            }
            , player
            , game
          | Join_game (player, game) ->
            { state with games=
              Map.update state.games game ~f:(function
                | Some {game=Players_joining g; listeners } ->
                  { game= Players_joining { g with players= Set.add g.players player }
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

let port = 8001

let main () =
  let get_state, set_state =
    let r = ref App_state.init in
    ( (fun () -> !r),
      (fun s ->
(*          Core.printf !"state: %{sexp: App_state.t}\n%!" s ; *)
         r := s) )
  in
  let%bind _ =
    Tcp.Server.create
      (Tcp.Where_to_listen.of_port port)
      ~on_handler_error:(`Call (fun _addr exn ->
          eprintf "handler_error: %s" (Exn.to_string exn) ))
      (fun _ sock_r sock_w -> 
         let to_client_r, to_client_w = Pipe.create () in
         let from_client_r, from_client_w = Pipe.create () in

         (*
         let ws_r, ws_w = Pipe.cre
         let bytes_r, bytes_w = Pipe.create () in
            *)
         let _stop = 
          Websocket_async.server
            ~check_request:(fun _req ->
                Core.printf "hi%!\n%!";
                return true )
            ~reader:sock_r ~writer:sock_w
            ~app_to_ws:to_client_r
            ~ws_to_app:from_client_w
            ()
         in
         let from_client_r =
           let from_client_r1, w1 = Pipe.create () in
           don't_wait_for (
           Pipe.iter_without_pushback from_client_r ~f:(fun x ->
               Pipe.write_without_pushback w1 x )  ) ;
           from_client_r1
         in
         let%bind transport =
           let f t =
             match t.Websocket.Frame.opcode with
             | Text | Binary -> Some t.content
             | _ -> None
           in
           let%bind r =
             Pipe.filter_map from_client_r ~f
            |> Reader.of_pipe (Info.of_string "")
           in
           let%map (w, _) =
             let str_r, str_w = Pipe.create () in
             don't_wait_for (
               Pipe.transfer str_r to_client_w
                 ~f:(fun s ->
                     Websocket.Frame.create ~opcode:Binary ~content:s ()) ) ;
             Writer.of_pipe (Info.of_string "") str_w
           in
           Rpc.Transport.of_reader_writer ~max_message_size:10_000 r w
        in
        Async.Rpc.Connection.serve_with_transport
          ~handshake_timeout:(Some (Time.Span.of_sec 10.))
          ~heartbeat_config:None
          ~description:(Info.of_string "foo")
          ~connection_state:(fun _ -> ())
          ~on_handshake_error:(`Call(fun exn ->
          eprintf "handler_error: %s\n%!" (Exn.to_string exn) ))
          ~implementations:(
            Rpc.Implementations.create_exn
              ~on_unknown_rpc:(`Call (fun () ~rpc_tag ~version ->
                  eprintf "unknown_rpc: %s, %d\n%!" rpc_tag version;
                `Continue))
              ~implementations:
                [ Rpc.Rpc.implement Rpcs.Register.t
                    (fun () q -> 
                        let s, resp = register (get_state()) q in
                        set_state s ;
                        Deferred.return resp )
                ; Rpc.State_rpc.implement Rpcs.Initiate.t
                    (fun () q ->
                        let open Or_error.Let_syntax in
                        Deferred.return begin
                          let%map (s, p, g, r) = initiate (get_state ()) q in
                          set_state s;

                          let {App_state.game;listeners} = Map.find_exn s.games g in

                          List.iter listeners ~f:(fun (p', w) ->
                              if Player_id.(p <> p') then
                              Pipe.write_without_pushback_if_open w
                                (Game.view s.players p' game) );

                          ({ Game.View.game=g
                           ; state=Game.view s.players p game
                           }, r ) 
                        end
                    )
                ; Rpc.One_way.implement Rpcs.Move.t 
                    (fun () (player, game_id, u) ->
                        match Map.find (get_state ()).games game_id with
                        | None -> ()
                        | Some { game; listeners } ->
                          Core.printf !"Move: %{sexp:Game.Update.t}\n%!" u ;
                          Core.printf !"before: %{sexp:Game.t}\n%!" game ;
                          match 
                            Game.update ~now:(Time.now()) ~player 
                              game u
                          with
                          | Error e -> 
                            eprintf !"move %s %{sexp:error}\n%!" __LOC__  e ;
                            ()
                          | Ok game ->
                            Core.printf !"after: %{sexp:Game.t}\n%!" game ;
                            set_state
                              { (get_state ()) with games= Map.set (get_state ()).games ~key:game_id ~data:{ game; listeners } } ;
                            List.iter listeners ~f:(fun (p, w) ->
                                let view = Game.view (get_state ()).players p game in
                                Pipe.write_without_pushback_if_open w
                                  view ;
                              ) )
                ] )
          transport
      )
  in
  never ()

let () = 
  Command.async
    ~summary:"server"
    Command.Let_syntax.(
      return main
    )
  |> Command.run
