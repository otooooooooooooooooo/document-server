open Thread
open Event
open List

exception InvalidOperation of string

type username = string
type password = string
type account = username * password
type id = int
type document = string * id * username * username list 
type document_server = document list * account list
type request = Add_account of account | 
                Publish_document of account * string * int channel| 
                View_document of account * id * string channel| 
                Add_viewer of account * id * username | 
                Change_owner of account * id * username

let document_server () = 
    let c = new_channel ()
    in
    let rec server_fun ((docs, users) : document_server) =
        let get_name (u, _) = u
        in
        let exists_account acc = assoc_opt (get_name acc) users != None
        in
        let authentify (u, p)  = if assoc_opt u users = Some p then () else raise (InvalidOperation "Authentification failed.")
        in
        let get_document id =try Option.get(find_opt (fun (_, x, _, _) -> x = id) docs) with _ -> raise (InvalidOperation "Document does not exist.")
        in
        let is_viewer u (_, _, _, viewers) = not((find_opt (fun x -> x = u) viewers) = None) 
        in
        let is_owner u (_, _, owner, _) = u = owner
        in
        let get_data (data, _, _, _) = data
        in
        let view u doc = if is_owner u doc || is_viewer u doc then get_data doc
            else raise (InvalidOperation "The user cannot see selected document.")
        in
        let add_viewer u (str, id, owner, viewers) = (str, id, owner, (u::viewers))
        in
        let remove_by_id id = filter (fun (_, id', _, _) -> id' != id) docs
        in
        match sync (receive c) with
            |Add_account acc -> 
                if exists_account acc  then
                     raise (InvalidOperation "User already exists.") 
                else server_fun (docs, acc::users)

            |Publish_document ((u, p) , doc_str, answ_c) -> 
                authentify (u, p);
                let id =  length docs
                    in
                    sync (send answ_c id);
                    server_fun ((doc_str, id, u, []) :: docs, users)

            |View_document (acc, id, answ_c) -> authentify acc;
                let doc = get_document id
                in 
                sync (send answ_c (view (get_name acc) doc));
                              server_fun (docs, users)
                              
            
            |Add_viewer ((u, p), id, us) -> (
                authentify (u, p);
                let doc = get_document id
                in
                if not(is_owner u doc) then
                            raise (InvalidOperation "You do not own this document.")
                
                else if is_owner us doc || is_viewer us doc then raise (InvalidOperation "User can already view document.")
                else
                (
                    server_fun ((add_viewer us doc) :: (remove_by_id id), users)
                )
                
            )
            |Change_owner (acc, id, u) -> (
                authentify acc;
                        let doc = get_document id
                        in
                        if not(is_owner (get_name acc) doc) then
                            raise (InvalidOperation "You do not own this document.")
                        else (
                            match doc with
                            |(str, id, owner, viewers) -> server_fun ((str, id, u, viewers)::(remove_by_id id), users)
                        )
                )
    
    in
    let _ = create server_fun ([], [])
    in
    c

let add_account u p c = sync (send c (Add_account (u, p)))

let publish u p d c = 
    let answ_c = new_channel ()
    in sync(send c (Publish_document ((u, p), d, answ_c)));
       sync(receive answ_c)

let view u p id c =
    let answ_c = new_channel ()
    in sync (send c (View_document ((u, p), id, answ_c)));
        sync (receive answ_c)

let add_viewer u p id v c = sync (send c (Add_viewer ((u, p), id, v)))

let change_owner u p id u' c = sync(send c (Change_owner ((u, p), id, u')))
