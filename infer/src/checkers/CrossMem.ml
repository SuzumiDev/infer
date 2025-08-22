open! IStd
module L = Logging
module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = CrossMemDomain

  type analysis_data = CrossMemDomain.summary InterproceduralAnalysis.t

  let find_arg_index formals ident =
      let arg_names = formals |> List.map ~f:(fun (name, _, _) -> Mangled.to_string name) in
      let rec find_idx lst idx =
        match lst with
        | [] -> None
        | x :: xs -> if String.equal x ident then Some idx else find_idx xs (idx + 1)
      in
      find_idx arg_names 0

  let this_freetype is_rust_function : CrossMemDomain.allocType  = if is_rust_function then FreeRust else FreeC

  let malloc_compatible is_rust_function (alloctype : CrossMemDomain.allocType) = this_freetype is_rust_function == alloctype

  let ref_to_norefstring (exp : Exp.t) = let str = Exp.to_string exp in if Char.equal str.[0] '&' then String.sub str ~pos:1 ~len:(String.length str - 1) else str

  let ref_to_norefstring_remove (exp : Exp.t) = let str = Exp.to_string exp in if Char.equal str.[0] '(' && Char.equal str.[1] ')' then String.sub str ~pos:2 ~len:(String.length str - 2) else str

  let var_to_arg varnum funcName = funcName ^ string_of_int varnum

  let exec_instr (astate : CrossMemDomain.t)
    {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency; _} _ _
    (instr : Sil.instr) = 
    let proc_att = Procdesc.get_attributes proc_desc in
      (*L.debug_dev "is rust func %b\n" proc_att.is_rust_function ; *)
      L.debug_dev "state: %a\n" CrossMemDomain.pp astate ;
    match instr with
    | Call ((returnIdent, _returnTyp), Const (Cfun callee_proc_name), actuals, _loc, _) ->
        (* function call of the form [_return = _callee_proc_name(..._actuals)] *)
        (* Call function with return identification returnIdent and prob actuals as arg*)
        (* calling malloc here then makes the pointer *)
        (* might have to make it dirty for the free thing *)
        L.debug_dev "Call 1 procname %a\n" Procname.pp callee_proc_name ;
        L.debug_dev "Call 1.1 returnIdent %a\n" Ident.pp returnIdent;
        let cname = match callee_proc_name with
          | Procname.C c -> QualifiedCppName.to_qual_string c.c_name
          | _ -> L.die InternalError "not a C function" in
        (match cname with
          | "malloc" -> 
            let returnIdentStr = Ident.to_string returnIdent in
            (if CrossMemDomain.mem astate returnIdentStr
              then
                (if malloc_compatible proc_att.is_rust_function (CrossMemDomain.find astate returnIdentStr)
                  then CrossMemDomain.remove astate returnIdentStr
                  else let astate1 = CrossMemDomain.remove astate returnIdentStr in CrossMemDomain.add astate1 returnIdentStr CrossMemDomain.Incompatible) (* todo: report error *)
              else astate)
          | "free"    -> 
            let args, _argtypes = List.unzip actuals in
              (match List.nth args 0 with
            | Some arg -> 
                L.debug_dev "adding free for %a" Exp.pp arg ;
                CrossMemDomain.add astate (ref_to_norefstring arg) (this_freetype proc_att.is_rust_function)
            | None -> astate)
            
          | name -> (match analyze_dependency callee_proc_name with
                      | Ok (sum : CrossMemDomain.t) -> 
                          (match Procdesc.load callee_proc_name with
                            | Some callee_proc_desc -> 
                              let formals = Procdesc.get_formals callee_proc_desc in
                              let arg_names = List.map formals ~f:(fun (mangled, _, _) -> Mangled.to_string mangled) in
                              let f (acc, i) key =
                                let acc1 = if CrossMemDomain.mem acc key 
                                           then 
                                            let args, _ = List.unzip actuals 
                                            in CrossMemDomain.rename_key acc key (ref_to_norefstring (List.nth_exn args i)) 
                                           else acc 
                                in (acc1, i + 1)
                              in let (astate1, _) = List.fold_left ~f:f ~init:(sum, 0) arg_names
                              in CrossMemDomain.join astate astate1
                            | None -> astate)
                      | Error _ -> astate))
        

        (*steps: first add all on free and remove on malloc*)
        (* if the alias is in argument names then change it to number of argument *)
        (* this summary is lifted to the upper function *)
        (* in upper function, change from number of argument to alias *)
        (* repeat until alias is malloced by a different language *)

    | Load {id= lhs; e= rhs; typ= _lhs_typ; loc= _loc} ->
        (* load of an address [_lhs:_lhs_typ = *_rhs] *)
        (* load into lhs what is in address rhs (&i)*)
        (* if &i is in the map then add as same language *)
        L.debug_dev "Load ident %a rhs %a\n" Ident.pp lhs Exp.pp rhs ;
        let identString = Ident.to_string lhs in
        let rhsString = ref_to_norefstring rhs in
        if CrossMemDomain.mem astate identString
          then CrossMemDomain.rename_key astate identString rhsString
        else astate
    | Store {e1= lhs; e2= rhs; typ= _rhs_typ; loc= _loc} ->
        L.debug_dev "Store lhs %a rhs %a\n" Exp.pp lhs Exp.pp rhs;
        (* store at an address [*_lhs = _rhs:_rhs_typ] *)
        let lhsString = ref_to_norefstring lhs in
        let rhsString = ref_to_norefstring_remove rhs in
        if CrossMemDomain.mem astate lhsString
          then CrossMemDomain.rename_key astate lhsString rhsString
          else astate
    | Prune (_assume_exp, _loc, _, _) ->
        (* a conditional [assume(assume_exp)] blocks if [assume_exp] evaluates to false *)
          (* this isn't even called *)
          L.debug_dev "prune\n" ;
        astate
    | Call ((returnIdent, _returnTyp), call_exp, _actuals, loc, _) ->
        (* Call to a function/method not statically known, eg a function pointer. This should never
           happen in Java; fail if it does. *)
        L.debug_dev "Call 2 %a at %a\n" Exp.pp call_exp Location.pp loc;
        L.debug_dev "Call 2.1 returnIdent %a\n" Ident.pp returnIdent;
        astate
    | Metadata _ ->
        astate

  let pp_session_name _node fmt = F.pp_print_string fmt "cross memory test"

end

module CFG = ProcCfg.Backward (ProcCfg.Normal)
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))

let report_if_crossfree {InterproceduralAnalysis.proc_desc; err_log; _} post =
  if CrossMemDomain.val_exists CrossMemDomain.Incompatible post then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "Cross-language memory freeing!" in
    Reporting.log_issue proc_desc err_log ~loc:last_loc CrossMemAnalysis
      IssueType.cross_mem_free message


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) = 
  let result = Analyzer.compute_post analysis_data ~initial:CrossMemDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_crossfree analysis_data post) ;
  result