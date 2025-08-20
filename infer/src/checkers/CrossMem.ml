open! IStd
module L = Logging
module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = CrossMemDomain

  type analysis_data = CrossMemDomain.summary InterproceduralAnalysis.t

  let exec_instr (astate : CrossMemDomain.t)
    {InterproceduralAnalysis.proc_desc; tenv; analyze_dependency=_; _} _ _
    (instr : Sil.instr) = 
    let proc_att = Procdesc.get_attributes proc_desc in
      (*L.debug_dev "is rust func %b\n" proc_att.is_rust_function ; *)
      L.debug_dev "len astate %i\n" (CrossMemDomain.len astate) ;
    match instr with
    | Call ((returnIdent, _returnTyp), Const (Cfun _callee_proc_name), _actuals, _loc, _) ->
        (* function call of the form [_return = _callee_proc_name(..._actuals)] *)
        (* Call function with return identification returnIdent and prob actuals as arg*)
        (* calling malloc here then makes the pointer *)
        (* might have to make it dirty for the free thing *)
        L.debug_dev "Call 1 procname %a\n" Procname.pp _callee_proc_name ;
        L.debug_dev "Call 1.1 returnIdent %a\n" Ident.pp returnIdent;
        let cname = match _callee_proc_name with
          | Procname.C c -> QualifiedCppName.to_qual_string c.c_name
          | _ -> L.die InternalError "not a C function" in
        let astate2 = match cname with (* I know this looks ugly but ocaml didn't let me do it any other way for some reason*)
          | "malloc" -> CrossMemDomain.add astate cname (Ident.to_string returnIdent)
          | "free"    -> astate
          | _        -> astate
        in

        astate2
    | Load {id= _lhs; e= _rhs; typ= _lhs_typ; loc= _loc} ->
        (* load of an address [_lhs:_lhs_typ = *_rhs] *)
        (* load into lhs what is in address rhs (&i)*)
        (* if &i is in the map then add as same language *)
        L.debug_dev "Load ident %a rhs %a\n" Ident.pp _lhs Exp.pp _rhs ;
        astate
    | Store {e1= _lhs; e2= _rhs; typ= _rhs_typ; loc= _loc} ->
        L.debug_dev "Store lhs %a rhs %a\n" Exp.pp _lhs Exp.pp _rhs;
        (* store at an address [*_lhs = _rhs:_rhs_typ] *)
        (* we don't really care about this as this is just store a val in a type *)
        astate
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

module CFG = ProcCfg.Normal
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) = 
  let result = Analyzer.compute_post analysis_data ~initial:CrossMemDomain.initial proc_desc in
  result