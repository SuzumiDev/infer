open! IStd
module L = Logging
module F = Format

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = CrossMemDomain

  type analysis_data = CrossMemDomain.summary InterproceduralAnalysis.t

  let exec_instr (astate : CrossMemDomain.t)
    {InterproceduralAnalysis.proc_desc; tenv=_; analyze_dependency=_; _} _ _
    (_instr : Sil.instr) = 
      let proc_att = Procdesc.get_attributes proc_desc in
      L.debug_dev "is rust func %b" proc_att.is_rust_function ;
      L.progress "testing" ; 
      astate

  let pp_session_name _node fmt = F.pp_print_string fmt "cross memory test"

end

module CFG = ProcCfg.Normal
module Analyzer = AbstractInterpreter.MakeRPO (TransferFunctions (CFG))


let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) = 
  let result = Analyzer.compute_post analysis_data ~initial:CrossMemDomain.initial proc_desc in
  result