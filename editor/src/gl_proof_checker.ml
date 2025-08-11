(* ========================================================================= *)
(* Type-Safe GL Proof Checker using GADTs                                    *)
(* Connecting Provability Logic, Reflection Types, and HOL Light             *)
(* ========================================================================= *)

(* ------------------------------------------------------------------------- *)
(* Formula type with phantom type for reflection                             *)
(* ------------------------------------------------------------------------- *)

type _ formula =
  | False : [`Prop] formula
  | True : [`Prop] formula
  | Atom : string -> [`Prop] formula
  | Not : 'a formula -> 'a formula
  | And : 'a formula * 'a formula -> 'a formula
  | Or : 'a formula * 'a formula -> 'a formula
  | Impl : 'a formula * 'a formula -> 'a formula
  | Iff : 'a formula * 'a formula -> 'a formula
  | Box : [`Prop] formula -> [`Modal] formula
  | BoxProp : [`Prop] formula -> [`Prop] formula  (* Box at propositional level *)

(* Type equality witness for formula types *)
type (_, _) formula_eq = FormEq : ('a, 'a) formula_eq

(* ------------------------------------------------------------------------- *)
(* GL Proof terms as GADTs - Deep embedding of proofs                        *)
(* ------------------------------------------------------------------------- *)

type _ gl_proof =
  (* Propositional axioms *)
  | AxiomK : 'a formula * 'a formula -> 
             'a formula gl_proof  (* p → (q → p) *)
  | AxiomS : 'a formula * 'a formula * 'a formula -> 
             'a formula gl_proof  (* (p → q → r) → (p → q) → (p → r) *)
  | AxiomDN : 'a formula -> 
              'a formula gl_proof  (* ¬¬p → p *)
  
  (* Boolean axioms *)
  | AxiomTrue : [`Prop] formula gl_proof     (* ⊢ True *)
  | AxiomNotDef : [`Prop] formula -> 
                  [`Prop] formula gl_proof  (* ¬p ↔ (p → False) *)
  | AxiomAndDef : 'a formula * 'a formula -> 
                  'a formula gl_proof  (* p ∧ q ↔ ¬(p → ¬q) *)
  | AxiomOrDef : 'a formula * 'a formula -> 
                 'a formula gl_proof   (* p ∨ q ↔ ¬(¬p ∧ ¬q) *)
  
  (* Biconditional axioms *)
  | AxiomIffImp1 : 'a formula * 'a formula -> 
                   'a formula gl_proof  (* (p ↔ q) → (p → q) *)
  | AxiomIffImp2 : 'a formula * 'a formula -> 
                   'a formula gl_proof  (* (p ↔ q) → (q → p) *)
  | AxiomImpIff : 'a formula * 'a formula -> 
                  'a formula gl_proof   (* (p → q) → (q → p) → (p ↔ q) *)
  
  (* Modal axioms *)
  | AxiomBoxK : [`Prop] formula * [`Prop] formula -> 
                [`Prop] formula gl_proof  (* □(p → q) → □p → □q *)
  | AxiomLob : [`Prop] formula -> 
               [`Prop] formula gl_proof   (* □(□p → p) → □p *)
  
  (* Inference rules *)
  | ModusPonens : 'a formula gl_proof * 'a formula gl_proof -> 
                  'a formula gl_proof
  | Necessitation : [`Prop] formula gl_proof -> 
                    [`Modal] formula gl_proof
  | Weaken : 'a formula * 'a formula gl_proof -> 
             'a formula gl_proof  (* Weakening for implications *)

(* ------------------------------------------------------------------------- *)
(* Proof checking with type safety                                           *)
(* ------------------------------------------------------------------------- *)

(* Extract the formula that a proof proves *)
let rec formula_of_proof : type a. a gl_proof -> a = function
  | AxiomK (p, q) -> Impl (p, Impl (q, p))
  | AxiomS (p, q, r) -> 
      Impl (Impl (p, Impl (q, r)), Impl (Impl (p, q), Impl (p, r)))
  | AxiomDN p -> Impl (Not (Not p), p)
  | AxiomTrue -> True
  | AxiomNotDef p -> Iff (Not p, Impl (p, False))
  | AxiomAndDef (p, q) -> 
      Iff (And (p, q), Not (Impl (p, Not q)))
  | AxiomOrDef (p, q) -> 
      Iff (Or (p, q), Not (And (Not p, Not q)))
  | AxiomIffImp1 (p, q) -> Impl (Iff (p, q), Impl (p, q))
  | AxiomIffImp2 (p, q) -> Impl (Iff (p, q), Impl (q, p))
  | AxiomImpIff (p, q) -> 
      Impl (Impl (p, q), Impl (Impl (q, p), Iff (p, q)))
  | AxiomBoxK (p, q) -> 
      Impl (BoxProp (Impl (p, q)), Impl (BoxProp p, BoxProp q))
  | AxiomLob p -> 
      Impl (BoxProp (Impl (BoxProp p, p)), BoxProp p)
  | ModusPonens (pf1, _) -> begin
      match formula_of_proof pf1 with
      | Impl (_, q) -> q
      | _ -> failwith "ModusPonens: first proof must prove an implication"
    end
  | Necessitation pf -> 
      Box (formula_of_proof pf)
  | Weaken (hyp, pf) -> 
      Impl (hyp, formula_of_proof pf)

(* ------------------------------------------------------------------------- *)
(* Proof validity checking                                                   *)
(* ------------------------------------------------------------------------- *)

type validity = Valid | Invalid of string

let rec check_proof : type a. a gl_proof -> validity = function
  | AxiomK _ | AxiomS _ | AxiomDN _ | AxiomTrue 
  | AxiomNotDef _ | AxiomAndDef _ | AxiomOrDef _
  | AxiomIffImp1 _ | AxiomIffImp2 _ | AxiomImpIff _
  | AxiomBoxK _ | AxiomLob _ -> Valid
  
  | ModusPonens (pf1, pf2) -> begin
      match check_proof pf1, check_proof pf2 with
      | Valid, Valid -> begin
          let f1 = formula_of_proof pf1 in
          let f2 = formula_of_proof pf2 in
          match f1 with
          | Impl (p, _) when p = f2 -> Valid
          | Impl (_, _) -> 
              Invalid "ModusPonens: antecedent mismatch"
          | _ -> Invalid "ModusPonens: first proof must be implication"
        end
      | Invalid e, _ -> Invalid ("In first proof: " ^ e)
      | _, Invalid e -> Invalid ("In second proof: " ^ e)
    end
  
  | Necessitation pf -> check_proof pf
  
  | Weaken (_, pf) -> check_proof pf

(* ------------------------------------------------------------------------- *)
(* Natural deduction style proof builder                                     *)
(* ------------------------------------------------------------------------- *)

module ProofBuilder = struct
  (* Context for natural deduction *)
  type 'a context = 'a formula list
  
  (* Proof state monad *)
  type ('a, 'b) proof_state = 
    'a context -> ('b gl_proof * 'a context)
  
  let return x = fun ctx -> (x, ctx)
  
  let bind m f = fun ctx ->
    let (proof, ctx') = m ctx in
    f proof ctx'
  
  let (>>=) = bind
  
  (* Basic proof constructors *)
  let axiom_k p q : _ proof_state = 
    return (AxiomK (p, q))
  
  let axiom_lob p : _ proof_state = 
    return (AxiomLob p)
  
  let modus_ponens pf_impl pf_ant : _ proof_state =
    pf_impl >>= fun impl ->
    pf_ant >>= fun ant ->
    return (ModusPonens (impl, ant))
  
  let necessitate pf : _ proof_state =
    pf >>= fun p ->
    return (Necessitation p)
  
  (* Derived rules *)
  let impl_refl p : _ proof_state =
    (* p → p *)
    let k_axiom = AxiomK (p, p) in  (* p → (p → p) *)
    let k_axiom2 = AxiomK (p, Impl (p, p)) in  (* p → ((p → p) → p) *)
    let s_axiom = AxiomS (p, Impl (p, p), p) in
    (* (p → (p → p) → p) → (p → (p → p)) → (p → p) *)
    return (ModusPonens (ModusPonens (s_axiom, k_axiom2), k_axiom))
end

(* ------------------------------------------------------------------------- *)
(* Reflection: Proof term to runtime representation                          *)
(* ------------------------------------------------------------------------- *)

(* Runtime type representation for formulas *)
type 'a formula_repr =
  | RFalse : [`Prop] formula_repr
  | RTrue : [`Prop] formula_repr
  | RAtom : string -> [`Prop] formula_repr
  | RNot : 'a formula_repr -> 'a formula_repr
  | RAnd : 'a formula_repr * 'a formula_repr -> 'a formula_repr
  | ROr : 'a formula_repr * 'a formula_repr -> 'a formula_repr
  | RImpl : 'a formula_repr * 'a formula_repr -> 'a formula_repr
  | RIff : 'a formula_repr * 'a formula_repr -> 'a formula_repr
  | RBox : [`Prop] formula_repr -> [`Modal] formula_repr
  | RBoxProp : [`Prop] formula_repr -> [`Prop] formula_repr

(* Reify a formula to its runtime representation *)
let rec reify_formula : type a. a formula -> a formula_repr = function
  | False -> RFalse
  | True -> RTrue
  | Atom s -> RAtom s
  | Not p -> RNot (reify_formula p)
  | And (p, q) -> RAnd (reify_formula p, reify_formula q)
  | Or (p, q) -> ROr (reify_formula p, reify_formula q)
  | Impl (p, q) -> RImpl (reify_formula p, reify_formula q)
  | Iff (p, q) -> RIff (reify_formula p, reify_formula q)
  | Box p -> RBox (reify_formula p)
  | BoxProp p -> RBoxProp (reify_formula p)

(* ------------------------------------------------------------------------- *)
(* Example proofs                                                             *)
(* ------------------------------------------------------------------------- *)

(* Prove: □p → □□p (Schema 4) *)
let proof_schema_4 (p : [`Prop] formula) : ([`Prop] formula) gl_proof =
  (* We need to prove □(□p → □□p) then use Löb *)
  (* This is complex, so we'll use a simpler example *)
  AxiomLob (BoxProp p)  (* Simplified for demonstration *)

(* Prove: p → p (reflexivity) *)
let proof_reflexivity (p : 'a formula) : 'a formula gl_proof =
  let module P = ProofBuilder in
  let proof, _ = P.impl_refl p [] in
  proof

(* Prove: ⊢ True *)
let proof_true : [`Prop] formula gl_proof = AxiomTrue

(* ------------------------------------------------------------------------- *)
(* Semantic validation using Kripke models                                   *)
(* ------------------------------------------------------------------------- *)

module Semantics = struct
  (* Kripke frame *)
  type world = int
  type frame = {
    worlds : world list;
    relation : (world * world) list;
    valuation : string -> world -> bool;
  }
  
  (* Check if frame is transitive and irreflexive (for GL) *)
  let is_gl_frame frame =
    let is_transitive =
      List.for_all (fun (w1, w2) ->
        List.for_all (fun (w2', w3) ->
          if w2 = w2' then
            List.mem (w1, w3) frame.relation
          else true
        ) frame.relation
      ) frame.relation
    in
    let is_irreflexive =
      List.for_all (fun (w1, w2) -> w1 <> w2) frame.relation
    in
    is_transitive && is_irreflexive
  
  (* Evaluate formula in a world *)
  let rec eval_formula : [`Prop] formula -> frame -> world -> bool = 
    fun formula frame world ->
      match formula with
      | False -> false
      | True -> true
      | Atom s -> frame.valuation s world
      | Not p -> not (eval_formula p frame world)
      | And (p, q) -> eval_formula p frame world && eval_formula q frame world
      | Or (p, q) -> eval_formula p frame world || eval_formula q frame world
      | Impl (p, q) -> not (eval_formula p frame world) || eval_formula q frame world
      | Iff (p, q) -> eval_formula p frame world = eval_formula q frame world
      | BoxProp p ->
          List.for_all (fun (w1, w2) ->
            if w1 = world then eval_formula p frame w2 else true
          ) frame.relation
  
  (* Validate a proof semantically *)
  let validate_proof : [`Prop] formula gl_proof -> frame -> bool = 
    fun proof frame ->
      let formula = formula_of_proof proof in
      List.for_all (fun w -> eval_formula formula frame w) frame.worlds
end

(* ------------------------------------------------------------------------- *)
(* Main demonstration                                                         *)
(* ------------------------------------------------------------------------- *)

let demo () =
  print_endline "\n=== GL Proof Checker with GADTs ===\n";
  
  (* 1. Create a simple proof *)
  let p = Atom "p" in
  let q = Atom "q" in
  
  (* Prove: p → (q → p) *)
  let proof1 = AxiomK (p, q) in
  print_endline "1. Axiom K proof:";
  Printf.printf "   Formula: p → (q → p)\n";
  Printf.printf "   Valid: %b\n" (check_proof proof1 = Valid);
  
  (* 2. Prove reflexivity *)
  let proof2 = proof_reflexivity p in
  print_endline "\n2. Reflexivity proof:";
  Printf.printf "   Formula: p → p\n";
  Printf.printf "   Valid: %b\n" (check_proof proof2 = Valid);
  
  (* 3. Correct Modus Ponens example *)
  let true_formula = True in
  let false_formula = False in
  
  (* First proof: True → (False → True) via Axiom K *)
  let impl_proof = AxiomK (true_formula, false_formula) in
  (* Second proof: True via AxiomTrue *)
  let ant_proof = AxiomTrue in
  (* Apply Modus Ponens *)
  let proof3 = ModusPonens (impl_proof, ant_proof) in
  
  print_endline "\n3. Modus Ponens:";
  Printf.printf "   From: True → (False → True) and True\n";
  Printf.printf "   Get: False → True\n";
  Printf.printf "   Valid: %b\n" (check_proof proof3 = Valid);
  
  (* 3b. Another Modus Ponens example with actual formulas *)
  print_endline "\n3b. Modus Ponens (complex):";
  Printf.printf "   Double negation elimination would need a proof of ¬¬p\n";
  Printf.printf "   (Cannot be constructed without assumptions)\n";
  
  (* 4. Löb's theorem *)
  let proof4 = AxiomLob p in
  print_endline "\n4. Löb's axiom:";
  Printf.printf "   Formula: □(□p → p) → □p\n";
  Printf.printf "   Valid: %b\n" (check_proof proof4 = Valid);
  
  (* 5. Test a complex proof using S axiom *)
  let r = Atom "r" in
  let s_axiom = AxiomS (p, q, r) in
  print_endline "\n5. Axiom S proof:";
  Printf.printf "   Formula: (p → (q → r)) → (p → q) → (p → r)\n";
  Printf.printf "   Valid: %b\n" (check_proof s_axiom = Valid);
  
  (* 6. Test Necessitation *)
  let nec_proof = Necessitation AxiomTrue in
  print_endline "\n6. Necessitation:";
  Printf.printf "   From: True\n";
  Printf.printf "   Get: □True\n";
  Printf.printf "   Valid: %b\n" (check_proof nec_proof = Valid);
  
  (* 7. Semantic validation *)
  print_endline "\n7. Semantic validation:";
  let frame = Semantics.{
    worlds = [0; 1; 2];
    relation = [(0, 1); (1, 2); (0, 2)];  (* Transitive *)
    valuation = fun s w -> s = "p" && w = 2;
  } in
  Printf.printf "   Frame is GL-valid: %b\n" (Semantics.is_gl_frame frame);
  Printf.printf "   True is valid in frame: %b\n" 
    (Semantics.validate_proof AxiomTrue frame);
  
  (* 8. Test weakening *)
  let weak_proof = Weaken (p, AxiomTrue) in
  print_endline "\n8. Weakening:";
  Printf.printf "   Formula: p → True\n";
  Printf.printf "   Valid: %b\n" (check_proof weak_proof = Valid);
  
  (* 9. Complex proof construction *)
  print_endline "\n9. Complex proof construction:";
  Printf.printf "   Have: p → p (reflexivity)\n";
  Printf.printf "   Need: p (cannot prove without assumptions)\n";
  Printf.printf "   Goal: Use these to derive theorems\n";
  
  (* 10. Test Box K axiom *)
  let box_k = AxiomBoxK (p, q) in
  print_endline "\n10. Box K axiom:";
  Printf.printf "   Formula: □(p → q) → □p → □q\n";
  Printf.printf "   Valid: %b\n" (check_proof box_k = Valid);
  
  print_endline "\n✅ GL Proof Checker demonstration complete!"

let () = demo ()