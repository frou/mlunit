structure MLUnit : ML_UNIT = struct
  (* Basic definitions *)

  datatype result = Pass
                  | Fail of string

  datatype testable = Suite of string * testable list
                    | Test of (unit -> result) * string

  fun suite name body = Suite (name, body)

  (* Assertions *)

  fun is test desc = Test (test, desc)

  fun pf e s = if e then Pass else Fail s

  fun isTrue c desc = is (fn () => pf c "Value was expected to be true, is false") desc
  fun isTrue' c = isTrue c "is true"

  fun isFalse c desc = is (fn () => pf (not c) "Value was expected to be false, is true") desc
  fun isFalse' c = isFalse c "is false"

  fun isEqual a b desc = is (fn () => pf (a = b) "Values were expected to be equal, are not equal") desc
  fun isEqual' a b = isEqual a b "is equal"

  fun isNotEqual a b desc = is (fn () => pf (a <> b) "Values were expected to be different, are equal") desc
  fun isNotEqual' a b = isNotEqual a b "is not equal"

  fun isSome a desc = is (fn () => pf (Option.isSome a) "Value was expected to be SOME, is NONE") desc
  fun isSome' a = isSome a "is SOME"

  fun isNone a desc = is (fn () => pf (not (Option.isSome a)) "Value was expected to be NONE, is SOME") desc
  fun isNone' a = isSome a "is NONE"

  fun isEmpty l desc = is (fn () => pf (List.null l) "List was expected to be empty, is non-empty") desc
  fun isEmpty' l = isEmpty l "is empty"

  fun isNonEmpty l desc = is (fn () => pf (not (List.null l)) "List was expected to be non-empty, is empty") desc
  fun isNonEmpty' l = isNonEmpty l "is non-empty"

  local
    (* @todo Colour the whole thing yellow *)
    (* fun formatExnMsg msg = "\"" ^ String.toCString msg ^ "\"" *)
    fun formatExnMsg msg = "\^[[0;33m\"" ^ String.toCString msg ^ "\"\^[[0m"
  in
    fun isRaising expectedExnMsg f desc =
      is
        (fn () =>
          let
            val raised = (f (); NONE) handle e => SOME {e=e, msg=(exnMessage e)}
          in
            case raised of
              NONE =>
                pf false ("Expected exception " ^ (formatExnMsg expectedExnMsg) ^ " to be raised, but none was")
            | SOME {e, msg} =>
                (*
                @todo Just compare based on exnName rather than exnMessage?
                @body Using exnMessage is more accurate and elimits false positives,
                @body but the format is the message varies between SML implementations
                @body so that that makes tests brittle?
                @body Though, both MLton and SML/NJ (https://www.tutorialspoint.com/execute_smlnj_online.php)
                @body seem to use the same format.
                *)
                pf
                  (msg = expectedExnMsg)
                  ("Expected exception " ^ (formatExnMsg expectedExnMsg) ^ " to be raised, but exception " ^ (formatExnMsg msg) ^ " was")
          end
        )
        desc
    fun isRaising' expectedExnMsg f =
      isRaising expectedExnMsg f ("raises " ^ formatExnMsg expectedExnMsg)
  end

  (* Running *)

  datatype testable_result = TestResult of string * result
                           | SuiteResult of string * testable_result list

  fun run (Test (f, desc)) = TestResult (desc, f ())
    | run (Suite (desc, body)) = SuiteResult (desc, map run body)

  (* Statistics *)

  fun count_tests (TestResult _) = 1
    | count_tests (SuiteResult (_, body)) = sum count_tests body
  and count_passed (TestResult (_, Pass)) = 1
    | count_passed (TestResult (_, (Fail _))) = 0
    | count_passed (SuiteResult (_, body)) = sum count_passed body
  and count_failed (TestResult (_, Pass)) = 0
    | count_failed (TestResult (_, (Fail _))) = 1
    | count_failed (SuiteResult (_, body)) = sum count_failed body
  and sum f body = foldl (op +) 0 (map f body)

  (* Reporting *)

  type reporter = testable_result -> string

  fun reportToplevel res = (report 0 res) ^ "\n\n" ^ (stats res)
  and report depth (TestResult (desc, Pass)) = pad depth "\^[[0;32mO\^[[0m " ^ desc
    | report depth (TestResult (desc, Fail msg)) = pad depth "\^[[0;31mX\^[[0m " ^ desc ^ " - error: " ^ msg
    | report depth (SuiteResult (desc, body)) = let val first = pad depth ("\^[[0;36m" ^ desc ^ ":\^[[0m")
                                                    and rest = map (fn t => report (depth + 1) t) body
                                                in
                                                    String.concatWith "\n" (first :: rest)
                                                end
  and pad depth s = StringCvt.padLeft #" " (String.size s + (depth * 2)) s
  and stats res = "Tests:  " ^ (Int.toString (count_tests res)) ^ "\n" ^
                  "Passed: " ^ (Int.toString (count_passed res)) ^ "\n" ^
                  "Failed: " ^ (Int.toString (count_failed res))

(*
Tests:  4
Passed: 3  // green if 100% (allPass)
Failed: 1  // red if not 0% (anyFail)

fun colored (s: string, positive: bool) : string =
  (* remember isatty, also https://no-color.org *)

*)


  val defaultReporter = reportToplevel

  fun runAndPrint t r = print (r (run t) ^ "\n")

  fun runAndQuit t r = let val res = run t
                       in
                           print (r res ^ "\n");
                           OS.Process.exit (if count_failed res = 0 then
                                                OS.Process.success
                                            else
                                                OS.Process.failure)
                       end
end
