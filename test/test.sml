(** @see https://people.mpi-sws.org/~rossberg/sml.html grammar for nodes *)
(*
 * TODO: comment fill
 *)
fun tst () =
  let
    val done = ref false
  in
    while
      not done
    do
      done := true
    ;
    case lst of
      [] => []
    | (p'::ps) => p'
  end

fun recs (
  lst: string list list,
  r,
  {
    first: string,
    middle: string,
    last: string
  }
) = case r of
      {} => 0
    | {f,m,l} => l

local
  val x = 1
in
  x
end
