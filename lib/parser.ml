type 'a state =
  | Partial of 'a partial
  | Done    of int * 'a
  | Fail    of int * string list * string
and 'a partial =
  { committed : int
  ; continue  : Bigstring.t -> More.t -> 'a state }

type 'a with_state = Input.t ->  int -> More.t -> 'a

type 'a failure = (string list -> string -> 'a state) with_state
type ('a, 'r) success = ('a -> 'r state) with_state

type 'a t =
  { run : 'r. ('r failure -> ('a, 'r) success -> 'r state) with_state }

let fail_k    buf pos _ marks msg = Fail(pos - Input.initial_commit_pos buf, marks, msg)
let succeed_k buf pos _       v   = Done(pos - Input.initial_commit_pos buf, v)

let fail_to_string marks err =
  String.concat " > " marks ^ ": " ^ err

let state_to_option = function
  | Done(_, v) -> Some v
  | _          -> None

let state_to_result = function
  | Done(_, v)          -> Result.Ok v
  | Partial _           -> Result.Error "incomplete input"
  | Fail(_, marks, err) -> Result.Error (fail_to_string marks err)

let parse ?(input=Bigstring.empty) p =
  p.run (Input.create 0 input) 0 Incomplete fail_k succeed_k

let parse_bigstring p input =
  state_to_result (p.run (Input.create 0 input) 0 Complete fail_k succeed_k)

module Monad = struct
  let return =
    fun v ->
      { run = fun input pos more _fail succ ->
        succ input pos more v
      }

  let fail msg =
    { run = fun input pos more fail _succ ->
      fail input pos more [] msg
    }

  let (>>=) p f =
    { run = fun input pos more fail succ ->
      let succ' input' pos' more' v = (f v).run input' pos' more' fail succ in
      p.run input pos more fail succ'
    }

  let (>>|) p f =
    { run = fun input pos more fail succ ->
      let succ' input' pos' more' v = succ input' pos' more' (f v) in
      p.run input pos more fail succ'
    }

  let (<$>) f m =
    m >>| f

  let (<*>) f m =
    (* f >>= fun f -> m >>| f *)
    { run = fun input pos more fail succ ->
      let succ0 input0 pos0 more0 f =
        let succ1 input1 pos1 more1 m = succ input1 pos1 more1 (f m) in
        m.run input0 pos0 more0 fail succ1
      in
      f.run input pos more fail succ0 }

  let lift f m =
    f <$> m

  let lift2 f m1 m2 =
    { run = fun input pos more fail succ ->
      let succ1 input1 pos1 more1 m1 =
        let succ2 input2 pos2 more2 m2 = succ input2 pos2 more2 (f m1 m2) in
        m2.run input1 pos1 more1 fail succ2
      in
      m1.run input pos more fail succ1 }

  let lift3 f m1 m2 m3 =
    { run = fun input pos more fail succ ->
      let succ1 input1 pos1 more1 m1 =
        let succ2 input2 pos2 more2 m2 =
          let succ3 input3 pos3 more3 m3 =
            succ input3 pos3 more3 (f m1 m2 m3) in
          m3.run input2 pos2 more2 fail succ3 in
        m2.run input1 pos1 more1 fail succ2
      in
      m1.run input pos more fail succ1 }

  let lift4 f m1 m2 m3 m4 =
    { run = fun input pos more fail succ ->
      let succ1 input1 pos1 more1 m1 =
        let succ2 input2 pos2 more2 m2 =
          let succ3 input3 pos3 more3 m3 =
            let succ4 input4 pos4 more4 m4 =
              succ input4 pos4 more4 (f m1 m2 m3 m4) in
            m4.run input3 pos3 more3 fail succ4 in
          m3.run input2 pos2 more2 fail succ3 in
        m2.run input1 pos1 more1 fail succ2
      in
      m1.run input pos more fail succ1 }

  let ( *>) a b =
    (* a >>= fun _ -> b *)
    { run = fun input pos more fail succ ->
      let succ' input' pos' more' _ = b.run input' pos' more' fail succ in
      a.run input pos more fail succ'
    }

  let (<* ) a b =
    (* a >>= fun x -> b >>| fun _ -> x *)
    { run = fun input pos more fail succ ->
      let succ0 input0 pos0 more0 x =
        let succ1 input1 pos1 more1 _ = succ input1 pos1 more1 x in
        b.run input0 pos0 more0 fail succ1
      in
      a.run input pos more fail succ0 }
end

module Choice = struct
  let (<?>) p mark =
    { run = fun input pos more fail succ ->
      let fail' input' pos' more' marks msg =
        fail input' pos' more' (mark::marks) msg in
      p.run input pos more fail' succ
    }

  let (<|>) p q =
    { run = fun input pos more fail succ ->
      let fail' input' pos' more' marks msg =
        (* The only two constructors that introduce new failure continuations are
         * [<?>] and [<|>]. If the initial input position is less than the length
         * of the committed input, then calling the failure continuation will
         * have the effect of unwinding all choices and collecting marks along
         * the way. *)
        if pos < Input.commit_pos input' then
          fail input' pos' more marks msg
        else
          q.run input' pos more' fail succ in
      p.run input pos more fail' succ
    }
end

module Monad_use_for_debugging = struct
  let return = Monad.return
  let fail   = Monad.fail
  let (>>=)  = Monad.(>>=)

  let (>>|) m f = m >>= fun x -> return (f x)

  let (<$>) f m = m >>| f
  let (<*>) f m = f >>= fun f -> m >>| f

  let lift  = (>>|)
  let lift2 f m1 m2       = f <$> m1 <*> m2
  let lift3 f m1 m2 m3    = f <$> m1 <*> m2 <*> m3
  let lift4 f m1 m2 m3 m4 = f <$> m1 <*> m2 <*> m3 <*> m4

  let ( *>) a b = a >>= fun _ -> b
  let (<* ) a b = a >>= fun x -> b >>| fun _ -> x
end
