type t =
  { mutable commit_pos : int
  ; initial_commit_pos : int
  ; buffer : Bigstring.t
  }

let create initial_commit_pos buffer =
  { commit_pos = initial_commit_pos
  ; initial_commit_pos
  ; buffer
  }

let length t =
  Bigstring.length t.buffer + t.initial_commit_pos

let committed_bytes t =
  t.commit_pos - t.initial_commit_pos

let initial_commit_pos t = t.initial_commit_pos
let commit_pos         t = t.commit_pos

let uncommitted_bytes t =
  Bigstring.length t.buffer - commit_pos t

let apply t pos len ~f =
  let off = pos - t.initial_commit_pos in
  f t.buffer ~off ~len

let get_char t pos =
  apply t pos 1 ~f:(fun buf ~off ~len:_ -> Bigstring.unsafe_get buf off)

let count_while t pos ~f =
  let buffer = t.buffer in
  let i = ref (pos - t.initial_commit_pos) in
  let len = Bigstring.length buffer in
  while !i < len && f (Bigstring.unsafe_get buffer !i) do 
    incr i
  done;
  !i - (pos - t.initial_commit_pos)

let commit t pos =
  t.commit_pos <- pos
