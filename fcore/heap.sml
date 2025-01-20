(* implementation based on Chris Okasaki's paper describing SkewBinomialQueues
 * from the following PDF, based on figure 6 and figure 7.
 * https://www.brics.dk/RS/96/37/BRICS-RS-96-37.pdf
 *
 * Differences: 
 * - No exception is raised as we return a default value
 *   in the case of findMin when queue is empty 
 *   and we return the empty queue when queue is empty
 *   in the case of deleteMin.
 * - Use foldDeleteMin function to eliminate 
 *   runtime cost of closure/defunctionalisation
 * *)
signature ORDERED =
sig
  type t

  val default: t
  val leq: t * t -> bool
end

signature PRIORITY_QUEUE =
sig
  structure Elem: ORDERED

  type t
  val empty: t
  val isEmpty: t -> bool
  val insert: Elem.t * t -> t
  val findMin: t -> Elem.t
  val deleteMin: t -> t
end

functor MakeSkewHeap(E: ORDERED): PRIORITY_QUEUE =
struct
  structure Elem = E

  type rank = int

  datatype tree = NODE of Elem.t * rank * tree list
  type t = tree list

  fun root (NODE (x, _, _)) = x

  fun rank (NODE (_, r, _)) = r

  fun link (t1, t2) =
    case (t1, t2) of
      (NODE (x1, r1, c1), NODE (x2, r2, c2)) =>
        if Elem.leq (x1, x2) then NODE (x1, r1 + 1, t2 :: c1)
        else NODE (x2, r2 + 1, t1 :: c2)

  fun skewLink (t0, t1, t2) =
    case (t0, t1, t2) of
      (NODE (x0, r0, _), NODE (x1, r1, c1), NODE (x2, r2, c2)) =>
        if Elem.leq (x1, x0) andalso Elem.leq (x1, x2) then
          NODE (x1, r1 + 1, t0 :: t2 :: c1)
        else if Elem.leq (x2, x0) andalso Elem.leq (x2, x1) then
          NODE (x2, r2 + 1, t0 :: t1 :: c2)
        else
          NODE (x0, r1 + 1, [t1, t2])

  fun ins (t, t' :: ts) =
        if rank t < rank t' then t :: t' :: ts else ins (link (t, t'), ts)
    | ins (t, []) = [t]

  val empty = []

  fun isEmpty [] = true
    | isEmpty _ = false

  fun insert (x, ts as t1 :: t2 :: rest) =
        if rank t1 = rank t2 then skewLink (NODE (x, 0, []), t1, t2) :: rest
        else NODE (x, 0, []) :: ts
    | insert (x, ts) =
        NODE (x, 0, []) :: ts

  fun helpFindMin (prev, []) = root prev
    | helpFindMin (prev, [t]) = root t
    | helpFindMin (prev, t :: ts) =
        let val x = helpFindMin (t, ts)
        in if Elem.leq (root t, x) then root t else x
        end

  fun findMin [] = Elem.default
    | findMin [t] = root t
    | findMin (t :: ts) = helpFindMin (t, ts)

  fun getMin (prevT, t) =
    case t of
      [t] => (t, [])
    | t :: ts =>
        let val (t', ts') = getMin (t, ts)
        in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts')
        end
    | [] => (prevT, [])

  fun split (ts, xs, []) = (ts, xs)
    | split (ts, xs, t :: c) =
        if rank t = 0 then split (ts, root t :: xs, c)
        else split (t :: ts, xs, c)

  fun unify [] = []
    | unify (t :: ts) = ins (t, ts)

  fun meldUniq ([], ts) = ts
    | meldUniq (ts, []) = ts
    | meldUniq (t1 :: ts1, t2 :: ts2) =
        if rank t1 < rank t2 then t1 :: meldUniq (ts1, t2 :: ts2)
        else if rank t2 < rank t1 then t2 :: meldUniq (t1 :: ts1, ts2)
        else ins (link (t1, t2), meldUniq (ts1, ts2))

  fun meld (ts, ts') =
    meldUniq (unify ts, unify ts')

  fun foldDeleteMin (lst, state) =
    case lst of
      [] => state
    | hd :: tl =>
        let val state = insert (hd, state)
        in foldDeleteMin (tl, state)
        end

  fun deleteMin [] = raise Empty
    | deleteMin (ts as hd :: tl) =
        let
          val (NODE (x, r, c), ts) = getMin (hd, tl)
          val (ts', xs') = split ([], [], c)
        in
          foldDeleteMin (xs', meld (ts, ts'))
        end
end

structure DistHeap =
  MakeSkewHeap
    (struct
       type t = {distance: int, id: int}
       type id = int

       (* default = defaultID returned when queue is empty *)
       val default = {distance = ~1, id = ~1}

       fun getID {id, distance = _} = id

       fun leq ({distance = d1, ...}: t, {distance = d2, ...}: t) = d1 <= d2
     end)
