structure Whip =
struct
  type box = {x: int, y: int}
  val size = 16
  val sizeReal: Real32.real = 16.0

  (* right frames *)
  val rf1 =
   #[ {x = 64, y = ~15}
    , {x = 64, y = 2}
    , {x = 64, y = 18}
    , {x = 49, y = 25}
    ]

  val rf2 =
   #[ {x = 67, y = ~15}
    , {x = 66, y = 2}
    , {x = 64, y = 18}
    , {x = 49, y = 25}
    ]

  val rf3 =
   #[ {x = 73, y = ~15}
    , {x = 70, y = 2}
    , {x = 64, y = 18}
    , {x = 49, y = 25}
    ]

  val rf4 =
   #[ {x = 192, y = 10}
    , {x = 128, y = 15}
    , {x = 64, y = 20}
    , {x = 49, y = 25}
    ]

  val rf5 =
   #[ {x = 305, y = 25}
    , {x = 241, y = 28}
    , {x = 177, y = 25}
    , {x = 113, y = 22}
    , {x = 49, y = 25}
    ]

  val rf6 =
   #[ {x = 305, y = 25}
    , {x = 241, y = 28}
    , {x = 177, y = 25}
    , {x = 113, y = 25}
    , {x = 49, y = 25}
    ]

  val rf7 =
   #[ {x = 241, y = 31}
    , {x = 177, y = 29}
    , {x = 113, y = 27}
    , {x = 49, y = 25}
    ]

  val rf8 =
   #[ {x = 177, y = 33}
    , {x = 113, y = 29}
    , {x = 49, y = 25}
    ]

  val rf9 =
   #[ {x = 113, y = 31}
    , {x = 49, y = 25}
    ]

  val rf10 =
   #[ {x = 49, y = 25} ]

  val rightFrames = #[ rf1, rf2, rf3, rf4, rf5, rf6, rf7, rf8, rf9, rf10 ]
end
