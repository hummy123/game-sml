signature GAME_TYPE =
sig
  type wall = {id: int, x: int, y: int, width: int, height: int}

  type platform = {id: int, x: int, y: int, width: int}

  datatype y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype player_recoil = NO_RECOIL | RECOIL_LEFT of int | RECOIL_RIGHT of int

  datatype player_attacked = NOT_ATTACKED | ATTACKED of int

  datatype facing = FACING_LEFT | FACING_RIGHT

  datatype main_attack =
    MAIN_NOT_ATTACKING
  | MAIN_ATTACKING
  | MAIN_CHARGING
  | MAIN_THROWING

  type defeated_enemies = {angle: int}

  type player_projectile = {x: int, y: int, facing: facing}

  type player =
    { yAxis: y_axis
    , xAxis: x_axis
    , recoil: player_recoil
    , attacked: player_attacked
    , mainAttack: main_attack
    , mainAttackPressed: bool
    , facing: facing
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    , enemies: defeated_enemies vector
    , charge: int
    , projectiles: player_projectile vector
    , platID: int
    }

  type enemy =
    { id: int
    , health: int
    , x: int
    , y: int
    , xAxis: x_axis
    , yAxis: y_axis
    , variant: EnemyVariants.t
    , platID: int
    , nextPlatID: int
    }

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
    , enemies: enemy vector
    }

  val initial: game_type
end

structure GameType :> GAME_TYPE =
struct
  type wall = {id: int, x: int, y: int, width: int, height: int}

  (* all platforms have a fixed visual height and a fixed collision height *)
  type platform = {id: int, x: int, y: int, width: int}

  datatype y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype player_recoil = NO_RECOIL | RECOIL_LEFT of int | RECOIL_RIGHT of int

  datatype player_attacked = NOT_ATTACKED | ATTACKED of int

  datatype facing = FACING_LEFT | FACING_RIGHT

  datatype main_attack =
    MAIN_NOT_ATTACKING
  | MAIN_ATTACKING
  | MAIN_CHARGING
  | MAIN_THROWING

  type defeated_enemies = {angle: int}

  type player_projectile = {x: int, y: int, facing: facing}

  type player =
    { yAxis: y_axis
    , xAxis: x_axis
    , recoil: player_recoil
    , attacked: player_attacked
    , mainAttack: main_attack
    , mainAttackPressed: bool
    , facing: facing
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    , enemies: defeated_enemies vector
    , charge: int
    , projectiles: player_projectile vector
    , platID: int
    }

  type enemy =
    { id: int
    , health: int
    , x: int
    , y: int
    , xAxis: x_axis
    , yAxis: y_axis
    , variant: EnemyVariants.t
    , platID: int
    , nextPlatID: int
    }

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
    , enemies: enemy vector
    }

  val initial: game_type =
    let
      val player =
        { yAxis = JUMPING 0
        , xAxis = STAY_STILL
        , recoil = NO_RECOIL
        , attacked = NOT_ATTACKED
        , mainAttack = MAIN_NOT_ATTACKING
        , mainAttackPressed = false
        , facing = FACING_RIGHT
        , health = 3
        , x = 500
        , y = 800
        , jumpPressed = false
        , enemies = Vector.fromList []
        , charge = Constants.maxCharge
        , projectiles = Vector.fromList []
        , platID = ~1
        }

      val wall1 = {id = 1, x = 0, y = 0, width = 100, height = 1080}
      val wall2 = {id = 2, x = 1820, y = 0, width = 100, height = 1080}
      val wall3 = {id = 3, x = 0, y = 980, width = 1920, height = 108}
      val walls = Vector.fromList [wall1, wall2, wall3]
      val wallTree = Wall.generateTree walls

      val plat1 = {id = 1, x = 111, y = 711, width = 199}
      val plat2 = {id = 2, x = 355, y = 759, width = 555}
      val plat3 = {id = 3, x = 955, y = 659, width = 111}
      val plat4 = {id = 4, x = 455, y = 855, width = 99}
      val plat5 = {id = 5, x = 555, y = 811, width = 199}
      val plat6 = {id = 6, x = 655, y = 710, width = 199}
      val plat7 = {id = 7, x = 701, y = 855, width = 99}
      val plat8 = {id = 8, x = 970, y = 815, width = 303}
      val plat9 = {id = 9, x = 959, y = 705, width = 303}
      val plat10 = {id = 10, x = 970, y = 759, width = 303}
      val plat11 = {id = 11, x = 970, y = 595, width = 303}
      val plat12 = {id = 12, x = 959, y = 535, width = 303}
      val plat13 = {id = 13, x = 970, y = 495, width = 303}
      val plat14 = {id = 14, x = 1000, y = 415, width = 303}
      val plat15 = {id = 15, x = 1000, y = 335, width = 303}
      val plat16 = {id = 16, x = 1000, y = 295, width = 303}
      val plat17 = {id = 17, x = 855, y = 599, width = 199}
      val plat18 = {id = 18, x = 755, y = 499, width = 199}
      val plat19 = {id = 19, x = 655, y = 399, width = 199}
      val platforms = Vector.fromList
        [ plat1
        , plat2
        , plat3
        , plat4
        , plat5
        , plat6
        , plat7
        , plat8
        , plat9
        , plat10
        , plat11
        , plat12
        , plat13
        , plat14
        , plat15
        , plat16
        , plat17
        , plat18
        , plat19
        ]
      val platformTree = Platform.generateTree platforms

      val enemy1 =
        { id = 1
        , x = 751
        , y = 555
        , health = 1
        , xAxis = STAY_STILL
        , yAxis = FALLING
        , variant = EnemyVariants.FOLLOW_SLIME
        , platID = ~1
        , nextPlatID = ~1
        }
      val enemy2 =
        { id = 2
        , x = 555
        , y = 945
        , health = 1
        , xAxis = MOVE_LEFT
        , yAxis = FALLING
        , variant = EnemyVariants.PATROL_SLIME
        , platID = ~1
        , nextPlatID = ~1
        }
      val enemy3 =
        { id = 3
        , x = 979
        , y = 945
        , health = 1
        , xAxis = MOVE_RIGHT
        , yAxis = FALLING
        , variant = EnemyVariants.PATROL_SLIME
        , platID = ~1
        , nextPlatID = ~1
        }
      val enemies = Vector.fromList [enemy1]
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      }
    end
end
