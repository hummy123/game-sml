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
    }

  type enemy =
    {id: int, health: int, x: int, y: int, xAxis: x_axis, yAxis: y_axis}

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
    }

  type enemy =
    {id: int, health: int, x: int, y: int, xAxis: x_axis, yAxis: y_axis}

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
        , y = 500
        , jumpPressed = false
        , enemies = Vector.fromList []
        , charge = Constants.maxCharge
        , projectiles = Vector.fromList []
        }

      val wall1 = {id = 1, x = 0, y = 0, width = 100, height = 1080}
      val wall2 = {id = 2, x = 1820, y = 0, width = 100, height = 1080}
      val wall3 = {id = 3, x = 0, y = 980, width = 1920, height = 108}
      val walls = Vector.fromList [wall1, wall2, wall3]
      val wallTree = Wall.generateTree walls

      val plat1 = {id = 1, x = 155, y = 911, width = 155}
      val platforms = Vector.fromList [plat1]
      val platformTree = Platform.generateTree platforms

      val enemy1 =
        { id = 1
        , x = 300
        , y = 745
        , health = 5
        , xAxis = MOVE_LEFT
        , yAxis = FALLING
        }
      val enemy2 =
        { id = 2
        , x = 555
        , y = 945
        , health = 5
        , xAxis = MOVE_LEFT
        , yAxis = FALLING
        }
      val enemy3 =
        { id = 3
        , x = 979
        , y = 945
        , health = 5
        , xAxis = MOVE_RIGHT
        , yAxis = FALLING
        }
      val enemies = Vector.fromList [enemy1, enemy2, enemy3]
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
