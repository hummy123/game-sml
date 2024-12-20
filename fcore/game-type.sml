signature GAME_TYPE =
sig
  type wall = {id: int, x: int, y: int, width: int, height: int}

  type platform = {id: int, x: int, y: int, width: int}

  datatype player_y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype player_x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype player_recoil = NO_RECOIL | RECOIL_LEFT of int | RECOIL_RIGHT of int

  datatype player_attacked = NOT_ATTACKED | ATTACKED of int

  datatype facing = FACING_LEFT | FACING_RIGHT

  type player =
    { yAxis: player_y_axis
    , xAxis: player_x_axis
    , recoil: player_recoil
    , attacked: player_attacked
    , facing: facing
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    }

  type enemy = {id: int, health: int, x: int, y: int}

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
    , enemies: enemy vector
    , enemyTree: QuadTree.t
    }

  val initial: game_type
end

structure GameType :> GAME_TYPE =
struct
  type wall = {id: int, x: int, y: int, width: int, height: int}

  (* all platforms have a fixed visual height and a fixed collision height *)
  type platform = {id: int, x: int, y: int, width: int}

  datatype player_y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype player_x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype player_recoil = NO_RECOIL | RECOIL_LEFT of int | RECOIL_RIGHT of int

  datatype player_attacked = NOT_ATTACKED | ATTACKED of int

  datatype facing = FACING_LEFT | FACING_RIGHT

  type player =
    { yAxis: player_y_axis
    , xAxis: player_x_axis
    , recoil: player_recoil
    , attacked: player_attacked
    , facing: facing
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    }

  type enemy = {id: int, health: int, x: int, y: int}

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
    , enemies: enemy vector
    , enemyTree: QuadTree.t
    }

  val initial: game_type =
    let
      val player =
        { yAxis = JUMPING 0
        , xAxis = STAY_STILL
        , recoil = NO_RECOIL
        , attacked = NOT_ATTACKED
        , facing = FACING_RIGHT
        , health = 3
        , x = 500
        , y = 500
        , jumpPressed = false
        }

      val wall1 = {id = 1, x = 0, y = 0, width = 100, height = 1080}
      val wall2 = {id = 2, x = 1820, y = 0, width = 100, height = 1080}
      val wall3 = {id = 3, x = 0, y = 980, width = 1920, height = 108}
      val walls = Vector.fromList [wall1, wall2, wall3]
      val wallTree = Wall.generateTree walls

      val plat1 = {id = 1, x = 155, y = 911, width = 155}
      val platforms = Vector.fromList [plat1]
      val platformTree = Platform.generateTree platforms

      val enemy1 = {id = 1, x = 300, y = 945, health = 5}
      val enemies = Vector.fromList [enemy1]
      val enemyTree = Enemy.generateTree enemies
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      , enemyTree = enemyTree
      }
    end
end
