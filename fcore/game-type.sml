signature GAME_TYPE =
sig
  type wall = {id: int, x: int, y: int, width: int, height: int}

  type platform = {id: int, x: int, y: int, width: int}

  datatype player_y_axis =
    ON_GROUND
  | FALLING
  | JUMPING of int
  | FLOATING of int

  datatype player_x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  type player =
    { yAxis: player_y_axis
    , xAxis: player_x_axis
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    }

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
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
  | JUMPING of int
  | FLOATING of int

  datatype player_x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  type player =
    { yAxis: player_y_axis
    , xAxis: player_x_axis
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    }

  type game_type =
    { player: player
    , walls: wall vector
    , wallTree: QuadTree.t
    , platforms: platform vector
    , platformTree: QuadTree.t
    }

  val initial: game_type =
    let
      val player =
        { yAxis = JUMPING 0
        , xAxis = STAY_STILL
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
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      }
    end
end
