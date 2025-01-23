signature PHYSICS_INPUT =
sig
  type t
  type patch

  val entitySize: int

  (* constants for physics *)
  val moveBy: int
  val floatLimit: int
  val jumpLimit: int

  (* destructuring functions *)
  val getX: t -> int
  val getY: t -> int
  val getXAxis: t -> GameType.x_axis
  val getYAxis: t -> GameType.y_axis

  val W_X: int -> patch
  val W_Y: int -> patch
  val W_Y_AXIS: GameType.y_axis -> patch
end

functor MakePhysics(Fn: PHYSICS_INPUT) =
struct
  open GameType

  fun getPhysicsPatches input =
    let
      val x = Fn.getX input
      val y = Fn.getY input
      val xAxis = Fn.getXAxis input
      val yAxis = Fn.getYAxis input

      val desiredX =
        case xAxis of
          STAY_STILL => x
        | MOVE_LEFT => x - Fn.moveBy
        | MOVE_RIGHT => x + Fn.moveBy
    in
      case yAxis of
        ON_GROUND => [Fn.W_X desiredX]
      | FLOATING floated =>
          let
            val yAxis =
              if floated = Fn.floatLimit then FALLING
              else FLOATING (floated + 1)
          in
            [Fn.W_X desiredX, Fn.W_Y_AXIS yAxis]
          end
      | FALLING =>
          let val desiredY = y + Fn.moveBy
          in [Fn.W_X desiredX, Fn.W_Y desiredY]
          end
      | DROP_BELOW_PLATFORM =>
          let val desiredY = y + Fn.moveBy
          in [Fn.W_X desiredX, Fn.W_Y desiredY]
          end
      | JUMPING jumped =>
          if jumped + Fn.moveBy > Fn.jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let val newYAxis = FLOATING 0
            in [Fn.W_X desiredX, Fn.W_Y_AXIS newYAxis]
            end
          else
            (* jump *)
            let
              val newJumped = jumped + Fn.moveBy
              val newYAxis = JUMPING newJumped
              val desiredY = y - Fn.moveBy
            in
              [Fn.W_X desiredX, Fn.W_Y desiredY, Fn.W_Y_AXIS newYAxis]
            end
    end

  fun standingOnArea (x, y, tree) =
    let
      val y = y + Fn.entitySize - 1

      val width = Fn.entitySize
      val height = Platform.platHeight

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight
    in
      QuadTree.hasCollisionAt (x, y, width, height, 0, 0, ww, wh, ~1, tree)
    end

  fun standingOnAreaID (x, y, tree) =
    let
      val y = y + Fn.entitySize - 1

      val width = Fn.entitySize
      val height = Platform.platHeight

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight
    in
      QuadTree.getItemID (x, y, width, height, 0, 0, ww, wh, tree)
    end

  fun getWallPatches (walls: wall vector, lst, acc) =
    let
      open QuadTree
    in
      case lst of
        (QUERY_ON_LEFT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX + wallWidth
            val acc = Fn.W_X newX :: acc
          in
            getWallPatches (walls, tl, acc)
          end
      | (QUERY_ON_RIGHT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX - Fn.entitySize
            val acc = Fn.W_X newX :: acc
          in
            getWallPatches (walls, tl, acc)
          end
      | (QUERY_ON_BOTTOM_SIDE, wallID) :: tl =>
          let
            val {y = wallY, ...} = Vector.sub (walls, wallID - 1)

            val newY = wallY - Fn.entitySize
            val acc = Fn.W_Y_AXIS ON_GROUND :: Fn.W_Y newY :: acc
          in
            getWallPatches (walls, tl, acc)
          end
      | (QUERY_ON_TOP_SIDE, wallID) :: tl => getWallPatches (walls, tl, acc)
      | [] => acc
    end

  fun getEnvironmentPatches (input, walls, wallTree, platforms, platformTree) =
    let
      (* react to platform and wall collisions  *)
      val x = Fn.getX input
      val y = Fn.getY input
      val yAxis = Fn.getYAxis input

      val size = Fn.entitySize
      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val platCollisions = QuadTree.getCollisionsBelow
        (x, y, size, size, 0, 0, ww, wh, 0, platformTree)

      val platID = standingOnAreaID (x, y, platformTree)

      val acc = []

      val acc =
        if platID <> ~1 then
          (case yAxis of
             DROP_BELOW_PLATFORM =>
               (* pass through, allowing player to drop below the platform *)
               acc
           | JUMPING _ =>
               (* pass through, allowing player to jump above the platform *)
               acc
           | FLOATING _ =>
               (* pass through, allowing player to jump above the platform *)
               acc
           | _ =>
               let
                 (* default case: 
                  * player will land on platform and stay on the ground there. *)
                 val {y = platY, ...}: GameType.platform =
                   Vector.sub (platforms, platID - 1)

                 val newY = platY - Fn.entitySize
                 val acc = Fn.W_Y_AXIS ON_GROUND :: Fn.W_Y newY :: acc
               in
                 acc
               end)
        else
          acc

      val acc =
        case yAxis of
          DROP_BELOW_PLATFORM =>
            (* if we dropped below platform before 
            * but we have fully passed the platform now 
            * such that there are no platform collisions
            * then set new yAxis to FALLING
            * so we do not drop below any platforms again
            * *)
            if
              QuadTree.hasCollisionAt
                (x, y, size, size, 0, 0, ww, wh, ~1, platformTree)
            then acc
            else Fn.W_Y_AXIS FALLING :: acc
        | _ => acc

      val wallCollisions = QuadTree.getCollisionSides
        (x, y, size, size, 0, 0, ww, wh, 0, wallTree)
    in
      getWallPatches (walls, wallCollisions, acc)
    end
end

structure PlayerPhysics =
  MakePhysics
    (struct
       type t = GameType.player
       type patch = PlayerPatch.player_patch

       val entitySize = Constants.playerSize

       (* constants for physics *)
       val moveBy = Constants.movePlayerBy
       val floatLimit = Constants.floatLimit
       val jumpLimit = Constants.jumpLimit

       (* destructuring functions *)
       fun getX ({x, ...}: t) = x
       fun getY ({y, ...}: t) = y

       fun getXAxis ({xAxis, ...}: t) = xAxis
       fun getYAxis ({yAxis, ...}: t) = yAxis

       val W_X = PlayerPatch.W_X
       val W_Y = PlayerPatch.W_Y
       val W_Y_AXIS = PlayerPatch.W_Y_AXIS
     end)

structure EnemyPhysics =
  MakePhysics
    (struct
       type t = GameType.enemy
       type patch = EnemyPatch.enemy_patch

       val entitySize = Constants.enemySize

       (* constants for physics *)
       val moveBy = Constants.moveEnemyBy
       val floatLimit = Constants.floatLimit
       val jumpLimit = Constants.jumpLimit

       (* destructuring functions *)
       fun getX ({x, ...}: t) = x
       fun getY ({y, ...}: t) = y

       fun getXAxis ({xAxis, ...}: t) = xAxis
       fun getYAxis ({yAxis, ...}: t) = yAxis

       val W_X = EnemyPatch.W_X
       val W_Y = EnemyPatch.W_Y
       val W_Y_AXIS = EnemyPatch.W_Y_AXIS
     end)
