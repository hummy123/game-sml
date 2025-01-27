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
  val W_PLAT_ID: int -> patch
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
      QuadHelp.hasCollisionAt (x, y, width, height, tree)
    end

  fun standingOnAreaID (x, y, tree) =
    let
      val y = y + Fn.entitySize - 1

      val width = Fn.entitySize
      val height = Platform.platHeight + 2

      val plat1 = {id = 1, x = 155, y = 911, width = 199}
      val plat2 = {id = 2, x = 355, y = 759, width = 555}
      val plat3 = {id = 3, x = 355, y = 659, width = 111}
      val plat4 = {id = 4, x = 155, y = 855, width = 99}
      val plat5 = {id = 5, x = 155, y = 811, width = 199}
      val plat6 = {id = 6, x = 155, y = 710, width = 199}
      val plat7 = {id = 7, x = 301, y = 855, width = 99}
      val plat8 = {id = 8, x = 970, y = 815, width = 303}
      val plat9 = {id = 9, x = 959, y = 705, width = 303}
      val plat10 = {id = 10, x = 970, y = 759, width = 303}

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val _ = print "START getItemID\n"
      val r = 
      QuadHelp.getItemID (x, y, width, height, tree)
      val _ = print "FINISH getItemID\n"
    in r
    end

  fun getWallPatches (x, y, walls, wallTree, acc) =
    let
      val size = Fn.entitySize
      val moveBy = Fn.moveBy
      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      (* check collision with wall to the left *)
      val acc =
        let
          val leftWallID = QuadHelp.getItemID
            (x - 1, y, 1, 1, wallTree)
        in
          if leftWallID <> ~1 then
            let
              val {x = wallX, width = wallWidth, ...} =
                Vector.sub (walls, leftWallID - 1)

              val newX = wallX + wallWidth
            in
              Fn.W_X newX :: acc
            end
          else
            acc
        end

      (* check collision with wall to the right *)
      val acc =
        let
          val rightWallID = QuadHelp.getItemID
            (x + size - 1, y, 1, 1, wallTree)
        in
          if rightWallID <> ~1 then
            let
              val {x = wallX, ...} = Vector.sub (walls, rightWallID - 1)

              val newX = wallX - size
            in
              Fn.W_X newX :: acc
            end
          else
            acc
        end

      (* check collision with wall below *)
      val downWallID = QuadHelp.getItemID
        (x + moveBy + 1, y + size, 1, 1, wallTree)
    in
      if downWallID <> ~1 then
        let
          val {y = wallY, ...} = Vector.sub (walls, downWallID - 1)

          val newY = wallY - size
        in
          Fn.W_Y_AXIS ON_GROUND :: Fn.W_Y newY :: acc
        end
      else
        acc
    end

  fun getEnvironmentPatches
    (input, walls: wall vector, wallTree, platforms, platformTree) =
    let
      (* react to platform and wall collisions  *)
      val x = Fn.getX input
      val y = Fn.getY input
      val yAxis = Fn.getYAxis input

      val size = Fn.entitySize
      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val platID = standingOnAreaID (x, y, platformTree)

      val acc = []

      val acc =
        if platID <> ~1 then
          (print ("platID: " ^ Int.toString platID ^ "\n"); case yAxis of
             JUMPING _ =>
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
              QuadHelp.hasCollisionAt
                (x, y, size, size, platformTree)
            then acc
            else Fn.W_Y_AXIS FALLING :: acc
        | _ => acc

      val acc = getWallPatches (x, y, walls, wallTree, acc)

      val standPlatID = standingOnAreaID (x, y, platformTree)
    in
      if standPlatID <> ~1 then Fn.W_PLAT_ID standPlatID :: acc else acc
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
       val W_PLAT_ID = PlayerPatch.W_PLAT_ID
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
       val W_PLAT_ID = EnemyPatch.W_PLAT_ID
     end)
