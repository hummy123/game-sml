signature PLAYER_PATCH =
sig
  datatype player_patch =
    W_X_AXIS of GameType.x_axis
  | W_Y_AXIS of GameType.y_axis
  | W_RECOIL of GameType.player_recoil
  | W_ATTACKED of GameType.player_attacked
  | W_MAIN_ATTACK of GameType.main_attack
  | W_FACING of GameType.facing
  | W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_JUMP_PRESSED of bool
  | W_ENEMIES of GameType.defeated_enemies vector
  | W_CHARGE of int
  | W_PROJECTILES of GameType.player_projectile vector
  | W_PLAT_ID of int

  val withPatch: GameType.player * player_patch -> GameType.player
  val withPatches: GameType.player * player_patch list -> GameType.player
end

structure PlayerPatch: PLAYER_PATCH =
struct
  datatype player_patch =
    W_X_AXIS of GameType.x_axis
  | W_Y_AXIS of GameType.y_axis
  | W_RECOIL of GameType.player_recoil
  | W_ATTACKED of GameType.player_attacked
  | W_MAIN_ATTACK of GameType.main_attack
  | W_FACING of GameType.facing
  | W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_JUMP_PRESSED of bool
  | W_ENEMIES of GameType.defeated_enemies vector
  | W_CHARGE of int
  | W_PROJECTILES of GameType.player_projectile vector
  | W_PLAT_ID of int

  fun mkPlayer
    ( health
    , xAxis
    , yAxis
    , x
    , y
    , jumpPressed
    , recoil
    , attacked
    , mainAttack
    , facing
    , mainAttackPressed
    , enemies
    , charge
    , projectiles
    , platID
    ) =
    { yAxis = yAxis
    , xAxis = xAxis
    , recoil = recoil
    , attacked = attacked
    , mainAttack = mainAttack
    , mainAttackPressed = mainAttackPressed
    , facing = facing
    , health = health
    , x = x
    , y = y
    , jumpPressed = jumpPressed
    , enemies = enemies
    , charge = charge
    , projectiles = projectiles
    , platID = platID
    }

  fun withPatch (player, patch) =
    let
      val
        { yAxis
        , xAxis
        , recoil
        , attacked
        , mainAttack
        , mainAttackPressed
        , facing
        , health
        , x
        , y
        , jumpPressed
        , enemies
        , charge
        , projectiles
        , platID
        } = player
    in
      case patch of
        W_X_AXIS xAxis =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_Y_AXIS yAxis =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_RECOIL recoil =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_ATTACKED attacked =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_MAIN_ATTACK mainAttack =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_FACING facing =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_HEALTH health =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_X x =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_Y y =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_JUMP_PRESSED jumpPressed =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_ENEMIES enemies =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_CHARGE charge =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_PROJECTILES projectiles =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
      | W_PLAT_ID platID =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            , enemies
            , charge
            , projectiles
            , platID
            )
    end

  fun withPatches (player: GameType.player, lst) =
    case lst of
      hd :: tl =>
        let val player = withPatch (player, hd)
        in withPatches (player, tl)
        end
    | [] => player
end
