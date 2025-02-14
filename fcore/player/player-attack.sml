structure PlayerAttack =
struct
  structure AttackEnemies =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = EnemyMap.t

         open EnemyType

         fun onAttacked
           (enemyID: int, enemy: EnemyType.enemy, enemyMap: EnemyMap.t) =
           case #variant enemy of
             PATROL_SLIME => EnemyMap.remove (enemyID, enemyMap)
           | FOLLOW_SLIME => EnemyMap.remove (enemyID, enemyMap)
           | STRAIGHT_BAT => EnemyMap.remove (enemyID, enemyMap)

         fun fold (enemyID, (), enemyMap) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy => onAttacked (enemyID, enemy, enemyMap)
           | NONE => enemyMap
       end)

  fun attackEnemies (player: PlayerType.player, enemyMap, enemyTree) =
    let
      open PlayerType
      val {x, y, facing, mainAttack, ...} = player
    in
      case mainAttack of
        MAIN_ATTACKING {length, ...} =>
          let
            open EntityType
            val height = Constants.playerSize
            val x =
              (case facing of
                 FACING_RIGHT => x + Constants.playerSize
               | FACING_LEFT => x - length)
          in
            AttackEnemies.foldRegion
              (x, y, length, height, (), enemyMap, enemyTree)
          end
      | _ => enemyMap
    end
end
