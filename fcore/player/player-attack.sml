structure PlayerAttack =
struct
  structure AttackEnemies =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = PlayerType.defeated_enemies list * EnemyMap.t

         open EnemyType

         fun onAttacked (enemyID, enemy, enemyMap, defeatedList) =
           case #variant enemy of
             PATROL_SLIME =>
               let
                 val defeatedList = {angle = 1} :: defeatedList
                 val enemyMap = EnemyMap.remove (enemyID, enemyMap)
               in
                 (defeatedList, enemyMap)
               end
           | FOLLOW_SLIME =>
               let
                 val defeatedList = {angle = 1} :: defeatedList
                 val enemyMap = EnemyMap.remove (enemyID, enemyMap)
               in
                 (defeatedList, enemyMap)
               end
           | STRAIGHT_BAT =>
               let
                 val defeatedList = {angle = 1} :: defeatedList
                 val enemyMap = EnemyMap.remove (enemyID, enemyMap)
               in
                 (defeatedList, enemyMap)
               end

         fun fold (enemyID, (), (defeatedList, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy => onAttacked (enemyID, enemy, enemyMap, defeatedList)
           | NONE => (defeatedList, enemyMap)
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

            val (defeatedList, enemyMap) = AttackEnemies.foldRegion
              (x, y, length, height, (), ([], enemyMap), enemyTree)

            val defeatedList = Vector.fromList defeatedList
            val defeatedList = Vector.concat [defeatedList, #enemies player]

            val player =
              PlayerPatch.withPatch (player, PlayerPatch.W_ENEMIES defeatedList)
          in
            (player, enemyMap)
          end
      | _ => (player, enemyMap)
    end
end
