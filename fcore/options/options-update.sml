structure OptionsUpdate =
struct
  fun update (options, input, userKeys) =
    {mode = GameType.OPTIONS options, userKeys = userKeys}
end
