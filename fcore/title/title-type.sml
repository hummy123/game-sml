signature TITLE_TYPE =
sig
  datatype focus = START_BUTTON | OPTIONS_BUTTON

  type title_type = {focus: focus}

  val initial: title_type
end

structure TitleType :> TITLE_TYPE =
struct
  datatype focus = START_BUTTON | OPTIONS_BUTTON

  type title_type = {focus: focus}

  val initial = {focus = START_BUTTON}
end
