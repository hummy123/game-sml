structure FrameInputType =
struct
  type t =
    { leftHeld: bool
    , rightHeld: bool
    , upHeld: bool
    , downHeld: bool
    , attackHeld: bool
    , jumpHeld: bool
    , escapeHeld: bool
    , newKeys: CoreKey.key_code list
    }
end
