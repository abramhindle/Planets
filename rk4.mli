val step :
  y: float array -> 
  x: float -> h: float ->
  yout: float array -> 
  derivs: (float -> float array -> float array -> unit) -> 
  unit
