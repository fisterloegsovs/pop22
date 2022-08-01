let rec simulate = function
  | (x, y, vx, vy, ax, ay, t, dt, T) when t < T -> simulate ( x+dt*vx, y+dt*vy, vx+dt*ax, vy+dt*ay, ax, ay, t+dt,dt, T )
  | (x, y, vx, vy, ax, ay, t, dt, T) ->  (x, y, vx, vy, ax, ay, t, dt, T)
let (x, y, vx, vy, ax, ay, t, dt, T) = simulate (0.0, 0.0, 10.0, 10.0, 0.0, -9.82, 0.0, 0.1, 1.0)
printfn "%f, %f, %f, %f, %f, %f, %f, %f, %f" x y vx vy ax ay t dt T  
