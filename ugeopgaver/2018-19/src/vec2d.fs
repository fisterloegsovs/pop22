module vec2d
let len v : float = sqrt ((fst v) ** 2.0 + (snd v) ** 2.0)
let ang v : float = atan2 (snd v) (fst v)
let scale a v : float * float = (a * fst v, a * snd v)
let add v w : float * float = (fst v + fst w, snd v + snd w)
let dot v w : float = (fst v) * (fst w) + (snd v) * (snd w)
