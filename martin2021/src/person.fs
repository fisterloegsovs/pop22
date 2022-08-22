type person = {first:string; last:string; age:int}

let xs = [{first="Lene"; last="Andersen"; age=56};
          {last="Hansen"; first="Jens"; age=39}]

let name (p:person) : string =
  p.first + " " + p.last

let incr_age (p:person) : person =
  {p with age=p.age+1}

let ys = List.map incr_age xs
