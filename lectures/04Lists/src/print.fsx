let rec print lst =   
  match lst with
    [] -> printfn ""
    | elm::rst ->         
      printf "%A " elm
      print rst

print ["hello"; "world"]
