type Person (name : string, address : string, phone : string) =
  member this.name = name
  member this.address = address
  member this.phone = phone
  override this.ToString () = name+", "+address+", "+phone
type Customer (name : string, address : string, phone : string) =
  inherit Person (name, address, phone)
  static let mutable nextId = 0
  let id = nextId
  do nextId <- nextId+1
  let mutable _mailingList = false
  member this.mailingList
    with get() = _mailingList
    and set(m) = _mailingList <- m
  override this.ToString() = base.ToString()+", "+string(id)+", "+string(_mailingList)
let c1 = Customer ("Jon", "Universitetsparken 1", "24482894")
let c2 = Customer ("Hans", "Universitetsparken 5", "35321400")
printfn "%A\n%A" c1 c2    
