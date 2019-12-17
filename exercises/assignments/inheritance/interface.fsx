type Person (name : string, address : string, phone : string) =
  member this.name = name
  member this.address = address
  member this.phone = phone
  override this.ToString () = name+", "+address+", "+phone
type Customer (name : string, address : string, phone : string) =
  inherit Person (name, address, phone)
  static let mutable nextId = 0
  let _id = nextId
  do nextId <- nextId+1
  let mutable _mailingList = false
  interface System.IComparable with
    member this.CompareTo obj =
      match obj with
        :? Customer as other -> compare this.id other.id
        | _ -> invalidArg "obj" "cannot compare values of different types"
  override this.Equals obj = 
      match obj with
        :? Customer as other -> this.id = other.id
        | _ -> invalidArg "obj" "cannot compare values of different types"
  override this.GetHashCode () = hash this.id
  member this.id = _id
  member this.mailingList
    with get() = _mailingList
    and set(m) = _mailingList <- m
  override this.ToString() = base.ToString()+", "+string(_id)+", "+string(_mailingList)
let c1 = Customer ("Jon", "Universitetsparken 1", "24482894")
let c2 = Customer ("Hans", "Universitetsparken 5", "35321400")
let lst = [c2;c1]
printfn "%A\n%A" lst (List.sort lst)
