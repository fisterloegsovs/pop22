(*
    F# solution to OOPD course assignment 5
*)

open System.Collections.Generic
open System.IO
open System.Net

exception BookingError

type Course(?name0) =

    let name = defaultArg name0 ""

    (*
        Method for getting course name
    *)
    member this.getName = name

type Room(name0) =

    let name = name0

    (*
        Data-structure for holding current bookings
        Here the string, the concatenation of day and time serves
        as a unique key for getting booked or booking courses.
    *)
    let bookings = new Dictionary<string, Course>()

    (*
        Method for getting room name
    *)
    member this.getName = name

    (*
        Method for checking room availability
    *)
    member this.isAvailable (day, time) = not (bookings.ContainsKey(day + " " + time))

    (*
        Method for reserving a room
    *)
    member this.reserveRoom (course, day, time) =
        let ntime = int time
        if ((this.isAvailable (day, time)) && ntime >= 8 && ntime < 17) then
            bookings.Add(day + " " + time, course)
        else
            raise BookingError ("Error in " + name + ", Time not available")

    member this.getReserver (day, time) = bookings.[day + " " + time]

    (*
        Method for printing room bookings of current room
    *)
    member this.printBookings =
        for booking in bookings do
            let time = booking.Key.Split [|' '|]
            printfn "Course: %s, %s at %s 'o' clock" (booking.Value.getName) (time.[0]) (time.[1])

type Planner() =


    // Data structures storing room and course instances
    let rooms = new Dictionary<string, Room>()
    let courses = new Dictionary<string, Course>()

    (*
        Method for booking a room
    *)
    member this.bookRoom (courseName, roomName, day, time) =
        if (rooms.ContainsKey(roomName) && courses.ContainsKey(courseName) && (rooms.[roomName].isAvailable(day, time))) then
            rooms.[roomName].reserveRoom(courses.[courseName], day, time)
        else
            raise BookingError "Room, Course not present or time not available"

    (*
        Method for loading rooms from file
    *)
    member this.loadRooms roomFileName =
        let inputLines = File.ReadLines(roomFileName)
        for curRoomName in inputLines do
            rooms.Add(curRoomName, new Room(curRoomName))

    (*
        Method for loading courses from file
    *)
    member this.loadCourses courseFileName =
        let inputLines = File.ReadLines(courseFileName)
        for curCourseName in inputLines do
            courses.Add(curCourseName, new Course(curCourseName))

    (*
        Method for loading bookings from file
    *)
    member this.loadBookings bookingFileName =
        let inputLines = File.ReadLines(bookingFileName)
        for booking in inputLines do
            let sBooking = booking.Split [|' '|]
            this.bookRoom (sBooking.[0], sBooking.[1], sBooking.[2], sBooking.[3])

    (*
        Method for printing all loaded room bookings
    *)
    member this.printBookings =
        for room in rooms do
            room.Value.printBookings

[<EntryPoint>]
let main _ =
    let p = new Planner()
    p.loadRooms("lokaler.txt")
    p.loadCourses("kurser.txt")
    p.loadBookings("skema.txt")

    p.printBookings
    0