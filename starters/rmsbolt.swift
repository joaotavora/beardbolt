import Foundation

// Swift beardbolt starter file

// Local Variables:
// beardbolt-disassemble: nil
// End:

func isRMS(_ a: Character) -> Int {
    switch (a) {
    case "R":
        return 1
    case "M":
        return 2
    case "S":
        return 3
    default:
        return 0
    }
}

func main() -> Int {
    let a: Character = "N"
    if isRMS(a) == 0 {
        print(a)
    }
    return 0
}
