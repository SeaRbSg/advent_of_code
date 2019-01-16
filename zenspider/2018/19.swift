import Foundation

let PC   = 6
var slot = 0

typealias Reg  = [Int]
typealias Prim = (Int, Int) -> Int
typealias Op   = (inout Reg, Inst) -> Void
typealias Inst = (inout Reg) -> Void

func b2i(test: Bool) -> Int {
    return test ? 1 : 0
}

let gti = { (a: Int, b: Int) in b2i(test: a > b) }
let eqi = { (a: Int, b: Int) in b2i(test: a == b) }

func readFileLines(path: String) -> [String] {
    do {
        return try String(contentsOfFile: path).components(separatedBy: "\n")
    } catch {
        return []
    }
}

func handle_slot(_ s: String) {
    guard let n = Int(s) else {
        return
    }
    slot = n
}

let t = true
let f = false

func wrap_inst(_ fn: @escaping Prim, _ a: Int, _ b: Int, _ c: Int, _ aReg: Bool, _ bReg: Bool?) -> Inst {
    switch (aReg, bReg) {
    case (t, t):   return { (r : inout Reg) in r[c] = fn(r[a], r[b]) }
    case (t, f):   return { (r : inout Reg) in r[c] = fn(r[a],   b ) }
    case (f, t):   return { (r : inout Reg) in r[c] = fn(  a , r[b]) }
    case (f, f):   return { (r : inout Reg) in /* no-op */           } // FIX how is this not exhaustive?!?
    case (t, nil): return { (r : inout Reg) in r[c] =    r[a]        }
    case (f, nil): return { (r : inout Reg) in r[c] =      a         }
    default:       return { (r : inout Reg) in /* no-op */           }
    }
}

func handle_op(_ words: [String]) -> Inst {
    let o = words[0]
    let a = Int(words[1]) ?? 0
    let b = Int(words[2]) ?? 0
    let c = Int(words[3]) ?? 0

    switch o {
    case "addr": return wrap_inst(+,   a, b, c, t, t)
    case "addi": return wrap_inst(+,   a, b, c, t, f)
    case "mulr": return wrap_inst(*,   a, b, c, t, t)
    case "muli": return wrap_inst(*,   a, b, c, t, f)
    case "banr": return wrap_inst(&,   a, b, c, t, t)
    case "bani": return wrap_inst(&,   a, b, c, t, f)
    case "borr": return wrap_inst(|,   a, b, c, t, t)
    case "bori": return wrap_inst(|,   a, b, c, t, f)
    case "setr": return wrap_inst(^,   a, b, c, t, nil)
    case "seti": return wrap_inst(^,   a, b, c, f, nil)
    case "gtir": return wrap_inst(gti, a, b, c, f, t)
    case "gtri": return wrap_inst(gti, a, b, c, t, f)
    case "gtrr": return wrap_inst(gti, a, b, c, t, t)
    case "eqir": return wrap_inst(eqi, a, b, c, f, t)
    case "eqri": return wrap_inst(eqi, a, b, c, t, f)
    case "eqrr": return wrap_inst(eqi, a, b, c, t, t)
    default:     return wrap_inst(^,   a, b, c, f, f) // f, f == no-op
    }
}

func parse(path: String) -> [Inst] {
    let lines = readFileLines(path: path)

    return lines.compactMap { str in
        let words = str.components(separatedBy: " ")

        switch words.count {
        case 2:
            handle_slot(words[1])
            return nil

        case 4:
            return handle_op(words)

        default:
            return nil
        }
    }
}

func exec(r: Reg, insts: [Inst]) -> Reg {
    var r = r // convert let to var... seems hacky
    let start = CFAbsoluteTimeGetCurrent()
    let max = insts.count

    repeat {
        let pc = r[PC]
        let op = insts[pc]

        r[slot] = pc
        op(&r)
        r[PC] = r[slot] + 1
    } while (r[PC] < max)

    let end = CFAbsoluteTimeGetCurrent() - start
    print("Took \(end) seconds")

    return r
}

func run() {
    let start = CFAbsoluteTimeGetCurrent()
    if CommandLine.arguments.count < 2 {
        for _ in 1...100 {
            CommandLine.arguments.append("/Users/ryan/Work/git/searbsg/advent_of_code/zenspider/2018/19.txt")
        }
    }

    for path in CommandLine.arguments[1...] {
        let insts = parse(path: path)

        let r  = [0, 0, 0, 0, 0, 0, 0]
        let r2 = exec(r: r, insts: insts)

        print(r2)
    }
    let end = CFAbsoluteTimeGetCurrent() - start
    print("Total took \(end) seconds")
}

let _top: Void = run()
