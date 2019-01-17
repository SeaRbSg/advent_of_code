import Foundation

struct Reg {
    var r0: Int = 0; var r1: Int = 0; var r2: Int = 0;
    var r3: Int = 0; var r4: Int = 0; var r5: Int = 0;
    var pc: Int = 0

    subscript(idx: Int) -> Int {
        get {
            switch idx {        // ordered via actual usage data
            case 2: return r2; case 5: return r5; case 3: return r3
            case 4: return r4; case 1: return r1; case 0: return r0
            case _: fatalError("no")
            }
        }

        set(val) {
            switch idx {
            case 2: r2 = val; case 5: r5 = val; case 3: r3 = val
            case 4: r4 = val; case 1: r1 = val; case 0: r0 = val
            case _: fatalError("no")
            }
        }
    }
}

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

func parse(path: String) -> (Int, [Inst]) {
    var slot = 0
    let lines = readFileLines(path: path)

    let insts: [Inst] = lines.compactMap { str in
        let words = str.components(separatedBy: " ")

        switch words.count {
        case 2:
            slot = Int(words[1]) ?? 0
            return nil

        case 4:
            return handle_op(words)

        default:
            return nil
        }
    }

    return (slot, insts)
}

func exec(r: Reg, insts: [Inst], slot: Int) -> Reg {
    var r = r // convert let to var... seems hacky
    let start = CFAbsoluteTimeGetCurrent()
    let max = insts.count

    while r.pc < max {
        let pc = r.pc
        let op = insts[pc]

        r[slot] = pc
        op(&r)
        r.pc = r[slot] + 1
    }

    let end = CFAbsoluteTimeGetCurrent() - start
    print("Took \(end) seconds")

    return r
}

func run() {
    setbuf(__stdoutp, nil) // don't buffer output

    let start = CFAbsoluteTimeGetCurrent()
    if CommandLine.arguments.count < 2 {
        for _ in 1...100 {
            CommandLine.arguments.append("/Users/ryan/Work/git/searbsg/advent_of_code/zenspider/2018/19.txt")
        }
    }

    for path in CommandLine.arguments[1...] {
        let (slot, insts) = parse(path: path)

        print(exec(r: Reg(), insts: insts, slot: slot).r0)
    }
    let end = CFAbsoluteTimeGetCurrent() - start
    print("Total took \(end) seconds")
}

let _top: Void = run()
