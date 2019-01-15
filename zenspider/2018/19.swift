import Foundation

let PC   = 6
var slot = 0

typealias Reg  = [Int]
typealias Prim = (Int, Int) -> Int
typealias Op   = (inout Reg, Inst) -> Void

struct Inst {
    var op: Op
    var a  = 0
    var b  = 0
    var c  = 0
}

func op_rr(fn : @escaping Prim) -> Op {
    return { r, i in r[i.c] = fn(r[i.a], r[i.b]) }
}

func op_ri(fn : @escaping Prim) -> Op {
    return { r, i in r[i.c] = fn(r[i.a], i.b) }
}

func op_ir(fn : @escaping Prim) -> Op {
    return { r, i in r[i.c] = fn(i.a, r[i.b]) }
}

func op_r() -> Op {
    return { r, i in r[i.c] = r[i.a] }
}

func op_i() -> Op {
    return { r, i in r[i.c] = i.a }
}

func b2i(test: Bool) -> Int {
    return test ? 1 : 0
}

let nop: Op = { r, i in /* do nothing */ }

let gti = { (a: Int, b: Int) in b2i(test: a > b) }
let eqi = { (a: Int, b: Int) in b2i(test: a == b) }

let fns: [String: Op] = [
    "addr": op_rr(fn: +),
    "addi": op_ri(fn: +),
    "mulr": op_rr(fn: *),
    "muli": op_ri(fn: *),
    "banr": op_rr(fn: &),
    "bani": op_ri(fn: &),
    "borr": op_rr(fn: |),
    "bori": op_ri(fn: |),
    "setr": op_r(),
    "seti": op_i(),
    "gtir": op_ir(fn: gti),
    "gtri": op_ri(fn: gti),
    "gtrr": op_rr(fn: gti),
    "eqir": op_ir(fn: eqi),
    "eqri": op_ri(fn: eqi),
    "eqrr": op_rr(fn: eqi),
]

func readFileLines(path: String) -> [String] {
    do {
        return try String(contentsOfFile: path).components(separatedBy: "\n")
    } catch {
        return []
    }
}

func parse(path: String) -> [Inst] {
    let lines = readFileLines(path: path)

    return lines.compactMap { str in
        let words = str.components(separatedBy: " ")

        switch words.count {
        case 2:
            guard let n = Int(words[1]) else {
                return nil
            }
            slot = n
            return nil

        case 4:
            let op = fns[words[0]] ?? nop
            let a  = Int(words[1]) ?? 0
            let b  = Int(words[2]) ?? 0
            let c  = Int(words[3]) ?? 0

            return Inst(op: op, a: a, b: b, c: c)

        default:
            return nil
        }
    }
}

func exec(r : inout Reg, insts: [Inst]) {
    let max = insts.count
    repeat {
        let pc = r[PC]
        let i = insts[pc]

        // print("pc = \(pc), r = \(r)")

        r[slot] = pc
        i.op(&r, i)
        r[PC] = r[slot] + 1
    } while (r[PC] < max)
}

func run() {
    if CommandLine.arguments.count < 2 {
        CommandLine.arguments.append("/Users/ryan/Work/git/searbsg/advent_of_code/zenspider/2018/19.txt")
    }

    for path in CommandLine.arguments[1...] {
        let insts = parse(path: path)

        var r = [0, 0, 0, 0, 0, 0, 0]

        exec(r: &r, insts: insts)

        print(r)
    }
}

let _top: Void = run()
