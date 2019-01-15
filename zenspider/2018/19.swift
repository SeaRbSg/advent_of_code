import Cocoa

typealias Reg  = Array<Int>
typealias Prim = (Int, Int) -> Int
typealias Op   = (inout Reg, Inst) -> Void

struct Inst {
    var op : Op
    var a  = 0
    var b  = 0
    var c  = 0
}

var slot = 0

func op_rr(fn : @escaping Prim) -> Op {
    return { (r : inout Reg, i : Inst) -> Void in
        r[i.c] = fn(r[i.a], r[i.b])
    }
}

func op_ri(fn : @escaping Prim) -> Op {
    return { (r : inout Reg, i : Inst) -> Void in
        r[i.c] = fn(r[i.a], i.b)
    }
}

func op_ir(fn : @escaping Prim) -> Op {
    return { (r : inout Reg, i : Inst) -> Void in
        r[i.c] = fn(i.a, r[i.b])
    }
}

func op_r() -> (inout Reg, Inst) -> Void {
    return { (r : inout Reg, i : Inst) -> Void in
        r[i.c] = r[i.a]
    }
}

func op_i() -> (inout Reg, Inst) -> Void {
    return { (r : inout Reg, i : Inst) -> Void in
        r[i.c] = i.a
    }
}

func b2i(test : Bool) -> Int {
    return test ? 1 : 0
}

let gti = { (a: Int, b: Int) in b2i(test:a > b) }
let eqi = { (a: Int, b: Int) in b2i(test:a == b) }

let fns : [String: Op] = [
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

enum Token {
    case slot(Int)
    case n(Int)
    case s(String)
}

func readFileLines(path: String) -> [String]? {
    do {
        let content = try String.init(contentsOfFile:path)
        return content.components(separatedBy: "\n")
    } catch {
        return nil
    }
}

func parse(path: String) -> [Inst] {
    let lines = readFileLines(path:path)!

    return lines.compactMap { (str : String) -> Inst? in
        let words = str.components(separatedBy: " ")

        if words.count == 2 {
            guard let n = Int(words[1]) else { return nil }
            slot = n
            return nil
        } else {
            let op = fns[words[0]]

            if op != nil {
                let a  = Int(words[1])!
                let b  = Int(words[2])!
                let c  = Int(words[3])!

                return Inst(op: op!, a:a, b:b, c:c)
            } else {
                return nil
            }
        }
    }
}

let PC = 6

func exec(r : inout Reg, insts : Array<Inst>) -> Void {
    let max = insts.count;

    repeat {
        let pc = r[PC]
        let i = insts[pc]

        // print("pc = \(pc), r = \(r)")

        r[slot] = pc
        i.op(&r, i)
        r[PC] = r[slot] + 1
    } while (r[PC] < max)
}

if CommandLine.arguments.count < 2 {
    CommandLine.arguments.append("/Users/ryan/Work/git/searbsg/advent_of_code/zenspider/2018/19.txt")
}

for path in CommandLine.arguments[1...] {
    let insts = parse(path: path)

    var r = [0, 0, 0, 0, 0, 0, 0]

    exec(r:&r, insts:insts)

    print(r)
}
