import Foundation             // String(contentsOfFile:) & .components

public
typealias Interactfn = (String) -> String

public
func inputFile(_ name: String) -> String {
    return "/Users/ryan/Work/git/searbsg/advent_of_code/zenspider/2018/" + name
}

public
func readStdin() -> String {
    var result: [String] = []
    while let s = readLine() {
        result.append(s)
    }
    return result.joined(separator: "\n")
}

public
func readFile(_ path: String) -> String {
    if let s = try? String(contentsOfFile: path) {
        return s
    } else {
        return ""
    }
}

public
func readFileLines(_ path: String) -> [String] {
    return readFile(path).lines()
}

public
func interact(_ fns: [Interactfn]) {
    let input = readStdin()
    for fn in fns {
        print(fn(input))
    }
}

public
func xinteract(_ fns: [Interactfn], _ input: String) -> [String] {
    return fns.map { fn in fn(input) }
}

extension String {
    public func lines() -> [String] {
        return self.components(separatedBy: .newlines)
    }
}

extension Array where Element == String {
    public func mapInts() -> [Int] {
        return self.compactMap { s in Int(s) } // TODO: make this more dynamic
    }
}

extension Array where Element == Int {
    public func sumInt() -> Int {
        return reduce(0, +)
    }
}

public
struct Cycle<Element> : IteratorProtocol, Sequence {
    let ary: [Element]
    let max: Int
    var idx: Int

    init(_ xs: [Element]) {
        (ary, max, idx) = (xs, xs.count, 0)
    }

    public mutating func next() -> Element? {
        let idx = self.idx

        self.idx = (idx + 1) % max

        return ary[idx]
    }
}

extension Array {
    public func cycle() -> Cycle<Element> {
        return Cycle(self)
    }
}

extension Collection {
    public func groupBy<K: Hashable>(_ mapfn: (Element) -> K) -> [K: [Element]] {
        return Dictionary(grouping: self, by: mapfn)
    }

    public func countBy<K: Hashable>(_ mapfn: (Element) -> K) -> [K: Int] { // TODO: benchmark
        var result: [K: Int] = [:]

        for v in self {
            let k = mapfn(v)
            if let v = result[k] {
                result[k] = v + 1
            } else {
                result[k] = 1
            }
        }

        return result
    }

    public func countBy2<K: Hashable>(mapfn: (Element) -> K) -> [K: Int] { // TODO: benchmark
        return self.groupBy(mapfn).mapValues { v in v.count }
    }
}

// TODO: understand and port

@_transparent
@discardableResult
public func measure(label: String? = nil, _ block: @escaping () -> Void) -> TimeInterval {
    let start = Date()
    block()
    let end = Date()
    if let label = label {
        print(label, "▿")
        print("\tExecution time: \(end.timeIntervalSince(start))s\n")
    } else {
        print("Execution time: \(end.timeIntervalSince(start))s\n")
    }
    return end.timeIntervalSince(start)
}
