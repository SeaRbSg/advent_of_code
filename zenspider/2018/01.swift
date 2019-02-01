#!/usr/bin/env swift -I .

import Foundation
import helpers

func parse(_ input: String) -> [Int] {
    return input.lines().mapInts()
}

func problem1(input: String) -> String {
    return parse(input).reduce(0, +).description
}

func problem2(input: String) -> String {
    var freq = 0
    var seen: Set<Int> = []

    for n in parse(input).cycle() { // using for as `break` doesn't work in `reduce`
        freq += n
        if seen.contains(freq) {
            break
        }
        seen.insert(freq)
    }

    return freq.description
}

func main() {
    interact([problem1, problem2])
}

main()
