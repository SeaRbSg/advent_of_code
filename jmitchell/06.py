#!/usr/bin/env python

def chars_at(s, i):
    for m in s:
        yield m[i]

def freq_table(xs):
    d = {}
    for x in xs:
        d[x] = d.get(x, 0) + 1
    return d

def most_common(xs):
    n = 0
    ans = None

    f = freq_table(xs)
    for k, v in f.iteritems():
        if v > n:
            n = v
            ans = k
    return ans

def least_common(xs):
    n = float("inf")
    ans = None

    f = freq_table(xs)
    for k, v in f.iteritems():
        if v < n:
            n = v
            ans = k
    return ans

def correct_errors(s):
    ans = []
    for i in range(len(s[0])):
        ans.append(most_common(chars_at(s, i)))
    return ''.join(ans)

def correct_errors_mod(s):
    ans = []
    for i in range(len(s[0])):
        ans.append(least_common(chars_at(s, i)))
    return ''.join(ans)

with open('06.txt', 'r') as f:
    samples = f.readlines()

    print correct_errors(samples)
    print correct_errors_mod(samples)
