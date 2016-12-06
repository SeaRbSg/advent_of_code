#!/usr/bin/env python

import hashlib

def special_hash(s, i):
    m = hashlib.md5()
    m.update(s + str(i))
    d = m.hexdigest()
    if d[:5] == '00000':
        return d
    else:
        return None


def crack_code(s):
    ans = ""
    i = 0
    while len(ans) < 8:
        d = special_hash(s, i)
        if d:
            ans += d[5]
        i += 1
    return ans

# PART ONE

# print crack_code("abc")
puzzle_input = "ffykfhsq"
print crack_code(puzzle_input)


# PART TWO

def crack_positional_code(s):
    ans = [None, None, None, None,
           None, None, None, None]
    i = 0
    while None in ans:
        d = special_hash(s, i)
        if d:
            p = int(d[5], 16)
            if p < len(ans) and ans[p] == None:
                ans[p] = d[6]
        i += 1
    return ''.join(ans)

print crack_positional_code(puzzle_input)
