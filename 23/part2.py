def is_none_or_empty(a):
    return a == None or len(a) == 0

def first(a):
    return None if is_none_or_empty(a) else a[0]

def second(a):
    return None if is_none_or_empty(a) else a[1]

NW = (-1,-1)
N  = (-1, 0)
NE = (-1, 1)
E  = ( 0, 1)
SE = ( 1, 1)
S  = ( 1, 0)
SW = ( 1,-1)
W  = ( 0,-1)

rules = [(N,NE,NW),
         (S,SE,SW),
         (W,NW,SW),
         (E,NE,SE)]

def add(a,b):
    return(a[0]+b[0], a[1]+b[1])

not_empty = lambda line: len(line) > 0

def parse_grid(filename):
    lines = open(filename,"r").read().split("\n")
    return list(filter(not_empty, lines))

grid = parse_grid("input.txt")

elves = set()
for r in range(len(grid)):
    for c in range(len(grid[0])):
        if grid[r][c] == "#":
           elves.add((r,c))

def rotate(xs):
    xs1 = xs[1:]
    xs1.append(xs[0])
    return xs1

def has_neighbors(e, elves, rules):
    for r in rules:
        for s in r:
            if add(s,e) in elves:
                return True
    return False

def propose_move(e, elves, rules):

    def conflict(moves):
        for m in moves:
            if m in elves:
                return True
        return False

    for r in rules:
        moves = [add(s,e) for s in r]
        if not conflict(moves):
            return (e, moves[0])
    return (e,e)

def propose_moves(elves, rules):
    proposed = set()
    for e in elves:
        if not has_neighbors(e, elves, rules):
            proposed.add((e,e))
        else:
            proposed.add(propose_move(e, elves, rules))
    return proposed

def invalid_dests(moves):
    invalid, seen = set(), set()
    for d in map(second, moves):
        if d in seen:
            invalid.add(d)
        seen.add(d)
    return invalid

def rounds(n, elves, rules):
    if (n % 100) == 0:
        print("round", n)
    proposed = propose_moves(elves, rules)
    invalid = invalid_dests(proposed)
    dests = set()
    for m in proposed:
        if m[1] in invalid:
            dests.add(m[0])
        else:
            dests.add(m[1])
    if dests == elves:
        return n+1
    return rounds(n+1, dests, rotate(rules))

result = rounds(0, elves, rules)
print("Part 2:", result)
