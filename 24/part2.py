not_empty = lambda s: len(s) > 0
lines = list(filter(not_empty, open("input.txt","r").read().split("\n")));
start = (0,lines[0].index("."))

end_row = len(lines)-1
end_col = lines[-1].index(".")
end = (end_row,end_col)
print("start=",start,"end=",end)

first = lambda b: b[0]

up    = (-1,0)
down  = (1,0)
left  = (0,-1)
right = (0,1)

deltas = [up, down, left, right, (0,0)]

walls = set()
blizzards = []
for r in range(len(lines)):
    for c in range(len(lines[0])):
        match lines[r][c]:
            case '#':
                walls.add((r,c)) 
            case 'v':
                blizzards.append(((r,c), down))
            case '>':
                blizzards.append(((r,c), right))
            case '<':
                blizzards.append(((r,c), left))
            case '^':
                blizzards.append(((r,c), up))

def next(b):
    rc, d = b
    r = rc[0] + d[0]
    c = rc[1] + d[1]
    return ((r,c), d)

def reverse(blizzard):
    rc, (dr,dc) = blizzard
    return (rc, (-dr, -dc))

def move(blizzard):
    rc, d = next(blizzard)
    if rc in walls:
        tmp = reverse(blizzard)
        while not tmp[0] in walls:
            tmp = next(tmp)
        return next(reverse(tmp))

    return (rc, d)

max_row = len(lines) - 1
max_col = len(lines[0]) - 1

valid_row = lambda r: r >= 0 and r <= max_row
valid_col = lambda r: r >= 0 and r <= max_col

def valid(r,c,b_locs):
    rc = (r,c)
    return valid_row(r) and valid_col(c) and rc not in walls and rc not in b_locs

def solve(start,end,blizzards):
    paths = [start]
    minute = 0
    while True:
        minute += 1
        blizzards = list(map(move, blizzards))
        b_locs = set(map(first,blizzards))
        new_paths = set()
        for p in paths:
            for d in deltas:
                (r,c),_ = next((p,d))
                if valid(r,c,b_locs):
                    new_paths.add((r,c))
        if end in new_paths:
            return (minute,blizzards)
        paths=new_paths

total = 0
for pair in [(start,end),(end,start),(start,end)]:
    src,dest = pair
    minutes,blizzards = solve(src,dest,blizzards)
    print(minutes)
    total += minutes

print("total:", total)
