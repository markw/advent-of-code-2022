import types
dirs = types.SimpleNamespace()

not_empty = lambda s: len(s) > 0
lines = list(filter(not_empty, open("input.txt","r").read().split("\n")));
start = (0,lines[0].index("."))

end_row = len(lines)-1
end_col = lines[-1].index(".")
end = (end_row,end_col)
print("start=",start,"end=",end)

first = lambda b: b[0]

dirs.up    = (-1,0)
dirs.down  = (1,0)
dirs.left  = (0,-1)
dirs.right = (0,1)

deltas = [dirs.up, dirs.down, dirs.left, dirs.right, (0,0)]

walls = set()
blizzards = []
for r in range(len(lines)):
    for c in range(len(lines[0])):
        match lines[r][c]:
            case '#':
                walls.add((r,c)) 
            case 'v':
                blizzards.append(((r,c), dirs.down))
            case '>':
                blizzards.append(((r,c), dirs.right))
            case '<':
                blizzards.append(((r,c), dirs.left))
            case '^':
                blizzards.append(((r,c), dirs.up))

def print_valley(blizzards):
    b_locs = map(first,blizzards)
    b_loc_set = set(b_locs)
    for r in range(len(lines)):
        for c in range(len(lines[0])):
            ch = "."
            if (r,c) in walls:
                ch = "#"
            if (r,c) in b_loc_set:
                count=0
                for b in blizzards:
                    if b[0] == (r,c):
                        count += 1
                        match b[1]:
                            case dirs.up:    ch = '^' if count == 1 else str(count)
                            case dirs.down:  ch = 'v' if count == 1 else str(count)
                            case dirs.left:  ch = '<' if count == 1 else str(count)
                            case dirs.right: ch = '>' if count == 1 else str(count)
                
            print(ch,end='')
        print("")

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

valid = lambda r,c,b_locs: r >= 0 and c >= 0 and (r,c) not in walls and (r,c) not in b_locs

paths = [start]
minute = 0
while True:
    minute += 1
    blizzards = list(map(move, blizzards))
    #print_valley(blizzards)
    b_locs = set(map(first,blizzards))
    new_paths = set()
    for p in paths:
        for d in deltas:
            (r,c),_ = next((p,d))
            if valid(r,c,b_locs):
                new_paths.add((r,c))
    if end in new_paths:
        print("minute", minute)
        break
    paths=new_paths
