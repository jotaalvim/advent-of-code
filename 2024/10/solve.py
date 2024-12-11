from collections import deque

def parseInput(  ):
    with open("input.txt") as f:
        text = f.read().strip()
    for y,line in enumerate(text.splitlines()):
        row = []
        for x,n in enumerate(line):
            row.append(int(n))
            if n == '0':
                trailheads.append((x,y))
        hiking.append(row)
    return hiking

def getNext( pos, visited):
    x,y = pos
    coords = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

    around = filter ( lambda c : c[0] >= 0 and c[0] < hikingWidth   and
                                 c[1] >= 0 and c[1] < hikingHeight  and
                                 (c[0],c[1]) not in visited         and 
                                 hiking[y][x] == hiking[c[1]][c[0]]-1, coords )

    return list(around)

def pathBFS ( start ):
    visited = set() 
    queue = deque([(start[0], start[1])])
    visited.add(start)
    count = 0
    while queue:
        x, y = queue.popleft()
        if hiking[y][x] == 9:
            count += 1
        positions = getNext((x,y), visited)
        for dx, dy in positions:
            visited.add((dx, dy))
            queue.append((dx, dy))
    
    return count


def pathDFS(start, path):
    x, y = start
    path.append(start)
    
    if hiking[y][x] == 9:
        path.pop()
        return 1
    
    positions = getNext(start, path)
    found = 0
    for np in positions:
        found += pathDFS(np, path)
    
    path.pop()
    return found

def part1( inp ) :
    paths = 0
    for th in trailheads:
        paths += pathBFS( th )
    return paths

def part2( inp ) :
    paths = 0
    for th in trailheads:
        f = pathDFS(th, [])


        paths += f
    return paths


hiking       = []
trailheads   = []

inp          = parseInput()

hikingWidth  = len(hiking[0])
hikingHeight = len(hiking)

print( "part 1:", part1(inp))
print( "part 2:", part2(inp))
