from collections import deque

def parseInput(  ):
    with open("inputExample.txt") as f:
        text = f.read().strip()
    garden = text.splitlines()
    return garden

def getNext( pos, visited):
    x,y = pos
    coords = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

    around = filter ( lambda c : c[0] >= 0 and c[0] < gardenWidth   and
                                 c[1] >= 0 and c[1] < gardenHeight  and
                                 (c[0],c[1]) not in visited         and 
                                 garden[y][x] == garden[c[1]][c[0]], coords )
    return list(around)

def chunk ( start ):  # BFS
    visited = set() 
    queue = deque([(start[0], start[1])])
    visited.add(start)
    count = 0
    while queue:
        x, y = queue.popleft()
        positions = getNext((x,y), visited)
        for dx, dy in positions:
            visited.add((dx, dy))
            queue.append((dx, dy))
    return list(visited)

def makeRegions():
    regions = []
    for y,line in enumerate(garden):
        for x,_ in enumerate(line):
            r = sorted(chunk((x,y)))
            if r not in regions :
                regions.append(r)
    return regions


def perimeter ( region ) :
    per = 0
    for x,y in region:
        for c in [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]:
            if ( c not in region ):
                per += 1
    return per

def corners(

def part1( inp ):
    price = 0
    for chunk in regions:
        price += len(chunk) * perimeter(chunk)
    return price 

def part2( inp ):
    price = 0
    for chunk in regions:
        price += len(chunk) * corners(chunk)
    return price 


garden       = parseInput()
gardenWidth  = len(garden[0])
gardenHeight = len(garden)
regions      = makeRegions()

print( "part 1:", part1(garden))
#print( "part 2:", part2(inp))

