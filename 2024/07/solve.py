def parseInput(  ):
    with open("input.txt") as f:
        text = f.read().strip()
    final = []
    for line in text.splitlines():
        [n,ln] = line.split(":")
        final.append( (int(n), [int(x) for x in ln.split()] ) )
    return final
    
def combination( factor, base ):
    size = len(factor) - 1
    if   base == 2: return [f"{comb:0{size}b}"  for comb in range(2** size)]
    elif base == 3: return [f"{to_base3(comb).zfill(size)}" for comb in range(3**size)]


def myEval(lf, lb):
    acc  = lf[0 ]
    rest = lf[1:]
    for e in lb:
        if   e == "0": acc += rest [0]
        elif e == "1": acc *= rest [0]
        elif e == "2": acc = int( str(acc) + str(rest[0]) )
        rest = rest[1:]
    return acc

def to_base3(n: int) -> str:
    if n == 0: return "0"
    if n <  0: return "-" + to_base3(-n)
    if n <  3: return str(n)
    return to_base3(n // 3) + str(n % 3)

def solve( inp,base ):
    result = 0
    for n,lf in inp:
        for o in combination(lf,base):
            if myEval (lf,o) == n:
                result += n 
                break
    return result

inp     = parseInput()
print( "part 1:", solve(inp,2))
print( "part 2:", solve(inp,3))
