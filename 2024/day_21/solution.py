import sys
from functools import cache
from itertools import permutations

with open(sys.argv[1] if len(sys.argv) > 1 else 'input.txt') as f:
    codes = f.read().splitlines()

numkeypad = ['789',
             '456',
             '123',
             ' 0A']
numkeypad = {key: (x, y) for y, line in enumerate(numkeypad) for x, key in enumerate(line) if key != ' '}

dirkeypad = [' ^A',
             '<v>']
dirkeypad = {key: (x, y) for y, line in enumerate(dirkeypad) for x, key in enumerate(line) if key != ' '}

dirs = {'^': (0, -1), '>': (1, 0), 'v': (0, 1), '<': (-1, 0)}

@cache
def get_presses(sequence, depth=2, dirkey=False, cur=None):
    keypad = dirkeypad if dirkey else numkeypad
    if not sequence:
        return 0
    if not cur:
        cur = keypad['A']

    cx, cy = cur
    px, py = keypad[sequence[0]]
    dx, dy = px-cx, py-cy

    buttons = '>'*dx + '<'*-dx + 'v'*dy + '^'*-dy

    if depth:
        perm_lens = []
        for perm in set(permutations(buttons)):
            cx, cy = cur
            for button in perm:
                dx, dy = dirs[button]
                cx += dx
                cy += dy
                if (cx, cy) not in keypad.values():
                    break
            else:
                perm_lens.append(get_presses(perm+('A',), depth-1, True))
        min_len = min(perm_lens)
    else:
        min_len = len(buttons)+1
    return min_len+get_presses(sequence[1:], depth, dirkey, (px, py))

p1 = 0
p2 = 0
for code in codes:
    codenum = int(code[:-1])
    p1 += codenum*get_presses(code)
    p2 += codenum*get_presses(code, 25)

print(p1)
print(p2)
