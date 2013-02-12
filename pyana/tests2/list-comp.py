vec = [2,4,6]
print([3*x for x in vec])
#[6,12, 18]

print([[x, x**2] for x in vec])
#[[2, 4], [4, 16], [6, 36]]

freshfruit = [' banana','  loganberry ', 'passion fruit  ']
print([weapon for weapon in freshfruit])
#['banana', 'loganberry', 'passion fruit']


print([3*x for x in vec if x > 3])
#[12, 18]

print([3*x for x in vec if x < 2])
#[]

print([(x, x**2) for x in vec])
#[(2, 4), (4, 16), (6, 36)]




