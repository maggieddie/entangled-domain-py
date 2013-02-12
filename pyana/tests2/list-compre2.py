#more fancy behavior
vec1 = [2, 4, 6]
vec2 = [4, 3, -9]
print([x*y for x in vec1 for y in vec2])
#[8, 6, -18, 16, 12, -36, 24, 18, -54]
print([x+y for x in vec1 for y in vec2 if x+y > 6], vec2)
#[6, 5, -7, 8, 7, -5, 10, 9, -3]
#print([vec1[i]*vec2[i] for i in range(len(vec1))])
#[8, 12, -54]

#print([str(round(355/113, i)) for i in range(1, 6)])
#['3.1', '3.14', '3.142', '3.1416', '3.14159']

#nested list comprehension
mat = [[1, 2, 3],[4, 5, 6],[7, 8, 9]]
print([[row[i] for row in mat] for i in [0, 1, 2]])

#for i in [0, 1, 2]:
#    for row in mat:
#        print(row[i], end="")
#    print()
