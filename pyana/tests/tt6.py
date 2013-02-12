def f():
 print("called f")
 return 1

def g():
 print("called g")
 return 0

a = [[10,20],[30,40],[50,60]]

b = a[f()][g()]

#c = b + a[g()][f()]

