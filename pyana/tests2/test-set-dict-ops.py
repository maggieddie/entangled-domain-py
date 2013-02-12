set1 = {1, 2, 3}
lst1 = {10, 20, 30}
print({100*x for x in set1})
print({-100 *x for x in lst1})

print("dict: ")
print({x:y for x in set1 for y in lst1})
print("a set of tuples")
print({(x, y) for x in set1 for y in lst1})
print("set of tuples with conditions: ")

print({(x,y) for x in set1 if x >=2 for y in lst1})







