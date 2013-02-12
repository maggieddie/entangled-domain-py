a = ["b", ["A", "B"]  , "c"]
print(a)
print(a[0])
print(a[1])
print(a[2])
#print(a[3])

print("before deleting....")
len(a)

del a[0]
print(a)




print("after deleting...")
len(a)


print(a[0][0])

for x in a:
    print(x)

a.append("D")
print(a)


