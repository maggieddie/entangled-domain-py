class Foo():
    def setx(self, x):
        self.x = x
    def bar(self):
        return self.x

f = Foo()
f.setx(5)
var1 = f.bar()  #5
print(var1)

Foo.setx(f, 500)
var2 = Foo.bar(f) #500
print(var2)

#del f.x
#f.bar()

Foo.y = 10
g = Foo()
var3 = g.y #10
print(var3)

g.y = 9
print("after setting g.y to 9")
var4 = g.y  #9
print(var4)

print("f.y should be the same to the Foo.y")
print("f.y = ")
var5 = f.y #10
print(var5)

print("Foo.y = ")
var6 = Foo.y  #10
print(var6)



