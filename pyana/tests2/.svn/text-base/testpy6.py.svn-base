class Foo():
	def bar(self):
		print("I'am doing Foo.bar()")
	x = 10
	
class Bar(Foo):
	def bar(self): 
		print("I am doing Bar.bar()")
		Foo.bar(self)
	y = 9
	
g = Bar()
Bar.bar(g)
print(g.y)
print(g.x)

