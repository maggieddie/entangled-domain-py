class CC():
   def __init__(self, x, y):
      self.x = x
      self.y = y
     # return self

   def add(self):
      return self.x + self.y

obj = CC(1,2)
res = obj.add()
print(res)

#def test_fun(a,b):
#   def fun_inside(aa,bb):
#      return ab+bb
#   return fun_inside(a,b)


