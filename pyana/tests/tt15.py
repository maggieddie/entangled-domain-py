def b():
  return "hi"

def c():
  return False

def d():
  return 2;
  
def a(n):
   if n == 1:
     b()
     c()
     return "hi"
     
   else:
     d()
     
     return a(1)

y = a(5)

