def foo():
   return  {"a": 1, 'b': "special", 'c': 3}
   
def bar(): 
   return (foo(), 1)

b = bar()
b1 = b[1]
b2 = b[0]['b']



