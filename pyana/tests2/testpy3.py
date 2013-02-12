class A():
    def __init__(self, x):
        self.x = x

    def getx(self):
        return self.x

   # class B():
    #    def sety(self, y):
     #       self.y = y
      #  def gety(self):
       #     return self.y

    #b_a = B()
    #b_a.sety("y in B")
    #print(b_a.gety())

a = A("x in A")
va = a.getx()
print(va)




    
