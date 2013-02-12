class A():
    def __init__(self):
        print(self.__class__.__mro__)
        self.str = "in A's init"
    
    def getStr(self):
        return self.str

class C():
    def __init__(self):
        print(self.__class__.__mro__)
        self.str = "in C's init"


class B(C, A):
   # def __init__(self):
     #   self.strB = "B's val"
    def bb(self):
        self.strNum = 10000
    
    #def getStr(self):
    #    return self.strB


b = B()
var1 = b.getStr()
print(var1)
b.bb()
var2 = b.strNum
print(var2)




