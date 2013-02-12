class A():
    def __init__(self):
        print ("A init")
        mro_a = self.__class__.__mro__
       	print (mro_a)

class B(A):
    def __init__(self):
        print ("B init")
        mro_b =	self.__class__.__mro__
        print (mro_b)
        super(B, self).__init__()

class C(A):
    def __init__(self):
        print ("C init")
        mro_c =	self.__class__.__mro__
        print (mro_c)
        super(C, self).__init__()

class D(B, C):
    def __init__(self):
        print ("D init")
        mro_d = self.__class__.__mro__
        print (mro_d)
        super(D, self).__init__()

x = D()


