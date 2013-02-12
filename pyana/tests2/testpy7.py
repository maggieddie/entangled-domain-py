class A():
    def __init__(self):
        print('A.__init__')
        super(A, self).__init__()

class B():
    def __init__(self):
        print('B.__init__')
        super(B, self).__init__()

class C(A, B):
    def __init__(self):
        print('C.__init__')
        super(C, self).__init__()

c = C()
