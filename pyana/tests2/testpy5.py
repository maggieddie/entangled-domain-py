class T():
    a = 2
    
    
class A(T):
    b=5

class B(T):
    a = 100
    
class C(A,B):
    b = 500

c = C()
print(c.a)
