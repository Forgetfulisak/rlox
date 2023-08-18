

def fib(side, n):
    if n > 0:
        print(side)
        print(n)
        fib("left", n-1)
        fib("right", n-2)

fib("start", 4)