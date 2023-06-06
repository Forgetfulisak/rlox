c = 0

for a in range(0, 30):
    b1 = a % 3
    b2 = a % 5
    if b1 == 0 and b2 == 0:
        c = 15
        # print("fizzbuzz")
    elif b1 == 0:
        c = 3
        # print("fizz")
    elif b2 == 0:
        c = 5
        # print("buzz")
    else:
        c = a
        # print(a)
