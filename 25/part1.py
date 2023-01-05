def snafu_to_decimal(s):
    xs = []
    for c in s:
        match c:
            case "0":
                xs.append(0)
            case "1":
                xs.append(1)
            case "2":
                xs.append(2)
            case "-":
                xs.append(-1)
            case "=":
                xs.append(-2)

    exp = len(xs)-1
    n = 0
    for x in xs:
       n += (x * (5 ** exp))
       exp -= 1
    return n

def decimal_to_snafu(n):
    s = ''
    while n > 0:
        m = n % 5
        match m:
            case 0 | 1 | 2:
                s = str(m) + s
                n -= m
            case 3:
                s = '=' + s
                n = n - 3 + 5
            case 4:
                s = '-' + s
                n = n - 4 + 5

        n = n // 5
    return s


# print(snafu_to_decimal("1=-0-2"))
# print(snafu_to_decimal("2=-1=0"))
# print(snafu_to_decimal("1--"))
# print(decimal_to_snafu(1747))
# print(decimal_to_snafu(4890))

lines = open("input.txt","r").read().split("\n")
nums = map(snafu_to_decimal,lines)
total = 0
for n in nums:
    total += n
print("Part 1:", decimal_to_snafu(total))
