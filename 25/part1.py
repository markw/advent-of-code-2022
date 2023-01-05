def snafu_to_decimal(s):
    xs = []
    for c in s:
        match c:
            case "0" | "1" | "2":
                xs.append(int(c))
            case "-":
                xs.append(-1)
            case "=":
                xs.append(-2)

    n, exp = 0, len(xs)-1
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
                n = n - m + 5
            case 4:
                s = '-' + s
                n = n - m + 5

        n //= 5
    return s

# print(snafu_to_decimal("1=-0-2"))
# print(snafu_to_decimal("2=-1=0"))
# print(snafu_to_decimal("1--"))
# print(decimal_to_snafu(1747))
# print(decimal_to_snafu(4890))

input = open("input.txt","r").read().split("\n")
total = 0
for line in input:
    total += snafu_to_decimal(line)
print("Part 1:", decimal_to_snafu(total))
