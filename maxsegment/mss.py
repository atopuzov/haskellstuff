def mss(a_list):
    e = 0
    sums = []
    for j in range(len(a_list)):
        i = -1 - j
        e = max(0, e + a_list[i])
        sums.append(e)
    return max(sums)

print mss([31, -41, 59, 26, -53, 58, 97, -93, -23, 84])
