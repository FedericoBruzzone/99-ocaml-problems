def generate_combinations(n, k):
    if n < k:
        return []

    if k == 0:
        return [[]]

    combinations = generate_combinations(n - 1, k)
    combinations += [sublist + [n] for sublist in generate_combinations(n - 1, k - 1)]

    return combinations

def generate_combinations_dp(n, k):
    if n < k:
        return []

    dp = [[[] for _ in range(k + 1)] for _ in range(n + 1)]

    for i in range(n + 1):
        dp[i][0] = [[]]

    for i in range(1, n + 1):
        for j in range(1, min(i, k) + 1):
            dp[i][j] = dp[i - 1][j] + [sublist + [i] for sublist in dp[i - 1][j - 1]]

    return dp[n][k]

N = 4
K = 2
combinations = generate_combinations(N, K)
print(combinations)
