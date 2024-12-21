from collections import defaultdict

from joblib import Parallel, cpu_count, delayed


def load_input(filename):
    track, start, end = [], None, None

    with open(filename, "rt") as fin:
        for row, line in enumerate(fin):
            track.append(line.strip())
            if "S" in line:
                start = (row, line.index("S"))
            if "E" in line:
                end = (row, line.index("E"))

    return track, start, end


def find_path(track, start, end):
    queue, distance = [start], defaultdict(int)
    distance[start] = 0

    while queue:
        row, col = queue.pop()
        if (row, col) == end:
            break

        if track[row - 1][col] != "#" and (row - 1, col) not in distance:
            queue.append((row - 1, col))
            distance[row - 1, col] = distance[row, col] + 1
        elif track[row + 1][col] != "#" and (row + 1, col) not in distance:
            queue.append((row + 1, col))
            distance[row + 1, col] = distance[row, col] + 1
        elif track[row][col - 1] != "#" and (row, col - 1) not in distance:
            queue.append((row, col - 1))
            distance[row, col - 1] = distance[row, col] + 1
        elif track[row][col + 1] != "#" and (row, col + 1) not in distance:
            queue.append((row, col + 1))
            distance[row, col + 1] = distance[row, col] + 1

    return distance


def find_cheats(track, race_path):
    cheats = 0

    for (row, col), dist in list(race_path.items()):
        if track[row][col - 1] == "#" and dist + 2 < race_path[row, col - 2]:
            cheats += race_path[row, col - 2] - dist - 2 >= 100
        if track[row][col + 1] == "#" and dist + 2 < race_path[row, col + 2]:
            cheats += race_path[row, col + 2] - dist - 2 >= 100
        if track[row - 1][col] == "#" and dist + 2 < race_path[row - 2, col]:
            cheats += race_path[row - 2, col] - dist - 2 >= 100
        if track[row + 1][col] == "#" and dist + 2 < race_path[row + 2, col]:
            cheats += race_path[row + 2, col] - dist - 2 >= 100

    return cheats


def find_long_cheats(race_path):

    def process_i(i):
        def get_dist(p1, p2):
            return abs(p1[0] - p2[0]) + abs(p1[1] - p2[1])

        local_cheats = 0
        for j in range(i + 100 + 1, len(race_path)):
            if get_dist(race_path[i], race_path[j]) <= 20:
                local_cheats += (j - i - get_dist(race_path[i], race_path[j])) >= 100
        return local_cheats

    return sum(
        Parallel(n_jobs=cpu_count(True), return_as="generator")(
            delayed(process_i)(i) for i in range(len(race_path) - 1 - 100)
        )
    )


def main():
    track, start, end = load_input("input.txt")
    race_path = find_path(track, start, end)

    print(f"Part 1: {find_cheats(track, race_path.copy())}")
    print(f"Part 2: {find_long_cheats(list(race_path.keys()))}")


if __name__ == "__main__":
    main()
