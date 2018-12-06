def bounds(positions):
    xs = map(lambda p: p[0], positions)
    ys = map(lambda p: p[1], positions)
    max_x = max(xs) + 1
    max_y = max(ys) + 1
    return (max_x, max_y)


def distance(pos1, pos2):
    return abs(pos1[0] - pos2[0]) + abs(pos1[1] - pos2[1])


def closest(pos, positions):
    distances = ((i, distance(p, pos)) for i, p in enumerate(positions))
    distances = sorted(distances, key=lambda x: x[1])
    if distances[0][1] == distances[1][1]:
        return 0
    else:
        return distances[0][0] + 1


def on_edge(pos, bounds):
    x_onedge = pos[0] == 0 or pos[0] == bounds[0] - 1
    y_onedge = pos[1] == 0 or pos[1] == bounds[1] - 1
    return x_onedge or y_onedge


def solve1(positions):
    # creating the grid
    (x_size, y_size) = bounds(positions)
    # populate each grid with the index of the closest anchor
    areas = dict()
    skip = {0}
    for x in range(x_size):
        for y in range(y_size):
            min_index = closest((x, y), positions)
            if min_index not in skip:
                if on_edge((x, y), (x_size, y_size)):
                    skip.add(min_index)
                    areas.pop(min_index, None)
                else:
                    areas[min_index] = areas.get(min_index, 0) + 1
    return max(areas.values())


def solve2(positions):
    (x_size, y_size) = bounds(positions)
    count = 0
    for x in range(x_size):
        for y in range(y_size):
            total_dist = sum(distance((x, y), pos) for pos in positions)
            if total_dist < 10000:
                count += 1
    return count


if __name__ == '__main__':
    positions = []
    with open('6.txt', 'r') as fp:
        for line in fp.readlines():
            pos = tuple(map(int, line.split(', ')))
            positions.append(pos)
    print("Solution 1: ", solve1(positions))
    print("Solution 2: ", solve2(positions))
