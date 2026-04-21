from collections import deque

Neighborhood = dict[int, set[int]]


def is_neighborhood_correct(neighborhood: Neighborhood) -> bool:
    if not isinstance(neighborhood, dict) or len(neighborhood) < 2:
        return False

    vertices = set(neighborhood.keys())
    if not all(isinstance(vertex, int) for vertex in vertices):
        return False

    for vertex, neighbors in neighborhood.items():
        if not isinstance(neighbors, set):
            return False
        for neighbor in neighbors:
            if not isinstance(neighbor, int):
                return False
            if neighbor not in vertices or neighbor == vertex:
                return False

    return _is_connected(neighborhood)


def is_undirected(neighborhood: Neighborhood) -> bool:
    if not is_neighborhood_correct(neighborhood):
        return False

    return all(
        vertex in neighborhood.get(neighbor, set())
        for vertex, neighbors in neighborhood.items()
        for neighbor in neighbors
    )


def is_tree(neighborhood: Neighborhood) -> bool:
    if not is_undirected(neighborhood):
        return False

    edge_count = sum(len(neighbors) for neighbors in neighborhood.values()) // 2
    return edge_count == len(neighborhood) - 1


def _is_connected(neighborhood: Neighborhood) -> bool:
    if not neighborhood:
        return False

    start = next(iter(neighborhood))
    visited = {start}
    queue = deque([start])

    while queue:
        current = queue.popleft()
        for neighbor in neighborhood[current]:
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return visited == set(neighborhood)
